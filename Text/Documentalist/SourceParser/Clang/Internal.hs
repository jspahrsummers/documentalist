{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser.Clang.Internal ( Index
                                                      , newIndex
                                                      , TranslationUnit
                                                      , newTranslationUnit
                                                      , Cursor(..)
                                                      , getCursor
                                                      , getComment
                                                      , isDeclaration
                                                      , cursorKind
                                                      , getCursorSpelling
                                                      , visitDescendants
                                                      , childrenOfKind
                                                      , getCursorType
                                                      , getCursorResultType
                                                      , getUnderlyingType
                                                      , getCursorFilename
                                                      ) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO.Unsafe
import Text.Documentalist.SourceParser.Clang.Types
import qualified Text.Documentalist.SourceParser.Clang.FFI as FFI

-- | Represents a collection of translation units.
newtype Index = Index (ForeignPtr ())

-- | A translation unit, which roughly corresponds to one source file.
data TranslationUnit = TranslationUnit Index (ForeignPtr ())

-- | A movable cursor into a translation unit.
data Cursor = Cursor TranslationUnit (ForeignPtr ())

-- | Builds an array of C strings.
--
--   The result must be freed using 'freeCStringArray' once you are done using it.
toCStringArray
    :: [String]                 -- ^ The input strings.
    -> IO (Ptr CString, Int)    -- ^ The C string array, and number of elements.

toCStringArray strs = do
    ptr <- mapM newCString strs >>= newArray
    return (ptr, length strs)

-- | Frees an array of C strings created with 'toCStringArray'.
freeCStringArray :: (Ptr CString, Int) -> IO ()
freeCStringArray (ptr, len) = do
    cStrs <- peekArray len ptr
    mapM_ free cStrs

-- | Creates an empty index that will automatically exclude declarations from PCH files.
newIndex :: IO Index
newIndex = do
    cxIdx <- FFI.createIndex 1 0
    ptr <- newForeignPtr FFI.p_disposeIndex cxIdx
    return $ Index ptr

-- | Creates a translation unit from a source file, and adds it to an index.
newTranslationUnit :: FilePath -> Index -> IO TranslationUnit
newTranslationUnit file idx@(Index idxPtr) = do
    cxTU <- withCString file $ \cStr ->
        withForeignPtr idxPtr $ \cxIdx -> do
            arr@(argv, argc) <- toCStringArray ["-ObjC", "-nostdinc"]
            cxTU <- FFI.parseTranslationUnit cxIdx cStr argv (fromInteger $ toInteger argc) nullPtr 0 FFI.noOptions

            freeCStringArray arr
            return cxTU
    
    ptr <- newForeignPtr FFI.p_disposeTranslationUnit cxTU
    return $ TranslationUnit idx ptr

-- | Converts a C string into a Haskell string, then frees the input.
wrapCString :: CString -> IO String
wrapCString cStr
    | cStr == nullPtr = error "C string is NULL"
    | otherwise = do
        str <- peekCString cStr
        FFI.free $ castPtr cStr

        return str

-- | Creates a cursor that begins at the level of a translation unit.
getCursor :: TranslationUnit -> Cursor
getCursor tu@(TranslationUnit _ tuPtr) =
    unsafePerformIO $ do
        cxCursor <- withForeignPtr tuPtr $ \cxTU ->
            FFI.getTranslationUnitCursor cxTU

        ptr <- newForeignPtr FFI.p_free cxCursor
        return $ Cursor tu ptr

-- | Gets the comment associated with the declaration at a cursor, if any.
getComment :: Cursor -> Maybe String
getComment cursor@(Cursor _ ptr) =
    let rawComment :: FFI.CXCursor -> IO (Maybe String)
        rawComment cxCursor = do
            cStr <- FFI.getRawCommentText cxCursor
            if cStr == nullPtr
                then return Nothing
                else Just <$> wrapCString cStr
    in if isDeclaration cursor
        then unsafePerformIO $ withForeignPtr ptr $ \cxCursor -> rawComment cxCursor
        else Nothing

-- | Gets the name for a cursor.
getCursorSpelling :: Cursor -> String
getCursorSpelling (Cursor _ ptr) =
    unsafePerformIO $ withForeignPtr ptr $ FFI.getCursorSpelling >=> wrapCString

-- | Visits the descendants of a cursor.
visitDescendants
    :: Cursor                                   -- ^ The cursor to visit the descendants of.
    -> (Cursor -> (Bool, CursorVisitResult))    -- ^ A function to run over each descendant, starting with immediate children.
                                                --   The 'Bool' is whether to include this cursor in the result.
                                                --   The 'CursorVisitResult' determines how enumeration should continue.
    -> [Cursor]                                 -- ^ A list of cursors for which 'True' was returned.

visitDescendants (Cursor tu ptr) f =
    unsafePerformIO $ do
        cursors <- newIORef []

        let visitor :: FFI.CXVisitor
            visitor cxCursor _ = do
                cxCursor' <- FFI.dupCursor cxCursor
                cursor <- Cursor tu <$> newForeignPtr FFI.p_free cxCursor'

                let (b, v) = f cursor
                when b $ modifyIORef cursors (++ [cursor])
                return v

        dynVisitor <- FFI.mkVisitor visitor
        withForeignPtr ptr $ \cxCursor ->
            FFI.visitChildren cxCursor dynVisitor

        readIORef cursors

-- | Returns whether a cursor refers to a declaration.
isDeclaration :: Cursor -> Bool
isDeclaration (Cursor _ cursorPtr) =
    unsafePerformIO $ withForeignPtr cursorPtr $ \cxCursor -> 
        return $ FFI.isDeclaration cxCursor /= 0

-- | Returns the kind of the specified cursor.
cursorKind :: Cursor -> CursorKind
cursorKind (Cursor _ cursorPtr) =
    unsafePerformIO $ withForeignPtr cursorPtr $ return . FFI.getCursorKind

-- | Returns the immediate children of the given cursor that are of the given kind.
childrenOfKind :: Cursor -> CursorKind -> [Cursor]
childrenOfKind cursor kind =
    visitDescendants cursor $ \child -> (cursorKind child == kind, continue)

-- | Applies a function to get a cursor's type, then obtains its spelling.
getTypeSpelling :: Cursor -> (FFI.CXCursor -> IO FFI.CXType) -> String
getTypeSpelling (Cursor _ ptr) f =
    unsafePerformIO $ withForeignPtr ptr $ \cxCursor -> do
        t <- f cxCursor
        sp <- FFI.getTypeSpelling t
        FFI.free t

        wrapCString sp

-- | Gets the spelling of a cursor's type.
getCursorType :: Cursor -> String
getCursorType cursor = getTypeSpelling cursor FFI.getCursorType

-- | Gets the spelling of a cursor's result type.
getCursorResultType :: Cursor -> String
getCursorResultType cursor = getTypeSpelling cursor FFI.getCursorResultType

-- | Gets the underlying type for a typedef or enum.
getUnderlyingType :: Cursor -> Maybe String
getUnderlyingType cursor
    | k == typedefDecl =
        Just $ getTypeSpelling cursor FFI.getTypedefDeclUnderlyingType

    | k == enumDecl =
        Just $ getTypeSpelling cursor FFI.getEnumDeclIntegerType

    | otherwise = Nothing
    where k = cursorKind cursor

-- | Gets the full path of the file containing the original location of the given cursor.
getCursorFilename :: Cursor -> Maybe String
getCursorFilename (Cursor _ ptr) =
    unsafePerformIO $ withForeignPtr ptr $ \cxCursor -> do
        filename <- FFI.getCursorFilename cxCursor
        if filename == nullPtr
            then return Nothing
            else Just <$> wrapCString filename
