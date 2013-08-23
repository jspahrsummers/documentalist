{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser.Clang.Internal ( Index
                                                      , newIndex
                                                      , TranslationUnit
                                                      , newTranslationUnit
                                                      , Cursor(..)
                                                      , getCursor
                                                      , getComment
                                                      , sourceStringAtCursor
                                                      , tokensAtCursor
                                                      , isDeclaration
                                                      , cursorKind
                                                      , getCursorSpelling
                                                      , visitDescendants
                                                      , childrenOfKind
                                                      ) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO
import System.IO.Unsafe
import Text.Documentalist.SourceParser.Clang.Types
import qualified Text.Documentalist.SourceParser.Clang.FFI as FFI

-- | Represents a collection of translation units.
newtype Index = Index (ForeignPtr ())

-- | A translation unit, which roughly corresponds to one source file.
data TranslationUnit = TranslationUnit Index (ForeignPtr ())

-- | A movable cursor into a translation unit.
data Cursor = Cursor TranslationUnit (ForeignPtr ())

-- | A string from the source file that represents a token.
newtype Token = Token String
    deriving (Eq, Show)

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
wrapCString cStr = do
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

-- | Reads between two source locations in a file.
readFileInRange
    :: (Integer, Integer)   -- ^ The 1-indexed line and column of the first character to read.
    -> (Integer, Integer)   -- ^ The 1-indexed line and column of the last character to read.
    -> FilePath             -- ^ The file to read from.
    -> IO String

readFileInRange (startLn, startCol) (endLn, endCol) file =
    withFile file ReadMode $ \fd -> do
        let readToLn :: Integer -> IO String
            readToLn 0 = error "Cannot skip to line 0"
            readToLn 1 = return ""
            readToLn ln = do
                s <- hGetLine fd
                t <- readToLn $ ln - 1
                return $ s ++ t

            readToCol :: Integer -> IO String
            readToCol 0 = error "Cannot skip to column 0"
            readToCol 1 = return ""
            readToCol col = do
                c <- hGetChar fd
                s <- readToCol $ col - 1
                return $ c : s

        readToLn startLn
        readToCol startCol

        str <- if startLn == endLn
                then readToCol $ endCol - startCol
                else liftM2 (++) (readToLn $ endLn - startLn) (readToCol endCol)

        -- The end column is inclusive, so read one more character.
        c <- hGetChar fd
        return $ str ++ [c]

-- | Gets the string of source code associated with a cursor, if any.
sourceStringAtCursor :: Cursor -> IO (Maybe String)
sourceStringAtCursor (Cursor _ cursorPtr) =
    let sourceStringBetweenRanges :: FFI.CXSourceRange -> FFI.CXSourceRange -> IO (Maybe String)
        sourceStringBetweenRanges start end = do
            startLnPtr <- malloc
            startColPtr <- malloc
            file <- FFI.getExpansionLocation start startLnPtr startColPtr nullPtr

            startLn <- toInteger <$> peek startLnPtr
            startCol <- toInteger <$> peek startColPtr
            free startLnPtr
            free startColPtr

            endLnPtr <- malloc
            endColPtr <- malloc
            FFI.getExpansionLocation end endLnPtr endColPtr nullPtr

            endLn <- toInteger <$> peek endLnPtr
            endCol <- toInteger <$> peek endColPtr
            free endLnPtr
            free endColPtr

            filename <- FFI.getFileName file
            FFI.free file

            if filename == nullPtr
                then return Nothing
                else do
                    str <- wrapCString filename
                    Just <$> readFileInRange (startLn, startCol) (endLn, endCol) str
    in do
        ex <- withForeignPtr cursorPtr $ \cxCursor ->
            FFI.getCursorExtent cxCursor

        start <- FFI.getRangeStart ex
        end <- FFI.getRangeEnd ex
        FFI.free ex

        str <- if FFI.isNullRange start == 0 && FFI.isNullRange end == 0
                then sourceStringBetweenRanges start end
                else return Nothing

        FFI.free start
        FFI.free end
        return str

-- | Creates tokens for the source code associated with a cursor, if any.
tokensAtCursor :: Cursor -> [Token]
tokensAtCursor (Cursor (TranslationUnit _ tuPtr) cursorPtr) =
    unsafePerformIO $ do
        range <- withForeignPtr cursorPtr $ \cxCursor ->
            FFI.getCursorExtent cxCursor

        strs <- withForeignPtr tuPtr $ \cxTU -> do
            countPtr <- malloc
            tokenSet <- FFI.tokenize cxTU range countPtr

            count <- peek countPtr
            free countPtr

            spellings <- FFI.getTokenSpellings cxTU tokenSet count
            FFI.disposeTokens cxTU tokenSet count

            cStrs <- peekArray (fromInteger $ toInteger count) spellings
            FFI.disposeTokenSpellings spellings count

            mapM wrapCString cStrs

        FFI.free range
        return $ map Token strs

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
