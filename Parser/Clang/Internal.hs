module Parser.Clang.Internal ( Index
                             , newIndex
                             , TranslationUnit
                             , newTranslationUnit
                             , Cursor
                             , getCursor
                             , getComment
                             , getAllChildren
                             , sourceStringAtCursor
                             , tokensAtCursor
                             , isDeclaration
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
import qualified Parser.Clang.FFI as FFI
import System.IO

newtype Index = Index (ForeignPtr ())
data TranslationUnit = TranslationUnit Index (ForeignPtr ())
data Cursor = Cursor TranslationUnit (ForeignPtr ())

newtype Token = Token String
    deriving (Eq, Show)

toCStringArray :: [String] -> IO (Ptr CString, Int)
toCStringArray strs = do
    ptr <- mapM newCString strs >>= newArray
    return (ptr, length strs)

freeCStringArray :: (Ptr CString, Int) -> IO ()
freeCStringArray (ptr, len) = do
    cStrs <- peekArray len ptr
    mapM_ free cStrs

newIndex :: IO Index
newIndex = do
    cxIdx <- FFI.createIndex 1 0
    ptr <- newForeignPtr FFI.p_disposeIndex cxIdx
    return $ Index ptr

newTranslationUnit :: Index -> String -> IO TranslationUnit
newTranslationUnit idx@(Index idxPtr) file = do
    cxTU <- withCString file $ \cStr ->
        withForeignPtr idxPtr $ \cxIdx -> do
            arr@(argv, argc) <- toCStringArray ["-ObjC", "-nostdinc"]
            cxTU <- FFI.parseTranslationUnit cxIdx cStr argv (fromInteger $ toInteger argc) nullPtr 0 FFI.skipFunctionBodies

            freeCStringArray arr
            return cxTU
    
    ptr <- newForeignPtr FFI.p_disposeTranslationUnit cxTU
    return $ TranslationUnit idx ptr

wrapCString :: CString -> IO String
wrapCString cStr = do
    str <- peekCString cStr
    FFI.free $ castPtr cStr

    return str

getCursor :: TranslationUnit -> IO Cursor
getCursor tu@(TranslationUnit _ tuPtr) = do
    cxCursor <- withForeignPtr tuPtr $ \cxTU ->
        FFI.getTranslationUnitCursor cxTU

    ptr <- newForeignPtr FFI.p_free cxCursor
    return $ Cursor tu ptr

getComment :: Cursor -> IO (Maybe String)
getComment cursor@(Cursor _ ptr) =
    let rawComment cxCursor = do
            cStr <- FFI.getRawCommentText cxCursor
            if cStr == nullPtr
                then return Nothing
                else Just <$> wrapCString cStr
    in do
        isDecl <- isDeclaration cursor
        if isDecl
            then withForeignPtr ptr $ \cxCursor -> rawComment cxCursor
            else return Nothing

getAllChildren :: Cursor -> IO [Cursor]
getAllChildren (Cursor tu ptr) = do
    cursors <- newIORef []

    let visitor cxCursor _ = do
            cxCursor' <- FFI.dupCursor cxCursor
            cursor <- Cursor tu <$> newForeignPtr FFI.p_free cxCursor'

            modifyIORef' cursors $ \xs -> xs ++ [cursor]
            return FFI.recurse

    dynVisitor <- FFI.mkVisitor visitor
    withForeignPtr ptr $ \cxCursor ->
        FFI.visitChildren cxCursor dynVisitor

    readIORef cursors

readFileInRange :: (Integer, Integer) -> (Integer, Integer) -> FilePath -> IO String
readFileInRange (startLn, startCol) (endLn, endCol) file =
    withFile file ReadMode $ \fd -> do
        let readToLn 0 = error "Cannot skip to line 0"
            readToLn 1 = return ""
            readToLn ln = do
                s <- hGetLine fd
                t <- readToLn $ ln - 1
                return $ s ++ t

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

sourceStringAtCursor :: Cursor -> IO (Maybe String)
sourceStringAtCursor (Cursor _ cursorPtr) = do
    ex <- withForeignPtr cursorPtr $ \cxCursor ->
        FFI.getCursorExtent cxCursor

    start <- FFI.getRangeStart ex
    end <- FFI.getRangeEnd ex
    FFI.free ex

    startIsNull <- FFI.isNullRange start
    endIsNull <- FFI.isNullRange end

    str <- if startIsNull == 0 && endIsNull == 0
            then sourceStringBetweenRanges start end
            else return Nothing

    FFI.free start
    FFI.free end
    return str

sourceStringBetweenRanges :: FFI.CXSourceRange -> FFI.CXSourceRange -> IO (Maybe String)
sourceStringBetweenRanges start end = do
    startLnPtr <- malloc
    startColPtr <- malloc
    file <- FFI.getFileLocation start startLnPtr startColPtr nullPtr

    startLn <- toInteger <$> peek startLnPtr
    startCol <- toInteger <$> peek startColPtr
    free startLnPtr
    free startColPtr

    endLnPtr <- malloc
    endColPtr <- malloc
    FFI.getFileLocation end endLnPtr endColPtr nullPtr

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

tokensAtCursor :: Cursor -> IO [Token]
tokensAtCursor (Cursor (TranslationUnit _ tuPtr) cursorPtr) = do
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

isDeclaration :: Cursor -> IO Bool
isDeclaration (Cursor _ cursorPtr) =
    withForeignPtr cursorPtr $ \cxCursor -> do
        b <- FFI.isDeclaration cxCursor
        return $ b /= 0
