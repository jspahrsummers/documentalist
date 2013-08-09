module Parser.Clang.Internal ( Index
                             , newIndex
                             , TranslationUnit
                             , newTranslationUnit
                             , Cursor
                             , getCursor
                             , getComment
                             , getAllChildren
                             , tokensAtCursor
                             ) where

import Control.Applicative
import Data.IORef
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Parser.Clang.FFI as FFI

newtype Index = Index (ForeignPtr ())
data TranslationUnit = TranslationUnit Index (ForeignPtr ())
data Cursor = Cursor TranslationUnit (ForeignPtr ())

newtype Token = Token String
    deriving (Eq, Show)

newIndex :: IO Index
newIndex = do
    cxIdx <- FFI.createIndex 0 0
    ptr <- newForeignPtr FFI.p_disposeIndex cxIdx
    return $ Index ptr

newTranslationUnit :: Index -> String -> IO TranslationUnit
newTranslationUnit idx@(Index idxPtr) file = do
    cxTU <- withCString file $ \cStr ->
        withForeignPtr idxPtr $ \cxIdx ->
            FFI.parseTranslationUnit cxIdx cStr nullPtr 0 nullPtr 0 FFI.skipFunctionBodies
    
    ptr <- newForeignPtr FFI.p_disposeTranslationUnit cxTU
    return $ TranslationUnit idx ptr

getCursor :: TranslationUnit -> IO Cursor
getCursor tu@(TranslationUnit _ tuPtr) = do
    cxCursor <- withForeignPtr tuPtr $ \cxTU ->
        FFI.getTranslationUnitCursor cxTU

    ptr <- newForeignPtr FFI.p_free cxCursor
    return $ Cursor tu ptr

getComment :: Cursor -> IO (Maybe String)
getComment (Cursor _ ptr) =
    let rawComment cxCursor = do
            cStr <- FFI.getRawCommentText cxCursor
            if cStr == nullPtr
                then return Nothing
                else Just <$> peekCString cStr
    in withForeignPtr ptr $ \cxCursor -> do
        isDecl <- FFI.isDeclaration cxCursor
        if isDecl == 0
            then return Nothing
            else rawComment cxCursor

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

tokensAtCursor :: Cursor -> IO [Token]
tokensAtCursor (Cursor (TranslationUnit _ tuPtr) cursorPtr) = do
    range <- withForeignPtr cursorPtr $ \cxCursor ->
        FFI.getCursorExtent cxCursor

    strs <- withForeignPtr tuPtr $ \cxTU -> do
        countPtr <- malloc
        tokenSet <- FFI.tokenize cxTU range countPtr

        count <- peek countPtr
        spellings <- FFI.getTokenSpellings cxTU tokenSet count
        FFI.disposeTokens cxTU tokenSet count

        cStrs <- peekArray (fromInteger $ toInteger count) spellings
        FFI.disposeTokenSpellings spellings count

        mapM peekCString cStrs

    FFI.free range
    return $ map Token strs
