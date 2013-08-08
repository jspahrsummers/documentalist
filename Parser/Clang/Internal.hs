module Parser.Clang.Internal where

import Control.Applicative
import Data.IORef
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Parser.Clang.FFI as FFI

newtype Index = Index (ForeignPtr ())
data TranslationUnit = TranslationUnit Index (ForeignPtr ())
data Cursor = Cursor TranslationUnit (ForeignPtr ())

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

getComment :: Cursor -> IO String
getComment (Cursor _ ptr) =
    withForeignPtr ptr $ \cxCursor ->
        FFI.getRawCommentText cxCursor >>= peekCString

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
