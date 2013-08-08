{-# LANGUAGE ForeignFunctionInterface #-}

module Parser.Clang.FFI where

import Control.Applicative
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include <clang-c/Index.h>

newtype CXCursor = CXCursor (Ptr ())
newtype CXIndex = CXIndex (Ptr ())
newtype CXSourceRange = CXSourceRange (Ptr ())
newtype CXTokenSet = CXTokenSet (Ptr ())
newtype CXTranslationUnit = CXTranslationUnit (Ptr ())
newtype CXUnsavedFile = CXUnsavedFile (Ptr ())

newtype CXTranslationUnitOption = CXTranslationUnitOption { unCXTranslationUnitOption :: CUInt }
    deriving (Eq, Show)

#{enum CXTranslationUnitOption, CXTranslationUnitOption
    , noOptions = CXTranslationUnit_None
    , skipFunctionBodies = CXTranslationUnit_SkipFunctionBodies
    }

combineOptions :: [CXTranslationUnitOption] -> CXTranslationUnitOption
combineOptions = CXTranslationUnitOption . foldr ((.|.) . unCXTranslationUnitOption) 0

foreign import ccall unsafe "FFI_wrappers.h doc_Cursor_getRawCommentText"
    getRawCommentText :: CXCursor -> IO CString

foreign import ccall unsafe "clang_createIndex"
    createIndex :: CInt -> CInt -> IO CXIndex

foreign import ccall unsafe "clang_disposeIndex"
    disposeIndex :: CXIndex -> IO ()

foreign import ccall unsafe "clang_parseTranslationUnit"
    parseTranslationUnit :: CXIndex -> CString -> Ptr CString -> CInt -> CXUnsavedFile -> CUInt -> CXTranslationUnitOption -> IO CXTranslationUnit

foreign import ccall unsafe "doc_getTranslationUnitCursor"
    getTranslationUnitCursor :: CXTranslationUnit -> IO CXCursor

foreign import ccall unsafe "doc_isDeclaration"
    isDeclaration :: CXCursor -> IO CUInt

foreign import ccall unsafe "doc_getCursorExtent"
    getCursorExtent :: CXCursor -> IO CXSourceRange

foreign import ccall unsafe "doc_tokenize"
    tokenize :: CXTranslationUnit -> CXSourceRange -> Ptr CUInt -> IO CXTokenSet

foreign import ccall unsafe "clang_disposeTokens"
    disposeTokens :: CXTranslationUnit -> CXTokenSet -> CUInt -> IO ()

foreign import ccall unsafe "doc_getTokenSpellings"
    getTokenSpellings :: CXTranslationUnit -> CXTokenSet -> CUInt -> IO (Ptr CString)

foreign import ccall unsafe "doc_disposeTokenSpellings"
    disposeTokenSpellings :: Ptr CString -> CUInt -> IO ()
