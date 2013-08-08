{-# LANGUAGE ForeignFunctionInterface #-}

module Parser.Clang.FFI where

import Control.Applicative
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

#include <clang-c/Index.h>

type CXCursor = Ptr ()
type CXIndex = Ptr ()
type CXSourceRange = Ptr ()
type CXTokenSet = Ptr ()
type CXTranslationUnit = Ptr ()
type CXUnsavedFile = Ptr ()

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

foreign import ccall unsafe "&clang_disposeIndex"
    p_disposeIndex :: FunPtr (CXIndex -> IO ())

foreign import ccall unsafe "clang_parseTranslationUnit"
    parseTranslationUnit :: CXIndex -> CString -> Ptr CString -> CInt -> CXUnsavedFile -> CUInt -> CXTranslationUnitOption -> IO CXTranslationUnit

foreign import ccall unsafe "&clang_disposeTranslationUnit"
    p_disposeTranslationUnit :: FunPtr (CXTranslationUnit -> IO ())

foreign import ccall unsafe "FFI_wrappers.h doc_getTranslationUnitCursor"
    getTranslationUnitCursor :: CXTranslationUnit -> IO CXCursor

foreign import ccall unsafe "FFI_wrappers.h doc_isDeclaration"
    isDeclaration :: CXCursor -> IO CUInt

foreign import ccall unsafe "FFI_wrappers.h doc_getCursorExtent"
    getCursorExtent :: CXCursor -> IO CXSourceRange

foreign import ccall unsafe "FFI_wrappers.h doc_tokenize"
    tokenize :: CXTranslationUnit -> CXSourceRange -> Ptr CUInt -> IO CXTokenSet

foreign import ccall unsafe "clang_disposeTokens"
    disposeTokens :: CXTranslationUnit -> CXTokenSet -> CUInt -> IO ()

foreign import ccall unsafe "FFI_wrappers.h doc_getTokenSpellings"
    getTokenSpellings :: CXTranslationUnit -> CXTokenSet -> CUInt -> IO (Ptr CString)

foreign import ccall unsafe "FFI_wrappers.h doc_disposeTokenSpellings"
    disposeTokenSpellings :: Ptr CString -> CUInt -> IO ()

foreign import ccall unsafe "&free"
    p_free :: FunPtr (Ptr () -> IO ())
