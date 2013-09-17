module Text.Documentalist.SourceParser.Clang.FFI where

import Foreign
import Foreign.C
import Foreign.C.Types
import Text.Documentalist.SourceParser.Clang.Types

#include <clang-c/Index.h>

type CXCursor = Ptr ()
type CXFile = Ptr ()
type CXIndex = Ptr ()
type CXSourceLocation = Ptr ()
type CXSourceRange = Ptr ()
type CXTokenSet = Ptr ()
type CXTranslationUnit = Ptr ()
type CXType = Ptr ()
type CXUnsavedFile = Ptr ()

-- | The type of a function that can visit a cursor's children.
type CXVisitor = CXCursor -> CXCursor -> IO CursorVisitResult

newtype CXTranslationUnitOption = CXTranslationUnitOption { unCXTranslationUnitOption :: CUInt }
    deriving (Eq, Show)

#{enum CXTranslationUnitOption, CXTranslationUnitOption
    , noOptions = CXTranslationUnit_None
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
    isDeclaration :: CXCursor -> CUInt

foreign import ccall unsafe "free"
    free :: Ptr () -> IO ()

foreign import ccall unsafe "&free"
    p_free :: FunPtr (Ptr () -> IO ())

foreign import ccall safe "FFI_wrappers.h doc_visitChildren"
    visitChildren :: CXCursor -> FunPtr CXVisitor -> IO CUInt

foreign import ccall unsafe "FFI_wrappers.h doc_dupCursor"
    dupCursor :: CXCursor -> IO CXCursor

foreign import ccall "wrapper"
    mkVisitor :: CXVisitor -> IO (FunPtr CXVisitor)

foreign import ccall unsafe "FFI_wrappers.h doc_getCursorKind"
    getCursorKind :: CXCursor -> CursorKind

foreign import ccall unsafe "FFI_wrappers.h doc_getCursorSpelling"
    getCursorSpelling :: CXCursor -> IO CString

foreign import ccall unsafe "FFI_wrappers.h doc_getCursorType"
    getCursorType :: CXCursor -> IO CXType

foreign import ccall unsafe "FFI_wrappers.h doc_getCursorResultType"
    getCursorResultType :: CXCursor -> IO CXType

foreign import ccall unsafe "FFI_wrappers.h doc_getTypedefDeclUnderlyingType"
    getTypedefDeclUnderlyingType :: CXCursor -> IO CXType

foreign import ccall unsafe "FFI_wrappers.h doc_getEnumDeclIntegerType"
    getEnumDeclIntegerType :: CXCursor -> IO CXType

foreign import ccall unsafe "FFI_wrappers.h doc_getTypeSpelling"
    getTypeSpelling :: CXType -> IO CString

foreign import ccall unsafe "FFI_wrappers.h doc_getCursorFilename"
    getCursorFilename :: CXCursor -> IO CString
