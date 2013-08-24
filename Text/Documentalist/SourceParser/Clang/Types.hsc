module Text.Documentalist.SourceParser.Clang.Types where

import Foreign.C

#include <clang-c/Index.h>

newtype CursorKind = CursorKind CInt
    deriving (Eq, Show)

#{enum CursorKind, CursorKind
    , structDecl = CXCursor_StructDecl
    , unionDecl = CXCursor_UnionDecl
    , enumDecl = CXCursor_EnumDecl
    , fieldDecl = CXCursor_FieldDecl
    , enumConstantDecl = CXCursor_EnumConstantDecl
    , functionDecl = CXCursor_FunctionDecl
    , varDecl = CXCursor_VarDecl
    , parmDecl = CXCursor_ParmDecl
    , objcInterfaceDecl = CXCursor_ObjCInterfaceDecl
    , objcCategoryDecl = CXCursor_ObjCCategoryDecl
    , objcProtocolDecl = CXCursor_ObjCProtocolDecl
    , objcPropertyDecl = CXCursor_ObjCPropertyDecl
    , objcIvarDecl = CXCursor_ObjCIvarDecl
    , objcInstanceMethodDecl = CXCursor_ObjCInstanceMethodDecl
    , objcClassMethodDecl = CXCursor_ObjCClassMethodDecl
    , typedefDecl = CXCursor_TypedefDecl
    , objcSuperclassRef = CXCursor_ObjCSuperClassRef
    , typeRef = CXCursor_TypeRef
    , ibActionAttr = CXCursor_IBActionAttr
    , ibOutletAttr = CXCursor_IBOutletAttr
    , macroDefinition = CXCursor_MacroDefinition
    }

newtype CursorVisitResult = CursorVisitResult CInt
    deriving (Eq, Show)

#{enum CursorVisitResult, CursorVisitResult
    , break = CXChildVisit_Break
    , continue = CXChildVisit_Continue
    , recurse = CXChildVisit_Recurse
    }
