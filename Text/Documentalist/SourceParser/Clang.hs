{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser.Clang ( SourceFile
                                             , newSourceFile
                                             ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map
import Data.Maybe
import System.IO
import Text.Documentalist.SourceParser
import Text.Documentalist.SourceParser.Clang.Internal
import Text.Documentalist.SourceParser.Clang.Types

-- | A file in a source language supported by Clang.
data SourceFile = SourceFile { filePath :: FilePath }

instance Show SourceFile where
    show = show . filePath

instance SourcePackage SourceFile where
    parse src = do
        tux <- newIndex >>= newTranslationUnit (filePath src)
        let tu = getCursor $ tux
            mod = Module (filePath src) $ walkFromCursor tu
        return $ Package "" [mod]

-- | Traverses the AST, beginning with the given cursor.
walkFromCursor :: Cursor -> [Declaration (Maybe Comment)]
walkFromCursor c =
    mapMaybe parseDecl $ descendantDecls c

-- | Finds all declaration cursors descendant from the given cursor.
descendantDecls :: Cursor -> [Cursor]
descendantDecls c =
    visitDescendants c $ \desc ->
        if isDeclaration desc
            then (True, continue)
            else (False, recurse)

-- | Attempts to parse the declaration at a cursor.
parseDecl :: Cursor -> Maybe (Declaration (Maybe Comment))
parseDecl c
    | k == typedefDecl =
        Just $ DecLeaf comment (Identifier $ getCursorSpelling c) (TypeAlias (Type "foobar"))

    | k == objcInterfaceDecl =
        let super = map getCursorSpelling $ childrenOfKind c objcSuperclassRef
            decls = mapMaybe parseDecl $ descendantDecls c
        in Just $ DecNode comment (Identifier $ getCursorSpelling c) (Class (map Type super)) decls

    | k == objcCategoryDecl =
        let decls = mapMaybe parseDecl $ descendantDecls c
        in Just $ DecNode comment (Identifier $ getCursorSpelling c) (Mixin (Type "")) decls

    | k == objcPropertyDecl =
        Just $ DecLeaf comment (Identifier $ getCursorSpelling c) (Property Nothing)

    | k == objcInstanceMethodDecl =
        Just $ DecNode comment (Identifier $ getCursorSpelling c) (InstanceMethod []) []

    | k == objcClassMethodDecl =
        Just $ DecNode comment (Identifier $ getCursorSpelling c) (ClassMethod []) []

    | otherwise = Nothing
    where k = cursorKind c
          comment = fmap Comment $ getComment c

-- | Creates a Clang 'SourceFile' from a file on disk.
newSourceFile :: FilePath -> SourceFile
newSourceFile = SourceFile