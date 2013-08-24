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
data SourceFile = SourceFile
    { filePath :: FilePath
    , translationUnit :: TranslationUnit
    }

instance Show SourceFile where
    show = show . filePath

instance SourcePackage SourceFile where
    parse src =
        let tu = getCursor $ translationUnit src
            (decls, declMap) = runWriter $ walkFromCursor tu
            mod = Module (filePath src) declMap decls
        in return $ Package "" [mod]

-- | A 'Writer' for accumulating 'DeclMap' entries while walking the AST.
type Walker = Writer (DeclMap Comment)

-- | Traverses the AST, beginning with the given cursor.
walkFromCursor :: Cursor -> Walker [Declaration]
walkFromCursor c =
    let declCursors = descendantDecls c
    in catMaybes <$> mapM parseDecl declCursors

-- | Finds all declaration cursors descendant from the given cursor.
descendantDecls :: Cursor -> [Cursor]
descendantDecls c =
    visitDescendants c $ \desc ->
        if isDeclaration desc
            then (True, continue)
            else (False, recurse)

-- | Parses the declaration at a cursor and adds it to the 'DeclMap'.
parseDecl :: Cursor -> Walker (Maybe Declaration)
parseDecl c =
    let k = cursorKind c
    
        parseDecl' :: Walker (Maybe Declaration)
        parseDecl'
            | k == typedefDecl =
                return $ Just $ TypeAlias (Identifier $ getCursorSpelling c) (Type "foobar")

            | k == objcInterfaceDecl =
                let super = map getCursorSpelling $ childrenOfKind c objcSuperclassRef
                in do
                    decls <- catMaybes <$> mapM parseDecl (descendantDecls c)
                    return $ Just $ Class (Identifier $ getCursorSpelling c) (map Type super) decls

            | k == objcCategoryDecl = do
                decls <- catMaybes <$> mapM parseDecl (descendantDecls c)
                return $ Just $ Mixin (Identifier $ getCursorSpelling c) (Type "") decls

            | k == objcPropertyDecl =
                return $ Just $ Property (Identifier $ getCursorSpelling c) Nothing

            | k == objcInstanceMethodDecl =
                return $ Just $ InstanceMethod (Identifier $ getCursorSpelling c) [] []

            | k == objcClassMethodDecl =
                return $ Just $ ClassMethod (Identifier $ getCursorSpelling c) [] []

            | otherwise = return Nothing
    in do
        mdecl <- parseDecl'

        case mdecl of
            (Just decl) -> tell $ DeclMap $ Map.singleton decl $ fmap Comment $ getComment c
            _ -> return ()

        return mdecl

-- | Creates a Clang 'SourceFile' from a file on disk.
newSourceFile :: FilePath -> IO SourceFile
newSourceFile path = do
    tu <- newIndex >>= newTranslationUnit path
    return SourceFile { translationUnit = tu, filePath = path }
