{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser.Clang ( SourceFile
                                             , newSourceFile
                                             ) where

import Data.Char
import Data.Maybe
import Text.Documentalist.SourceParser
import Text.Documentalist.SourceParser.Clang.Internal
import Text.Documentalist.SourceParser.Clang.Types

-- | A file in a source language supported by Clang.
newtype SourceFile = SourceFile { filePath :: FilePath }

instance Show SourceFile where
    show = show . filePath

instance SourcePackage SourceFile where
    parse src = do
        tu <- newIndex >>= newTranslationUnit (filePath src)

        let mod = Module (filePath src) $ walkFromCursor $ getCursor tu
        return $ Package "" [mod]

-- | Traverses the AST, beginning with the given cursor.
walkFromCursor :: Cursor -> [Declaration (Maybe Comment)]
walkFromCursor c =
    mapMaybe parseDecl $ descendantDecls c

-- | Retrieves the comment associated with the cursor, and strips out comment markers if needed.
strippedComment :: Cursor -> Maybe Comment
strippedComment c =
    let pred :: Char -> Bool
        pred '/' = True
        pred '*' = True
        pred '!' = True
        pred c = isSpace c

        stripLines :: [String] -> [String]
        stripLines = map $ dropWhile pred

        transform :: String -> Comment
        transform = Comment . init . unlines . stripLines . lines
    in fmap transform $ getComment c

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
        Just $ TypeAlias comment (Identifier $ getCursorSpelling c) (Type "foobar")

    | k == objcInterfaceDecl =
        let super = map getCursorSpelling $ childrenOfKind c objcSuperclassRef
            decls = mapMaybe parseDecl $ descendantDecls c
        in Just $ Class comment (Identifier $ getCursorSpelling c) (map Type super) decls

    | k == objcCategoryDecl =
        let decls = mapMaybe parseDecl $ descendantDecls c
        in Just $ Mixin comment (Identifier $ getCursorSpelling c) (Type "") decls

    | k == objcPropertyDecl =
        Just $ Property comment (Identifier $ getCursorSpelling c) Nothing

    | k == objcInstanceMethodDecl =
        Just $ InstanceMethod comment (Identifier $ getCursorSpelling c) [] []

    | k == objcClassMethodDecl =
        Just $ ClassMethod comment (Identifier $ getCursorSpelling c) [] []

    | otherwise = Nothing
    where k = cursorKind c
          comment = strippedComment c

-- | Creates a Clang 'SourceFile' from a file on disk.
newSourceFile :: FilePath -> SourceFile
newSourceFile = SourceFile
