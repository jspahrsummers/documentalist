{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser.Clang ( SourceFile
                                             , newSourceFile
                                             ) where

import Data.Char
import Data.List
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
    let isCommentMarker :: Char -> Bool
        isCommentMarker '/' = True
        isCommentMarker '*' = True
        isCommentMarker '!' = True
        isCommentMarker _ = False

        stripLine :: String -> String
        stripLine str =
            let str' = dropWhile isSpace str
            in if "/*" `isPrefixOf` str' || "//" `isPrefixOf` str' || "*" `isPrefixOf` str'
                then dropWhile isSpace $ dropWhile isCommentMarker $ drop 2 str'
                else str'

        transform :: String -> Comment
        transform = Comment . init . unlines . map stripLine . lines
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
    | k == objcInterfaceDecl =
        Just $ DecNode comment (Identifier $ getCursorSpelling c) (Class $ map Type $ super objcSuperclassRef) decls

    | k == objcProtocolDecl =
        Just $ DecNode comment (Identifier $ getCursorSpelling c) (Interface $ map Type $ super objcProtocolRef) decls

    | k == objcCategoryDecl =
        let types = super objcClassRef
        in if null types
            then Nothing
            else Just $ DecNode comment (Identifier $ getCursorSpelling c) (Mixin $ Type $ head types) decls

    | k == objcInstanceMethodDecl =
        Just $ DecNode comment (Identifier $ '-' : getCursorSpelling c) (InstanceMethod []) decls

    | k == objcClassMethodDecl =
        Just $ DecNode comment (Identifier $ '+' : getCursorSpelling c) (ClassMethod []) decls

    | k == functionDecl =
        Just $ DecNode comment (Identifier $ getCursorSpelling c) (Function []) decls

    | k == enumDecl =
        Just $ DecNode comment (Identifier $ getCursorSpelling c) (Enumeration Nothing) decls

    | k == objcPropertyDecl =
        Just $ DecLeaf comment (Identifier $ getCursorSpelling c) (Property Nothing)

    | k == enumConstantDecl || k == varDecl =
        Just $ DecLeaf comment (Identifier $ getCursorSpelling c) (Constant Nothing)

    | k == parmDecl =
        Just $ DecLeaf comment (Identifier $ getCursorSpelling c) (Parameter Nothing)

    | k == typedefDecl =
        Just $ DecLeaf comment (Identifier $ getCursorSpelling c) (TypeAlias (Type "foobar"))

    | otherwise = Nothing
    where k = cursorKind c
          comment = strippedComment c
          decls = mapMaybe parseDecl $ descendantDecls c
          super t = map getCursorSpelling $ childrenOfKind c t

-- | Creates a Clang 'SourceFile' from a file on disk.
newSourceFile :: FilePath -> SourceFile
newSourceFile = SourceFile
