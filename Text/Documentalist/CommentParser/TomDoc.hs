module Text.Documentalist.CommentParser.TomDoc ( TomDocParser(..)
                                               ) where

import Text.Documentalist.CommentParser
import Text.Documentalist.SourceParser

-- | Parses comments in tomdoc.org format.
data TomDocParser = TomDocParser

instance CommentParser TomDocParser where
    parseDocs _ (Package pkg mods) = Right $ Package pkg $ map parseModule mods

parseModule :: Module (Maybe Comment) -> Module (Maybe DocBlock)
parseModule (Module mod decls) = Module mod $ parseDecls decls

parseDecls :: [Declaration (Maybe Comment)] -> [Declaration (Maybe DocBlock)]
parseDecls = map parseDecl

parseDecl :: Declaration (Maybe Comment) -> Declaration (Maybe DocBlock)
parseDecl (Class c i st decls) = Class (parseComment c) i st $ parseDecls decls
parseDecl (Mixin c i t decls) = Mixin (parseComment c) i t $ parseDecls decls
parseDecl (InstanceMethod c i rt decls) = InstanceMethod (parseComment c) i rt $ parseDecls decls
parseDecl (ClassMethod c i rt decls) = ClassMethod (parseComment c) i rt $ parseDecls decls
parseDecl (Property c i t) = Property (parseComment c) i t
parseDecl _ = undefined

parseComment :: Maybe Comment -> Maybe DocBlock
parseComment c =
    let parseComment' :: Comment -> DocBlock
        parseComment' (Comment str) =
            let spans = map PlainText $ filter (not . null) $ lines str
                para = TextParagraph spans
            in DocBlock { summary = para, description = [], parameters = [], example = Nothing, result = Nothing }
    in fmap parseComment' c
