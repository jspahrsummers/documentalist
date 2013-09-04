module Text.Documentalist.CommentParser.TomDoc ( TomDocParser(..)
                                               ) where

import Text.Documentalist.CommentParser
import Text.Documentalist.CommentParser.TomDoc.Parser

-- | Parses comments in tomdoc.org format.
data TomDocParser = TomDocParser

instance CommentParser TomDocParser where
    parseDocs _ (Package pkg mods) = Right $ Package pkg $ map parseModule mods

-- | Parses all the comments in a 'Module'.
parseModule :: Module (Maybe Comment) -> Module (Maybe DocBlock)
parseModule (Module mod decls) = Module mod $ map parseDecl decls

-- | Parses the comment of a 'Declaration'.
parseDecl :: Declaration (Maybe Comment) -> Declaration (Maybe DocBlock)
parseDecl d = fmap (>>= \(Comment str) -> parseComment d str) d