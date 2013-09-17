module Text.Documentalist.CommentParser.TomDoc ( TomDocParser(..)
                                               ) where

import Text.Documentalist.CommentParser
import Text.Documentalist.CommentParser.TomDoc.Parser

-- | Parses comments in tomdoc.org format.
data TomDocParser = TomDocParser

-- | Alias for convenience
type EDocBlock = Either CommentParseException DocBlock

instance CommentParser TomDocParser where
    parseDocs _ (Package pkg mods) = Package pkg $ map parseModule mods

-- | Parses all the comments in a 'Module'.
parseModule :: Module (Maybe Comment) -> Module EDocBlock
parseModule (Module mod decls) = Module mod $ parseDecls decls

-- | Parses all the comments in a list of 'Declaration's.
parseDecls :: [Declaration (Maybe Comment)] -> [Declaration EDocBlock]
parseDecls = map parseDecl

-- | Determines whether a string contains a parameter declaration.
isParam :: String -> Bool
isParam = ("- " `isInfixOf`)

-- | Parses the comment of a 'Declaration'.
parseDecl :: Declaration (Maybe Comment) -> Declaration (Maybe DocBlock)
parseDecl d = (>>= \(Comment str) -> parseComment d str) `fmap` d
