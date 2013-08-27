module Text.Documentalist.CommentParser.TomDoc ( TomDocParser(..)
                                               ) where

import Data.List
import Data.List.Split
import Text.Documentalist.CommentParser
import Text.Documentalist.SourceParser

-- | Parses comments in tomdoc.org format.
data TomDocParser = TomDocParser

instance CommentParser TomDocParser where
    parseDocs _ (Package pkg mods) = Right $ Package pkg $ map parseModule mods

-- | Parses all the comments in a 'Module'.
parseModule :: Module (Maybe Comment) -> Module (Maybe DocBlock)
parseModule (Module mod decls) = Module mod $ parseDecls decls

-- | Parses all the comments in a list of 'Declaration's.
parseDecls :: [Declaration (Maybe Comment)] -> [Declaration (Maybe DocBlock)]
parseDecls = map parseDecl

-- | Parses the comment of a 'Declaration'.
parseDecl :: Declaration (Maybe Comment) -> Declaration (Maybe DocBlock)
parseDecl = fmap (>>= parseComment)

-- | Parses a single comment.
parseComment :: Comment -> Maybe DocBlock
parseComment (Comment str) =
    let paras = splitOn "\n\n" str

        isResult :: String -> Bool
        isResult [] = False
        isResult str = "Returns" `isPrefixOf` str

        isParams :: String -> Bool
        isParams str
            | not (null lns) = "- " `isInfixOf` head lns
            | otherwise = False
            where lns = lines str

        extract :: (String -> Bool) -> [String] -> (Maybe String, [String])
        extract _ [] = (Nothing, [])
        extract f xs =
            let (pref, rem) = span (not . f) xs
            in if null rem
                then (Nothing, xs)
                else (Just $ head rem, pref ++ tail rem)
    
        parseComment' :: DocBlock
        parseComment' =
            let (res, paras') = extract isResult paras
                (_, paras'') = extract isParams paras'
            in DocBlock { summary = parseParagraph $ head $ paras'' ++ [""]
                        , description = map parseParagraph $ filter (not . null) $ tail $ paras'' ++ [""]
                        , parameters = []
                        , example = Nothing
                        , result = fmap (Result . parseSpans) res
                        }
    in if null paras || null (head paras)
        then Nothing
        else Just parseComment'

parseParagraph :: String -> Paragraph
parseParagraph = TextParagraph . parseSpans

parseSpans :: String -> [Span]
parseSpans str = [PlainText str]
