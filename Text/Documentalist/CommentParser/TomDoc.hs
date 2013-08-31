module Text.Documentalist.CommentParser.TomDoc ( TomDocParser(..)
                                               ) where

import Data.Functor
import Data.List
import Data.List.Split
import Text.Documentalist.CommentParser

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
parseDecl :: Declaration (Maybe Comment) -> Declaration EDocBlock
parseDecl d =
    let parseComment :: Comment -> EDocBlock
        parseComment (Comment str) =
            let paras = splitOn "\n\n" str

                isResult :: String -> Bool
                isResult = ("Returns" `isPrefixOf`)

                isParams :: String -> Bool
                isParams = any isParam . take 1 . lines

                isExampleHeading :: String -> Bool
                isExampleHeading "Example" = True
                isExampleHeading "Example:" = True
                isExampleHeading "Examples" = True
                isExampleHeading "Examples:" = True
                isExampleHeading _ = False

                extract :: (String -> Bool) -> [String] -> (Maybe String, [String])
                extract _ [] = (Nothing, [])
                extract f xs =
                    let (pref, rem) = break f xs
                    in if null rem
                        then (Nothing, xs)
                        else (Just $ head rem, pref ++ tail rem)

                parseComment' :: DocBlock
                parseComment' =
                    let (res, paras') = extract isResult paras
                        (params, paras'') = extract isParams paras'
                        (body, examples) = break isExampleHeading paras''
                    in DocBlock { summary = parseParagraph $ head $ body ++ [""]
                                , description = map parseParagraph $ filter (not . null) $ drop 1 body
                                , parameters = maybe [] (parseParams d) params
                                , example = if null examples then Nothing else Just $ Code $ intercalate "\n\n" $ drop 1 examples
                                , result = fmap (Result . parseSpans) res
                                }
            in if null paras || null (head paras)
                then Left undefined
                else Right parseComment'
    in fmap (maybe (Left $ CommentParseException Nothing Nothing "Comment not found") parseComment) d

parseParagraph :: String -> Paragraph
parseParagraph = TextParagraph . parseSpans

parseParams :: Declaration t -> String -> [DocParam]
parseParams decl str =
    let groupParams :: [String] -> [String]
        groupParams [] = []
        groupParams (x : xs) =
            -- Group the lines associated with each parameter.
            let (ext, rem) = break isParam xs
                result = unwords (x : ext)
            in result : groupParams rem
    in map (parseParam decl) (groupParams $ lines str)

parseParam :: Declaration t -> String -> DocParam
-- TODO: Pass in a real DocBlock here.
parseParam decl str = DocParam (Nothing <$ decl) (parseSpans str)

parseSpans :: String -> [Span]
parseSpans str = [PlainText str]
