module Text.Documentalist.CommentParser.TomDoc ( TomDocParser(..)
                                               ) where

import Data.Functor
import Data.List
import Data.List.Split
import Text.Documentalist.CommentParser
import Text.Parsec
import Text.Parsec.String

-- | Parses comments in tomdoc.org format.
data TomDocParser = TomDocParser

instance CommentParser TomDocParser where
    parseDocs _ (Package pkg mods) = Right $ Package pkg $ map parseModule mods

-- | Parses all the comments in a 'Module'.
parseModule :: Module (Maybe Comment) -> Module (Maybe DocBlock)
parseModule (Module mod decls) = Module mod $ map parseDecl decls

-- | Parses the comment of a 'Declaration'.
parseDecl :: Declaration (Maybe Comment) -> Declaration (Maybe DocBlock)
parseDecl d =
    let parseComment :: Comment -> Maybe DocBlock
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
                then Nothing
                else Just parseComment'
    in fmap (>>= parseComment) d

-- | Determines whether a string contains a parameter declaration.
isParam :: String -> Bool
isParam = ("- " `isInfixOf`)

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

-- | Make a paragraph from a string
parseParagraph :: String -> Paragraph
parseParagraph x | x `isInfixOf` "> " = QuotedText $ parseParagraph $ dequote x 
                 | x `isInfixOf` "```" = CodeBlock $ Code $ x
                 | otherwise = TextParagraph $ parseSpans x
    where
      -- TODO: Removes 1 level of qutoes
      dequote :: String -> String
      dequote _ = ""

-- | Parse a string of Markdown into spans
parseSpans :: String -> [Span]
parseSpans str = case (parse (manyTill (pRef <|> pLink <|> pImage <|> pCode <|> pEm <|> pStrong <|> pUnderline <|> (fmap (\x -> PlainText [x]) anyChar)) eof) "" str) of
      Left _ -> []
      Right y -> flattenText y
    where
      flattenText :: [Span] -> [Span]
      flattenText xs = foldr flat [] xs
          where
            flat :: Span -> [Span] -> [Span]
            -- if single char
            flat (PlainText [y]) ((PlainText x):xs) = (PlainText (y : x)) : xs
            flat ys xs = ys : xs
      -- pText :: Parser Span
      -- pText = fmap PlainText $ manyTill anyChar eof
      pRef :: Parser Span
      pRef = fail "unimplemented"
      pLink :: Parser Span
      pLink = do
        char '['
        _ <- manyTill anyChar (char ']') -- Name
        char '('
        url <- manyTill anyChar (char ')')
        return $ WebLink url
      pImage :: Parser Span
      pImage = do
        char '!'
        char '['
        _ <- manyTill anyChar (char ']') -- Name
        char '('
        url <- manyTill anyChar (char ')')
        return $ InlineImage url
      pCode :: Parser Span
      pCode = do
        char '`'
        code <- manyTill anyChar (char '`')
        return $ InlineCode $ Code code
      pEm :: Parser Span
      pEm = do
        char '_'
        text <- manyTill anyChar (char '_')
        return $ EmphasizedText $ PlainText text
      pStrong :: Parser Span
      pStrong = do
        char '*'
        text <- manyTill anyChar (char '*')
        return $ StrongText $ PlainText text
      pUnderline :: Parser Span
      pUnderline = do
        char '_' >> char '_'
        text <- manyTill anyChar (char '_' >> char '_')
        return $ UnderlinedText $ PlainText text
