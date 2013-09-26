module Text.Documentalist.CommentParser.TomDoc.Parser where

import Data.Functor

import Text.Documentalist.CommentParser
import Text.Parsec
import Text.Parsec.String
import Data.List
import Data.List.Split
import Data.Foldable (toList)
import Data.Maybe
import Control.Monad.Error.Class

-- | Maybe parse a comment into a DocBlock
parseComment :: Declaration (Maybe Comment) -> Either CommentParseException DocBlock
parseComment d' =
    let m = head $ toList d'
        Just (Comment commentStr) = m
        paras = splitOn "\n\n" commentStr

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
                        , parameters = maybe [] (parseParams d') params
                        , example = if null examples then Nothing else Just $ Code $ intercalate "\n\n" $ drop 1 examples
                        , result = fmap (Result . parseSpans) res
                        }
    in if isNothing m || null paras || null (head paras)
        then Left $ strMsg "Unable to parse comment"
        else Right parseComment'
        
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
parseParagraph x | "> " `isPrefixOf` x = QuotedText $ parseParagraph $ dequote x 
                 | "```" `isPrefixOf` x = CodeBlock $ Code $ x
                 | otherwise = TextParagraph $ parseSpans x
    where
      -- TODO: Removes 1 level of qutoes
      dequote :: String -> String
      dequote _ = ""

-- | Parse a string of Markdown into spans
parseSpans :: String -> [Span]
parseSpans str = case (parse (manyTill (pRef <|> pLink <|> pImage <|> pCode <|> pStrong <|> pUnderline <|> pEm <|> pStrike <|> (fmap (PlainText . return) anyChar)) eof) "" str) of
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
      
      pRef :: Parser Span
      pRef = fail "unimplemented"
      
      pLink :: Parser Span
      pLink = try $ do
        char '['
        _ <- manyTill anyChar (char ']') -- Name
        char '('
        url <- manyTill anyChar (char ')')
        return $ WebLink url
      
      pImage :: Parser Span
      pImage = try $ do
        char '!'
        char '['
        _ <- manyTill anyChar (char ']') -- Name
        char '('
        url <- manyTill anyChar (char ')')
        return $ InlineImage url
        
      pCode :: Parser Span
      pCode = do
        code <- sides (char '`')
        return $ InlineCode $ Code code
      
      pStrong :: Parser Span
      pStrong = do
        text <- sides (char '*' >> char '*')
        return $ StrongText $ PlainText text
      
      pUnderline :: Parser Span
      pUnderline = do
        text <- sides (char '_' >> char '_')
        return $ UnderlinedText $ PlainText text
      
      -- em must occur after strong and underline
      pEm :: Parser Span
      pEm = do
        -- XXX: This is incorrect. This will match on _a*
        text <- sides (char '_') <|> sides (char '*')
        return $ EmphasizedText $ PlainText text
      
      pStrike :: Parser Span
      pStrike = do
        text <- sides (char '~' >> char '~')
        return $ StrikethroughText $ PlainText text
      
      -- helper
      sides x = try (x >> manyTill alphaNum x)
