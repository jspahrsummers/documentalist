module Text.Documentalist.Types.DocBlock ( DocBlock(..)
                                         , Paragraph(..)
                                         , Span(..)
                                         , Code(..)
                                         , DocParam(..)
                                         , Result(..)
                                         ) where

import Control.Monad.Writer.Strict
import Data.List
import Data.Maybe
import Text.Documentalist.Types.Package
import Text.Documentalist.PrettyPrint

-- | A documentation block for a declaration.
data DocBlock = DocBlock
    { summary :: Paragraph
    , description :: [Paragraph]
    , parameters :: [DocParam]
    , example :: Maybe Code
    , result :: Maybe Result
    } deriving (Show, Eq)

instance PrettyPrint DocBlock where
    pprint doc =
        let desc = description doc
            params = parameters doc
            ex = example doc
            res = result doc
        
            show' :: Writer [String] ()
            show' = do
                tell ["# Summary: " ++ show (summary doc)]
                unless (null desc) $ tell ["# Description:\n" ++ show desc ++ "\n"]
                unless (null params) $ tell ["# Parameters:\n" ++ show params ++ "\n"]
                when (isJust ex) $ tell ["# Example:\n" ++ show (fromJust ex) ++ "\n"]
                when (isJust res) $ tell ["# Result: " ++ show (fromJust res)]
        in '\n' : intercalate "\n" (execWriter show')

-- | A newline-delimited section of text.
data Paragraph = TextParagraph [Span]
               | CodeBlock Code
               | QuotedText Paragraph
               deriving (Show, Eq)

instance PrettyPrint Paragraph where
    pprint (TextParagraph spans) = show spans
    pprint (CodeBlock (Code str)) = "```\n" ++ str ++ "\n```"
    pprint (QuotedText para) = init $ unlines $ map ("> " ++) $ lines $ show para
    pprintList paras = (++) $ intercalate "\n\n" $ map show paras

-- | Represents a portion of text in a documentation string.
data Span = PlainText String
          | Reference (Declaration (Maybe DocBlock))
          | WebLink String
          | InlineImage String
          | InlineCode Code
          | EmphasizedText Span
          | StrongText Span
          | UnderlinedText Span
          deriving (Show, Eq)

instance PrettyPrint Span where
    pprint (PlainText str) = str
    pprint (Reference decl) = "ref:" ++ show decl
    pprint (WebLink url) = "link:" ++ url
    pprint (InlineImage url) = "img:" ++ url
    pprint (InlineCode (Code str)) = "`" ++ str ++ "`"
    pprint (EmphasizedText span) = "_" ++ show span ++ "_"
    pprint (StrongText span) = "*" ++ show span ++ "*"
    pprint (UnderlinedText span) = "_" ++ show span ++ "_"
    pprintList spans = (++) $ unwords $ map show spans

-- | A block or span of code in the source language.
newtype Code = Code String
    deriving (Show, Eq)

-- | One of the parameters to a Function, or one of the values in an Enumeration.
data DocParam = DocParam (Declaration (Maybe DocBlock)) [Span]
    deriving (Show, Eq)

instance PrettyPrint DocParam where
    pprint (DocParam _ spans) = show spans
    pprintList docs = (++) $ intercalate "\n" $ map show docs

-- | Describes the value that a Function returns to its caller.
newtype Result = Result [Span]
    deriving (Show, Eq)

instance PrettyPrint Result where
    pprint (Result spans) = show spans
