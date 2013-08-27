{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Documentalist.CommentParser where

import Control.Monad.Error.Class
import Control.Monad.Writer.Lazy
import Data.List
import Data.Maybe
import Text.Documentalist.SourceParser

-- | A documentation block for a declaration.
data DocBlock = DocBlock
    { summary :: Paragraph
    , description :: [Paragraph]
    , parameters :: [DocParam]
    , example :: Maybe Code
    , result :: Maybe Result
    } deriving Eq

instance Show DocBlock where
    show doc =
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
               deriving Eq

instance Show Paragraph where
    show (TextParagraph spans) = show spans
    show (CodeBlock (Code str)) = "```\n" ++ str ++ "\n```"
    show (QuotedText para) = init $ unlines $ map ("> " ++) $ lines $ show para
    showList paras = (++) $ intercalate "\n\n" $ map show paras

-- | Represents a portion of text in a documentation string.
data Span = PlainText String
          | Reference (Declaration ())
          | WebLink String
          | InlineImage String
          | InlineCode Code
          | EmphasizedText Span
          | StrongText Span
          | UnderlinedText Span
          deriving Eq

instance Show Span where
    show (PlainText str) = str
    show (Reference decl) = "ref:" ++ show decl
    show (WebLink url) = "link:" ++ url
    show (InlineImage url) = "img:" ++ url
    show (InlineCode (Code str)) = "`" ++ str ++ "`"
    show (EmphasizedText span) = "_" ++ show span ++ "_"
    show (StrongText span) = "*" ++ show span ++ "*"
    show (UnderlinedText span) = "_" ++ show span ++ "_"
    showList spans = (++) $ unwords $ map show spans

-- | A block or span of code in the source language.
newtype Code = Code String
    deriving (Eq, Show)

-- | One of the parameters to a Function, or one of the values in an Enumeration.
data DocParam = DocParam (Declaration ()) [Span]
    deriving Eq

instance Show DocParam where
    show (DocParam _ spans) = show spans

-- | Describes the value that a Function returns to its caller.
newtype Result = Result [Span]
    deriving Eq

instance Show Result where
    show (Result spans) = show spans

-- | An error that occurred during comment parsing.
data CommentParseError = CommentParseError
    { file :: Maybe FilePath
    , line :: Maybe Integer
    , message :: String
    } deriving (Eq, Show)

instance Error CommentParseError where
    strMsg s = CommentParseError { file = Nothing, line = Nothing, message = s }

-- | Parses a specific 'Comment' syntax.
class CommentParser p where
    -- | Parses the comments in the given package into 'DocBlock's.
    parseDocs :: p -> Package (Maybe Comment) -> Either CommentParseError (Package (Maybe DocBlock))
