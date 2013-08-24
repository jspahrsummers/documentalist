{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Documentalist.CommentParser where

import Control.Monad.Error.Class
import Text.Documentalist.SourceParser

-- | A documentation block for a declaration.
data DocBlock = DocBlock
    { summary :: Paragraph
    , description :: [Paragraph]
    , parameters :: [DocParam]
    , example :: Maybe Code
    , result :: Maybe Result
    } deriving (Eq, Show)

-- | A newline-delimited section of text.
data Paragraph = TextParagraph [Span]
               | CodeBlock Code
               | QuotedText Paragraph
               deriving (Eq, Show)

-- | Represents a portion of text in a documentation string.
data Span = PlainText String
          | Reference (Declaration ())
          | WebLink String
          | InlineImage String
          | InlineCode Code
          | EmphasizedText Span
          | StrongText Span
          | UnderlinedText Span
          deriving (Eq, Show)

-- | A block or span of code in the source language.
newtype Code = Code String
    deriving (Eq, Show)

-- | One of the parameters to a Function, or one of the values in an Enumeration.
data DocParam = DocParam (Declaration ()) [Span]
    deriving (Eq, Show)

-- | Describes the value that a Function returns to its caller.
newtype Result = Result [Span]
    deriving (Eq, Show)

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
