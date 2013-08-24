{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Documentalist.CommentParser where

import Control.Monad.Error.Class
import Text.Documentalist.SourceParser

-- | A documentation block for a declaration.
data DocBlock t = DocBlock
    { summary :: (Paragraph t)
    , description :: [Paragraph t]
    , parameters :: [Parameter t]
    , example :: Maybe Code
    , result :: Maybe (Result t)
    }

-- | A newline-delimited section of text.
data Paragraph t = TextParagraph [Span t]
                 | CodeBlock Code
                 | QuotedText (Paragraph t)


-- | Represents a portion of text in a documentation string.
data Span t = PlainText String
            | Reference (Declaration t)
            | WebLink String
            | InlineImage String
            | InlineCode Code
            | EmphasizedText (Span t)
            | StrongText (Span t)
            | UnderlinedText (Span t)


-- | A block or span of code in the source language.
newtype Code = Code String
    deriving (Eq, Show)

-- | One of the parameters to a Function, or one of the values in an Enumeration.
data Parameter t = Parameter (Declaration t) [Span t]


-- | Describes the value that a Function returns to its caller.
newtype Result t = Result [Span t]


-- | A monad capable of parsing a specific 'Comment' syntax.
class MonadError e p => CommentParser e p where
    -- | Parses the comments in the given package into 'DocBlock's.
    --
    --   Any errors will be indicated using @throwError@.
    parse :: Package Comment -> p (Package (DocBlock t))
