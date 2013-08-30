module Text.Documentalist.CommentParser ( module Types
                                        , CommentParseError(..)
                                        , CommentParser(..)
                                        ) where

import Control.Monad.Error.Class
import Text.Documentalist.Types.Comment as Types
import Text.Documentalist.Types.DocBlock as Types
import Text.Documentalist.Types.Package as Types

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
