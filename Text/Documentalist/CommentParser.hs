module Text.Documentalist.CommentParser ( module Text.Documentalist.Types.Comment
                                        , module Text.Documentalist.Types.DocBlock
                                        , module Text.Documentalist.Types.Package
                                        , CommentParseError(..)
                                        , CommentParser(..)
                                        ) where

import Control.Monad.Error.Class
import Text.Documentalist.Types.Comment
import Text.Documentalist.Types.DocBlock
import Text.Documentalist.Types.Package

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
