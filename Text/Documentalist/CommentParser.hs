{-# LANGUAGE DeriveDataTypeable #-}
module Text.Documentalist.CommentParser ( module Text.Documentalist.Types.Comment
                                        , module Text.Documentalist.Types.DocBlock
                                        , module Text.Documentalist.Types.Package
                                        , CommentParseException(..)
                                        , CommentParser(..)
                                        ) where

import Control.Exception
import Control.Monad.Error.Class
import Data.Typeable
import Text.Documentalist.Types.Comment
import Text.Documentalist.Types.DocBlock
import Text.Documentalist.Types.Package

-- | An exception that occurred during comment parsing.
data CommentParseException = CommentParseException
    { file :: Maybe FilePath
    , line :: Maybe Integer
    , message :: String
    } deriving (Eq, Show, Typeable)

instance Error CommentParseException where
    strMsg s = CommentParseException { file = Nothing, line = Nothing, message = s }

instance Exception CommentParseException

-- | Parses a specific 'Comment' syntax.
class CommentParser p where
    -- | Parses the comments in the given package into 'DocBlock's.
    parseDocs :: p -> Package (Maybe Comment) -> Package (Either CommentParseException DocBlock)
