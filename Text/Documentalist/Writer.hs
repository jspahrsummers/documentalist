{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Documentalist.Writer where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Text.Documentalist.SourceParser
import Text.Documentalist.CommentParser

-- | A monad capable of generating output from 'DocBlock's.
class (Error e, MonadIO w) => Writer e w where
    -- | Writes formatted documentation.
    --
    --   The destination of the output is determined by the specific 'Writer' used.
    write :: Package DocBlock -> w (Either e ())
