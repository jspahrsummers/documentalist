{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Documentalist.Writer ( module Text.Documentalist.Types.DocBlock
                                 , module Text.Documentalist.Types.Package
                                 , Writer(..)
                                 ) where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Text.Documentalist.Types.DocBlock
import Text.Documentalist.Types.Package

-- | A monad capable of generating output from 'DocBlock's.
class (Error e, MonadIO w) => Writer e w where
    -- | Writes formatted documentation.
    --
    --   The destination of the output is determined by the specific 'Writer' used.
    write :: Package (Maybe DocBlock) -> w (Either e ())
