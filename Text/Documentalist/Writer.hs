{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Documentalist.Writer ( module Text.Documentalist.Types.DocBlock
                                 , module Text.Documentalist.Types.Package
                                 , Writer(..)
                                 ) where

import Control.Monad.IO.Class
import Text.Documentalist.Types.DocBlock
import Text.Documentalist.Types.Package

-- | Generates formatted output from 'DocBlock's.
class MonadIO w => Writer w where
    -- | Writes formatted documentation to a destination determined by the specific 'Writer' used.
    --
    --   Any errors will be indicated with a thrown 'Exception'.
    write :: Package (Maybe DocBlock) -> w ()
