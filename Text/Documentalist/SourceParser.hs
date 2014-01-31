module Text.Documentalist.SourceParser ( module Text.Documentalist.Types.Comment
                                       , module Text.Documentalist.Types.Package
                                       , SourceParser(..)
                                       ) where

import Control.Monad.IO.Class
import Text.Documentalist.Types.Comment
import Text.Documentalist.Types.Package

-- | Extracts comments from a specific source language.
class MonadIO p => SourceParser p where
    -- | Parses a package into a language-independent form.
    --
    --   Any errors will be indicated with a thrown 'Exception'.
    parse :: FilePath -> [String] -> p (Package (Maybe Comment))
