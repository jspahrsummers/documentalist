module Text.Documentalist.SourceParser ( module Text.Documentalist.Types.Comment
                                       , module Text.Documentalist.Types.Package
                                       , SourcePackage(..)
                                       ) where

import Text.Documentalist.Types.Comment
import Text.Documentalist.Types.Package

-- | Represents a unparsed package in a source language.
class SourcePackage p where
    -- | Parses the package into a language-independent form.
    --
    --   Errors may be indicated with `IOException`s.
    parse :: p -> IO (Package (Maybe Comment))
