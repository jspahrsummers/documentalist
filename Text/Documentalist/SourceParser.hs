module Text.Documentalist.SourceParser ( module Types
                                       , SourcePackage(..)
                                       ) where

import Text.Documentalist.Types.Comment as Types
import Text.Documentalist.Types.Package as Types

-- | Represents a unparsed package in a source language.
class SourcePackage p where
    -- | Parses the package into a language-independent form.
    --
    --   Errors may be indicated with `IOException`s.
    parse :: p -> IO (Package (Maybe Comment))
