module Text.Documentalist.Parser.Parseable ( Parseable(..)
                                           ) where

import Text.Documentalist.IL
import Text.ParserCombinators.Parsec.Error

-- | Represents a parser for any frontend language.
class Parseable p where
    parse :: p
          -> IO (Either ParseError Module)  -- ^ The parsed module, or the error that occurred.