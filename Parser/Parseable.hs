module Parser.Parseable ( Parseable
                        ) where

import IL
import Text.ParserCombinators.Parsec

-- | Represents a parser for any frontend language.
class Parseable p where
    parse :: String                     -- ^ The name of the module being processed.
          -> p                          -- ^ The raw source to process.
          -> Either ParseError Module   -- ^ The parsed module, or the error that occurred.
