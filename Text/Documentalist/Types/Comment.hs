module Text.Documentalist.Types.Comment ( Comment(..)
                                        ) where

import Text.Documentalist.PrettyPrint

-- | The meaningful body of a comment in the source language.
--
--   This string should be preprocessed to remove markers, like -- in Haskell or // in C.
newtype Comment = Comment String
    deriving (Show, Eq)

instance PrettyPrint Comment where
    pprint (Comment str) = "\n{- " ++ str ++ " -}"
