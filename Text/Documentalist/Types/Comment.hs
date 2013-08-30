module Text.Documentalist.Types.Comment ( Comment(..)
                                        ) where

-- | The meaningful body of a comment in the source language.
--
--   This string should be preprocessed to remove markers, like -- in Haskell or // in C.
newtype Comment = Comment String
    deriving Eq

instance Show Comment where
    show (Comment str) = "\n{- " ++ str ++ " -}"
