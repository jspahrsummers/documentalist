{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.Util where

import Data.List

-- | Pretty-prints a list of items.
showFormattedList :: Show t => [t] -> String
showFormattedList xs = 
    let showIndented :: Show t => t -> String
        showIndented x = init $ unlines $ map ((:) '\t') $ lines $ show x
    in "[\n" ++ intercalate ",\n" (map showIndented xs) ++ "\n]"
