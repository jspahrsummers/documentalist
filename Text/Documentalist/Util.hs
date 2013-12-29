{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.Util where

import Data.List
import Text.Documentalist.PrettyPrint

-- | Pretty-prints a list of items.
pprintFormattedList :: PrettyPrint t => [t] -> String
pprintFormattedList xs =
    let showIndented :: PrettyPrint t => t -> String
        showIndented x = init $ unlines $ map ((:) '\t') $ lines $ pprint x
    in "[\n" ++ intercalate ",\n" (map showIndented xs) ++ "\n]"
