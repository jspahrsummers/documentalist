module Text.Documentalist.PrettyPrint (PrettyPrint(..)) where

class PrettyPrint a where
    pprint :: a -> String
    pprintList :: [a] -> String -> String
    pprintList = showList__ (\x -> (++) (pprint x))

-- from GHC's Text.Show. (c) The University of Glasgow 2001
-- License: BSD-style (see the file libraries/base/LICENSE)
showList__ :: (a -> String -> String) -> [a] -> String -> String
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
    where
      showl []     = ']' : s
      showl (y:ys) = ',' : showx y (showl ys)
