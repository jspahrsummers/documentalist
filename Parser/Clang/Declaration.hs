module Parser.Clang.Declaration ( Declaration(..)
                                , isDeclaration
                                , toDeclaration
                                ) where

import Data.Maybe
import Parser.Clang.Internal

data Declaration = Declaration
    { sourceString :: String
    , comment :: Maybe String
    } deriving Eq

toDeclaration :: Cursor -> IO (Maybe Declaration)
toDeclaration cursor = do
    b <- isDeclaration cursor
    src <- sourceStringAtCursor cursor

    if not b || isNothing src
        then return Nothing
        else getComment cursor >>= return . Just . Declaration (fromJust src)

instance Show Declaration where
    show d = "Declaration {\n\tcomment: " ++ fromMaybe "" (comment d) ++ "\n\tcode: " ++ sourceString d ++ "\n}"
