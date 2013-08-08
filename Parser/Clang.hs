module Parser.Clang ( SourceFile(..)
                    ) where

import Control.Monad
import Data.Maybe
import Parser.Clang.Internal
import Parser.Parseable
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

newtype SourceFile = SourceFile String
    deriving (Eq, Show)

instance Parseable SourceFile where
    parse (SourceFile path) = do
        ind <- newIndex
        tu <- newTranslationUnit ind path
        cursors <- getCursor tu >>= getAllChildren
        comments <- mapM getComment cursors

        print $ catMaybes comments
        return $ Left $ newErrorUnknown $ initialPos path
