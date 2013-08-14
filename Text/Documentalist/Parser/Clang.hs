module Text.Documentalist.Parser.Clang ( SourceFile(..)
                                       ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Foreign.ForeignPtr
import Text.Documentalist.Parser.Clang.Declaration
import qualified Text.Documentalist.Parser.Clang.FFI as FFI
import Text.Documentalist.Parser.Clang.Internal
import Text.Documentalist.Parser.Parseable
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

newtype SourceFile = SourceFile String
    deriving (Eq, Show)

cursorInfo :: Cursor -> IO (Maybe (FFI.CXCursorKind, String))
cursorInfo c@(Cursor _ cursorPtr) = do
    kind <- withForeignPtr cursorPtr $ \cxCursor ->
        FFI.getCursorKind cxCursor

    str <- sourceStringAtCursor c
    return $ maybe Nothing (Just . (,) kind) str

instance Parseable SourceFile where
    parse (SourceFile path) = do
        ind <- newIndex
        tu <- newTranslationUnit ind path

        cursors <- getCursor tu >>= getAllChildren
        infos <- catMaybes <$> mapM cursorInfo cursors
        print infos

        return $ Left $ newErrorUnknown $ initialPos path
