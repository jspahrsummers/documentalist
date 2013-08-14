module Text.Documentalist.SourceParser.Clang ( SourceFile(..)
                                             ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Foreign.ForeignPtr
import Text.Documentalist.SourceParser
import Text.Documentalist.SourceParser.Clang.Declaration
import qualified Text.Documentalist.SourceParser.Clang.FFI as FFI
import Text.Documentalist.SourceParser.Clang.Internal

newtype SourceFile = SourceFile String
    deriving (Eq, Show)

cursorInfo :: Cursor -> IO (Maybe (FFI.CXCursorKind, String))
cursorInfo c@(Cursor _ cursorPtr) = do
    kind <- withForeignPtr cursorPtr $ \cxCursor ->
        FFI.getCursorKind cxCursor

    str <- sourceStringAtCursor c
    return $ maybe Nothing (Just . (,) kind) str

instance SourcePackage SourceFile where
    parse (SourceFile path) = do
        ind <- newIndex
        tu <- newTranslationUnit ind path

        cursors <- getCursor tu >>= getAllChildren
        infos <- catMaybes <$> mapM cursorInfo cursors
        print infos

        return $ Package "" [] 
