module Text.Documentalist.SourceParser.Clang ( SourceFile
                                             ) where

import Control.Applicative
import Control.Monad
import Data.Map.Strict as Map
import Data.Maybe
import Foreign.ForeignPtr
import System.IO
import Text.Documentalist.SourceParser
import Text.Documentalist.SourceParser.Clang.Declaration
import Text.Documentalist.SourceParser.Clang.Internal

data SourceFile = SourceFile
    { filePath :: FilePath
    , translationUnit :: TranslationUnit
    }

instance Show SourceFile where
    show = show . filePath

instance SourcePackage SourceFile where
    parse src = do
        cursors <- getCursor (translationUnit src) >>= getAllChildren
        comments <- mapM getComment cursors
        print $ catMaybes comments

        let mod = Module (filePath src) $ DeclMap Map.empty
        return $ Package "" [mod]

-- | Creates a Clang 'SourceFile' from a file on disk.
newSourceFile :: FilePath -> IO SourceFile
newSourceFile path = do
    tu <- newIndex >>= newTranslationUnit path
    return SourceFile { translationUnit = tu, filePath = path }
