{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser.Clang ( SourceFile
                                             , newSourceFile
                                             ) where

import Control.Applicative
import Control.Monad
import Data.Map.Strict as Map
import Data.Maybe
import Foreign.ForeignPtr
import System.IO
import Text.Documentalist.SourceParser
import Text.Documentalist.SourceParser.Clang.Internal
import Text.Documentalist.SourceParser.Clang.Types

-- | A file in a source language supported by Clang.
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
        let cmts = Prelude.map Comment $ catMaybes comments -- TODO: remove

        let mod = Module (filePath src) $ DeclMap Map.empty
        
        return (Package "" [mod], cmts)

-- | Creates a 'Declaration' from the information at a cursor.
declFromCursor :: Cursor -> IO (Maybe Declaration)
declFromCursor c =
    let declFromCursor' :: CursorKind -> IO (Maybe Declaration)
        declFromCursor' typedefDecl = do
            str <- fromJust <$> sourceStringAtCursor c
            return $ Just $ TypeAlias (Identifier str) (Type "foobar")

        declFromCursor' _ = return Nothing
    in cursorKind c >>= declFromCursor'

-- | Creates a Clang 'SourceFile' from a file on disk.
newSourceFile :: FilePath -> IO SourceFile
newSourceFile path = do
    tu <- newIndex >>= newTranslationUnit path
    return SourceFile { translationUnit = tu, filePath = path }
