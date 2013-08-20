{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser.Clang ( SourceFile
                                             , newSourceFile
                                             ) where

import Control.Applicative
import Control.Monad
import qualified Data.Map.Strict as Map
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
        cursors <- getCursor (translationUnit src) >>= children True
        declCursors <- filterM isDeclaration cursors

        cmtStrs <- mapM getComment cursors
        decls <- catMaybes <$> mapM declFromCursor cursors

        let cmts = map (fmap Comment) cmtStrs
            declMap = DeclMap $ Map.fromList $ zip decls cmts
            mod = Module (filePath src) declMap
        
        return $ Package "" [mod]

-- | Creates a 'Declaration' from the information at a cursor.
declFromCursor :: Cursor -> IO (Maybe Declaration)
declFromCursor c =
    let declFromCursor' :: CursorKind -> IO (Maybe Declaration)
        declFromCursor' k
            | k == typedefDecl = do
                str <- getCursorSpelling c
                return $ Just $ TypeAlias (Identifier str) (Type "foobar")

            | k == objcInterfaceDecl = do
                str <- getCursorSpelling c
                super <- childrenOfKind c objcSuperclassRef >>= mapM getCursorSpelling
                children <- children True c >>= mapM declFromCursor
                return $ Just $ Class (Identifier str) (map Type super) (catMaybes children)

            | k == objcPropertyDecl = do
                str <- getCursorSpelling c
                return $ Just $ Property (Identifier str) Nothing

            | k == objcInstanceMethodDecl = do
                str <- getCursorSpelling c
                return $ Just $ InstanceMethod (Identifier str) [] []

            | otherwise = return Nothing
    in cursorKind c >>= declFromCursor'

-- | Creates a Clang 'SourceFile' from a file on disk.
newSourceFile :: FilePath -> IO SourceFile
newSourceFile path = do
    tu <- newIndex >>= newTranslationUnit path
    return SourceFile { translationUnit = tu, filePath = path }
