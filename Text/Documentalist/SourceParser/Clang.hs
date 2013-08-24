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
data SourceFile = SourceFile { filePath :: FilePath }

instance Show SourceFile where
    show = show . filePath

instance SourcePackage SourceFile where
    parse src = do {
        tu <- newIndex >>= newTranslationUnit (filePath src);
        let declCursors = filter isDeclaration $ children True $ getCursor tu
            decls = mapMaybe declFromCursor declCursors
            cmts = map (fmap Comment . getComment) declCursors
            declMap = DeclMap $ Map.fromList $ zip decls cmts
            mod = Module (filePath src) declMap
        in return $ Package "" [mod]
    }

-- | Creates a 'Declaration' from the information at a cursor.
declFromCursor :: Cursor -> Maybe Declaration
declFromCursor c =
    let declFromCursor' :: CursorKind -> Maybe Declaration
        declFromCursor' k
            | k == typedefDecl =
                Just $ TypeAlias (Identifier $ getCursorSpelling c) (Type "foobar")

            | k == objcInterfaceDecl =
                let super = map getCursorSpelling $ childrenOfKind c objcSuperclassRef
                    decls = map declFromCursor $ children True c
                in Just $ Class (Identifier $ getCursorSpelling c) (map Type super) (catMaybes decls)

            | k == objcPropertyDecl =
                Just $ Property (Identifier $ getCursorSpelling c) Nothing

            | k == objcInstanceMethodDecl =
                Just $ InstanceMethod (Identifier $ getCursorSpelling c) [] []

            | otherwise = Nothing
    in declFromCursor' $ cursorKind c

-- | Creates a Clang 'SourceFile' from a file on disk.
newSourceFile :: FilePath -> SourceFile
newSourceFile path = SourceFile { filePath = path }
