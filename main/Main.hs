import Text.Documentalist
import Text.Documentalist.SourceParser.Clang
import Text.Documentalist.CommentParser.TomDoc
import System.Environment
import Control.Applicative
import Control.Monad

import Control.Monad.Error.Class
import qualified Control.Monad.Writer.Lazy as W
import Data.List
import Data.Maybe

main :: IO ()
main = do
    args <- getArgs
    let x = newSourceFile $ head args
    cs <- parse x
    let (Right ds) = parseDocs TomDocParser cs
    packageTraverse pkgName modName declLeaf declNode ds

class PackageT a where
  pkgName  :: String -> a ()
  modName  :: String -> a ()
  declLeaf :: Int -> (Maybe DocBlock) -> Identifier -> DecFLeaf -> a ()
  declNode :: Int -> (Maybe DocBlock) -> Identifier -> DecFNode -> a ()

instance PackageT IO where
  pkgName x = putStrLn $ "# " ++ x
  modName x = putStrLn $ "## " ++ x
  declLeaf d (Just dc) (Identifier i) e = putStrLn ("### " ++ i) >> putStrLn (showDocBlock dc)
  declLeaf d Nothing   (Identifier i) e = putStrLn $ "### " ++ i
  declNode d (Just dc) (Identifier i) e = putStr ("##" ++ (ind d '#') ++ " ") >> putStrLn i >> putStrLn (showDocBlock dc)
  declNode d Nothing   (Identifier i) e = return () -- putStr ("##" ++ (ind d '#') ++ " ") >> putStrLn i

ind x s = replicate x s

showDocBlock doc =
        let desc = description doc
            params = parameters doc
            ex = example doc
            res = result doc
        
            show' :: W.Writer [String] ()
            show' = do
                W.tell ["**Summary:** " ++ show (summary doc)]
                when (length desc > 0)   $ W.tell ["**Description:** " ++ show desc]
                when (length params > 0) $ W.tell ["**Parameters:** " ++ show params]
                when (isJust ex)         $ W.tell ["**Example:** " ++ show (fromJust ex)]
                when (isJust res)        $ W.tell ["**Result:** " ++ show (fromJust res)]
        in "\n" ++ intercalate "\n\n" (W.execWriter show') ++ "\n"
