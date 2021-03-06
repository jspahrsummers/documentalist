import Control.Applicative
import Data.Traversable hiding (mapM)
import System.Environment
import System.IO (hPutStrLn, stderr)
import Text.Documentalist
import Text.Documentalist.CommentParser.TomDoc
import Text.Documentalist.SourceParser.Clang

main :: IO ()
main = do
    sources <- getArgs
    docs <- runClangParser $ mapM parse sources
    let commented = map (parseDocs TomDocParser) docs
    displayable <- mapM (traverse showErrors) commented
    print displayable

showErrors :: Either CommentParseException DocBlock -> IO (Maybe DocBlock)
showErrors =
    let showError e = hPutStrLn stderr (show e) >> return Nothing
    in either showError (return . Just)
