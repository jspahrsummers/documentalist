import Control.Applicative
import Control.Monad
import System.Environment
import Text.Documentalist
import Text.Documentalist.CommentParser.TomDoc
import Text.Documentalist.SourceParser.Clang

main :: IO ()
main = do
    sources <- fmap newSourceFile <$> getArgs
    docs <- mapM parse sources
    print $ map (parseDocs TomDocParser) docs
