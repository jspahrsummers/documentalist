import Parser.Clang
import Parser.Parseable
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    parse $ SourceFile $ head args
    return ()
