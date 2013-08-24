import Text.Documentalist
import Text.Documentalist.SourceParser.Clang
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let x = newSourceFile $ head args
    cs <- parse x
    print cs
