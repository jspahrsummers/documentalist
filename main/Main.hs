import Text.Documentalist
import System.Environment
import Control.Applicative

main :: IO ()
main = do
    args <- getArgs
    let x = newSourceFile $ head args
    cs <- parse x
    print cs
