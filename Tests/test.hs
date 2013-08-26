import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Documentalist
import Text.Documentalist.CommentParser.TomDoc
import Text.Documentalist.SourceParser.Clang
import qualified Data.Traversable as T

main :: IO ()
main = do
    let f_test = newSourceFile "Tests/Fixtures/test_header.h"
    let f_rac  = newSourceFile "Tests/Fixtures/RACSignal.h"

    p_test <- parse f_test 
    p_rac  <- parse f_rac
    print p_rac

    let (Right c_test) = parseDocs TomDocParser p_test
        (Right c_rac)  = parseDocs TomDocParser p_rac
    T.mapM (putStrLn . show) c_rac

    defaultMainWithOpts
      [ testCase "test_num" (testNum p_test)
      , testCase "rac_num" (racNum p_rac)
      ] mempty


testNum :: (Package (Maybe Comment)) -> Assertion
testNum (Package p modules) = length modules @?= 1

-- TODO: work out right numbers
racNum (Package p modules) = length modules @?= 1
