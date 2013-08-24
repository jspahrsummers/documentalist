import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Documentalist
import Text.Documentalist.SourceParser

main :: IO ()
main = do
    let f_test = newSourceFile "Tests/Fixtures/test_header.h"
    let f_rac = newSourceFile "Tests/Fixtures/RACSignal.h"
    p_test <- parse f_test
    p_rac <- parse f_rac
    print p_rac

    defaultMainWithOpts
      [ testCase "test_num" (testNum p_test)
      , testCase "rac_num" (racNum p_rac)
      ] mempty

-- testNum :: Package Comment -> Assertion
testNum (Package p modules) = length modules @?= 1

-- TODO: work out right numbers
-- racNum :: Package Comment -> Assertion
racNum (Package p modules) = length modules @?= 1
