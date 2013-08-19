import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Documentalist

main :: IO ()
main = do
    f_test <- newSourceFile "Fixtures/test_header.h"
    f_rac <- newSourceFile "Fixtures/RACSignal.h"
    p_test <- parse f_test
    p_rac <- parse f_rac
    print p_rac

    defaultMainWithOpts
      [ testCase "test_num" (testNum p_test)
      , testCase "rac_num" (racNum p_rac)
      ] mempty

-- We don't want to expose these data types just yet
testNum :: (t, [a]) -> Assertion
testNum (_, comments) = length comments @?= 10

racNum :: (t, [a]) -> Assertion
racNum (_, comments) = length comments @?= 10
