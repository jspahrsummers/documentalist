import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Documentalist
import Text.Documentalist.CommentParser as CommentParser
import Text.Documentalist.CommentParser.TomDoc
import Text.Documentalist.SourceParser as SourceParser

main :: IO ()
main = do
    let f_test = newSourceFile "Tests/Fixtures/test_header.h"
    let f_rac = newSourceFile "Tests/Fixtures/RACSignal.h"

    p_test <- SourceParser.parse f_test
    p_rac <- SourceParser.parse f_rac
    print p_rac

    let c_test = CommentParser.parse TomDocParser p_test
        c_rac = CommentParser.parse TomDocParser p_rac
    print c_rac

    defaultMainWithOpts
      [ testCase "test_num" (testNum p_test)
      , testCase "rac_num" (racNum p_rac)
      ] mempty

testNum (Package p modules) = length modules @?= 1

-- TODO: work out right numbers
racNum (Package p modules) = length modules @?= 1
