import Data.Monoid
import Data.Traversable
import Prelude hiding (mapM)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Documentalist
import Text.Documentalist.CommentParser.TomDoc
import Text.Documentalist.SourceParser.Clang
import qualified Tests.TomDocExamples as T

main :: IO ()
main = do
    p_test <- runClangParser $ parse "Tests/Fixtures/test_header.h"
    p_rac  <- runClangParser $ parse "Tests/Fixtures/RACSignal.h"
    print p_rac

    let (Right c_test) = parseDocs TomDocParser p_test
        (Right c_rac)  = parseDocs TomDocParser p_rac
    mapM (putStrLn . show) c_rac

    defaultMainWithOpts
      [ testCase "test_num" (testNum p_test)
      , testCase "rac_num" (racNum p_rac)
      ] mempty


testNum :: (Package (Maybe Comment)) -> Assertion
testNum (Package p modules) = length modules @?= 1

-- TODO: work out right numbers
racNum (Package p modules) = length modules @?= 1
