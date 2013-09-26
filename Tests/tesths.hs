import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Documentalist
import Text.Documentalist.CommentParser.TomDoc
import Text.Documentalist.SourceParser.Haskell
import Text.Documentalist.SourceParser.Clang

main :: IO ()
main = do
    p_hs <- runHaskellParser $ parseHS "Fixtures/RCL.hs"
    p_rac <- runClangParser $ parse "Fixtures/test_header.h"
    let c_rac = parseDocs TomDocParser p_rac
        c_rac' = p_hs ~> c_rac
    print c_rac'
    print p_hs
    -- print c_rac'
    -- "-divideWithAmount:fromEdge:"
    
    -- 
    -- let c_test = parseDocs TomDocParser p_test
    --     c_rac  = parseDocs TomDocParser p_rac
    -- traversePackage (putStrLn . show) (putStrLn . show) (const . putStrLn . show) c_rac
    -- 
    -- defaultMainWithOpts
    --   [ testCase "test_num" (testNum p_test)
    --   , testCase "rac_num" (racNum p_rac)
    --   ] mempty
