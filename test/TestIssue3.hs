module TestIssue3 ( issue3Tests ) where

import           System.FilePath ( (</>), takeFileName )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar


issue3Tests :: IO [TT.TestTree]
issue3Tests = do tsts <- sequence [ issue3Test1 ]
                 return [ TT.testGroup "Issue #3" $ concat tsts ]

issue3Test1 :: IO [TT.TestTree]
issue3Test1 = do
  let testInpPath = "test/data/issue3"
  let cube = mkCUBE { inputDirs = [testInpPath]
                    , rootName = "*.c"
                    , expectedSuffix = "c"
                    , validParams = [ ("ver", Just ["42", "43"]) ]
                    }
  sweets <- findSugar cube
  -- putStrLn $ show sweets

  let exp0 f v = Expectation
                 { expectedFile = testInpPath </> f
                 , expParamsMatch = [ ("ver", v) ]
                 , associated = []
                 }
  let exp f v = exp0 f v
      testExp s i e f v = testCase ("Exp #" <> show i) $ e @?= exp f v
      validInstNum n ns = testCase ("instnum valid: " <> show n)
                          (n `elem` ns @? "unexpected instnum")

  tests <- withSugarGroups sweets TT.testGroup $ \sweet instnum exp ->
    return $
    case rootMatchName sweet of
      "test.c" ->
        validInstNum instnum [1]
        :
        testCase "# expectations" (length (expected sweet) @?= 2)
        :
        case takeFileName (expectedFile exp) of
          "test.42.c" -> [testExp sweet instnum exp "test.42.c" (Explicit "42")]
          "test.c"    -> [testExp sweet instnum exp "test.c"    (Assumed  "43")]
          o -> [ testCase "unexpected" $ False @?
                 "Unexpected exp file for " <> rootMatchName sweet
                 <> " sweet: " <> o
               ]

      "test.42.c" ->
        validInstNum instnum [1]
        : testCase "# expectations" (length (expected sweet) @?= 1)
        : [ testExp sweet instnum exp "test.42.c" (Explicit "42") ]

      _ -> [ testCase "unexpected" $ False @?
             "Unexpected root sweet: " <> rootMatchName sweet
           ]

  return $
    testCase "correct # of sweets" (length sweets @?= 2)
    : tests
