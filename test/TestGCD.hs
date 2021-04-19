{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGCD ( gcdSampleTests ) where

import qualified Data.Text as T
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.RawString.QQ


testInpPath = "test-data/samples"

testParams = [ ("solver", Just ["z3", "yices", "boolector", "cvc4"])
             , ("loop-merging", Just ["loopmerge", "loop"])
             ]

sugarCube = mkCUBE
              { rootName = "*.c"
              , expectedSuffix = "good"
              , inputDir = testInpPath
              , associatedNames = [ ("config", "config")
                                  , ("stdio", "print")
                                  , ("haskell", "hs")
                                  ]
              , validParams = testParams
              }

gcdSampleTests :: [TT.TestTree]
gcdSampleTests =
  let (sugar,sdesc) = findSugarIn sugarCube gcdSamples
  in [ testCase "valid sample" $ 20 @=? length gcdSamples
     , sugarTestEq "correct found count" sugarCube gcdSamples 1 length

     , testCase "sweets rendering" $
       let actual = sweetsTextTable [sugarCube] sugar
       in do putStrLn "Table" -- try to start table on its own line
             putStrLn $ T.unpack actual
             T.length actual > 0 @? "change this to see the table"

     , testCaseSteps "sweets info" $ \step -> do
         step "rootMatchName"
         (rootMatchName <$> sugar) @?= ["gcd-test.c"]
         step "rootBaseName"
         (rootBaseName <$> sugar) @?= ["gcd-test"]
         step "rootFile"
         (rootFile <$> sugar) @?= [ testInpPath </> "gcd-test.c" ]
         step "cubeParams"
         (cubeParams <$> sugar) @?= [ validParams sugarCube ]

     , testCase "Expectations" $ compareBags "expected" (expected $ head sugar) $
       let p = (testInpPath </>) in
         [
           Expectation
           { expectedFile = p "gcd-test.boolector.good"
           , expParamsMatch = [ ("solver", Explicit "boolector")
                              , ("loop-merging", Assumed "loopmerge")
                              ]
           , associated = [ ("config", p "gcd-test.loopmerge.config")
                          , ("stdio", p "gcd-test.boolector.print")
                          ]
           }
         , Expectation
           { expectedFile = p "gcd-test.boolector.good"
           , expParamsMatch = [ ("solver", Explicit "boolector")
                              , ("loop-merging", Assumed "loop")
                              ]
           , associated = [ ("config", p "gcd-test.config")
                          , ("stdio", p "gcd-test.boolector.print")
                          ]
           }

         , Expectation
           { expectedFile = p "gcd-test.good"
           , expParamsMatch = [ ("solver", Assumed "cvc4")
                              , ("loop-merging", Assumed "loopmerge")
                              ]
           , associated = [ ("config", p "gcd-test.loopmerge.config")
                          , ("stdio", p "gcd-test.print")
                          ]
           }
         , Expectation
           { expectedFile = p "gcd-test.good"
           , expParamsMatch = [ ("solver", Assumed "cvc4")
                              , ("loop-merging", Assumed "loop")
                              ]
           , associated = [ ("config", p "gcd-test.config")
                          , ("stdio", p "gcd-test.print")
                          ]
           }

         , Expectation
           { expectedFile = p "gcd-test.good"
           , expParamsMatch = [ ("solver", Assumed "yices")
                              , ("loop-merging", Assumed "loopmerge")
                              ]
           , associated = [ ("config", p "gcd-test.loopmerge.config")
                          , ("stdio", p "gcd-test.print")
                          ]
           }
         , Expectation
           { expectedFile = p "gcd-test.good"
           , expParamsMatch = [ ("solver", Assumed "yices")
                              , ("loop-merging", Assumed "loop")
                              ]
           , associated = [ ("config", p "gcd-test.config")
                          , ("stdio", p "gcd-test.print")
                          ]
           }

         , Expectation
           { expectedFile = p "gcd-test.good"
           , expParamsMatch = [ ("solver", Assumed "z3")
                              , ("loop-merging", Assumed "loopmerge")
                              ]
           , associated = [ ("config", p "gcd-test.loopmerge.config")
                          , ("stdio", p "gcd-test.print")
                          ]
           }
         , Expectation
           { expectedFile = p "gcd-test.good"
           , expParamsMatch = [ ("solver", Assumed "z3")
                              , ("loop-merging", Assumed "loop")
                              ]
           , associated = [ ("config", p "gcd-test.config")
                          , ("stdio", p "gcd-test.print")
                          ]
           }
         ]
     ]

gcdSamples = lines [r|
gcd-test.boolector.boolector.out
gcd-test.boolector.boolector.print.out
gcd-test.boolector.good
gcd-test.boolector.print
gcd-test.c
gcd-test.config
gcd-test.cvc4.out
gcd-test.cvc4.print.out
gcd-test.good
gcd-test.loopmerge.config
gcd-test.print
gcd-test.stp.good
gcd-test.stp.print
gcd-test.stp.stp.out
gcd-test.stp.stp.print.out
gcd-test.yices.out
gcd-test.yices.print.out
gcd-test.z3.out
gcd-test.z3.print.out
|]
