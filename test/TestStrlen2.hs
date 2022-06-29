{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestStrlen2 ( strlen2SampleTests ) where

import           Data.List
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
              , inputDirs = [ testInpPath ]
              , associatedNames = [ ("config", "config")
                                  , ("stdio", "print")
                                  ]
              , validParams = testParams
              }

strlen2SampleTests :: [TT.TestTree]
strlen2SampleTests =
  let (sugar,sdesc) = findSugarIn sugarCube strlen2Samples
  in [ testCase "valid sample" $ 24 @=? length strlen2Samples
     , sugarTestEq "correct found count" sugarCube strlen2Samples 1 length

     , testCaseSteps "sweets info" $ \step -> do
         step "rootMatchName"
         (rootMatchName <$> sugar) @?= ["strlen_test2.c"]
         step "rootBaseName"
         (rootBaseName <$> sugar) @?= ["strlen_test2"]
         step "rootFile"
         (rootFile <$> sugar) @?= [ testInpPath </> "strlen_test2.c" ]
         step "cubeParams"
         (cubeParams <$> sugar) @?= [ validParams sugarCube ]

     , testCase "Expectations" $ compareBags "expected" (expected $ head sugar) $
       let p = (testInpPath </>) in
         [
           Expectation
           { expectedFile = p "strlen_test2.boolector.loopmerge.good"
           , expParamsMatch = [ ("solver", Explicit "boolector")
                              , ("loop-merging", Explicit "loopmerge")
                              ]
           , associated = [ ("config", p "strlen_test2.loopmerge.config")
                          , ("stdio", p "strlen_test2.print")
                          ]
           }
         , Expectation
           { expectedFile = p "strlen_test2.boolector.good"
           , expParamsMatch = [ ("solver", Explicit "boolector")
                              , ("loop-merging", Assumed "loop")
                              ]
           , associated = [ ("stdio", p "strlen_test2.print")
                          ]
           }

         , Expectation
           { expectedFile = p "strlen_test2.loopmerge.cvc4.good"
           , expParamsMatch = [ ("solver", Explicit "cvc4")
                              , ("loop-merging", Explicit "loopmerge")
                              ]
           , associated = [ ("config", p "strlen_test2.loopmerge.config")
                          , ("stdio", p "strlen_test2.print")
                          ]
           }
         , Expectation
           { expectedFile = p "strlen_test2.cvc4.loop.good"
           , expParamsMatch = [ ("solver", Explicit "cvc4")
                              , ("loop-merging", Explicit "loop")
                              ]
           , associated = [ ("stdio", p "strlen_test2.print")
                          ]
           }

         , Expectation
           { expectedFile = p "strlen_test2.loopmerge.good"
           , expParamsMatch = [ ("loop-merging", Explicit "loopmerge")
                              , ("solver", Assumed "yices")
                              ]
           , associated = [ ("config", p "strlen_test2.loopmerge.config")
                          , ("stdio", p "strlen_test2.print")
                          ]
           }
         , Expectation
           { expectedFile = p "strlen_test2.good"
           , expParamsMatch = [ ("loop-merging", Assumed "loop")
                              , ("solver", Assumed "yices")
                              ]
           , associated = [ ("stdio", p "strlen_test2.print")
                          ]
           }

         , Expectation
           { expectedFile = p "strlen_test2.z3.good"
           , expParamsMatch = [ ("solver", Explicit "z3")
                              , ("loop-merging", Assumed "loopmerge")
                              ]
           , associated = [ ("config", p "strlen_test2.loopmerge.config")
                          , ("stdio", p "strlen_test2.print")
                          ]
           }
         , Expectation
           { expectedFile = p "strlen_test2.z3.good"
           , expParamsMatch = [ ("solver", Explicit "z3")
                              , ("loop-merging", Assumed "loop")
                              ]
           , associated = [ ("stdio", p "strlen_test2.print")
                          ]
           }
         , Expectation
           { expectedFile = p "strlen_test2.loopmerge.good"
           , expParamsMatch = [ ("loop-merging", Explicit "loopmerge")
                              , ("solver", Assumed "z3")
                              ]
           , associated = [ ("config", p "strlen_test2.loopmerge.config")
                          , ("stdio", p "strlen_test2.print")
                          ]
           }
         ]
     ]

-- Note that there exists strlen_test2.z3.good and
-- strlen_test2.loopmerge.good, so this will create Expectations
-- against _each_ file with different sets of Assumed and Explicit
-- parameters.

strlen2Samples = fmap (\f -> CandidateFile { candidateDir = "test-data/samples"
                                      , candidateSubdirs = []
                                      , candidateFile = f })
                 $ filter (not . null)
                 $ lines [r|
strlen_test2.boolector.boolector.out
strlen_test2.boolector.boolector.print.out
strlen_test2.boolector.good
strlen_test2.boolector.loopmerge.good
strlen_test2.c
strlen_test2.cvc4.cvc4.out
strlen_test2.cvc4.cvc4.print.out
strlen_test2.cvc4.loop.good
strlen_test2.loopmerge.cvc4.good
strlen_test2.good
strlen_test2.loopmerge.config
strlen_test2.loopmerge.good
strlen_test2.loopmerge.stp.out
strlen_test2.loopmerge.stp.print.out
strlen_test2.loopmerge.yices.out
strlen_test2.loopmerge.yices.print.out
strlen_test2.print
strlen_test2.stp.out
strlen_test2.stp.print.out
strlen_test2.yices.out
strlen_test2.yices.print.out
strlen_test2.z3.good
strlen_test2.z3.z3.out
strlen_test2.z3.z3.print.out
|]
