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
  let (sugar,sdesc) = findSugarIn sugarCube (strlen2Samples sugarCube)
      chkCandidate nm pm =
        testCase (nm <> " candidate")
        $ find ((nm ==) . candidateFile) (strlen2Samples sugarCube)
        @?= Just (CandidateFile { candidateDir = testInpPath
                                , candidateSubdirs = []
                                , candidateFile = nm
                                , candidatePMatch = pm
                                })
  in [ testCase "valid sample" $ 24 @=? length (strlen2Samples sugarCube)
     , sugarTestEq "correct found count" sugarCube strlen2Samples 1 length

     , TT.testGroup "candidates"
       [
         chkCandidate "strlen_test2.boolector.boolector.out"
         [ ("solver", Explicit "boolector") ]
       , chkCandidate "strlen_test2.boolector.boolector.print.out"
         [ ("solver", Explicit "boolector") ]
       , chkCandidate "strlen_test2.boolector.good"
         [ ("solver", Explicit "boolector") ]
       , chkCandidate "strlen_test2.boolector.loopmerge.good"
         [ ("loop-merging", Explicit "loopmerge")
         , ("solver", Explicit "boolector")
         ]
       , chkCandidate "strlen_test2.c" []
       , chkCandidate "strlen_test2.cvc4.cvc4.out"
         [ ("solver", Explicit "cvc4") ]
       , chkCandidate "strlen_test2.cvc4.loop.good"
         [ ("loop-merging", Explicit "loop")
         , ("solver", Explicit "cvc4")
         ]
       , chkCandidate "strlen_test2.loopmerge.cvc4.good"
         [ ("loop-merging", Explicit "loopmerge")
         , ("solver", Explicit "cvc4")
         ]
       , chkCandidate "strlen_test2.good" []
       , chkCandidate "strlen_test2.loopmerge.config"
         [ ("loop-merging", Explicit "loopmerge") ]
       , chkCandidate "strlen_test2.loopmerge.stp.out"
         [ ("loop-merging", Explicit "loopmerge")
         ]
       , chkCandidate "strlen_test2.loopmerge.yices.out"
         [ ("loop-merging", Explicit "loopmerge")
         , ("solver", Explicit "yices")
         ]
       , chkCandidate "strlen_test2.loopmerge.yices.print.out"
         [ ("loop-merging", Explicit "loopmerge")
         , ("solver", Explicit "yices")
         ]
       , chkCandidate "strlen_test2.print" []
       , chkCandidate "strlen_test2.stp.out" []
       , chkCandidate "strlen_test2.stp.print.out" []
       , chkCandidate "strlen_test2.yices.out"
         [ ("solver", Explicit "yices") ]
       , chkCandidate "strlen_test2.yices.print.out"
         [ ("solver", Explicit "yices") ]
       , chkCandidate "strlen_test2.z3.good"
         [ ("solver", Explicit "z3") ]
       , chkCandidate "strlen_test2.z3.z3.out"
         [ ("solver", Explicit "z3") ]
       , chkCandidate "strlen_test2.z3.z3.print.out"
         [ ("solver", Explicit "z3") ]
       ]

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
         -- Note that there exists strlen_test2.z3.good and
         -- strlen_test2.loopmerge.good, so this will create Expectations
         -- against _each_ file with different sets of Assumed and Explicit
         -- parameters.
         --
         -- The removeNonExplicitMatchingExpectations function will then try to
         -- select which one has the least number of explicit matches, but one is
         -- always Explicit and one is always Assumed.  The resuilt then comes
         -- down to a comparison of the values, and "loopmerge" < "z3", so "z3"
         -- is selected as the _maximum_ (i.e. best) match.
         --
         -- Thus, the following Expectation is eliminated.
         --
         -- , Expectation
         --   { expectedFile = p "strlen_test2.loopmerge.good"
         --   , expParamsMatch = [ ("loop-merging", Explicit "loopmerge")
         --                      , ("solver", Assumed "z3")
         --                      ]
         --   , associated = [ ("config", p "strlen_test2.loopmerge.config")
         --                  , ("stdio", p "strlen_test2.print")
         --                  ]
         --   }
         ]
     ]

strlen2Samples cube = fmap (makeCandidate cube "test-data/samples" [])
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
