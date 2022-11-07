{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestParamsAssoc ( paramsAssocTests ) where

import           Data.List
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.RawString.QQ


sample :: CUBE -> [CandidateFile]
sample cube = fmap (makeCandidate cube testInpPath [])
         $ filter (not . null)
         $ lines [r|
recursive.rs
recursive.fast.exe
recursive.fast.expct
simple.c
simple.expct
simple.noopt.clang.exe
simple.noopt.gcc.exe
simple.noopt-gcc.expct
simple.opt-clang.exe
simple.clang-noopt-clang.exe
simple.clang-gcc.exe
simple-opt.expct
simple-opt.gcc-exe
|]

testInpPath = "test/params/samples"

-- Note: in addition to other differences when compared to the tests
-- here (TestNoAssoc, TestSingleAssoc, TestMultiAssoc), this test is
-- somewhat different in that the *output* is used as the rootname
-- instead of the source (e.g. a .c file), so parameter values must be
-- identified and removed from the root to match corresponding expects
-- and associated files.

sugarCube = mkCUBE { inputDirs = [ testInpPath ]
                   , rootName = "*.exe"
                   , separators = "-."
                   , expectedSuffix = "expct"
                   , associatedNames = [ ("c-source", "c")
                                       , ("rust-source", "rs")
                                       , ("haskell", "hs")
                                       ]
                   , validParams = [
                       ("optimization", Nothing)
                       ,("c-compiler", Just ["gcc", "clang"])
                       ]
                   }

paramsAssocTests :: [TT.TestTree]
paramsAssocTests =
  let (sugar1,_s1desc) = findSugarIn sugarCube (sample sugarCube)
      chkCandidate nm pm =
        testCase (nm <> " candidate")
        $ find ((nm ==) . candidateFile) (sample sugarCube)
        @?= Just (CandidateFile { candidateDir = testInpPath
                                , candidateSubdirs = []
                                , candidateFile = nm
                                , candidatePMatch = pm
                                })
  in [ testCase "valid sample" $ 13 @=? length (sample sugarCube)

     , TT.testGroup "candidates"
       [
         chkCandidate "recursive.rs" []
       , chkCandidate "recursive.fast.exe" [("optimization", Explicit "fast")]
       , chkCandidate "simple.noopt.clang.exe" [("c-compiler", Explicit "clang")]
       , chkCandidate "simple.noopt.gcc.exe" [("c-compiler", Explicit "gcc")]
       , chkCandidate "simple.noopt-gcc.expct" [("c-compiler", Explicit "gcc")]
       , chkCandidate "simple.opt-clang.exe" [("c-compiler", Explicit "clang")]
       , chkCandidate "simple.clang-noopt-clang.exe" [("c-compiler", Explicit "clang")]
       , chkCandidate "simple.clang-gcc.exe" [("c-compiler", Explicit "clang")
                                               ,("c-compiler", Explicit "gcc")]
       , chkCandidate "simple-opt.expct" [("optimization", Explicit "opt")]
       , chkCandidate "simple-opt.gcc-exe" [("c-compiler", Explicit "gcc")]
       ]

     , sugarTestEq "correct found count" sugarCube sample 6 length
     , testCase "results" $ compareBags "results" sugar1 $
       let p = (testInpPath </>) in
       [
         Sweets { rootMatchName = "recursive.fast.exe"
                , rootBaseName = "recursive"
                , rootFile = p "recursive.fast.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  [
                    Expectation
                    { expectedFile = p "recursive.fast.expct"
                    , expParamsMatch = [ ("optimization", Explicit "fast")
                                       , ("c-compiler", Assumed "clang")
                                       ]
                    , associated = [ ("rust-source", p "recursive.rs") ]
                    }
                  , Expectation
                    { expectedFile = p "recursive.fast.expct"
                    , expParamsMatch = [ ("optimization", Explicit "fast")
                                       , ("c-compiler", Assumed "gcc")
                                       ]
                    , associated = [ ("rust-source", p "recursive.rs") ]
                    }
                  ]
                }

         , Sweets { rootMatchName = "simple.noopt.clang.exe"
                  , rootBaseName = "simple"
                  , rootFile = p "simple.noopt.clang.exe"
                  , cubeParams = validParams sugarCube
                  , expected =
                    [
                      Expectation
                      { expectedFile = p "simple.expct"
                      , expParamsMatch = [ ("optimization", Explicit "noopt")
                                         , ("c-compiler", Explicit "clang")
                                         ]
                      , associated = [ ("c-source", p "simple.c")]
                      }
                    ]
                  }

       , Sweets { rootMatchName = "simple.noopt.gcc.exe"
                , rootBaseName = "simple"
                , rootFile = p "simple.noopt.gcc.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  [ Expectation
                    { expectedFile = p "simple.noopt-gcc.expct"
                    , expParamsMatch = [ ("optimization", Explicit "noopt")
                                       , ("c-compiler", Explicit "gcc")
                                       ]
                    , associated = [ ("c-source", p "simple.c")]
                    }
                  ]
                }

       , Sweets { rootMatchName = "simple.opt-clang.exe"
                , rootBaseName = "simple"
                , rootFile = p "simple.opt-clang.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  [ Expectation
                    { expectedFile = p "simple-opt.expct"
                    , expParamsMatch = [ ("optimization", Explicit "opt")
                                       , ("c-compiler", Explicit "clang")
                                       ]
                    , associated = [ ("c-source", p "simple.c")]
                    }
                  ]
                }

         -- This repeats a parameter value, which nullifies any
         -- parameter matching and so those elements that look like
         -- parameters are just part of the base.
       , Sweets { rootMatchName = "simple.clang-noopt-clang.exe"
                , rootBaseName = "simple"
                , rootFile = p "simple.clang-noopt-clang.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  [ Expectation
                    { expectedFile = p "simple.expct"
                    , expParamsMatch = [ ("optimization", NotSpecified)
                                       , ("c-compiler", Assumed "clang")
                                       ]
                    , associated = [ ("c-source", p "simple.c")]
                    }
                  , Expectation
                    { expectedFile = p "simple.expct"
                    , expParamsMatch = [ ("optimization", NotSpecified)
                                       , ("c-compiler", Assumed "gcc")
                                       ]
                    , associated = [ ("c-source", p "simple.c")]
                    }
                  ]
                }

         -- This repeats a parameter with a different value, which
         -- nullifies any parameter matching and so those elements
         -- that look like parameters are just part of the base.
       , Sweets { rootMatchName = "simple.clang-gcc.exe"
                , rootBaseName = "simple"
                , rootFile = p "simple.clang-gcc.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  [ Expectation
                    { expectedFile = p "simple.expct"
                    , expParamsMatch = [ ("optimization", NotSpecified)
                                       , ("c-compiler", Assumed "clang")
                                       ]
                    , associated = [ ("c-source", p "simple.c")]
                    }
                  , Expectation
                    { expectedFile = p "simple.expct"
                    , expParamsMatch = [ ("optimization", NotSpecified)
                                       , ("c-compiler", Assumed "gcc")
                                       ]
                    , associated = [ ("c-source", p "simple.c")]
                    }
                  ]
                }

         -- n.b. simple-opt.gcc-exe is *not* matched: the rootname is
         -- "*.exe" so the '.' separator is required.
       ]
     ]
