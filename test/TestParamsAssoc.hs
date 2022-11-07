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
      chkCandidate = checkCandidate sugarCube sample testInpPath [] 0
      mkE' a p f c o = Expectation
                     { expectedFile = p f
                     , expParamsMatch = [ ("c-compiler", c)
                                        , ("optimization", o)
                                        ]
                     , associated = a
                     }
      mkEr p = mkE' [ ("rust-source", p "recursive.rs") ] p
      mkEc p f c = mkE' [ ("c-source", p "simple.c") ] p f (Explicit c)
  in [ testCase "valid sample" $ 13 @=? length (sample sugarCube)

     , TT.testGroup "candidates"
       [
         chkCandidate "recursive.rs" []
       , chkCandidate "recursive.fast.exe" [("optimization", Explicit "fast")]
       , chkCandidate "simple.noopt.clang.exe" [("c-compiler", Explicit "clang")
                                               , ("optimization", Explicit "noopt")]
       , chkCandidate "simple.noopt.gcc.exe" [("c-compiler", Explicit "gcc")
                                             , ("optimization", Explicit "noopt")]
       , chkCandidate "simple.noopt-gcc.expct" [("c-compiler", Explicit "gcc")
                                               , ("optimization", Explicit "noopt")]
       , chkCandidate "simple.opt-clang.exe" [("c-compiler", Explicit "clang")
                                             , ("optimization", Explicit "opt")]
       , chkCandidate "simple.clang-noopt-clang.exe" [("c-compiler", Explicit "clang")
                                                     , ("optimization", Explicit "noopt")]
       , chkCandidate "simple.clang-gcc.exe" [("c-compiler", Explicit "clang")
                                             ,("c-compiler", Explicit "gcc")]
       , chkCandidate "simple-opt.expct" [("optimization", Explicit "opt")]
       , chkCandidate "simple-opt.gcc-exe" [("c-compiler", Explicit "gcc")
                                           , ("optimization", Explicit "opt")]
       ]

     , sugarTestEq "correct found count" sugarCube sample 6 length
     , testCase "results" $ compareBags "results" sugar1 $
       let p = (testInpPath </>) in
       [
         Sweets
         { rootMatchName = "recursive.fast.exe"
         , rootBaseName = "recursive"
         , rootFile = p "recursive.fast.exe"
         , cubeParams = validParams sugarCube
         , expected =
             let mkE = mkEr p
             in [ mkE "recursive.fast.expct" (Assumed "clang") (Explicit "fast")
                , mkE "recursive.fast.expct" (Assumed "gcc")   (Explicit "fast")
                ]
         }

         , Sweets
           { rootMatchName = "simple.noopt.clang.exe"
           , rootBaseName = "simple"
           , rootFile = p "simple.noopt.clang.exe"
           , cubeParams = validParams sugarCube
           , expected =
               let mkE = mkEc p
               in
                 -- Note that opt is an unconstrained parameter, so it can
                 -- match "noopt" or it can match something else.
                 [ mkE "simple-opt.expct" "clang" (Explicit "opt")
                 , mkE "simple.expct"     "clang" (Explicit "noopt")
                 , mkE "simple.expct"     "clang" NotSpecified
                 ]
           }

       , Sweets
         { rootMatchName = "simple.noopt.gcc.exe"
         , rootBaseName = "simple"
         , rootFile = p "simple.noopt.gcc.exe"
         , cubeParams = validParams sugarCube
         , expected =
             let mkE = mkEc p
             in
               -- Note that opt is an unconstrained parameter, so it can
               -- match "noopt" or it can match something else.
               [ mkE "simple-opt.expct"       "gcc" (Explicit "opt")
               , mkE "simple.noopt-gcc.expct" "gcc" (Explicit "noopt")
               , mkE "simple.noopt-gcc.expct" "gcc" NotSpecified
               ]
         }

       , Sweets { rootMatchName = "simple.opt-clang.exe"
                , rootBaseName = "simple"
                , rootFile = p "simple.opt-clang.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  let mkE = mkEc p
                  in
                    -- Note that opt is an unconstrained parameter, so it can
                    -- match "noopt" or it can match something else.
                    [ mkE "simple-opt.expct" "clang" (Explicit "opt")
                    , mkE "simple.expct"     "clang" NotSpecified
                    ]
                }

         -- This repeats a parameter value and also matches with a different
         -- value.  The duplicate value should be ignored, but both values should
         -- result in different Expectations.
       , Sweets
         { rootMatchName = "simple.clang-noopt-clang.exe"
         , rootBaseName = "simple"
         , rootFile = p "simple.clang-noopt-clang.exe"
         , cubeParams = validParams sugarCube
         , expected =
             let mkE = mkEc p
             in
               -- Note that opt is an unconstrained parameter, so it can
               -- match "noopt" or it can match something else.
               [ mkE "simple-opt.expct" "clang" (Explicit "opt")
               , mkE "simple.expct"     "clang" (Explicit "noopt")
               , mkE "simple.expct"     "clang" NotSpecified
               ]
         }

         -- This repeats a parameter with a different value, which causes a
         -- separate explicit match to be claimed for each value.  In addition,
         -- the optimization parameter has no value specified, so multiple values
         -- are found that could be tried, along with an expect file that doesn't
         -- supply a potential value for this parameter.
       , Sweets { rootMatchName = "simple.clang-gcc.exe"
                , rootBaseName = "simple"
                , rootFile = p "simple.clang-gcc.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  let mkE = mkEc p
                  in [ mkE "simple-opt.expct"       "clang" (Explicit "opt")
                     , mkE "simple-opt.expct"       "gcc"   (Explicit "opt")
                     , mkE "simple.expct"           "clang" NotSpecified
                     , mkE "simple.noopt-gcc.expct" "gcc"   (Explicit "noopt")
                     , mkE "simple.noopt-gcc.expct" "gcc"   NotSpecified
                  ]
                }

         -- n.b. simple-opt.gcc-exe is *not* matched: the rootname is
         -- "*.exe" so the '.' separator is required.
       ]
     ]
