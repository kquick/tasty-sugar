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
alpha.exe
alpha.expct
alpha.clang.expct
alpha.x.expct
beta.exe
beta.expct
beta.clang.expct
beta......x.expct
gamma.exe
gamma.expct
gamma.clang.expct
gamma.x......expct
delta.exe
delta.expct
delta.clang.expct
delta......y.expct
epsilon.exe
epsilon.expct
epsilon.clang.expct
epsilon.y......expct
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
                       ,("other", Just ["x"]) -- follows c-compiler
                       ,("a param", Just ["y"]) -- preceeds c-compiler
                       ]
                   }

paramsAssocTests :: [TT.TestTree]
paramsAssocTests =
  let (sugar1,_s1desc) = findSugarIn sugarCube (sample sugarCube)
      chkCandidate = checkCandidate sugarCube sample testInpPath [] 0
      mkE' a cx cy p f c o = Expectation
                          { expectedFile = p f
                          , expParamsMatch = [ ("c-compiler", c)
                                             , ("optimization", o)
                                             , ("other", cx "x")
                                             , ("a param", cy "y")
                                             ]
                          , associated = a
                          }
      mkEr p = mkE' [ ("rust-source", p "recursive.rs") ] Assumed Assumed p
      mkEc p f c = mkE' [ ("c-source", p "simple.c") ] Assumed Assumed p f (Explicit c)
      mkEo cs p f c ca = mkE' [] cs ca p f c NotSpecified
  in [ testCase "valid sample" $ 33 @=? length (sample sugarCube)

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
       , chkCandidate "alpha.exe" []
       , chkCandidate "alpha.expct" []
       , chkCandidate "alpha.clang.expct" [("c-compiler", Explicit "clang")]
       , chkCandidate "alpha.x.expct" [("other", Explicit "x")]
       , chkCandidate "beta.exe" []
       , chkCandidate "beta.expct" []
       , chkCandidate "beta.clang.expct" [("c-compiler", Explicit "clang")]
       , chkCandidate "beta......x.expct" [("other", Explicit "x")]
       , chkCandidate "gamma.exe" []
       , chkCandidate "gamma.expct" []
       , chkCandidate "gamma.clang.expct" [("c-compiler", Explicit "clang")]
       , chkCandidate "gamma.x......expct" [("other", Explicit "x")]
       , chkCandidate "delta.exe" []
       , chkCandidate "delta.expct" []
       , chkCandidate "delta.clang.expct" [("c-compiler", Explicit "clang")]
       , chkCandidate "delta......y.expct" [("a param", Explicit "y")]
       , chkCandidate "epsilon.exe" []
       , chkCandidate "epsilon.expct" []
       , chkCandidate "epsilon.clang.expct" [("c-compiler", Explicit "clang")]
       , chkCandidate "epsilon.y......expct" [("a param", Explicit "y")]
       ]

     , sugarTestEq "correct found count" sugarCube sample 11 length
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
                 , mkE "simple.expct"     "clang" NotSpecified
                 , mkE "simple.expct"     "clang" (Explicit "noopt")
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
               , mkE "simple.noopt-gcc.expct" "gcc" NotSpecified
               , mkE "simple.noopt-gcc.expct" "gcc" (Explicit "noopt")
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
               , mkE "simple.expct"     "clang" NotSpecified
               , mkE "simple.expct"     "clang" (Explicit "noopt")
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
                     , mkE "simple.noopt-gcc.expct" "gcc"   NotSpecified
                     , mkE "simple.noopt-gcc.expct" "gcc"   (Explicit "noopt")
                  ]
                }

         -- n.b. simple-opt.gcc-exe is *not* matched: the rootname is
         -- "*.exe" so the '.' separator is required.

       -- Verify that Expectations are selected based on better ParamMatches
       -- (sorted).
       , Sweets { rootMatchName = "alpha.exe"
                , rootBaseName = "alpha"
                , rootFile = p "alpha.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  [
                    mkEo Assumed  p "alpha.clang.expct" (Explicit "clang") Assumed
                  , mkEo Explicit p "alpha.x.expct"     (Assumed "gcc")    Assumed
                  ]
                }

       -- Verify that lengthening the expected filename by adding separators will
       -- not change the parameter identification but will select the longer
       -- expected filename *if* the parameter is higher (sorted on parameter
       -- name).
       , Sweets { rootMatchName = "beta.exe"
                , rootBaseName = "beta"
                , rootFile = p "beta.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  -- sorted: "c-compiler":"clang", "other":"x", so clang is
                  -- preferred.
                  [
                    mkEo Explicit p "beta......x.expct" (Assumed "gcc")    Assumed
                  , mkEo Assumed  p "beta.clang.expct"  (Explicit "clang") Assumed
                  ]
                }

       -- This is the same as beta, but with the separator extensions on the
       -- other side.
       , Sweets { rootMatchName = "gamma.exe"
                , rootBaseName = "gamma"
                , rootFile = p "gamma.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  -- sorted: "c-compiler":"clang", "other":"x", so clang is
                  -- preferred.
                  [
                    mkEo Assumed  p "gamma.clang.expct" (Explicit "clang") Assumed
                  , mkEo Explicit p "gamma.x......expct" (Assumed "gcc")   Assumed
                  ]
                }

       -- This is the same as beta, but with a higher-precedence parameter
       , Sweets { rootMatchName = "delta.exe"
                , rootBaseName = "delta"
                , rootFile = p "delta.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  -- sorted: "a param":"y", "c-compiler":"clang" so "a param" is
                  -- preferred.
                  [
                    mkEo Assumed p "delta......y.expct" (Assumed "clang") Explicit
                  , mkEo Assumed p "delta......y.expct" (Assumed "gcc")   Explicit
                  ]
                }

       -- This is the same as beta, but with the separator extensions on the
       -- other side.
       , Sweets { rootMatchName = "epsilon.exe"
                , rootBaseName = "epsilon"
                , rootFile = p "epsilon.exe"
                , cubeParams = validParams sugarCube
                , expected =
                  -- sorted: "a param":"y", "c-compiler":"clang" so "a param" is
                  -- preferred.
                  [
                    mkEo Assumed p "epsilon.y......expct" (Assumed "clang") Explicit
                  , mkEo Assumed p "epsilon.y......expct" (Assumed "gcc")   Explicit
                  ]
                }
       ]
     ]
