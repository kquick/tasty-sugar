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
                          , expParamsMatch = [ ("a param", cy "y")
                                             , ("c-compiler", c)
                                             , ("optimization", o)
                                             , ("other", cx "x")
                                             ]
                          , associated = a
                          }
      mkEr p = mkE' [ ("rust-source", p "recursive.rs") ] Assumed Assumed p
      mkEc p f c = mkE' [ ("c-source", p "simple.c") ] Assumed Assumed p f (Explicit c)
      mkEo cs p f c ca = mkE' [] cs ca p f c NotSpecified
      mkEq cs ca p f c = mkE' [] cs ca p f c
      p = (testInpPath </>)
      chkFld :: Eq a => Show a
             => Maybe Sweets -> String -> (Sweets -> a) -> a -> TT.TestTree
      chkFld s n f w = testCase n $ maybe (error "fail") f s @?= w
      chkExp s g n f c o = testCase ("Exp #" <> show n)
                           $ (safeElem n . expected =<< s)
                           @?= Just (g f c o)
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

     , let sweetNum = 5
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE = chkExp sweet (mkEr p)
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "recursive.fast.exe"
          , chkF "rootBaseName"  rootBaseName  "recursive"
          , chkF "rootFile"      rootFile      $ p "recursive.fast.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
          , chkE 1 "recursive.fast.expct" (Assumed "clang") (Explicit "fast")
          , chkE 0 "recursive.fast.expct" (Assumed "gcc")   (Explicit "fast")
          ]

     , let sweetNum = 8
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE = chkExp sweet (mkEc p)
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "simple.noopt.clang.exe"
          , chkF "rootBaseName"  rootBaseName  "simple"
          , chkF "rootFile"      rootFile      $ p "simple.noopt.clang.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
            -- Note that opt is an unconstrained parameter, so it can
            -- match "noopt" or it can match something else.
          , chkE 0 "simple-opt.expct" "clang" (Explicit "opt")
          , chkE 1 "simple.expct"     "clang" NotSpecified
          , chkE 2 "simple.expct"     "clang" (Explicit "noopt")
          ]

     , let sweetNum = 9
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE = chkExp sweet (mkEc p)
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "simple.noopt.gcc.exe"
          , chkF "rootBaseName"  rootBaseName  "simple"
          , chkF "rootFile"      rootFile      $ p "simple.noopt.gcc.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
            -- Note that opt is an unconstrained parameter, so it can
            -- match "noopt" or it can match something else.
          , chkE 0 "simple-opt.expct"       "gcc" (Explicit "opt")
          , chkE 1 "simple.noopt-gcc.expct" "gcc" NotSpecified
          , chkE 2 "simple.noopt-gcc.expct" "gcc" (Explicit "noopt")
          ]

     , let sweetNum = 10
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE = chkExp sweet (mkEc p)
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "simple.opt-clang.exe"
          , chkF "rootBaseName"  rootBaseName  "simple"
          , chkF "rootFile"      rootFile      $ p "simple.opt-clang.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
            -- Note that opt is an unconstrained parameter, so it can
            -- match "noopt" or it can match something else.
          , chkE 0 "simple-opt.expct" "clang" (Explicit "opt")
          , chkE 1 "simple.expct"     "clang" NotSpecified
          ]

         -- This repeats a parameter value and also matches with a different
         -- value.  The duplicate value should be ignored, but both values should
         -- result in different Expectations.
     , let sweetNum = 7
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE = chkExp sweet (mkEc p)
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "simple.clang-noopt-clang.exe"
          , chkF "rootBaseName"  rootBaseName  "simple"
          , chkF "rootFile"      rootFile      $ p "simple.clang-noopt-clang.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
            -- Note that opt is an unconstrained parameter, so it can
            -- match "noopt" or it can match something else.
          , chkE 0 "simple-opt.expct" "clang" (Explicit "opt")
          , chkE 1 "simple.expct"     "clang" NotSpecified
          , chkE 2 "simple.expct"     "clang" (Explicit "noopt")
          ]

         -- This repeats a parameter with a different value, which causes a
         -- separate explicit match to be claimed for each value.  In addition,
         -- the optimization parameter has no value specified, so multiple values
         -- are found that could be tried, along with an expect file that doesn't
         -- supply a potential value for this parameter.
     , let sweetNum = 6
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE = chkExp sweet (mkEc p)
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "simple.clang-gcc.exe"
          , chkF "rootBaseName"  rootBaseName  "simple"
          , chkF "rootFile"      rootFile      $ p "simple.clang-gcc.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
          , chkE 1 "simple-opt.expct"       "clang" (Explicit "opt")
          , chkE 0 "simple-opt.expct"       "gcc"   (Explicit "opt")
          , chkE 2 "simple.expct"           "clang" NotSpecified
          , chkE 3 "simple.noopt-gcc.expct" "gcc"   NotSpecified
          , chkE 4 "simple.noopt-gcc.expct" "gcc"   (Explicit "noopt")
          ]

         -- n.b. simple-opt.gcc-exe is *not* matched: the rootname is
         -- "*.exe" so the '.' separator is required.

       -- Verify that Expectations are selected based on better ParamMatches
       -- (sorted).
     , let sweetNum = 0
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE n cs ca f c = chkExp sweet (mkEq cs ca p) n f c NotSpecified
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "alpha.exe"
          , chkF "rootBaseName"  rootBaseName  "alpha"
          , chkF "rootFile"      rootFile      $ p "alpha.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
          , chkE 0 Assumed  Assumed "alpha.clang.expct" (Explicit "clang")
          , chkE 1 Explicit Assumed "alpha.x.expct"     (Assumed "gcc")
          ]

       -- Verify that lengthening the expected filename by adding separators will
       -- not change the parameter identification but will select the longer
       -- expected filename *if* the parameter is higher (sorted on parameter
       -- name).
     , let sweetNum = 1
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE n cs ca f c = chkExp sweet (mkEq cs ca p) n f c NotSpecified
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "beta.exe"
          , chkF "rootBaseName"  rootBaseName  "beta"
          , chkF "rootFile"      rootFile      $ p "beta.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
            -- sorted: "c-compiler":"clang", "other":"x", so clang is preferred.
          , chkE 0 Explicit Assumed "beta......x.expct" (Assumed "gcc")
          , chkE 1 Assumed  Assumed "beta.clang.expct"  (Explicit "clang")
          ]

       -- This is the same as beta, but with the separator extensions on the
       -- other side.
     , let sweetNum = 4
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE n cs ca f c = chkExp sweet (mkEq cs ca p) n f c NotSpecified
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "gamma.exe"
          , chkF "rootBaseName"  rootBaseName  "gamma"
          , chkF "rootFile"      rootFile      $ p "gamma.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
            -- sorted: "c-compiler":"clang", "other":"x", so clang is preferred.
          , chkE 0 Assumed  Assumed "gamma.clang.expct"  (Explicit "clang")
          , chkE 1 Explicit Assumed "gamma.x......expct" (Assumed "gcc")
          ]

       -- This is the same as beta, but with a higher-precedence parameter
     , let sweetNum = 2
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE n cs ca f c = chkExp sweet (mkEq cs ca p) n f c NotSpecified
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "delta.exe"
          , chkF "rootBaseName"  rootBaseName  "delta"
          , chkF "rootFile"      rootFile      $ p "delta.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
            -- sorted: "a param":"y", "c-compiler":"clang" so "a param" is
            -- preferred.
          , chkE 1 Assumed Explicit "delta......y.expct" (Assumed "clang")
          , chkE 0 Assumed Explicit "delta......y.expct" (Assumed "gcc")
          ]

       -- This is the same as beta, but with the separator extensions on the
       -- other side.
     , let sweetNum = 3
           sweet = safeElem sweetNum sugar1
           chkF = chkFld sweet
           chkP = chkFld sweet
           chkE n cs ca f c = chkExp sweet (mkEq cs ca p) n f c NotSpecified
       in TT.testGroup ("Sweet #" <> show sweetNum)
          [
            chkF "rootMatchName" rootMatchName "epsilon.exe"
          , chkF "rootBaseName"  rootBaseName  "epsilon"
          , chkF "rootFile"      rootFile      $ p "epsilon.exe"
          , chkP "cubeParams"    cubeParams    $ validParams sugarCube
            -- sorted: "a param":"y", "c-compiler":"clang" so "a param" is
            -- preferred.
          , chkE 1 Assumed Explicit "epsilon.y......expct" (Assumed "clang")
          , chkE 0 Assumed Explicit "epsilon.y......expct" (Assumed "gcc")
          ]
     ]
