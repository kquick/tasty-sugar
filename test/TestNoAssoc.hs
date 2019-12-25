{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestNoAssoc ( noAssocTests ) where

import           Data.List
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.RawString.QQ


testInpPath = "tests/samples"

sugarCube = mkCUBE
              { sourceName = "*.c"
              , expectedSuffix = "expected"
              , inputDir = testInpPath
              , associatedNames = []
              , validParams = [ ("arch", Just ["x86", "ppc"])
                              , ("form", Just ["base", "refined"])
                              ]
              }

noAssocTests :: [TT.TestTree]
noAssocTests =
  let (sugar1,s1desc) = findSugarIn sugarCube sample1
      params = [ ("arch", Just ["x86", "ppc"])
               , ("form", Just ["base", "refined"])
               ]
      gmgTests =
        [ \e -> (testCase "sugar0" $ do
                    "global-max-good" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected" (gmgExpectedTests "global-max-good") (expected e)
        , \e -> (testCase "sugar1" $ do
                    "jumpfar" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected" (gmgExpectedTests "jumpfar") (expected e)
        , \e -> (testCase "sugar2" $ do
                    "looping" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected" (gmgExpectedTests "looping") (expected e)
        , \e -> (testCase "sugar2" $ do
                    "switching" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected" (gmgExpectedTests2 "switching") (expected e)
        , \e -> (testCase "sugar2" $ do
                    "tailrecurse" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected" (gmgExpectedTests "tailrecurse") (expected e)
        ]
      gmgExpectedTests nm =
        [ \e -> (testCase "expect0" $ do
                    (testInpPath </> nm <> ".x86.expected") @=? expectedFile e
                    [ ("arch", Explicit "x86"), ("form", Assumed "base") ] @=? expParamsMatch e
                    [] @=? associated e
                ) : []
        , \e -> (testCase "expect1" $ do
                    (testInpPath </> nm <> ".x86.expected") @=? expectedFile e
                    [ ("arch", Explicit "x86"), ("form", Assumed "refined") ] @=? expParamsMatch e
                    [] @=? associated e
                ) : []
        , \e -> (testCase "expect2" $ do
                    (testInpPath </> nm <> ".ppc.expected") @=? expectedFile e
                    [ ("arch", Explicit "ppc"), ("form", Assumed "base") ] @=? expParamsMatch e
                    [] @=? associated e
                ) : []
        , \e -> (testCase "expect3" $ do
                    (testInpPath </> nm <> ".ppc.expected") @=? expectedFile e
                    [ ("arch", Explicit "ppc"), ("form", Assumed "refined") ] @=? expParamsMatch e
                    [] @=? associated e
                ) : []
        ]
      gmgExpectedTests2 nm =
        [ \e -> (testCase "expect0" $ do
                    (testInpPath </> nm <> ".x86.base-expected") @=? expectedFile e
                    [ ("arch", Explicit "x86"), ("form", Explicit "base") ] @=? expParamsMatch e
                    [] @=? associated e
                ) : []
        , \e -> (testCase "expect1" $ do
                    (testInpPath </> nm <> ".x86.refined-expected") @=? expectedFile e
                    [ ("arch", Explicit "x86"), ("form", Explicit "refined") ] @=? expParamsMatch e
                    [] @=? associated e
                ) : []
        , \e -> (testCase "expect2" $ do
                    (testInpPath </> nm <> ".ppc.base-expected") @=? expectedFile e
                    [ ("arch", Explicit "ppc"), ("form", Explicit "base") ] @=? expParamsMatch e
                    [] @=? associated e
                ) : []
        ]
      gmgAssociatedTest =
        [
        ]

  in [ testCase "valid sample" $ 37 @=? length sample1
     , sugarTestEq "correct found count" sugarCube sample1 5 length
     ]
     <> testArray "found" gmgTests sugar1



sample1 = lines [r|
global-max-good.c
global-max-good.ppc.exe
global-max-good.ppc.expected
global-max-good.x86.exe
global-max-good.x86.expected
jumpfar.c
jumpfar.ppc.exe
jumpfar.ppc.expected
jumpfar.x86.exe
jumpfar.x86.expected
looping.c
looping.ppc.exe
looping.ppc.expected
looping.x86.exe
looping.x86.expected
Makefile
README.org
switching.c
switching_llvm.c
switching_llvm.x86.exe
switching_many.c
switching_many_llvm.c
switching_many_llvm.x86.exe
switching_many.ppc.exe
switching.ppc.base-expected
switching.ppc.exe
switching.x86.base-expected
switching.x86.exe
switching.x86.refined-expected
switching.x86.refined-expected-orig
switching.x86.refined-last-actual
tailrecurse.c
tailrecurse.ppc.exe
tailrecurse.ppc.expected
tailrecurse.x86.exe
tailrecurse.x86.expected
|]
