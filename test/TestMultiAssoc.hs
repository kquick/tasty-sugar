{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestMultiAssoc ( multiAssocTests ) where

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
              , associatedNames = [ ("exe", "exe"), ("obj", "o"), ("include", "h"), ("c++-include", "hh") ]
              , validParams = [ ("arch", Just ["x86", "ppc"])
                              , ("form", Just ["base", "refined"])
                              ]
              }

multiAssocTests :: [TT.TestTree]
multiAssocTests =
  let (sugar1,s1desc) = findSugarIn sugarCube sample1
      params = [ ("arch", Just ["x86", "ppc"])
               , ("form", Just ["base", "refined"])
               ]
      gmgTests =
        [ \e -> (testCase "sugar0" $ do
                    "global-max-good" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected"
                  (gmgExpectedTests "global-max-good" gmgAssoc1) (expected e)
        , \e -> (testCase "sugar1" $ do
                    "jumpfar" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected"
                  (gmgExpectedTests "jumpfar" gmgAssoc2) (expected e)
        , \e -> (testCase "sugar2" $ do
                    "looping" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected"
                  (gmgExpectedTests "looping" (gmgAssoc3 "looping")) (expected e)
        , \e -> (testCase "sugar2" $ do
                    "switching" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected"
                  (gmgExpectedTests2 "switching" gmgAssoc4) (expected e)
        , \e -> (testCase "sugar2" $ do
                    "tailrecurse" @=? (inputName e)
                    params @=? (cubeParams e))
                : testArray "expected"
                  (gmgExpectedTests "tailrecurse" (gmgAssoc3 "tailrecurse")) (expected e)
        ]
      gmgAssoc1 =
        let nm = testInpPath </> "global-max-good"
        in [ [ ("exe", nm <> ".x86.exe") ]
           , [ ("exe", nm <> ".x86.exe") ]
           , [ ("exe", nm <> ".ppc.exe") , ("obj", nm <> ".ppc.o") ]
           , [ ("exe", nm <> ".ppc.exe") , ("obj", nm <> ".ppc.o") ]
           ]
      gmgAssoc2 =
        let nm = testInpPath </> "jumpfar"
        in [ [ ("exe", nm <> ".x86.exe") , ("include", nm <> ".h") ]
           , [ ("exe", nm <> ".x86.exe") , ("include", nm <> ".h") ]
           , [ ("exe", nm <> ".ppc.exe")
             , ("include", nm <> ".h")
             ]
           , [ ("exe", nm <> ".ppc.exe")
             , ("include", nm <> ".h")
             ]
           ]
      gmgAssoc3 n =
        let nm = testInpPath </> n
        in [ [ ("exe", nm <> ".x86.exe") ]
           , [ ("exe", nm <> ".x86.exe") ]
           , [ ("exe", nm <> ".ppc.exe") ]
           , [ ("exe", nm <> ".ppc.exe") ]
           ]
      gmgAssoc4 =
        let nm = testInpPath </> "switching"
        in [ [
               ("exe", nm <> ".x86.exe")
             , ("obj", nm <> ".x86.o")
             , ("include", nm <> ".h")
             , ("c++-include", nm <> ".hh")
             ]
           , [
               ("exe", nm <> ".x86.exe")
             , ("obj", nm <> ".x86.o")
             , ("include", nm <> ".h")
             , ("c++-include", nm <> ".hh")
             ]
           , [
               ("exe", nm <> ".ppc.exe")
             , ("obj", nm <> ".ppc.o")
             , ("include", nm <> ".h")
             , ("c++-include", nm <> ".hh")
             ]
           ]
      gmgExpectedTests nm assoc =
        [ \e -> (testCase "expect0" $ do
                    (testInpPath </> nm <> ".x86.expected") @=? expectedFile e
                    [ ("arch", Explicit "x86"), ("form", Assumed "base") ] @=? expParamsMatch e
                    (assoc !! 0) @=? associated e
                ) : []
        , \e -> (testCase "expect1" $ do
                    (testInpPath </> nm <> ".x86.expected") @=? expectedFile e
                    [ ("arch", Explicit "x86"), ("form", Assumed "refined") ] @=? expParamsMatch e
                    (assoc !! 1) @=? associated e
                ) : []
        , \e -> (testCase "expect2" $ do
                    (testInpPath </> nm <> ".ppc.expected") @=? expectedFile e
                    [ ("arch", Explicit "ppc"), ("form", Assumed "base") ] @=? expParamsMatch e
                    (assoc !! 2) @=? associated e
                ) : []
        , \e -> (testCase "expect3" $ do
                    (testInpPath </> nm <> ".ppc.expected") @=? expectedFile e
                    [ ("arch", Explicit "ppc"), ("form", Assumed "refined") ] @=? expParamsMatch e
                    (assoc !! 3) @=? associated e
                ) : []
        ]
      gmgExpectedTests2 nm assoc =
        [ \e -> (testCase "expect0" $ do
                    (testInpPath </> nm <> ".x86.base-expected") @=? expectedFile e
                    [ ("arch", Explicit "x86"), ("form", Explicit "base") ] @=? expParamsMatch e
                    (assoc !! 0) @=? associated e
                ) : []
        , \e -> (testCase "expect1" $ do
                    (testInpPath </> nm <> ".x86.refined-expected") @=? expectedFile e
                    [ ("arch", Explicit "x86"), ("form", Explicit "refined") ] @=? expParamsMatch e
                    (assoc !! 1) @=? associated e
                ) : []
        , \e -> (testCase "expect2" $ do
                    (testInpPath </> nm <> ".ppc.base-expected") @=? expectedFile e
                    [ ("arch", Explicit "ppc"), ("form", Explicit "base") ] @=? expParamsMatch e
                    (assoc !! 2) @=? associated e
                ) : []
        ]

  in [ testCase "valid sample" $ 46 @=? length sample1
     , sugarTestEq "correct found count" sugarCube sample1 5 length
     ]
     <> testArray "found" gmgTests sugar1


sample1 = lines [r|
global-max-good.c
global-max-good.ppc.o
global-max-good.ppc.exe
global-max-good.ppc.expected
global-max-good.x86.exe
global-max-good.x86.expected
jumpfar.c
jumpfar.h
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
switching.h
switching.hh
switching_llvm.c
switching_llvm.h
switching_llvm.x86.exe
switching_many.c
switching_many_llvm.c
switching_many_llvm.x86.exe
switching_many.ppc.exe
switching.ppc.base-expected
switching.ppc.o
switching.ppc.other
switching.ppc.exe
switching.x86.base-expected
switching.x86.exe
switching.x86.o
switching.x86.orig
switching.x86.refined-expected
switching.x86.refined-expected-orig
switching.x86.refined-last-actual
tailrecurse.c
tailrecurse.ppc.exe
tailrecurse.ppc.expected
tailrecurse.x86.exe
tailrecurse.x86.expected
|]
