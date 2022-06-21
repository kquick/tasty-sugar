module TestFileSys ( fileSysTests ) where

import qualified Data.Text as T
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.RawString.QQ
import           Text.Show.Pretty


testInpPath = "test/data/single"
testInpPath2 = "test/data/second"

testParams = [ ("llvm", Just [ "llvm9", "llvm10", "llvm13" ])
             , ("opt", Nothing )
             ]

sugarCube1 = mkCUBE
             { inputDirs = [ testInpPath ]
             , rootName = "*.exe"
             , validParams = testParams
             , associatedNames = [ ("config", "config")
                                 , ("ld-config", "lnk")
                                 , ("source", "c")
                                 ]
             }

sugarCube2 = mkCUBE
             { inputDirs = [ testInpPath, "foo/baz", testInpPath2, "/foo/bar" ]
             , rootName = "*.exe"
             , validParams = testParams
             , associatedNames = [ ("config", "config")
                                 , ("ld-config", "lnk")
                                 , ("source", "c")
                                 ]
             }


fileSysTests :: IO [TT.TestTree]
fileSysTests = do tsts <- sequence [ fsTests1, fsTests2 ]
                  return [ TT.testGroup "FileSys" $ concat tsts ]


fsTests1 :: IO [TT.TestTree]
fsTests1 = do
  sweets <- findSugar sugarCube1
  -- putStrLn $ ppShow sweets
  return
    [ TT.testGroup "Cube 1"
      [ testCase "correct # of sweets" $ 3 @=? length sweets
      , TT.testGroup "Sweet #1" $
        let sweet = head sweets in
          [
            testCase "root match" $ "foo.llvm10.O2.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm10.O2.exe" @=? rootFile sweet
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("llvm", Explicit "llvm10")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm10-O2-exp"
            , expParamsMatch = [ ("llvm", Explicit "llvm10")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
        ]
      , TT.testGroup "Sweet #2" $
        let sweet = head $ drop 1 sweets in
          [
            testCase "root match" $ "foo.llvm13.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm13.exe" @=? rootFile sweet
          , testCase "# expectations" $ 1 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("llvm", Explicit "llvm13")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          ]
      , TT.testGroup "Sweet #3" $
        let sweet = head $ drop 2 sweets in
          [
            testCase "root match" $ "foo.llvm9.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm9.exe" @=? rootFile sweet
          , testCase "# expectations" $ 1 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm9.exp"
            , expParamsMatch = [ ("llvm", Explicit "llvm9")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          ]
      ]
    ]


fsTests2 :: IO [TT.TestTree]
fsTests2 = do
  sweets <- findSugar sugarCube2
  -- putStrLn $ ppShow sweets
  return
    [ TT.testGroup "Cube 2"
      [ testCase "correct # of sweets" $ 5 @=? length sweets
      , TT.testGroup "Sweet #1" $
        let sweet = head sweets in
          [
            testCase "root match" $ "cow-O2.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath2 </> "cow-O2.exe" @=? rootFile sweet
          , testCase "# expectations" $ 3 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "cow.O2.exp"
            , expParamsMatch = [ ("llvm", Assumed "llvm9")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "cow.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "cow.O2.exp"
            , expParamsMatch = [ ("llvm", Assumed "llvm10")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "cow.c") ]
            }
          , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "cow.O2.exp"
            , expParamsMatch = [ ("llvm", Assumed "llvm13")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "cow.c") ]
            }
        ]
      , TT.testGroup "Sweet #2" $
        let sweet = head $ drop 1 sweets in
          [
            testCase "root match" $ "foo.O1-llvm10.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath2 </> "foo.O1-llvm10.exe" @=? rootFile sweet
          , testCase "# expectations" $ 1 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("llvm", Explicit "llvm10")
                               , ("opt", Explicit "O1")
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
        ]
      , TT.testGroup "Sweet #3" $
        let sweet = head $ drop 2 sweets in
          [
            testCase "root match" $ "foo.llvm10.O2.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm10.O2.exe" @=? rootFile sweet
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("llvm", Explicit "llvm10")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm10-O2-exp"
            , expParamsMatch = [ ("llvm", Explicit "llvm10")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
        ]
      , TT.testGroup "Sweet #4" $
        let sweet = head $ drop 3 sweets in
          [
            testCase "root match" $ "foo.llvm13.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm13.exe" @=? rootFile sweet
          , testCase "# expectations" $ 1 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            { expectedFile = testInpPath2 </> "foo-llvm13.exp"
            , expParamsMatch = [ ("llvm", Explicit "llvm13")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          ]
      , TT.testGroup "Sweet #5" $
        let sweet = head $ drop 4 sweets in
          [
            testCase "root match" $ "foo.llvm9.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm9.exe" @=? rootFile sweet
          , testCase "# expectations" $ 1 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm9.exp"
            , expParamsMatch = [ ("llvm", Explicit "llvm9")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          ]
      ]
    ]
