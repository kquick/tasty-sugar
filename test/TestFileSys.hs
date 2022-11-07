module TestFileSys ( fileSysTests ) where

import           Data.Either ( lefts, rights )
import qualified Data.List as L
import           Data.Maybe ( isJust )
import qualified Data.Text as T
import           System.FilePath ( (</>), takeDirectory )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.RawString.QQ
import           Text.Show.Pretty


testInpPath = "test/data/single"
testInpPath2 = "test/data/second"
testInpPath3 = "test/builds/*"

testParams = [ ("llvm", Just [ "llvm9", "llvm10", "llvm13" ])
             , ("debug", Just [ "yes", "no" ])
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

sugarCube3 = mkCUBE
             { inputDirs = [ testInpPath
                           , "foo/baz"
                           , testInpPath2
                           , testInpPath3
                           ]
             , rootName = "*.exe"
             , validParams = testParams
             , associatedNames = [ ("config", "config")
                                 , ("ld-config", "lnk")
                                 , ("source", "c")
                                 ]
             }


fileSysTests :: IO [TT.TestTree]
fileSysTests = do tsts <- sequence [ fsTests1, fsTests2, fsTests3 ]
                  return [ TT.testGroup "FileSys" $ concat tsts ]


fsTests1 :: IO [TT.TestTree]
fsTests1 = do
  sweets <- findSugar sugarCube1
  cands <- findCandidates sugarCube1 testInpPath
  -- putStrLn $ ppShow sweets
  let chkCandidate = checkCandidate sugarCube1 (const $ rights cands)
                     testInpPath [] 0
  return
    [ TT.testGroup "Cube 1"
      [ testCase "correct # of sweets" $ 3 @=? length sweets

      , testCase "correct # of candidate warnings" $ 0 @=? length (lefts cands)
      , testCase "correct # of candidate files" $ 11 @=? length (rights cands)
      , TT.testGroup "Candidates"
        [
          chkCandidate "bar.exe" []
        , chkCandidate "cow.O2.exp" [("opt", Explicit "O2")]
        , chkCandidate "cow.c" []
        , chkCandidate "foo.c" []
        , chkCandidate "foo.exp" []
        , chkCandidate "foo.llvm10-O2-exp" [("llvm", Explicit "llvm10")
                                           ,("opt", Explicit "O2")]
        , chkCandidate "foo.llvm10.O2.exe" [("llvm", Explicit "llvm10")
                                           ,("opt", Explicit "O2")]
        , chkCandidate "foo.llvm13.exe" [("llvm", Explicit "llvm13")]
        , chkCandidate "foo.llvm199.exp" [("opt", Explicit "llvm199")]
        , chkCandidate "foo.llvm9.exe" [("llvm", Explicit "llvm9")]
        , chkCandidate "foo.llvm9.exp" [("llvm", Explicit "llvm9")]
        ]

      , TT.testGroup "Sweet #1" $
        let sweet = head sweets in
          [
            testCase "root match" $ "foo.llvm10.O2.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm10.O2.exe" @=? rootFile sweet
          , testCase "# expectations" $ 4 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Explicit "llvm10")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Explicit "llvm10")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm10-O2-exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Explicit "llvm10")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm10-O2-exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Explicit "llvm10")
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
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Explicit "llvm13")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Explicit "llvm13")
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
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm9.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Explicit "llvm9")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm9.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Explicit "llvm9")
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
  cands <- concat <$> mapM (findCandidates sugarCube2) (inputDirs sugarCube2)
  let warns = lefts cands
  -- putStrLn $ ppShow sweets
  let chkCand d = checkCandidate sugarCube2 (const $ rights cands) d [] 0
  let chkCandidate1 = chkCand testInpPath
  let chkCandidate2 = chkCand testInpPath2
  return
    [ TT.testGroup "Cube 2"
      [ testCase "correct # of sweets" $ 5 @=? length sweets

      , testCase "correct # of candidate warnings" $ 2 @=? length warns
      , testCase "Warn about foo/baz"
        $ isJust (L.find ("]foo/baz does not exist" `L.isSuffixOf`) warns)
        @? ("foo/baz warning missing in " <> show warns)
      , testCase "Warn about /foo/bar"
        $ "/foo/bar does not exist" `elem` warns
        @? ("/foo/bar warning missing in " <> show warns)
      , testCase "correct # of candidate files" $ 14 @=? length (rights cands)
      , TT.testGroup "Candidates"
        [
          chkCandidate1 "bar.exe" []
        , chkCandidate1 "cow.O2.exp" [("opt", Explicit "O2")]
        , chkCandidate1 "cow.c" []
        , chkCandidate1 "foo.c" []
        , chkCandidate1 "foo.exp" []
        , chkCandidate1 "foo.llvm10-O2-exp" [("llvm", Explicit "llvm10")
                                            ,("opt", Explicit "O2")]
        , chkCandidate1 "foo.llvm10.O2.exe" [("llvm", Explicit "llvm10")
                                            ,("opt", Explicit "O2")]
        , chkCandidate1 "foo.llvm13.exe" [("llvm", Explicit "llvm13")]
        , chkCandidate1 "foo.llvm199.exp" [("opt", Explicit "llvm199")]
        , chkCandidate1 "foo.llvm9.exe" [("llvm", Explicit "llvm9")]
        , chkCandidate1 "foo.llvm9.exp" [("llvm", Explicit "llvm9")]
        , chkCandidate2 "cow-O2.exe" [("opt", Explicit "O2")]
        , chkCandidate2 "foo-llvm13.exp" [("llvm", Explicit "llvm13")]
        , chkCandidate2 "foo.O1-llvm10.exe" [("llvm", Explicit "llvm10")
                                            , ("opt", Explicit "O1")]
        ]

      , TT.testGroup "Sweet #1" $
        let sweet = head sweets in
          [
            testCase "root match" $ "cow-O2.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath2 </> "cow-O2.exe" @=? rootFile sweet
          , testCase "# expectations" $ 6 @=? length (expected sweet)
          , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "cow.O2.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Assumed "llvm10")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "cow.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "cow.O2.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Assumed "llvm13")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "cow.c") ]
            }
          , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "cow.O2.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Assumed "llvm9")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "cow.c") ]
            }
          , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "cow.O2.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Assumed "llvm10")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "cow.c") ]
            }
          , testCase "Exp #5" $ head (drop 4 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "cow.O2.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Assumed "llvm13")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "cow.c") ]
            }
          , testCase "Exp #6" $ head (drop 5 $ expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "cow.O2.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Assumed "llvm9")
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
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Explicit "llvm10")
                               , ("opt", Explicit "O1")
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Explicit "llvm10")
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
          , testCase "# expectations" $ 4 @=? length (expected sweet)
          , testCase "Exp #1" $ head (expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Explicit "llvm10")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            {
              expectedFile = testInpPath </> "foo.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Explicit "llvm10")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm10-O2-exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Explicit "llvm10")
                               , ("opt", Explicit "O2")
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm10-O2-exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Explicit "llvm10")
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
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath2 </> "foo-llvm13.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Explicit "llvm13")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath2 </> "foo-llvm13.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Explicit "llvm13")
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
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm9.exp"
            , expParamsMatch = [ ("debug", Assumed "no")
                               , ("llvm", Explicit "llvm9")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
            Expectation
            { expectedFile = testInpPath </> "foo.llvm9.exp"
            , expParamsMatch = [ ("debug", Assumed "yes")
                               , ("llvm", Explicit "llvm9")
                               , ("opt", NotSpecified)
                               ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
          ]
      ]
    ]

fsTests3 :: IO [TT.TestTree]
fsTests3 = do
  sweets <- findSugar sugarCube3
  -- putStrLn $ ppShow sweets
  cands <- concat <$> mapM (findCandidates sugarCube3) (inputDirs sugarCube3)
  let warns = lefts cands
  let chkCand t s = checkCandidate sugarCube3 (const $ rights cands) t s 0
  let chkCandidate1 = chkCand testInpPath []
  let chkCandidate2 = chkCand testInpPath2 []
  let tbDir = "test/builds"
  return
    [ TT.testGroup "Cube 3"
      [ testCase "correct # of sweets" $ 12 @=? length sweets

      , testCase "correct # of candidate warnings" $ 1 @=? length warns
      , testCase "Warn about foo/baz"
        $ isJust (L.find ("]foo/baz does not exist" `L.isSuffixOf`) warns)
        @? ("foo/baz warning missing in " <> show warns)
      , testCase "correct # of candidate files" $ 29 @=? length (rights cands)
      , TT.testGroup "Candidates"
        [
          chkCandidate1 "bar.exe" []
        , chkCandidate1 "cow.O2.exp" [("opt", Explicit "O2")]
        , chkCandidate1 "cow.c" []
        , chkCandidate1 "foo.c" []
        , chkCandidate1 "foo.exp" []
        , chkCandidate1 "foo.llvm10-O2-exp" [("llvm", Explicit "llvm10")
                                            ,("opt", Explicit "O2")
                                            ]
        , chkCandidate1 "foo.llvm10.O2.exe" [("llvm", Explicit "llvm10")
                                            ,("opt", Explicit "O2")
                                            ]
        , chkCandidate1 "foo.llvm13.exe" [("llvm", Explicit "llvm13")]
        , chkCandidate1 "foo.llvm199.exp"  [("opt", Explicit "llvm199")]
        , chkCandidate1 "foo.llvm9.exe" [("llvm", Explicit "llvm9")]
        , chkCandidate1 "foo.llvm9.exp" [("llvm", Explicit "llvm9")]
        , chkCandidate2 "cow-O2.exe" [("opt", Explicit "O2")]
        , chkCandidate2 "foo-llvm13.exp" [("llvm", Explicit "llvm13")]
        , chkCandidate2 "foo.O1-llvm10.exe" [("llvm", Explicit "llvm10")
                                            , ("opt", Explicit "O1")
                                            ]
        , chkCand tbDir ["O0"] "cow-llvm13.exp" [("llvm", Explicit "llvm13")
                                                ,("opt", Explicit "O0")
                                                ]
        , chkCand tbDir ["O0"] "cow.exe" [("opt", Explicit "O0")]
        , chkCand tbDir ["O0"] "cow.exp" [("opt", Explicit "O0")]
        , chkCand tbDir ["O0", "llvm9"] "cow.exe" [("llvm", Explicit "llvm9")
                                                  ,("opt", Explicit "O0")
                                                  ]
        , chkCand tbDir ["O0", "llvm9"] "cow.lnk" [("llvm", Explicit "llvm9")
                                                  ,("opt", Explicit "O0")
                                                  ]
        , chkCand tbDir [] "cow.exp" []
        , chkCand tbDir ["gen", "llvm10"] "frog.exe" [("llvm", Explicit "llvm10")
                                                     ,("opt", Explicit "gen")
                                                     ]
        , chkCand tbDir ["gen", "llvm13"] "frog.exe" [("llvm", Explicit "llvm13")
                                                     ,("opt", Explicit "gen")
                                                     ]
        , chkCand tbDir ["gen", "llvm9"] "frog.exe" [("llvm", Explicit "llvm9")
                                                    ,("opt", Explicit "gen")
                                                    ]
        , chkCand tbDir ["llvm10"] "foo.exp" [("llvm", Explicit "llvm10")]
        , chkCand tbDir ["llvm13"] "cow.exe" [("llvm", Explicit "llvm13")]
        , chkCand tbDir ["llvm13", "opts", "O3"] "cow.exe"
          [("llvm", Explicit "llvm13")
          ,("opt", Explicit "O3")
          ,("opt", Explicit "opts")
          ]
        , chkCand tbDir ["want"] "frog-llvm9-no.exp" [("debug", Explicit "no")
                                                     ,("llvm", Explicit "llvm9")
                                                     ,("opt", Explicit "want")
                                                     ]
        , chkCand tbDir ["want"] "frog-no.exp" [("debug", Explicit "no")
                                               ,("opt", Explicit "want")
                                               ]
        , chkCand tbDir ["want"] "frog-yes.exp" [("debug", Explicit "yes")
                                                ,("opt", Explicit "want")
                                                ]
        ]

      , let sweetNum = 8
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "cow-O2.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath2 </> "cow-O2.exe" @=? rootFile sweet
           , testCase "# expectations" $ 6 @=? length (expected sweet)
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath </> "cow.O2.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Assumed "llvm10")
                                , ("opt", Explicit "O2")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath </> "cow.O2.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Assumed "llvm13")
                                , ("opt", Explicit "O2")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
             Expectation
             {
               expectedFile = testInpPath </> "cow.O2.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Assumed "llvm9")
                                , ("opt", Explicit "O2")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath </> "cow.O2.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Assumed "llvm10")
                                , ("opt", Explicit "O2")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #5" $ head (drop 4 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath </> "cow.O2.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Assumed "llvm13")
                                , ("opt", Explicit "O2")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #6" $ head (drop 5 $ expected sweet) @?=
             Expectation
             {
               expectedFile = testInpPath </> "cow.O2.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Assumed "llvm9")
                                , ("opt", Explicit "O2")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           ]


      , let sweetNum = 6
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "cow.exe" @=? rootMatchName sweet
           , testCase "root file"
             $ takeDirectory testInpPath3 </> "llvm13/cow.exe" @=? rootFile sweet
           , testCase "# expectations" $ 4 @=? length (expected sweet)
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           ]

      , let sweetNum = 7
            sweet = head $ drop (sweetNum - 1) sweets
            exp n = head $ drop (n-1) $ expected sweet
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "cow.exe" @=? rootMatchName sweet
           , testCase "root file"
             $ takeDirectory testInpPath3 </> "llvm13/opts/O3/cow.exe" @=? rootFile sweet
           , testCase "# expectations" $ 8 @=? length (expected sweet)
           , testCase "Exp #1" $ exp 1 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #2" $ exp 2 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           -- Note: the expectations match an exp with O0 in the name, which
           -- would seem to conflict with O3, but since "opt" is a wildcard
           -- parameter there's no way for tasty-sweet to know that O3 and O0
           -- conflict, and the match on the llvm13 value makes that .exp more
           -- attractive than the generic one.
           , testCase "Exp #3" $ exp 3 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #4" $ exp 4 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "O3")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #5" $ exp 5 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "opts")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #6" $ exp 6 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #7" $ exp 7 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "O3")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #8" $ exp 8 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "opts")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           ]

      , let sweetNum = 1
            sweet = head $ drop (sweetNum - 1) sweets
            exp n = head $ drop (n-1) $ expected sweet
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           -- n.b. Although O0 appears in the root file and in the O0/cow.exp
           -- expected files in Exp 3 and Exp 4 below, O0 is a wildcard, so it's
           -- also possible to match NotSpecified for the "opt" parameter, and in
           -- that case, the base cow.exp from Exp 1 and Exp 2 are found.
           -- Eliminating these would involve matching the O0 portions of the
           -- filepath even though it is not associated with any parameter; that
           -- becomes even more complex and the results will be less
           -- consistent/explainable than the current situation, thus the current
           -- functionality.
           [
             testCase "root match" $ "cow.exe" @=? rootMatchName sweet
           , testCase "root file"
             $ takeDirectory testInpPath3 </> "O0/cow.exe" @=? rootFile sweet
           , testCase "# expectations" $ 12 @=? length (expected sweet)
           , testCase "Exp #1" $ exp 1 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Assumed "llvm10")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "cow.c")
                            ]
             }
           , testCase "Exp #2" $ exp 2 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Assumed "llvm9")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("ld-config", takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                            , ("source", testInpPath </> "cow.c")
                            ]
             }
           , testCase "Exp #3" $ exp 3 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Assumed "llvm10")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "cow.c")
                            ]
             }
           , testCase "Exp #4" $ exp 4 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Assumed "llvm9")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("ld-config", takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                            , ("source", testInpPath </> "cow.c")
                            ]
             }
           , testCase "Exp #5" $ exp 5 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "cow.c")
                            ]
             }


           , testCase "Exp #6" $ exp 6 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Assumed "llvm10")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #7" $ exp 7 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Assumed "llvm9")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("ld-config", takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                            , ("source", testInpPath </> "cow.c")
                            ]
             }
           , testCase "Exp #8" $ exp 8 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #9" $ exp 9 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Assumed "llvm10")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #10" $ exp 10 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Assumed "llvm9")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("ld-config", takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                            , ("source", testInpPath </> "cow.c")
                            ]
             }
           , testCase "Exp #11" $ exp 11 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #12" $ exp 12 @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("source", testInpPath </> "cow.c") ]
             }
           ]

      , let sweetNum = 2
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "cow.exe" @=? rootMatchName sweet
           , testCase "root file" $ takeDirectory testInpPath3 </> "O0/llvm9/cow.exe" @=? rootFile sweet
           , testCase "# expectations" $ 4 @=? length (expected sweet)
           -- n.b. See note for Sweet #1
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("ld-config", takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                            , ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("ld-config", takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                            , ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("ld-config", takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                            , ("source", testInpPath </> "cow.c") ]
             }
           , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "O0" </> "cow.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", Explicit "O0")
                                ]
             , associated = [ ("ld-config", takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                            , ("source", testInpPath </> "cow.c") ]
             }
           ]

      , let sweetNum = 9
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "foo.O1-llvm10.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath2 </> "foo.O1-llvm10.exe" @=? rootFile sweet
           , testCase "# expectations" $ 2 @=? length (expected sweet)
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "llvm10" </> "foo.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", Explicit "O1")
                                ]
             , associated = [ ("source", testInpPath </> "foo.c") ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "llvm10" </> "foo.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", Explicit "O1")
                                ]
             , associated = [ ("source", testInpPath </> "foo.c") ]
             }
           ]

      , let sweetNum = 10
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "foo.llvm10.O2.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath </> "foo.llvm10.O2.exe" @=? rootFile sweet
           , testCase "# expectations" $ 4 @=? length (expected sweet)
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "llvm10" </> "foo.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "foo.c") ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             {
               expectedFile = takeDirectory testInpPath3 </> "llvm10" </> "foo.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "foo.c") ]
             }
           , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath </> "foo.llvm10-O2-exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", Explicit "O2")
                                ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
           , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath </> "foo.llvm10-O2-exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", Explicit "O2")
                                ]
            , associated = [ ("source", testInpPath </> "foo.c") ]
            }
           ]

      , let sweetNum = 11
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "foo.llvm13.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath </> "foo.llvm13.exe" @=? rootFile sweet
           , testCase "# expectations" $ 2 @=? length (expected sweet)
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath2 </> "foo-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "foo.c") ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath2 </> "foo-llvm13.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "foo.c") ]
             }
           ]

      , let sweetNum = 12
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "foo.llvm9.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath </> "foo.llvm9.exe" @=? rootFile sweet
           , testCase "# expectations" $ 2 @=? length (expected sweet)
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath </> "foo.llvm9.exp"
             , expParamsMatch = [ ("debug", Assumed "no")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "foo.c") ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             { expectedFile = testInpPath </> "foo.llvm9.exp"
             , expParamsMatch = [ ("debug", Assumed "yes")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ("source", testInpPath </> "foo.c") ]
             }
           ]

      , let sweetNum = 5
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "frog.exe" @=? rootMatchName sweet
           , testCase "root file" $ takeDirectory testInpPath3 </> "gen/llvm9/frog.exe" @=? rootFile sweet
           , testCase "# expectations" $ 6 @=? length (expected sweet)
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-llvm9-no.exp"
             , expParamsMatch = [ ("debug", Explicit "no")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-yes.exp"
             , expParamsMatch = [ ("debug", Explicit "yes")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ]
             }
           , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-llvm9-no.exp"
             , expParamsMatch = [ ("debug", Explicit "no")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", Explicit "gen")
                                ]
             , associated = [ ]
             }
           , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-llvm9-no.exp"
             , expParamsMatch = [ ("debug", Explicit "no")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", Explicit "want")
                                ]
             , associated = [ ]
             }
           , testCase "Exp #5" $ head (drop 4 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-yes.exp"
             , expParamsMatch = [ ("debug", Explicit "yes")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", Explicit "gen")
                                ]
             , associated = [ ]
             }
           , testCase "Exp #6" $ head (drop 5 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-yes.exp"
             , expParamsMatch = [ ("debug", Explicit "yes")
                                , ("llvm", Explicit "llvm9")
                                , ("opt", Explicit "want")
                                ]
             , associated = [ ]
             }
           ]

      , let sweetNum = 3
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "frog.exe" @=? rootMatchName sweet
           , testCase "root file" $ takeDirectory testInpPath3 </> "gen/llvm10/frog.exe" @=? rootFile sweet
           , testCase "# expectations" $ 6 @=? length (expected sweet)
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-no.exp"
             , expParamsMatch = [ ("debug", Explicit "no")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-yes.exp"
             , expParamsMatch = [ ("debug", Explicit "yes")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ]
             }
           , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-no.exp"
             , expParamsMatch = [ ("debug", Explicit "no")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", Explicit "gen")
                                ]
             , associated = [ ]
             }
           , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-no.exp"
             , expParamsMatch = [ ("debug", Explicit "no")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", Explicit "want")
                                ]
             , associated = [ ]
             }
           , testCase "Exp #5" $ head (drop 4 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-yes.exp"
             , expParamsMatch = [ ("debug", Explicit "yes")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", Explicit "gen")
                                ]
             , associated = [ ]
             }
           , testCase "Exp #6" $ head (drop 5 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-yes.exp"
             , expParamsMatch = [ ("debug", Explicit "yes")
                                , ("llvm", Explicit "llvm10")
                                , ("opt", Explicit "want")
                                ]
             , associated = [ ]
             }
           ]

      , let sweetNum = 4
            sweet = head $ drop (sweetNum - 1) sweets
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "frog.exe" @=? rootMatchName sweet
           , testCase "root file" $ takeDirectory testInpPath3 </> "gen/llvm13/frog.exe" @=? rootFile sweet
           , testCase "# expectations" $ 6 @=? length (expected sweet)
           , testCase "Exp #1" $ head (drop 0 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-no.exp"
             , expParamsMatch = [ ("debug", Explicit "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ]
             }
           , testCase "Exp #2" $ head (drop 1 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-yes.exp"
             , expParamsMatch = [ ("debug", Explicit "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", NotSpecified)
                                ]
             , associated = [ ]
             }
           , testCase "Exp #3" $ head (drop 2 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-no.exp"
             , expParamsMatch = [ ("debug", Explicit "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "gen")
                                ]
             , associated = [ ]
             }
           , testCase "Exp #4" $ head (drop 3 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-no.exp"
             , expParamsMatch = [ ("debug", Explicit "no")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "want")
                                ]
             , associated = [ ]
             }
           , testCase "Exp #5" $ head (drop 4 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-yes.exp"
             , expParamsMatch = [ ("debug", Explicit "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "gen")
                                ]
             , associated = [ ]
             }
           , testCase "Exp #6" $ head (drop 5 $ expected sweet) @?=
             Expectation
             { expectedFile = takeDirectory testInpPath3 </> "want/frog-yes.exp"
             , expParamsMatch = [ ("debug", Explicit "yes")
                                , ("llvm", Explicit "llvm13")
                                , ("opt", Explicit "want")
                                ]
             , associated = [ ]
             }
           ]

      , testCase "correct # of foo sweets" $ 4 @=?
        length (filter (("foo" `L.isPrefixOf`) . rootMatchName) sweets)
      , testCase "correct # of cow sweets" $ 5 @=?
        length (filter (("cow" `L.isPrefixOf`) . rootMatchName) sweets)
      , testCase "correct # of frog sweets" $ 3 @=?
        length (filter (("frog" `L.isPrefixOf`) . rootMatchName) sweets)
      , testCase "foo sweet roots"
        $ [ "test/data/second/foo.O1-llvm10.exe"
          , "test/data/single/foo.llvm10.O2.exe"
          , "test/data/single/foo.llvm13.exe"
          , "test/data/single/foo.llvm9.exe"] @=?
        rootFile <$> (filter (("foo" `L.isPrefixOf`) . rootMatchName) sweets)
      , testCase "cow sweet roots"
        $ [ "test/builds/O0/cow.exe"
          , "test/builds/O0/llvm9/cow.exe"
          , "test/builds/llvm13/cow.exe"
          , "test/builds/llvm13/opts/O3/cow.exe"
          , "test/data/second/cow-O2.exe"
          ] @=?
        rootFile <$> (filter (("cow" `L.isPrefixOf`) . rootMatchName) sweets)
      , testCase "frog sweet roots"
        $ [ "test/builds/gen/llvm10/frog.exe"
          , "test/builds/gen/llvm13/frog.exe"
          , "test/builds/gen/llvm9/frog.exe"] @=?
        rootFile <$> (filter (("frog" `L.isPrefixOf`) . rootMatchName ) sweets)
      ]
    ]
