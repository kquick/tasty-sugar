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
  let exp0 f d l o = Expectation
                    { expectedFile = testInpPath </> f
                    , expParamsMatch = [ ("debug", d) , ("llvm", l) , ("opt", o)]
                    , associated = [ ("source", testInpPath </> "foo.c") ]
                    }
  let exp f d l o = exp0 f (Assumed d) (Explicit l) o
      testExp s i f d l o = testCase ("Exp #" <> show i)
                            $ safeElem i (expected s) @?= Just (exp f d l o)
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

          -- the direct match for foo.llvm10.O2.exe is foo.llvm10-O2-exp, with a
          -- fallback to foo.exp, but since llvm199 is not a valid value for the
          -- llvm parameter, and opt is an unconstrained value, the
          -- foo.llvm199.exp file can be matched as well.
          , testExp sweet 0 "foo.exp"           "no"  "llvm10" NotSpecified
          , testExp sweet 1 "foo.exp"           "yes" "llvm10" NotSpecified
          , testExp sweet 2 "foo.llvm10-O2-exp" "no"  "llvm10" (Explicit "O2")
          , testExp sweet 3 "foo.llvm10-O2-exp" "yes" "llvm10" (Explicit "O2")
        ]
      , TT.testGroup "Sweet #2" $
        let sweet = head $ drop 1 sweets in
          [
            testCase "root match" $ "foo.llvm13.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm13.exe" @=? rootFile sweet
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testExp sweet 0 "foo.exp" "no"  "llvm13" NotSpecified
          , testExp sweet 1 "foo.exp" "yes" "llvm13" NotSpecified
          ]
      , TT.testGroup "Sweet #3" $
        let sweet = head $ drop 2 sweets in
          [
            testCase "root match" $ "foo.llvm9.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm9.exe" @=? rootFile sweet
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testExp sweet 0 "foo.llvm9.exp" "no"  "llvm9" NotSpecified
          , testExp sweet 1 "foo.llvm9.exp" "yes" "llvm9" NotSpecified
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
  let exp p sf f d l o = Expectation
                         { expectedFile = p </> f
                         , expParamsMatch = [ ("debug", Assumed d)
                                            , ("llvm", l)
                                            , ("opt", o)
                                            ]
                         , associated = [ ("source", testInpPath </> sf) ]
                       }
      testExp s sf i f d l o =
        testCase ("Exp #" <> show i)
        $ safeElem i (expected s) @?= Just (exp testInpPath sf f d l o)
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
        let testExp' n d l = testExp sweet "cow.c" n "cow.O2.exp" d
                               (Assumed l) (Explicit "O2")
          in
          [
            testCase "root match" $ "cow-O2.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath2 </> "cow-O2.exe" @=? rootFile sweet
          , testCase "# expectations" $ 6 @=? length (expected sweet)
          , testExp' 0 "no"  "llvm10"
          , testExp' 1 "no"  "llvm13"
          , testExp' 2 "no"  "llvm9"
          , testExp' 3 "yes" "llvm10"
          , testExp' 4 "yes" "llvm13"
          , testExp' 5 "yes" "llvm9"
        ]
      , TT.testGroup "Sweet #2" $
        let sweet = head $ drop 1 sweets in
        let testExp' n d = testExp sweet "foo.c" n "foo.exp" d
                           (Explicit "llvm10") (Explicit "O1")
          in
          [
            testCase "root match" $ "foo.O1-llvm10.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath2 </> "foo.O1-llvm10.exe" @=? rootFile sweet
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testExp' 0 "no"
          , testExp' 1 "yes"
        ]
      , TT.testGroup "Sweet #3" $
        let sweet = head $ drop 2 sweets in
        let testExp' n e d o = testExp sweet "foo.c" n e d (Explicit "llvm10") o
          in
          [
            testCase "root match" $ "foo.llvm10.O2.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm10.O2.exe" @=? rootFile sweet
          , testCase "# expectations" $ 4 @=? length (expected sweet)
          , testExp' 0 "foo.exp"           "no"  NotSpecified
          , testExp' 1 "foo.exp"           "yes" NotSpecified
          , testExp' 2 "foo.llvm10-O2-exp" "no"  (Explicit "O2")
          , testExp' 3 "foo.llvm10-O2-exp" "yes" (Explicit "O2")
        ]
      , TT.testGroup "Sweet #4" $
        let sweet = head $ drop 3 sweets in
        let testExp' n e d =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp testInpPath2 "foo.c" e d (Explicit "llvm13") NotSpecified)
          in
          [
            testCase "root match" $ "foo.llvm13.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm13.exe" @=? rootFile sweet
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testExp' 0 "foo-llvm13.exp" "no"
          , testExp' 1 "foo-llvm13.exp" "yes"
          ]
      , TT.testGroup "Sweet #5" $
        let sweet = head $ drop 4 sweets in
        let testExp' n e d =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp testInpPath "foo.c" e d (Explicit "llvm9")  NotSpecified)
        in
          [
            testCase "root match" $ "foo.llvm9.exe" @=? rootMatchName sweet
          , testCase "root file" $ testInpPath </> "foo.llvm9.exe" @=? rootFile sweet
          , testCase "# expectations" $ 2 @=? length (expected sweet)
          , testExp' 0 "foo.llvm9.exp" "no"
          , testExp' 1 "foo.llvm9.exp" "yes"
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
  let exp0 p sf f d l o = Expectation
                    { expectedFile = p </> f
                    , expParamsMatch = [ ("debug", d) , ("llvm", l) , ("opt", o)]
                    , associated = [ ("source", testInpPath </> sf) ]
                    }
  let exp p sf f d l o = exp0 p sf f (Assumed d) l o
      testExp s sf i f d l o =
        testCase ("Exp #" <> show i)
        $ safeElem i (expected s) @?= Just (exp testInpPath sf f d l o)
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
            testExp' n e d l o =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp testInpPath "cow.c" e d (Assumed l) o)
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "cow-O2.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath2 </> "cow-O2.exe" @=? rootFile sweet
           , testCase "# expectations" $ 6 @=? length (expected sweet)
           , testExp' 0 "cow.O2.exp" "no"  "llvm10" (Explicit "O2")
           , testExp' 1 "cow.O2.exp" "no"  "llvm13" (Explicit "O2")
           , testExp' 2 "cow.O2.exp" "no"  "llvm9"  (Explicit "O2")
           , testExp' 3 "cow.O2.exp" "yes" "llvm10" (Explicit "O2")
           , testExp' 4 "cow.O2.exp" "yes" "llvm13" (Explicit "O2")
           , testExp' 5 "cow.O2.exp" "yes" "llvm9"  (Explicit "O2")
           ]


      , let sweetNum = 6
            sweet = head $ drop (sweetNum - 1) sweets
            testExp' n e d l o =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp (takeDirectory testInpPath3) "cow.c" e d (Explicit l) o)
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "cow.exe" @=? rootMatchName sweet
           , testCase "root file"
             $ takeDirectory testInpPath3 </> "llvm13/cow.exe" @=? rootFile sweet
           , testCase "# expectations" $ 4 @=? length (expected sweet)
           , testExp' 0 "O0/cow-llvm13.exp" "no"  "llvm13" NotSpecified
           , testExp' 1 "O0/cow-llvm13.exp" "no"  "llvm13" (Explicit "O0")
           , testExp' 2 "O0/cow-llvm13.exp" "yes" "llvm13" NotSpecified
           , testExp' 3 "O0/cow-llvm13.exp" "yes" "llvm13" (Explicit "O0")
           ]

      , let sweetNum = 7
            sweet = head $ drop (sweetNum - 1) sweets
            testExp' n e d l o =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp (takeDirectory testInpPath3) "cow.c" e d (Explicit l) o)
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "cow.exe" @=? rootMatchName sweet
           , testCase "root file"
             $ takeDirectory testInpPath3 </> "llvm13/opts/O3/cow.exe" @=? rootFile sweet
           , testCase "# expectations" $ 8 @=? length (expected sweet)
           , testExp' 0 "O0/cow-llvm13.exp" "no"  "llvm13" NotSpecified
           , testExp' 1 "O0/cow-llvm13.exp" "yes" "llvm13" NotSpecified
           -- Note: the expectations match an exp with O0 in the name, which
           -- would seem to conflict with O3, but since "opt" is a wildcard
           -- parameter there's no way for tasty-sweet to know that O3 and O0
           -- conflict, and the match on the llvm13 value makes that .exp more
           -- attractive than the generic one.
           , testExp' 2 "O0/cow-llvm13.exp" "no"  "llvm13" (Explicit "O0")
           , testExp' 3 "O0/cow-llvm13.exp" "no"  "llvm13" (Explicit "O3")
           , testExp' 4 "O0/cow-llvm13.exp" "no"  "llvm13" (Explicit "opts")
           , testExp' 5 "O0/cow-llvm13.exp" "yes" "llvm13" (Explicit "O0")
           , testExp' 6 "O0/cow-llvm13.exp" "yes" "llvm13" (Explicit "O3")
           , testExp' 7 "O0/cow-llvm13.exp" "yes" "llvm13" (Explicit "opts")
           ]

      , let sweetNum = 1
            sweet = head $ drop (sweetNum - 1) sweets
            exp' n = head $ drop (n-1) $ expected sweet
            tE1 n e d l o =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp (takeDirectory testInpPath3) "cow.c" e d l o)
            tE2 n e d l o =
              let lda = ("ld-config"
                        , takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                  ex = exp (takeDirectory testInpPath3) "cow.c" e d l o
              in testCase ("Exp #" <> show n)
                 $ safeElem n (expected sweet)
                 @?= Just (ex { associated = lda : associated ex })
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
           , tE1 0 "cow.exp"            "no"  (Assumed "llvm10")  NotSpecified
           , tE2 1 "cow.exp"            "no"  (Assumed "llvm9")   NotSpecified
           , tE1 2 "cow.exp"            "yes" (Assumed "llvm10")  NotSpecified
           , tE2 3 "cow.exp"            "yes" (Assumed "llvm9")   NotSpecified
           , tE1 4 "O0/cow-llvm13.exp"  "no"  (Explicit "llvm13") NotSpecified
           , tE1 5 "O0/cow.exp"         "no"  (Assumed "llvm10")  (Explicit "O0")
           , tE2 6 "O0/cow.exp"         "no"  (Assumed "llvm9")   (Explicit "O0")
           , tE1 7 "O0/cow-llvm13.exp"  "yes" (Explicit "llvm13") NotSpecified
           , tE1 8 "O0/cow.exp"         "yes" (Assumed "llvm10")  (Explicit "O0")
           , tE2 9 "O0/cow.exp"         "yes" (Assumed "llvm9")   (Explicit "O0")
           , tE1 10 "O0/cow-llvm13.exp" "no"  (Explicit "llvm13") (Explicit "O0")
           , tE1 11 "O0/cow-llvm13.exp" "yes" (Explicit "llvm13") (Explicit "O0")
           ]

      , let sweetNum = 2
            sweet = head $ drop (sweetNum - 1) sweets
            tExp n e d l o =
              let lda = ("ld-config"
                        , takeDirectory testInpPath3 </> "O0/llvm9/cow.lnk")
                  ex = exp (takeDirectory testInpPath3) "cow.c" e d (Explicit l) o
              in testCase ("Exp #" <> show n)
                 $ safeElem n (expected sweet)
                 @?= Just (ex { associated = lda : associated ex })
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "cow.exe" @=? rootMatchName sweet
           , testCase "root file" $ takeDirectory testInpPath3 </> "O0/llvm9/cow.exe" @=? rootFile sweet
           , testCase "# expectations" $ 4 @=? length (expected sweet)
           -- n.b. See note for Sweet #1
           , tExp 0 "cow.exp"    "no"  "llvm9" NotSpecified
           , tExp 1 "cow.exp"    "yes" "llvm9" NotSpecified
           , tExp 2 "O0/cow.exp" "no"  "llvm9" (Explicit "O0")
           , tExp 3 "O0/cow.exp" "yes" "llvm9" (Explicit "O0")
           ]

      , let sweetNum = 9
            sweet = head $ drop (sweetNum - 1) sweets
            tE1 n e d l o =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp (takeDirectory testInpPath3) "foo.c" e d (Explicit l) o)
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "foo.O1-llvm10.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath2 </> "foo.O1-llvm10.exe" @=? rootFile sweet
           , testCase "# expectations" $ 2 @=? length (expected sweet)
           , tE1 0 "llvm10/foo.exp" "no"  "llvm10" (Explicit "O1")
           , tE1 1 "llvm10/foo.exp" "yes" "llvm10" (Explicit "O1")
           ]

      , let sweetNum = 10
            sweet = head $ drop (sweetNum - 1) sweets
            tE1 n e d l o =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp testInpPath "foo.c" e d (Explicit l) o)
            tE3 n e d l o =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp (takeDirectory testInpPath3) "foo.c" e d (Explicit l) o)
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "foo.llvm10.O2.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath </> "foo.llvm10.O2.exe" @=? rootFile sweet
           , testCase "# expectations" $ 4 @=? length (expected sweet)
           , tE3 0 "llvm10/foo.exp"    "no"  "llvm10" NotSpecified
           , tE3 1 "llvm10/foo.exp"    "yes" "llvm10" NotSpecified
           , tE1 2 "foo.llvm10-O2-exp" "no"  "llvm10" (Explicit "O2")
           , tE1 3 "foo.llvm10-O2-exp" "yes" "llvm10" (Explicit "O2")
           ]

      , let sweetNum = 11
            sweet = head $ drop (sweetNum - 1) sweets
            tE1 n e d =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp testInpPath2 "foo.c" e d (Explicit "llvm13") NotSpecified)
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "foo.llvm13.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath </> "foo.llvm13.exe" @=? rootFile sweet
           , testCase "# expectations" $ 2 @=? length (expected sweet)
           , tE1 0 "foo-llvm13.exp" "no"
           , tE1 1 "foo-llvm13.exp" "yes"
           ]

      , let sweetNum = 12
            sweet = head $ drop (sweetNum - 1) sweets
            tE1 n e d =
              testCase ("Exp #" <> show n)
              $ safeElem n (expected sweet)
              @?= Just (exp testInpPath "foo.c" e d (Explicit "llvm9") NotSpecified)
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "foo.llvm9.exe" @=? rootMatchName sweet
           , testCase "root file" $ testInpPath </> "foo.llvm9.exe" @=? rootFile sweet
           , testCase "# expectations" $ 2 @=? length (expected sweet)
           , tE1 0 "foo.llvm9.exp" "no"
           , tE1 1 "foo.llvm9.exp" "yes"
           ]

      , let sweetNum = 5
            sweet = head $ drop (sweetNum - 1) sweets
            tE1 n e d o =
              let ex = exp0 (takeDirectory testInpPath3) "foo.c" e d (Explicit "llvm9") o
              in testCase ("Exp #" <> show n)
                 $ safeElem n (expected sweet)
                 @?= Just (ex { associated = [] })
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "frog.exe" @=? rootMatchName sweet
           , testCase "root file" $ takeDirectory testInpPath3 </> "gen/llvm9/frog.exe" @=? rootFile sweet
           , testCase "# expectations" $ 6 @=? length (expected sweet)
           , tE1 0 "want/frog-llvm9-no.exp"  (Explicit "no")  NotSpecified
           , tE1 1 "want/frog-yes.exp"       (Explicit "yes") NotSpecified
           , tE1 2 "want/frog-llvm9-no.exp"  (Explicit "no")  (Explicit "gen")
           , tE1 3 "want/frog-llvm9-no.exp"  (Explicit "no")  (Explicit "want")
           , tE1 4 "want/frog-yes.exp"       (Explicit "yes")  (Explicit "gen")
           , tE1 5 "want/frog-yes.exp"       (Explicit "yes")  (Explicit "want")
           ]

      , let sweetNum = 3
            sweet = head $ drop (sweetNum - 1) sweets
            tE1 n e d o =
              let ex = exp0 (takeDirectory testInpPath3) "foo.c" e d (Explicit "llvm10") o
              in testCase ("Exp #" <> show n)
                 $ safeElem n (expected sweet)
                 @?= Just (ex { associated = [] })
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "frog.exe" @=? rootMatchName sweet
           , testCase "root file" $ takeDirectory testInpPath3 </> "gen/llvm10/frog.exe" @=? rootFile sweet
           , testCase "# expectations" $ 6 @=? length (expected sweet)
           , tE1 0 "want/frog-no.exp"  (Explicit "no")  NotSpecified
           , tE1 1 "want/frog-yes.exp" (Explicit "yes") NotSpecified
           , tE1 2 "want/frog-no.exp"  (Explicit "no")  (Explicit "gen")
           , tE1 3 "want/frog-no.exp"  (Explicit "no")  (Explicit "want")
           , tE1 4 "want/frog-yes.exp" (Explicit "yes") (Explicit "gen")
           , tE1 5 "want/frog-yes.exp" (Explicit "yes") (Explicit "want")
           ]

      , let sweetNum = 4
            sweet = head $ drop (sweetNum - 1) sweets
            tE1 n e d o =
              let ex = exp0 (takeDirectory testInpPath3) "foo.c" e d (Explicit "llvm13") o
              in testCase ("Exp #" <> show n)
                 $ safeElem n (expected sweet)
                 @?= Just (ex { associated = [] })
        in TT.testGroup ("Sweet #" <> show sweetNum) $
           [
             testCase "root match" $ "frog.exe" @=? rootMatchName sweet
           , testCase "root file" $ takeDirectory testInpPath3 </> "gen/llvm13/frog.exe" @=? rootFile sweet
           , testCase "# expectations" $ 6 @=? length (expected sweet)
           , tE1 0 "want/frog-no.exp"  (Explicit "no")  NotSpecified
           , tE1 1 "want/frog-yes.exp" (Explicit "yes") NotSpecified
           , tE1 2 "want/frog-no.exp"  (Explicit "no")  (Explicit "gen")
           , tE1 3 "want/frog-no.exp"  (Explicit "no")  (Explicit "want")
           , tE1 4 "want/frog-yes.exp" (Explicit "yes") (Explicit "gen")
           , tE1 5 "want/frog-yes.exp" (Explicit "yes") (Explicit "want")
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
