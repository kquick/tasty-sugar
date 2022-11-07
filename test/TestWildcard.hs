{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| This test verifies behavior when the mkCUBE rootName is a generic
  wildcard ("*") which will match everything in the target directory.
  Proper tasty-sugar behavior will check for the expected and
  associated files and identify those as associated test files *not*
  include those as input rootFiles.
-}

module TestWildcard ( wildcardAssocTests ) where

import           Data.List
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.RawString.QQ


testInpPath = "some/path/to/test/samples"


sample1 cube = fmap (makeCandidate cube testInpPath [])
          $ filter (not . null)
          $ lines [r|
foo
foo.exp
foo.ex
bar.exp
bar.
bar-ex
cow.moo
cow.mooexp
cow.mooex
cow.mooexe
readme.txt
dog.bark
dog.bark-exp
|]


wildcardAssocTests :: [TT.TestTree]
wildcardAssocTests =
     [ let sugarCube = mkCUBE
                       { rootName = "*"
                       , expectedSuffix = "exp"
                       , inputDirs = [ testInpPath ]
                       , associatedNames = [ ("extern", "ex") ]
                       }
           p = (testInpPath </>)
       in testCase "valid sample" $ 13 @=? length (sample1 sugarCube)

     -- The first CUBE uses the default set of separators, and the expected
     -- suffix does not limit which separators can preceed the suffix.
     , TT.testGroup "with default seps" $
       let sugarCube = mkCUBE
                       { rootName = "*"
                       , expectedSuffix = "exp"
                       , inputDirs = [ testInpPath ]
                       , associatedNames = [ ("extern", "ex") ]
                       }
           p = (testInpPath </>)
           exp e = Expectation { expectedFile = p e
                               , associated = []
                               , expParamsMatch = []
                               }
           expE e a = (exp e) { associated = [("extern", p a)] }
           sw m b f e = Sweets { rootBaseName = b
                               , rootMatchName = m
                               , rootFile = p f
                               , cubeParams = []
                               , expected = e
                               }
       in [ sugarTestEq "correct found count" sugarCube sample1 5 length

            -- foo.ex is an associated name for foo, but removing its
            -- extension makes it a sibling for the expected file and
            -- therefore a valid root as well.
          , sugarTestEq "foo is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, rootMatchName x == "foo" ]
          , sugarTestEq "foo.ex is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, rootMatchName x == "foo.ex" ]

            -- Similarly, bar.ex is an associated name, but also
            -- matches the expected Suffix when its .ex suffix is
            -- removed.
          , sugarTestEq "bar. is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, rootMatchName x == "bar." ]
          , sugarTestEq "bar-ex is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, rootMatchName x == "bar-ex" ]

          , sugarTestEq "dog.bark is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, rootFile x == p "dog.bark" ]
          -- n.b. cow.moo is not matched because there is no separator in cow.mooexp
          , testCase "full results" $
            compareBags "default result"
            (fst $ findSugarIn sugarCube (sample1 sugarCube)) $
            let p = (testInpPath </>) in
              [ sw "foo"      "foo"      "foo"      [ expE "foo.exp" "foo.ex" ]
              , sw "foo.ex"   "foo"      "foo.ex"   [ exp "foo.exp" ]
              , sw "bar."     "bar"      "bar."     [ expE "bar.exp" "bar-ex" ]
              , sw "bar-ex"   "bar"      "bar-ex"   [ exp "bar.exp" ]
              , sw "dog.bark" "dog.bark" "dog.bark" [ exp "dog.bark-exp" ]
              ]
          ]

     -- The second CUBE specifies no separators: the expected suffix
     -- immediately follows the source name with no separator.
     , TT.testGroup "no seps" $
       let sugarCube = mkCUBE
                       { rootName = "*"
                       , expectedSuffix = "exp"
                       , separators = ""
                       , inputDirs = [ testInpPath ]
                       , associatedNames = [ ("extern", "ex") ]
                       }
           p = (testInpPath </>)
           exp e = Expectation { expectedFile = p e
                               , associated = []
                               , expParamsMatch = []
                               }
           expE e a = (exp e) { associated = [("extern", p a)] }
           sw m b f e = Sweets { rootBaseName = b
                               , rootMatchName = m
                               , rootFile = p f
                               , cubeParams = []
                               , expected = e
                               }
       in [ sugarTestEq "correct found count" sugarCube sample1 2 length
          , sugarTestEq "bar is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, rootFile x == p "bar." ]
          , sugarTestEq "cow.moo is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, rootFile x == p "cow.moo" ]
            -- n.b. neither foo nor dog.bark is matched because they
            -- both have a character between the name and the
            -- expectedSuffix that is not a known separator
          , testCase "full results" $
            compareBags "default result"
            (fst $ findSugarIn sugarCube (sample1 sugarCube)) $
            let p = (testInpPath </>) in
              [ sw "bar."    "bar."    "bar."    [ exp "bar.exp" ]
              , sw "cow.moo" "cow.moo" "cow.moo" [ expE "cow.mooexp" "cow.mooex" ]
              ]
          ]


     -- The third CUBE specifies one of the separators as part of the
     -- suffix, so only that separator is matched.
     , TT.testGroup "seps with suffix sep" $
       let sugarCube = mkCUBE
                       { rootName = "*"
                       , expectedSuffix = ".exp"
                       , inputDirs = [ testInpPath ]
                       , associatedNames = [ ("extern", "ex") ]
                       }
           p = (testInpPath </>)
           exp e = Expectation { expectedFile = p e
                               , associated = []
                               , expParamsMatch = []
                               }
           expE e a = (exp e) { associated = [("extern", p a)] }
           sw m b f e = Sweets { rootBaseName = b
                               , rootMatchName = m
                               , rootFile = p f
                               , cubeParams = []
                               , expected = e
                               }
       in  [ sugarTestEq "correct found count" sugarCube sample1 4 length

             -- see notes for default seps tests above

           , sugarTestEq "foo is a case" sugarCube sample1 1 $
             \sugar -> length [ x | x <- sugar, rootMatchName x == "foo" ]
           , sugarTestEq "foo.ex is a case" sugarCube sample1 1 $
             \sugar -> length [ x | x <- sugar, rootMatchName x == "foo.ex" ]

           -- n.b. dog.bark is not matched because the separator in
           -- dog.bark-exp is not the separator specified for the
           -- expected suffix.

           , sugarTestEq "bar. is a case" sugarCube sample1 1 $
             \sugar -> length [ x | x <- sugar, rootMatchName x == "bar." ]
           , sugarTestEq "bar-ex is a case" sugarCube sample1 1 $
             \sugar -> length [ x | x <- sugar, rootMatchName x == "bar-ex" ]

           -- n.b. cow.moo is not matched because there is no separator in cow.mooexp
          , testCase "full results" $
            compareBags "default result"
            (fst $ findSugarIn sugarCube (sample1 sugarCube)) $
            let p = (testInpPath </>) in
              [ sw "foo"    "foo" "foo"    [ expE "foo.exp" "foo.ex" ]
              , sw "foo.ex" "foo" "foo.ex" [ exp "foo.exp" ]
              , sw "bar."   "bar" "bar."   [ expE "bar.exp" "bar-ex" ]
              , sw "bar-ex" "bar" "bar-ex" [ exp "bar.exp" ]
              ]
           ]

     ]
