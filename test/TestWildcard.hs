{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| This test verifies behavior when the mkCUBE sourceName is a generic
  wildcard ("*") which will match everything in the target directory.
  Proper tasty-sugar behavior will check for the expected and
  associated files and identify those as associated test files *not*
  include those as input sourceFiles.
-}

module TestWildcard ( wildcardAssocTests ) where

import           Data.List
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.RawString.QQ


testInpPath = "test/samples"


sample1 = lines [r|
foo
foo.exp
bar.exp
bar.
cow.moo
cow.mooexp
readme.txt
dog.bark
dog.bark-exp
|]


wildcardAssocTests :: [TT.TestTree]
wildcardAssocTests =
  let junk = 1
      -- (sugar1,s1desc) = findSugarIn sugarCube1 sample1
  in [ testCase "valid sample" $ 10 @=? length sample1

     -- The first CUBE uses the default set of separators, and the expected
     -- suffix does not limit which separators can preceed the suffix.
     , TT.testGroup "with seps" $
       let sugarCube = mkCUBE
                       { sourceName = "*"
                       , expectedSuffix = "exp"
                       , inputDir = testInpPath
                       }
       in [ sugarTestEq "correct found count" sugarCube sample1 3 length
          , sugarTestEq "foo is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, sourceFile x == testInpPath </> "foo" ]
          , sugarTestEq "bar is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, sourceFile x == testInpPath </> "bar." ]
          , sugarTestEq "dog.bark is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, sourceFile x == testInpPath </> "dog.bark" ]
          -- n.b. cow.moo is not matched because there is no separator in cow.mooexp
          ]

     -- The second CUBE specifies no separators: the expected suffix
     -- immediately follows the source name with no separator.
     , TT.testGroup "no seps" $
       let sugarCube = mkCUBE
                       { sourceName = "*"
                       , expectedSuffix = "exp"
                       , separators = ""
                       , inputDir = testInpPath
                       }
       in [ sugarTestEq "correct found count" sugarCube sample1 2 length
          , sugarTestEq "bar is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, sourceFile x == testInpPath </> "bar." ]
          , sugarTestEq "cow.moo is a case" sugarCube sample1 1 $
            \sugar -> length [ x | x <- sugar, sourceFile x == testInpPath </> "cow.moo" ]
            -- n.b. neither foo nor dog.bark is matched because they
            -- both have a character between the name and the
            -- expectedSuffix that is not a known separator
          ]


     -- The third CUBE specifies one of the separators as part of the
     -- suffix, so only that separator is matched.
     , TT.testGroup "seps with suffix sep" $
       let sugarCube = mkCUBE
                       { sourceName = "*"
                       , expectedSuffix = ".exp"
                       , inputDir = testInpPath
                       }
       in  [ sugarTestEq "correct found count" sugarCube sample1 2 length
           , sugarTestEq "foo is a case" sugarCube sample1 1 $
             \sugar -> length [ x | x <- sugar, sourceFile x == testInpPath </> "foo" ]
           -- n.b. dog.bark is not matched because the separator in
           -- dog.bark-exp is not the separator specified for the
           -- expected suffix.
           , sugarTestEq "bar is a case" sugarCube sample1 1 $
             \sugar -> length [ x | x <- sugar, sourceFile x == testInpPath </> "bar." ]
           -- n.b. cow.moo is not matched because there is no separator in cow.mooexp
           ]

     ]
