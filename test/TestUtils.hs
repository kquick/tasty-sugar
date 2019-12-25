{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestUtils where

import qualified Control.Exception as E
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.Sugar


instance Arbitrary CUBE where
  arbitrary = CUBE <$> arbitrary <*> arbitrary <*> arbitrary <*>
              arbitrary <*> arbitrary <*> arbitrary


testWithFailInfo desc test testInp = E.catch (test testInp) (\(_e::HUnitFailure) -> assertFailure (show desc))


eqTestWithFailInfo desc val = assertEqual (show desc) val


testArray name elemTests lst =
  let testElem (n,e,t) = TT.testGroup (name <> " elem#" <> show n) $ t e
      testEach = map testElem $ zip3 [0..] lst elemTests
  in testCase (name <> " count") (assertEqual "length" (length elemTests) (length lst)) : testEach


sugarTest name cube sample test =
  let (r,d) = findSugarIn cube sample
  in testCase name $ testWithFailInfo d test r


sugarTestEq name cube sample expVal test =
  let (r,d) = findSugarIn cube sample
  in testCase name $ eqTestWithFailInfo d expVal $ test r
