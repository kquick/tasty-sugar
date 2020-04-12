module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Sugar
import TestMultiAssoc
import TestNoAssoc
import TestSingleAssoc
import TestWildcard
import TestUtils


main = defaultMain $
       testGroup "tasty-sweet tests"
       [ testProperty "empty file list" $ \cube -> null $ fst $ findSugarIn cube []
       , testGroup "no associated file" $ noAssocTests
       , testGroup "single associated file" $ singleAssocTests
       , testGroup "multiple associated files" $ multiAssocTests
       , testGroup "wildcard tests" $ wildcardAssocTests
       ]
