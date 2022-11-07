module Main where

import Control.Monad ( unless )
import qualified Data.List as L
import Test.Tasty
import Test.Tasty.HUnit
import Text.Show.Pretty

import Test.Tasty.Sugar.Types
import Test.Tasty.Sugar.ExpectCheck


main = defaultMain $
  let sample =
        [
          Expectation
          {
            expectedFile = "test.file"
          , expParamsMatch =
            [ ("foo", Explicit "a")
            , ("bar", Explicit "b")
            , ("cow", Assumed "moo")
            ]
          , associated = []
          }

        , Expectation
          {
            expectedFile = "test.file"
          , expParamsMatch =
            [ ("foo", Explicit "a")
            , ("bar", Explicit "b")
            , ("cow", Assumed "milk")
            ]
          , associated = []
          }

        -- KWQ: inverse here, more specific 0th entry supercedes this one
        -- , Expectation
        --   {
        --     expectedFile = "test.file"
        --   , expParamsMatch =
        --     [ ("foo", Explicit "a")
        --     , ("cow", Assumed "moo")
        --     ]
        --   , associated = []
        --   }

        , Expectation
          {
            expectedFile = "test.file"
          , expParamsMatch =
            [ ("foo", Explicit "other")
            , ("bar", Explicit "b")
            , ("cow", Assumed "moo")
            ]
          , associated = []
          }

        , Expectation
          {
            expectedFile = "test.file"
          , expParamsMatch =
            [ ("foo", Assumed "a")
            , ("bar", Explicit "b")
            , ("cow", Assumed "moo")
            ]
          , associated = []
          }

        -- -- KWQ: more specific than the 0th entry, so this supercedes that entry (separate test)
        -- , Expectation
        --             { expectedFile = "test.file"
        --             , expParamsMatch =
        --               [ ("foo", Explicit "a")
        --               , ("bar", Explicit "b")
        --               , ("cow", Assumed "moo")
        --               , ("frog", Explicit "croak")
        --               ]
        --             , associated = []
        --             }

        ]
  in testGroup "Expected trimming"
     [ testCase "non-explicit removals" $
       pCmpExp (init sample)
       (collateExpectations sample)

     , testCase "supermatch supercedes" $
       -- Normally the trimming occurs on Expectations which all have
       -- a matching named set of expParamsMatch even though the
       -- values may be different.  If, however, and Expectation
       -- exists which has _more_ params than the rest, it will
       -- supercede any Expectation that has the same set but fewer,
       -- as this test checks.  This test is not critical to overall
       -- functionality, but serves to capture behavior.
       --
       -- Here, "adding" replaces the first element of sample because it
       -- otherwise matches sample but has more parameters.
       let adding = Expectation
                    { expectedFile = "test.file"
                    , expParamsMatch =
                      [ ("foo", Explicit "a")
                      , ("bar", Explicit "b")
                      , ("cow", Assumed "moo")
                      , ("frog", Explicit "croak")
                      ]
                    , associated = []
                    }
           test l = pCmpExp ((take 2 $ tail $ init sample)
                             <> [adding]
                             <> (drop 3 $ init sample)
                            )
                    (collateExpectations l)
       in mapM_ test (L.permutations $ adding : sample)

     , testCase "submatch removed" $
       -- This is the inverse of the supermatch: an entry is added
       -- that has fewer params, and it is elided in favor of an entry
       -- that has matching params plus additional params.
       let adding = Expectation
             {
               expectedFile = "test.file"
             , expParamsMatch =
               [ ("foo", Explicit "a")
               , ("cow", Assumed "moo")
               ]
             , associated = []
             }
           test l = pCmpExp (init sample) (collateExpectations l)
       in mapM_ test (L.permutations $ adding : sample)

     , testCase "superset and subset distinct" $
       -- As a variation of the supermatch and submatch tests, if an
       -- Expectation exists that has a different number of params but
       -- the params that are present are not a strict subset, then
       -- that is simply treated as a different Expectation than the
       -- others and doesn't affect matching removal.
       let adding = [ Expectation
                      { expectedFile = "test.file"
                      , expParamsMatch =
                          [ ("foo", Explicit "a")
                          , ("bar", Explicit "bell")
                          , ("cow", Assumed "moo")
                          , ("frog", Explicit "croak")
                          ]
                      , associated = []
                      }
                    , Expectation
                      { expectedFile = "test.file"
                      , expParamsMatch =
                          [ ("foo", Explicit "a")
                          , ("cow", Explicit "moo")
                            -- Explicit "moo" here against Assumed "moo" plus two
                            -- more explicits above... this gets removed.  Note
                            -- also that this matches the first element of
                            -- sample, but that element and the adding element
                            -- above are incompatible, so this will be associated
                            -- with one or the other, but not both.
                          ]
                      , associated = []
                      }
                    ]
           test l = pCmpExp (take 1 adding <> init sample) (collateExpectations l)
       in mapM_ test (L.permutations $ adding <> sample)


     , testCase "distinct if different" $
       let adding = [ Expectation
                      { expectedFile = "test.file"
                      , expParamsMatch =
                          [ ("foo", Explicit "a")
                          , ("bar", Explicit "bell")
                          , ("cow", Assumed "moo")
                          ]
                      , associated = []
                      }
                    ]
           test l = pCmpExp (adding <> init sample) (collateExpectations l)
       in mapM test (L.permutations $ adding <> sample)
          >> return ()

     ]


pCmpExp expected actual =
  unless (expected `elem` L.permutations actual) $
  assertFailure $ unlines
  ["MISMATCH ---vvv---"
  ,"----- Expected [" <> show (length expected) <> "]:"
  , ppShow expected
  ,"----- Actual [" <> show (length actual) <> "]:"
  , ppShow actual
  ,"MISMATCH ---^^^---"
  ]
