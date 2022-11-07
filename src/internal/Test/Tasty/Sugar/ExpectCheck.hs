{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Function to find expected results files for a specific root file,
-- along with any parameter values identified by the root file.

module Test.Tasty.Sugar.ExpectCheck
  (
    findExpectation
  , removeNonExplicitMatchingExpectations
  )
  where

import           Control.Applicative ( (<|>) )
import           Control.Monad
import           Data.Bifunctor ( first )
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Maybe ( isNothing )

import           Test.Tasty.Sugar.AssocCheck
import           Test.Tasty.Sugar.Candidates
import           Test.Tasty.Sugar.Iterations
import           Test.Tasty.Sugar.ParamCheck
import           Test.Tasty.Sugar.Types


-- | Finds the possible expected files matching the selected
-- source. There will be either one or none.
findExpectation :: CUBE
                -> [ParameterPattern]
                -> CandidateFile   --  original name of source
                -> [CandidateFile] --  all of the names to choose from
                -> ([NamedParamMatch], CandidateFile) -- param constraints from the root name
                -> (Maybe ( Sweets, SweetExplanation ), IterStat)
findExpectation pat params rootN allNames (rootPMatches, matchPrefix) =
  let r = first (mkSweet . trimExpectations)
          $ observeIAll
          $ do guard (not $ null candidates)
               expectedSearch
                 matchPrefix
                 rootPMatches
                 seps params expSuffix o
                 candidates


      o = associatedNames pat
      seps = separators pat
      expSuffix = expectedSuffix pat
      sfxMatch = if null expSuffix then const True else (expSuffix `L.isSuffixOf`)
      candidates = filter possible allNames
      possible f = and [ candidateFile matchPrefix `L.isPrefixOf` candidateFile f
                       , rootN /= f
                       ]
      mkSweet e = Just
                  $ Sweets { rootMatchName = candidateFile rootN
                           , rootBaseName = candidateFile matchPrefix
                           , rootFile = candidateToPath rootN
                           , cubeParams = validParams pat
                           , expected = L.sortBy (compare `on` expectedFile) e
                           }

      -- The expectedSearch tries various combinations and ordering of
      -- parameter values, separators, and such to find all valid
      -- expected file matches.  However, the result is an
      -- over-sampling, so this function trims the excess and unwanted
      -- expectations.
      trimExpectations :: [Expectation] -> [Expectation]
      trimExpectations =
        -- If a parameter is Explicitly matched, discard any
        -- Expectation with the same Assumed matches.
        removeNonExplicitMatchingExpectations
        -- remove duplicates (uses the Eq instance for Expectation
        -- that ignores the order of the expParamsMatch and associated
        -- to ensure that different ordering with the same values
        -- doesn't cause multiple Expectation.
        . L.nub

  in case r of
       (Nothing, stats) -> (Nothing, stats)
       (Just r', stats) | [] <- expected r' -> (Nothing, stats)
       (Just r', stats) ->
         ( Just ( r'
                , SweetExpl { rootPath = candidateToPath rootN
                            , base = candidateToPath matchPrefix
                            , expectedNames =
                                filter sfxMatch (candidateToPath <$> candidates)
                            , results = r'
                            })
         , stats )


-- Find all Expectations matching this rootMatch.
--
-- Note that rootPVMatches may contain multiple entries for the same parameter
-- value: the root file name may contain these duplications.  The code here
-- should be careful to check against each value instead of assuming just one.
expectedSearch :: CandidateFile
               -> [NamedParamMatch]
               -> Separators
               -> [ParameterPattern]
               -> FileSuffix
               -> [ (String, FileSuffix) ]
               -> [CandidateFile]
               -> LogicI Expectation
expectedSearch rootPrefix rootPVMatches seps params expSuffix assocNames allNames =
  do let expMatch cf = and [ candidateMatchPrefix seps rootPrefix cf
                           , candidateMatchSuffix seps expSuffix rootPrefix cf
                           ]

     let unconstrained = fst <$> L.filter (isNothing . snd) params

     (rmatch, pvals) <- getSinglePVals rootPVMatches params
                        -- If some of rootPVMatches were related to values that
                        -- might have been useable for an unconstrained
                        -- parameter, then we also need to consider roots that
                        -- don't match those unconstrained values (because those
                        -- might not be a match for that parameter):
                        <|>
                        (if null unconstrained
                          then mzero
                          else let unConstr = (`elem` unconstrained) . fst
                                   rm = L.filter (not . unConstr) rootPVMatches
                               in if null rm
                                  then mzero
                                  else getSinglePVals rm params
                        )


     efile <- eachFrom "exp candidate"
              $ L.reverse
              $ L.sortBy (compare `on` matchStrength . fmap snd . candidatePMatch)
              $ filter expMatch allNames
     guard $ isCompatible pvals efile

     let onlyOneOfEach (p,v) r = case lookup p r of
                                   Nothing -> (p,v) : r
                                   Just _ -> r
     rAndeMatches <- return (foldr onlyOneOfEach rmatch (candidatePMatch efile))
                     <|> (if null unconstrained
                           then mzero
                          else let unConstr = (`elem` unconstrained) . fst
                                   rm = filter (not . unConstr) (candidatePMatch efile)
                               in if null rm
                                  then mzero
                                  else return (foldr onlyOneOfEach rmatch rm)
                         )

     let pmatch = namedPMatches rAndeMatches pvals
     assocFiles <- getAssoc rootPrefix seps
                   pmatch
                   assocNames allNames
     return $ Expectation { expectedFile = candidateToPath efile
                          , associated = fmap candidateToPath <$> assocFiles
                          , expParamsMatch = L.sort pmatch
                          }



-- | Determines the best Expectations to use from a list of Expectations that may
-- have different parameter match status against an expected file.  When two
-- Expectations differ only in an Explicit v.s. Assumed (or wildcard) the
-- Explicit is preferred.  Expectations with more parameter matches are preferred
-- over those with less.

removeNonExplicitMatchingExpectations :: [Expectation] -> [Expectation]
removeNonExplicitMatchingExpectations =
  let removeNonExplicits e l =
        let (similarExpl, diffExpl) = L.partition (cmpPVals e) l
            cmpPVals ref ps =
              -- Compare the two on the intersection subset of parameters
              if length (expParamsMatch ref) < length (expParamsMatch ps)
              then expPVals ref ref == expPVals ref ps
              else expPVals ps ps == expPVals ps ref
            expPVals ref ps =
              -- Compare parameters by comparing the values of matching names
              let ps' = expParamsMatch ps
                  ref' = expParamsMatch ref
                  refNames = fst <$> ref'
              in (\n -> lookup n ps' >>= getParamVal) <$> refNames
        in if null similarExpl
           then e : l
           else (pmatchMax expParamsMatch e <$> similarExpl) <> diffExpl
  in foldr removeNonExplicits mempty
