{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Function to find expected results files for a specific root file,
-- along with any parameter values identified by the root file.

module Test.Tasty.Sugar.ExpectCheck
  (
    findExpectation
  , removeNonExplicitMatchingExpectations
  )
  where

import           Control.Monad
import           Control.Monad.Logic
import           Data.Function ( on )
import qualified Data.List as L

import           Test.Tasty.Sugar.AssocCheck
import           Test.Tasty.Sugar.ParamCheck
import           Test.Tasty.Sugar.Types


-- | Finds the possible expected files matching the selected
-- source. There will be either one or none.
findExpectation :: CUBE
                -> CandidateFile   --  original name of source
                -> [CandidateFile] --  all of the names to choose from
                -> ([NamedParamMatch], CandidateFile, String) -- param constraints from the root name
                -> Maybe ( Sweets, SweetExplanation )
findExpectation pat rootN allNames (rootPMatches, matchPrefix, _) =
  let r = mkSweet <$>
          trimExpectations $
          observeAll $
          do guard (not $ null candidates)
             expectedSearch
                matchPrefix
                rootPMatches seps params expSuffix o
                candidates
      o = associatedNames pat
      seps = separators pat
      params = validParams pat
      expSuffix = expectedSuffix pat
      candidates = filter possible allNames
      possible f = and [ candidateFile matchPrefix `L.isPrefixOf` candidateFile f
                       , rootN /= f
                       ]
      mkSweet e = Just $ Sweets { rootMatchName = candidateFile rootN
                                , rootBaseName = candidateFile matchPrefix
                                , rootFile = candidateToPath rootN
                                , cubeParams = validParams pat
                                , expected = e
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
       Nothing -> Nothing
       Just r' | [] <- expected r' -> Nothing
       Just r' -> Just ( r'
                       , SweetExpl { rootPath = candidateToPath rootN
                                   , base = candidateToPath matchPrefix
                                   , expectedNames =
                                       filter
                                       (if null expSuffix then const True
                                        else (expSuffix `L.isSuffixOf`))
                                     (candidateToPath <$> candidates)
                                   , results = r'
                                   })


-- Find all Expectations matching this rootMatch
expectedSearch :: CandidateFile
               -> [NamedParamMatch]
               -> Separators
               -> [ParameterPattern]
               -> FileSuffix
               -> [ (String, FileSuffix) ]
               -> [CandidateFile]
               -> Logic Expectation
expectedSearch rootPrefix rootPVMatches seps params expSuffix assocNames allNames =
  do (expFile, pmatch, assocFiles) <-
       let bestRanked :: [((a, Int, [b]),c)] -> Logic (a, [b], c)
           bestRanked l =
             if null l then mzero
             else let m = maximum $ fmap rankValue l
                      rankValue ((_,r,_),_) = r
                      rankMatching v ((_,r,_),_) = v == r
                      dropRank ((a,_,b),c) = (a,b,c)
                  in eachFrom $ fmap dropRank $ filter (rankMatching m) l

       in bestRanked $
          observeAll $
          do pseq <- eachFrom $
                     ([] :) $
                     filter (not . null) $
                     concatMap L.inits $
                     L.permutations params

             pvals <- getPVals pseq
             let compatNames = filter (isCompatible seps params pvals) allNames
             guard (not $ null compatNames)
             e@(_,_,pmatch) <- getExp rootPrefix rootPVMatches seps pvals
                               expSuffix compatNames
             a <- getAssoc rootPrefix seps pmatch assocNames compatNames
             return (e,a)
     -- assocFiles <- getAssoc rootPrefix seps pmatch assocNames allNames
     return $ Expectation { expectedFile = candidateToPath expFile
                          , associated = fmap candidateToPath <$> assocFiles
                          , expParamsMatch = L.sortBy (compare `on` fst) pmatch
                          }


-- | Get all expected files for a particular sequence of param+value.
-- Returns the expected file, the sequence of parameter values that
-- match that expect file, and a ranking (the number of those paramter
-- values that actually appear in the expect file.
getExp :: CandidateFile
       -> [NamedParamMatch]
       -> Separators
       -> [(String, Maybe String)]
       -> FileSuffix
       -> [CandidateFile]
       -> Logic (CandidateFile, Int, [NamedParamMatch])
getExp rootPrefix rootPMatches seps pvals expSuffix allNames =
  do -- Some of the params may be encoded in the subdirectories instead of in the
     -- target filename (each param value could appear in either).  If a
     -- rootPMatches value is in a subdirectory, no other values for that
     -- parameter can appear, otherwise all possible values could appear.  A
     -- subset of the rootPMatches may appear in the subdirs, but only the
     -- maximal subset can be considered.

     let rootMatchesInSubdir :: CandidateFile -> [NamedParamMatch]
         rootMatchesInSubdir f =
           let sp = candidateSubdirs f
               chkRootMatch d r =
                 let chkRPMatch p r' =
                       case getExplicit $ snd p of
                         Just v -> if d == v then p : r' else r'
                         Nothing -> r'
                 in foldr chkRPMatch r rootPMatches
           in foldr chkRootMatch mempty sp

     let inpDirMatches = fmap rootMatchesInSubdir <$> zip allNames allNames

     (dirName, inpDirMatch) <- eachFrom inpDirMatches

     let nonRootMatchPVals = removePVals pvals inpDirMatch

     (otherMatchesInSubdir, _) <-
           dirMatches dirName $ (fmap (fmap (:[])) <$> nonRootMatchPVals)

     let remPVals = removePVals nonRootMatchPVals otherMatchesInSubdir

     let remRootMatches = removePVals rootPMatches inpDirMatch
     let validNames = [ dirName ]

     (fp, cnt, npm) <- getExpFileParams rootPrefix
                       remRootMatches
                       seps remPVals expSuffix validNames

     -- Corner case: a wildcard parameter could be selected from both a subdir
     -- and the filename... if the values are the same, that's OK, but if the
     -- values are different it should be rejected.

     let dpm = inpDirMatch <> otherMatchesInSubdir

     let conflict = let chkNPM (pn,pv) acc =
                          acc || case lookup pn dpm of
                                   Nothing -> False
                                   Just v -> v /= pv
                    in foldr chkNPM False npm
     guard (not conflict)

     return (fp, length dpm + cnt, dpm <> npm)


getExpFileParams :: CandidateFile
                 -> [NamedParamMatch]
                 -> Separators
                 -> [(String, Maybe String)]
                 -> FileSuffix
                 -> [CandidateFile]
                 -> Logic (CandidateFile, Int, [NamedParamMatch])
getExpFileParams rootPrefix rootPMatches seps pvals expSuffix hereNames =
  do let suffixSpecifiesSep = and [ not (null expSuffix)
                                  , head expSuffix `elem` seps
                                  ]
     (pm, pmcnt, pmstr) <- pvalMatch seps rootPMatches pvals

     -- If the expSuffix starts with a separator then *only that*
     -- separator is allowed for the suffix (other seps are still
     -- allowed for parameter value separation).
     let suffixSepMatch = not suffixSpecifiesSep
                          || and [ not (null pmstr)
                                 , last pmstr == head expSuffix
                                 ]
     guard suffixSepMatch

     let ending = if suffixSpecifiesSep then tail expSuffix else expSuffix

     expFile <-
       eachFrom
       $ filter (((candidateFile rootPrefix <> pmstr <> ending) ==) . candidateFile)
       $ hereNames

     return (expFile, pmcnt, pm)


-- | Removes all Expectations that don't have an explicit match
removeNonExplicitMatchingExpectations :: [Expectation] -> [Expectation]
removeNonExplicitMatchingExpectations l =
  let removeNonExplicits entry lst =
        let (explParams, assumedParams) =
              L.partition (isExplicit . snd) (expParamsMatch entry)

            -- only return False if oneExp should be
            -- removed: i.e. it is an Expectation that
            -- matches all non-explicit parameters and
            -- has non-explicit matches for any of the
            -- Explicit matches.
            nonExplMatch oneExp =
              or [ oneExp == entry
                 , not $ all nonExplParamCheck $ expParamsMatch oneExp
                 ]

            -- return True if this parameter check would
            -- allow removal of this Explicit based on
            -- _this_ parameter.
            nonExplParamCheck (pn, pv) =
              case lookup pn explParams of
                Just (Explicit ev) ->
                  case pv of
                    Assumed av -> ev == av
                    NotSpecified -> True
                    Explicit ev' -> ev == ev'
                _ ->  -- generally nothing; other Just values not possible from explParams
                  case lookup pn assumedParams of
                    Nothing -> False
                    Just av -> av == pv

        in filter nonExplMatch lst

  in foldr removeNonExplicits l l
