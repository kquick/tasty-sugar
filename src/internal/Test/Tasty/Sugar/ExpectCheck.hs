-- | Function to find expected results files for a specific root file,
-- along with any parameter values identified by the root file.

module Test.Tasty.Sugar.ExpectCheck
  (
    findExpectation
  )
  where

import           Control.Monad.Logic
import           System.FilePath ( (</>) )
import qualified Data.List as L

import           Test.Tasty.Sugar.AssocCheck
import           Test.Tasty.Sugar.ParamCheck
import           Test.Tasty.Sugar.Types



-- | Finds the possible expected files matching the selected
-- source. There will be either one or none.
findExpectation :: CUBE
                -> FilePath   --  original name of source
                -> [FilePath] --  all of the names to choose from
                -> ([NamedParamMatch], FilePath, FilePath) -- param constraints from the root name
                -> Maybe ( Sweets, SweetExplanation )
findExpectation pat rootN allNames (rootPMatches, matchPrefix, _) =
  let r = mkSweet <$>
          trimExpectations $
          observeAll $
          expectedSearch d matchPrefix rootPMatches seps params expSuffix o
          candidates
      d = inputDir pat
      o = associatedNames pat
      seps = separators pat
      params = validParams pat
      expSuffix = expectedSuffix pat
      candidates = filter possible allNames
      possible f = and [ matchPrefix `L.isPrefixOf` f
                       , rootN /= f
                       ]
      mkSweet e = Just $ Sweets { rootMatchName = rootN
                                , rootBaseName = matchPrefix
                                , rootFile = inputDir pat </> rootN
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
        -- Expectation with Assumed matches.
        (\l -> let removeNonExplicits lst entry =
                     let explParams = filter (isExplicit . snd)
                                      (expParamsMatch entry)
                         removeNonExpl es explParam =
                           filter (noNonExplicit explParam) es
                         noNonExplicit (pn, Explicit pv) expl=
                           let chkPV (pn', pv') =
                                 pn /= pn' || case pv' of
                                                Explicit _ -> True
                                                Assumed v -> v /= pv
                                                NotSpecified -> False
                           in all chkPV $ expParamsMatch expl
                         noNonExplicit _ _ = True
                     in foldl removeNonExpl lst explParams
               in foldl removeNonExplicits l l)

        -- remove duplicates (uses the Eq instance for Expectation
        -- that ignores the order of the expParamsMatch and associated
        -- to ensure that different ordering with the same values
        -- doesn't cause multiple Expectation.
        . L.nub

  in case r of
       Nothing -> Nothing
       Just r' | [] <- expected r' -> Nothing
       Just r' -> Just ( r'
                       , SweetExpl { rootPath = rootN
                                   , base = matchPrefix
                                   , expectedNames =
                                       filter
                                       (if null expSuffix then const True
                                        else (expSuffix `L.isSuffixOf`))
                                     candidates
                                   , results = [ r' ]
                                   })

-- Find all Expectations matching this rootMatch
expectedSearch :: FilePath
               -> FilePath
               -> [NamedParamMatch]
               -> Separators
               -> [ParameterPattern]
               -> FileSuffix
               -> [ (String, FileSuffix) ]
               -> [FilePath]
               -> Logic Expectation
expectedSearch inpDir rootPrefix rootPVMatches seps params expSuffix assocNames allNames =
  do (expFile, pmatch) <-
       let bestRanked :: [(FilePath, Int, [NamedParamMatch])]
                      -> Logic (FilePath, [NamedParamMatch])
           bestRanked l =
             if null l then mzero
             else let m = maximum $ fmap rankValue l
                      rankValue (_,r,_) = r
                      rankMatching v (_,r,_) = v == r
                      dropRank (a,_,b) = (a,b)
                  in eachFrom $ fmap dropRank $ filter (rankMatching m) l

       in bestRanked $
          observeAll $
          do pseq <- eachFrom $
                     ([] :) $
                     filter (not . null) $
                     concatMap L.inits $
                     L.permutations params
             pvals <- getPVals pseq
             getExp rootPrefix rootPVMatches seps pvals expSuffix allNames
     assocFiles <- getAssoc rootPrefix seps pmatch assocNames $
                   -- filter out source and exp for small optimization
                   filter (not . flip elem [ rootPrefix, expFile ])
                   allNames
     return $ Expectation { expectedFile = inpDir </> expFile
                          , associated = fmap (inpDir </>) <$> assocFiles
                          , expParamsMatch = pmatch
                          }

-- Get all expected files for a particular sequence of param+value.
-- Returns the expected file, the sequence of parameter values that
-- match that expect file, and a ranking (the number of those paramter
-- values that actually appear in the expect file.
getExp :: FilePath
       -> [NamedParamMatch]
       -> Separators
       -> [(String, Maybe String)]
       -> FileSuffix
       -> [FilePath]
       -> Logic (FilePath, Int, [NamedParamMatch])
getExp rootPrefix rootPMatches seps pvals expSuffix allNames =
  do (pm, pmcnt, pmstr) <- pvalMatch seps rootPMatches pvals
     -- If the expSuffix starts with a separator then *only that*
     -- separator is allowed for the suffix (other seps are still
     -- allowed for parameter value separation).
     let suffixSpecifiesSep = and [ not (null expSuffix)
                                  , head expSuffix `elem` seps
                                  ]
     let suffixSepMatch = not suffixSpecifiesSep
                          || and [ not (null pmstr)
                                 , last pmstr == head expSuffix
                                 ]
     guard suffixSepMatch
     let expFile = if suffixSpecifiesSep
                   then rootPrefix <> pmstr <> tail expSuffix
                   else rootPrefix <> pmstr <> expSuffix
     guard (expFile `elem` allNames)
     return (expFile, pmcnt, pm)
