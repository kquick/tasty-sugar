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
import           System.FilePath ( (</>), takeDirectory, takeFileName
                                 , splitPath )

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
          do guard (not $ null candidates)
             expectedSearch d matchPrefix rootPMatches seps params expSuffix o
               candidates
      d = filter (not . null) $ inputDir pat : inputDirs pat
      o = associatedNames pat
      seps = separators pat
      params = validParams pat
      expSuffix = expectedSuffix pat
      subdirConflict f =
        -- A regular inpDir match won't conflict, but any inpDir ending in /*
        -- might have a subdir component that has a *different* value for a
        -- rootPMatch and should therefore return False to be excluded
        let rd = not $ any (`L.isPrefixOf` f) d
            sd = let chkDir inpD acc =
                       let iDLen = length (splitPath inpD) - 1
                           iSubdirs :: [ FilePath ]
                           iSubdirs = init <$> (init $ drop iDLen $ splitPath f)
                           wrongParamVal :: String -> NamedParamMatch -> Bool
                           wrongParamVal sv (pn, pv) =
                             case join $ lookup pn (validParams pat) of
                               Nothing -> False
                               Just vals -> case getParamVal pv of
                                              Nothing -> False
                                              Just v -> sv `elem` vals && sv /= v
                           chkParamVal subdir conflict =
                             conflict || any (wrongParamVal subdir) rootPMatches
                       in foldr chkParamVal acc iSubdirs
                 in foldr chkDir False $ filter ((== "*") . takeFileName) d

        in and [ rd, sd ]
      candidates = filter possible allNames
      possible f = and [ takeFileName matchPrefix `L.isPrefixOf` takeFileName f
                       , rootN /= f
                       , not $ subdirConflict f
                       ]
      mkSweet e = Just $ Sweets { rootMatchName = takeFileName rootN
                                , rootBaseName = takeFileName matchPrefix
                                , rootFile = rootN
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
                       , SweetExpl { rootPath = rootN
                                   , base = matchPrefix
                                   , expectedNames =
                                       filter
                                       (if null expSuffix then const True
                                        else (expSuffix `L.isSuffixOf`))
                                     candidates
                                   , results = r'
                                   })

-- Find all Expectations matching this rootMatch
expectedSearch :: [FilePath]
               -> FilePath
               -> [NamedParamMatch]
               -> Separators
               -> [ParameterPattern]
               -> FileSuffix
               -> [ (String, FileSuffix) ]
               -> [FilePath]
               -> Logic Expectation
expectedSearch inpDirs rootPrefix rootPVMatches seps params expSuffix assocNames allNames =
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
             getExp inpDirs rootPrefix rootPVMatches seps pvals expSuffix allNames
     assocFiles <- getAssoc inpDirs rootPrefix seps pmatch assocNames allNames
     return $ Expectation { expectedFile = expFile
                          , associated = assocFiles
                          , expParamsMatch = L.sortBy (compare `on` fst) pmatch
                          }

-- Get all expected files for a particular sequence of param+value.
-- Returns the expected file, the sequence of parameter values that
-- match that expect file, and a ranking (the number of those paramter
-- values that actually appear in the expect file.
getExp :: [FilePath]
       -> FilePath
       -> [NamedParamMatch]
       -> Separators
       -> [(String, Maybe String)]
       -> FileSuffix
       -> [FilePath]
       -> Logic (FilePath, Int, [NamedParamMatch])
getExp inpDirs rootPrefix rootPMatches seps pvals expSuffix allNames =
  do inpDir <- eachFrom inpDirs
     let idLen = length $ splitPath inpDir

     if takeFileName inpDir == "*"
       then
       do
         -- Some of the params may be encoded in the subdirectories instead of in
         -- the target filename (each param value could appear in either).  If a
         -- rootPMatches value is in a subdirectory, no other values for that
         -- parameter can appear, otherwise all possible values could appear.  A
         -- subset of the rootPMatches may appear in the subdirs, but only the
         -- maximal subset can be considered.

         let dirNames = filter (takeDirectory inpDir `L.isPrefixOf`) allNames

         let rootMatchesInSubdir :: FilePath -> [NamedParamMatch]
             rootMatchesInSubdir f =
               let sp = init <$> (init $ drop (idLen - 1) $ splitPath f)
                   chkRootMatch d r =
                     let chkRPMatch p r' =
                           case getExplicit $ snd p of
                             Just v -> if d == v then p : r' else r'
                             Nothing -> r'
                     in foldr chkRPMatch r rootPMatches
               in foldr chkRootMatch mempty sp

         let inpDirMatches = fmap rootMatchesInSubdir <$> zip dirNames dirNames

         (dirName, inpDirMatch) <- eachFrom inpDirMatches

         let nonRootMatchPVals =
               removePVals pvals inpDirMatch

         (otherMatchesInSubdir, _) <-
           dirMatches inpDir dirName $ (fmap (fmap (:[])) <$> nonRootMatchPVals)

         let remPVals = removePVals nonRootMatchPVals otherMatchesInSubdir

         let remRootMatches = removePVals rootPMatches inpDirMatch
         let validNames = [ dirName ]

         (fp, cnt, npm) <- getExpFileParams inpDir rootPrefix remRootMatches
                           seps remPVals expSuffix validNames
         return (fp, cnt, inpDirMatch <> otherMatchesInSubdir <> npm)

       else getExpFileParams inpDir rootPrefix rootPMatches seps pvals
              expSuffix allNames



getExpFileParams :: FilePath
                 -> FilePath
                 -> [NamedParamMatch]
                 -> Separators
                 -> [(String, Maybe String)]
                 -> FileSuffix
                 -> [FilePath]
                 -> Logic (FilePath, Int, [NamedParamMatch])
getExpFileParams inpDir rootPrefix rootPMatches seps pvals expSuffix allNames =
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
       if takeFileName inpDir == "*"
       then do
         -- Some unknown number of path elements, so try each file in
         -- allNames that matches the path prefix and filename suffix
         -- portions
         eachFrom
         $ filter ((pmstr <> ending) `L.isSuffixOf`)
         $ filter (takeDirectory inpDir `L.isPrefixOf`) allNames
       else
         return $ inpDir </> takeFileName rootPrefix <> pmstr <> ending

     guard (expFile `elem` allNames)
     return (expFile, pmcnt, pm)


removeNonExplicitMatchingExpectations :: [Expectation] -> [Expectation]
removeNonExplicitMatchingExpectations l =
  let removeNonExplicits lst entry =
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

  in foldl removeNonExplicits l l
