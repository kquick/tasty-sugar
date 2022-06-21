{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Main internal entry point for determining the various test
-- configurations specified by a CUBE input.

module Test.Tasty.Sugar.Analysis
  (
    checkRoots
  )
where

import           Control.Monad.Logic
import           Data.Bifunctor ( bimap )
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Maybe ( catMaybes )
import           Data.Ord ( comparing )
import qualified System.FilePath as FP
import qualified System.FilePath.GlobPattern as FPGP

import           Test.Tasty.Sugar.ExpectCheck
import           Test.Tasty.Sugar.RootCheck
import           Test.Tasty.Sugar.ParamCheck ( pmatchCmp )
import           Test.Tasty.Sugar.Types


-- | Given a 'CUBE' and a list of filepaths in the target directories,
-- return all 'Sweets' matches along with an explanation of the search
-- process.  This is the core implementation for the
-- 'Test.Tasty.Sugar.findSugar' API interface.
checkRoots :: CUBE -> [FilePath]
           -> (Int, [([Sweets], [SweetExplanation])])
checkRoots pat allFilePaths =
  let isRootMatch n = FP.takeFileName n FPGP.~~ (rootName pat)
      rootNames = filter isRootMatch allFilePaths
      checked = filter (not . null . fst)
                ((checkRoot pat allFilePaths) <$> rootNames)
  in (length checked, checked)


-- checkRoot will attempt to split the identified root file into three
-- parts:
--
--     basename + [param-values] + [suffix/extension]
--
-- Once it has performed this split, the calls findExpectation to
-- check if there are any expected file that matches the basename,
-- expSuffix, and any param-values provided.  A 'Sweets' will be
-- returned for each expected file matching this root configuration
checkRoot :: CUBE
          -> [FilePath] --  all possible expect candidates
          -> FilePath  --  root path
          -> ([Sweets], [SweetExplanation])
checkRoot pat allNames rootNm =
  let seps = separators pat
      params = validParams pat
      combineExpRes (swts, expl) = bimap (swts :) (expl :)

      inpDirForRoot =
        let dirmatch d = if FP.takeFileName d == "*"
                         then FP.takeDirectory d `L.isPrefixOf` rootNm
                         else d == FP.takeDirectory rootNm
            dirs = filter (not . null) $ inputDir pat : inputDirs pat
        in case filter dirmatch dirs of
             (d:[]) -> Just $ if FP.takeFileName d == "*"
                              then FP.takeDirectory d
                              else d
             [] -> Nothing
             _ -> error ("Internal error: inpDirForRoot " <> rootNm
                         <> " not uniquely identifiable in "
                         <> show dirs
                        )

      mergeSweets swl =
        -- If multiple Sweets have the same rootMatchName this likely means that
        -- there were multiple expected files that could have matched.  Merge the
        -- Expectations: for each of the second Sweet's expectations:
        --
        --   - If one has a longer rootBaseName, that one represents the more
        --     explicit match and should be used.  Otherwise,
        --
        --   - If no explicit (expParamsMatch) elements match the first, this
        --     is a unique Expectation, add it to the first Sweet
        --
        --   - Find the Expectation in the first Sweet with the most number of
        --     Explicit matches, then select the one that has the most number of
        --     remaining Explicit that don't match the other (one should be a
        --     strict subset of the other!)
        let combineIfRootsMatch s sl =
              -- Add s to sl, or if s matches a root in sl, merge s with that sl
              uncurry (flip (:))
              ( combineSweets s <$> L.partition (not . isRootMatch s) sl)
            isRootMatch = (==) `on` (rootMatchName . fst)
            combineSweets s slm =
              -- Merge all the expectations from each of slm sweets into the main
              -- sweet s.
              foldr chooseOrCombineExpectations s slm
            chooseOrCombineExpectations (s,e) (sm,sme) =
              case comparing rootBaseName s sm of
                GT -> (s,e)
                LT -> (sm, sme)
                EQ -> bestExpectations (s,e) (sm,sme)
            bestExpectations (s,e) (sm,_sme) =
              -- combine the expectations in s with the expectations in each of
              -- sm, where expectations overlap based on explicit expParamsMatch
              -- matchups.
              let swts = s { expected =
                               foldr mergeExp (expected s) (expected sm)
                           }
                  mergeExp oneExp exps =
                    concat
                    $ fmap (take 1)
                    $ L.groupBy ((==) `on`
                                  (fmap (fmap getParamVal) . expParamsMatch))
                    $ L.sortBy (pmatchCmp `on` expParamsMatch)
                    $ oneExp : exps
              in ( swts, e { results = swts } )
        in foldr combineIfRootsMatch [] swl

  in case inpDirForRoot of
       Just inDir -> foldr combineExpRes ([], []) $
                     mergeSweets $
                     catMaybes $
                     fmap (findExpectation pat rootNm allNames) $
                     observeAll $
                     rootMatch inDir rootNm seps params (rootName pat)
       Nothing -> ([],[])
