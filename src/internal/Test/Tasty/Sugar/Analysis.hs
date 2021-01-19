-- | Main internal entry point for determining the various test
-- configurations specified by a CUBE input.

module Test.Tasty.Sugar.Analysis
  (
    checkRoots
  )
where

import           Control.Monad.Logic
import           Data.Bifunctor ( bimap )
import           Data.Maybe ( catMaybes )
import qualified System.FilePath as FP
import qualified System.FilePath.GlobPattern as FPGP

import           Test.Tasty.Sugar.ExpectCheck
import           Test.Tasty.Sugar.RootCheck
import           Test.Tasty.Sugar.Types


-- | Given a 'CUBE' and a list of files in the target directory,
-- return all 'Sweets' matches along with an explanation of the search
-- process.  This is the core implementation for the
-- 'Test.Tasty.Sugar.findSugar' API interface.
checkRoots :: CUBE -> [FilePath]
           -> (Int, [([Sweets], [SweetExplanation])])
checkRoots pat allFiles =
  let isRootMatch n = n FPGP.~~ (rootName pat)
      rootNames = FP.takeFileName <$> (filter isRootMatch allFiles)
  in (length rootNames, fmap (checkRoot pat allFiles) rootNames)


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
          -> FilePath  --  root name
          -> ([Sweets], [SweetExplanation])
checkRoot pat allNames rootNm =
  let seps = separators pat
      params = validParams pat
      combineExpRes (swts, expl) = bimap (swts :) (expl :)

      trimSweets :: [(Sweets, SweetExplanation)] ->
                    [(Sweets, SweetExplanation)]
      trimSweets =
        -- If multiple Sweets have the same rootMatchName, use the one
        -- with the longer rootBaseName.  This prevents "foo.exp" and
        -- "foo-bar.exp" from both matching "foo-bar.c".
        (\l -> let removeShorterBases lst (entry,_) =
                     let notShorterBase (s,_) = not $
                           and [ rootMatchName s == rootMatchName entry
                               , rootBaseName s < rootBaseName entry
                               ]
                     in filter notShorterBase lst
               in foldl removeShorterBases l l)

  in foldr combineExpRes ([], []) $
     trimSweets $
     catMaybes $
     fmap (findExpectation pat rootNm allNames) $
     observeAll $
     rootMatch rootNm seps params (rootName pat)
