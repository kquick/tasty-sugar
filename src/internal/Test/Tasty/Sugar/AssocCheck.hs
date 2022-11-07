-- | Function and implementation to find association files for an
-- identified test root file.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Sugar.AssocCheck
  (
    getAssoc
  )
  where

import           Control.Monad ( guard )
import           Data.Function ( on )
import qualified Data.List as DL

import           Test.Tasty.Sugar.Iterations
import           Test.Tasty.Sugar.Types


-- | For a specific NamedParamMatch, find all associated files having
-- the rootMatch plus the named parameter values (in the same order
-- but with any combination of separators) and the specified suffix
-- match.
getAssoc :: CandidateFile
         -> Separators
         -> [NamedParamMatch]
         -> [ (String, FileSuffix) ]
         -> [CandidateFile]
         -> LogicI [(String, CandidateFile)]
getAssoc rootPrefix seps pmatch assocNames allNames = assocSet
  where

    assocSet = concat <$> mapM fndBestAssoc assocNames

    fStart = candidateFile rootPrefix

    fndBestAssoc :: (String, FileSuffix)
                 -> LogicI [(String, CandidateFile)] -- usually just one
    fndBestAssoc assoc = addSubLogicStats (observeIT (fndAssoc assoc))

    fndAssoc :: (String, FileSuffix) -> LogicI (String, CandidateFile)
    fndAssoc assoc =
      do let fEnd = snd assoc
         -- First, eliminate any files that don't start with rootPrefix or end in
         -- this assoc suffix (do this before trying any backtracking).
         let sfxMatch f = and [ fStart `DL.isPrefixOf` f
                              , null fEnd || fEnd `DL.isSuffixOf` f
                              ]
             assocNms = filter (sfxMatch . candidateFile) allNames
         guard $ not $ null assocNms
         -- Now try combinations of parameters based on what the rootPrefix
         -- specified and variations of the others, with various separators.
         pseq <- npseq (snd <$> pmatch)
         (assocPfx, assocSfx) <- sepParams seps pseq
         let possible =
               if null assocSfx
               then let justSep = null (snd assoc) && length assocPfx == 1
                        assocFName = if justSep
                                     then fStart
                                     else fStart <> assocPfx <> (snd assoc)
                    in (assocFName ==)
               else let assocStart = candidateFile rootPrefix <> assocPfx
                        assocEnd = assocSfx <> snd assoc
                        aSL = length assocStart
                        aEL = length assocEnd
                        chk f =
                          and [ assocStart `DL.isPrefixOf` f
                              , assocEnd `DL.isSuffixOf` f
                              , length f > (aSL + aEL)
                              , let mid = drop aSL (take (length f - aEL) f)
                                in and $ fmap (not . flip elem mid) seps
                              ]
                    in chk
         f <- eachFrom "possible assoc file"
              $ filter (possible . candidateFile) assocNms
         return (fst assoc, f)

    sepParams :: Separators -> [ParamMatch] -> LogicI (String, String)
    sepParams sl = \case
        [] -> if null sl
              then return ([], [])
              else do s <- eachFrom "sep param for arbitrary assoc" sl
                      return ([s], [])
        (NotSpecified:ps) -> sepParams sl ps
        ((Explicit v):ps) -> do (l,r) <- sepParams sl ps
                                if null sl
                                  then return (v <> l, r)
                                  else do s <- eachFrom "sep param for explicit assoc" sl
                                          return ([s] <> v <> l, r)
        ((Assumed  v):ps) -> do (l,r) <- sepParams sl ps
                                if null sl
                                  then return (v <> l, r)
                                  else do s <- eachFrom "sep param for assumed assoc" sl
                                          return ([s] <> v <> l, r)

    npseq = eachFrom "assoc params permutations"
            . reverse
            . DL.sortBy (\a b -> case (compare `on` length) a b of
                                   EQ -> (compare `on` matchStrength) a b
                                   o -> o
                        )
            . reverse
            . combosLongToShort
