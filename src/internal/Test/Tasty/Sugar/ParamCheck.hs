-- | Functions for checking different parameter/value combinations.

module Test.Tasty.Sugar.ParamCheck
  (
    eachFrom
  , getPVals
  , pvalMatch
  , pmatchCmp
  )
  where

import           Control.Monad
import           Control.Monad.Logic
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Maybe ( fromMaybe )

import           Test.Tasty.Sugar.Types


-- | Core Logic function to iteratively return elements of a list via
-- backtracking.
eachFrom :: [a] -> Logic a
eachFrom = foldr (mplus . return) mzero


-- | Returns various combinations of parameter value selections
getPVals :: [ParameterPattern] -> Logic [(String, Maybe String)]
getPVals = mapM getPVal
  where
    getPVal (pn, Nothing) = return (pn, Nothing)
    getPVal (pn, Just pv) = do pv' <- eachFrom pv
                               return (pn, Just pv')

-- | Generate each possible combination of Explicit or non-Explicit
-- (Assumed or NotSpecified) parameter value and the corresponding
-- string with each combination of separators.  The string will be
-- used to match against input files.
--
-- Note that valid combinations require that if a parameter is
-- non-Explicit, all following parameters must also be non-Explicit.
--
-- The preset set of parameters are any parameters *already* matched
-- against (usually in the rootName); these parameters may or may not
-- be present in the filename matched from the output of this
-- function, but if they are present, they must have the values
-- specified in the preset (instead of having any of the possible
-- values allowed for that parameter).
--
-- It's also possible that since this returns varying combinations of
-- parameters, that there may be multiple files that will match
-- against these combinations.  Therefore, the results also indicate
-- how many of the parameters are used in the associated matching
-- string since the caller will usually select the match with the
-- highest ranking (number of matched parameters) in the filename.
-- [Note that it is not possibly to simply use the length of the
-- @[NamedParamMatch]@ return component since that may contain values
-- from the preset that don't actually occur in the match string.
pvalMatch :: Separators
          -> [NamedParamMatch]
          -> [(String, Maybe String)]
          -> Logic ([NamedParamMatch], Int, String)
pvalMatch seps preset pvals =
  let (ppv, rpv) = L.partition isPreset pvals
      isPreset p = fst p `elem` (fmap fst preset)

      matchesPreset = all matchPreset ppv
      matchPreset (pn,mpv) = maybe False (matchPresetVal mpv) $
                             lookup pn preset
      matchPresetVal mpv pv = case mpv of
                                Just v -> paramMatchVal v pv
                                Nothing -> True

      pvVal :: [(String, Maybe String)] -> Logic [NamedParamMatch]
      pvVal [] = return []
      pvVal ((pn, mpv):ps) =
        let explicit v = do nxt <- pvVal ps
                            return $ (pn, Explicit v) : nxt
            notExplicit = let pMatchImpl = maybe NotSpecified Assumed
                              remPVMS = fmap (fmap pMatchImpl) ps
                          in return $ (pn, pMatchImpl mpv) : remPVMS
        in (maybe mzero explicit mpv) `mplus` notExplicit

      genPVStr :: [NamedParamMatch] -> Logic String
      genPVStr pvs =
        let vstr = fromMaybe "" . getExplicit . snd
            sepJoin :: String -> NamedParamMatch -> Logic String
            sepJoin r v = if isExplicit (snd v)
                          then do s <- eachFrom seps
                                  return $ [s] <> vstr v <> r
                          else return r
        in if null seps
           then return $ foldr (\v r -> vstr v <> r) "" pvs
           else do s <- eachFrom seps
                   foldM sepJoin [s] pvs

  in do guard matchesPreset
        candidateVals <- pvVal rpv
        let rset = preset <> candidateVals
            orderedRset = fmap from_rset $ fmap fst pvals
            from_rset n = let v = maybe NotSpecified id $ L.lookup n rset in (n,v)
        pvstr <- genPVStr orderedRset
        return (rset, length orderedRset, pvstr)


pmatchCmp :: [ NamedParamMatch ] -> [ NamedParamMatch ] -> Ordering
pmatchCmp p1 p2 =
  let comparisons =
        [
          -- the one with more Explicit matches is better
          compare `on` (length . filter (isExplicit . snd))
          -- the one iwth more parameters (usually the same)
        , compare `on` (length . fmap fst)
          -- comparing keys
        , compare `on` (L.sort . fmap fst)
        ]
        -- comparing the correlated ParamMatch values
        <> map (\k -> compare `on` (lookup k)) (fst <$> p1)
  in cascadeCompare comparisons p1 p2

cascadeCompare :: [ a -> a -> Ordering ] -> a -> a -> Ordering
cascadeCompare [] _ _ = EQ
cascadeCompare (o:os) a b = case o a b of
                              EQ -> cascadeCompare os a b
                              x -> x
