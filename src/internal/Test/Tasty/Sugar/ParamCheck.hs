{-# LANGUAGE LambdaCase #-}

-- | Functions for checking different parameter/value combinations.

module Test.Tasty.Sugar.ParamCheck
  (
    eachFrom
  , getPVals
  , singlePVals
  , pvalMatch
  , removePVals
  , pmatchCmp
  , pmatchMax
  , dirMatches
  , inEachNothing
  , isCompatible
  )
  where

import           Control.Monad
import           Control.Monad.Logic
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Maybe ( catMaybes, fromJust, isNothing, listToMaybe )
import           Data.Bifunctor ( first )
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

-- | Returns a ParameterPattern admitting only a single value for each parameter,
-- ensuring that the value is compatible with any existing NamedParamMatch.  This
-- is useful for callers wishing to handle each combination of parameter values
-- separately.
singlePVals :: [NamedParamMatch] -> [ParameterPattern]
            -> Logic [ParameterPattern]
singlePVals sel = eachVal . L.sort
  where eachVal [] = return []
        eachVal ((pn,Nothing):ps) =
          let this = (pn, (:[]) <$> (lookup pn sel >>= getParamVal))
           in (this :) <$> eachVal ps
        eachVal ((pn,Just pvs):ps) =
          do pv <- eachFrom $ case lookup pn sel >>= getParamVal of
                                Nothing -> L.sort pvs
                                Just v -> [v]
             ((pn, Just [pv]) :) <$> eachVal ps


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
  let (ppv, _rpv) = L.partition isPreset pvals
      isPreset p = fst p `elem` (fmap fst preset)

      matchesPreset = all matchPreset ppv
      matchPreset (pn,mpv) = maybe False (matchPresetVal mpv) $
                             lookup pn preset
      matchPresetVal mpv pv = case mpv of
                                Just v -> paramMatchVal v pv
                                Nothing -> True

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

  in do guard $ matchesPreset
        candidateVals <- pvVals preset pvals
        let rset = preset <> removePVals candidateVals preset
            orderedRset = fmap from_rset $ fmap fst pvals
            from_rset n = let v = maybe NotSpecified id $ L.lookup n rset in (n,v)
        pvstr <- genPVStr orderedRset
        return (rset, length orderedRset, pvstr)


-- | Generate the various combinations of parameters+values from the possible
-- set specified by the input.

pvVals :: [NamedParamMatch] -> [(String, Maybe String)] -> Logic [NamedParamMatch]
pvVals _ [] = return []
pvVals presets ((pn, mpv):ps) =
  do nxt <- pvVals presets ps
     let explicit v = return $ (pn, Explicit v) : nxt
         notExplicit = let pMatchImpl =
                             case lookup pn presets of
                               Nothing -> maybe NotSpecified Assumed
                               Just presetV -> const presetV
                       in return $ (pn, pMatchImpl mpv) : nxt
     (maybe mzero explicit mpv) `mplus` notExplicit


-- | Removes the second set of named params from the first set, leaving the
-- remainder of the first set that isn't matched in the second set.

removePVals :: [(String, a)] -> [(String, b)] -> [(String, a)]
removePVals main rmv = filter (not . (`elem` (fst <$> rmv)) . fst) main


-- | This provides an Ordering result of comparing two sets of NamedParamMatch.
-- This can be used for sorting or other prioritization of named matches.

pmatchCmp :: [ NamedParamMatch ] -> [ NamedParamMatch ] -> Ordering
pmatchCmp p1 p2 =
  let comparisons =
        [
          -- the one with more Explicit matches is better
          compare `on` (length . filter (isExplicit . snd))
          -- the one with more parameters (usually the same)
        , compare `on` length
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

-- | Returns the maximum of two arguments based on comparing the
-- [NamedParamMatch] extracted from each argument (via the passed function).

pmatchMax :: (a -> [NamedParamMatch]) -> a -> a -> a
pmatchMax f a b = case pmatchCmp (f a) (f b) of
                    LT -> b
                    _ -> a


-- | Given the root directory and a file in that directory, along with the
-- possible parameters and values, return each valid set of parameter matches
-- from that file, along with the remaining unmatched parameter possibilities.
--
-- The first set of parameters is the total set, and the second set represents
-- those that could be identified in the path subdirs; this is needed to prevent
-- a wildcard ParameterPattern in the second set from matching values explicit to
-- other parameters.

dirMatches :: CandidateFile -> [ParameterPattern] -> [ParameterPattern]
           -> Logic ([NamedParamMatch], [ParameterPattern])
dirMatches fname fullParams params = do
  let pathPart = candidateSubdirs fname

  let findVMatch :: FilePath -> (String, Maybe [String]) -> Maybe String
      findVMatch e (pn,pv) =
        case pv of
          Nothing -> Nothing
          Just vs -> if e `elem` vs then Just pn else Nothing
  let findPVMatch parms pthPartE found =
        listToMaybe (catMaybes (map (findVMatch pthPartE) parms)) : found

  let pmatches = foldr (findPVMatch params) [] pathPart

  let freeParam = fst <$> L.find (isNothing . snd) params

  let freeParts =
        let allpvals = concat $ catMaybes (snd <$> fullParams)
        in (not . (`elem` allpvals)) <$> pathPart

  dmatch <- fmap (fmap Explicit)
            . fmap (first fromJust)
            . filter (not . isNothing . fst)
            <$> ((return (zip pmatches pathPart))
                 `mplus`
                 (inEachNothing freeParam $ zip3 pmatches freeParts pathPart))

  let drem = removePVals params dmatch

  return (dmatch, drem)


-- | Return each substitution of the first argument for each location in the
-- second list that has a Nothing label and a True parameter; leave non-Nothings
-- in the second list unchanged.

inEachNothing :: Maybe a -> [(Maybe a,Bool,b)] -> Logic [(Maybe a,b)]
inEachNothing mark into = do
  let canSubst (a,b,_) = b && isNothing a
  let spots = filter (\i -> canSubst (into !! i)) $ [0..(length into) - 1]
  i <- eachFrom spots
  let deBool (a,_,c) = (a,c)
  let thrd (_,_,c) = c
  return
    $ (deBool <$> take i into)
    <> [ (mark, thrd (into !! i)) ]
    <> (deBool <$> drop (i + 1) into)


-- | isCompatible can be used as a filter predicate to determine if the specified
-- file is compatible with the provided parameters and chosen parameter values.
-- One principle compatibility check is ensuring that there is no *other*
-- parameter value in the filename that conflicts with a chosen parameter value.
isCompatible :: Separators
             -> [ParameterPattern]
             -> [(String, Maybe String)]
             -> CandidateFile
             -> Bool
isCompatible seps params pvals fname =
  let splitFName n = let (p,r) = break (`elem` seps) n
                     in p : if null r then [] else splitFName (tail r)
      parts = let n' = splitFName $ candidateFile fname
              in candidateSubdirs fname <> n'
      noConflict _ (_,Nothing) = True
      noConflict ps (pn,Just vs) = all (not . isConflict pn vs) ps
      isConflict pn vs p = and [ p `elem` vs
                               , maybe False (Just p /=) $ lookup pn pvals
                               ]
  in all (noConflict parts) params
