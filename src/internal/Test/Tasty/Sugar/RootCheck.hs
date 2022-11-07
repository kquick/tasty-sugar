-- | Function and associated helpers to determine the matching root
-- name.  The root name may contain zero or more parameter values.

{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Sugar.RootCheck
  (
    rootMatch
  )
  where

import           Control.Monad
import           Control.Monad.Logic
import qualified Data.List as L
import           Data.Maybe ( catMaybes, isNothing )

import           Test.Tasty.Sugar.Iterations ( eachFrom )
import           Test.Tasty.Sugar.ParamCheck
import           Test.Tasty.Sugar.Types


-- | Determine which parts of the input name form the basePrefix and any
-- parameter values for searching for related files (expected and associated).
-- Parameter values are taken from subdirectory paths or filename elements (in
-- that order).
rootMatch :: CandidateFile -> Separators -> [ParameterPattern] -> String
          -> Logic ([NamedParamMatch], CandidateFile, String)
rootMatch origRoot seps params rootCmp = do
  (dmatch, drem) <- dirMatches origRoot params params
  (rpm, p, s) <- ifte
                 (rootParamMatch origRoot seps drem rootCmp)
                 return
                 (noRootParamMatch origRoot seps)
  return (dmatch <> rpm, p, s)


data RootPart = RootSep String
              | RootParNm String String
              | RootText String
              | RootSuffix String
              deriving Show

isRootParNm :: RootPart -> Bool
isRootParNm (RootParNm _ _) = True
isRootParNm _ = False

isRootSep :: RootPart -> Bool
isRootSep (RootSep _) = True
isRootSep _ = False

rpStr :: [RootPart] -> String
rpStr = let s = \case
              RootSep x -> x
              RootParNm _ x -> x
              RootText x -> x
              RootSuffix x -> x
            bld b a = a <> s b
        in foldr bld ""

rpNPM :: [RootPart] -> [NamedParamMatch]
rpNPM = let bld (RootParNm n v) = Just [(n, Explicit v)]
            bld (RootSep _) = Nothing
            bld p = error ("Invalid RootPart for NamedParamMatch: " <> show p)
        in concat . catMaybes . fmap bld


-- Return the prefix and suffix of the root name along with the
-- explicit parameter matches that comprise the central portion.
rootParamMatch :: CandidateFile
               -> Separators -> [ParameterPattern] -> String
               -> Logic ([NamedParamMatch], CandidateFile, String)
rootParamMatch origRoot seps params rootCmp =
  if null seps
  then rootParamMatchNoSeps origRoot seps params
  else rootParamFileMatches origRoot seps params rootCmp


rootParamFileMatches :: CandidateFile
                     -> Separators -> [ParameterPattern] -> String
                     -> Logic ([NamedParamMatch], CandidateFile, String)
rootParamFileMatches rootF seps parms rMatch = do
  let rnSplit = sepSplit $ candidateFile rootF
      sepSplit = L.groupBy sepPoint
      sepPoint a b = not $ or [a `elem` seps, b `elem` seps ]
      rnPartIndices = [ n | n <- [0 .. length rnParts - 1] , even n ]
      freeValueParm = L.find (isNothing . snd) parms

      txtRootSfx = sepSplit $ reverse $
                   -- Find the concrete extension in the
                   -- rootName. Somewhat crude, but basically stops at
                   -- any character that could be part of a filemanip
                   -- GlobPattern.
                   takeWhile (not . flip elem "[*]\\(|)") $ reverse rMatch

      -- if a part of the root filename matches a known parameter value, that is
      -- the only way that part can be interpreted, and that anchors it.

      rnParts :: [RootPart]
      rnParts =
        let assignPart (ptxt,pidx) =
              let matchesParmValue (_, Nothing) = False
                  matchesParmValue (_, Just vl) = ptxt `elem` vl
              in if pidx `elem` rnPartIndices
                 then
                   if length rnSplit - pidx < length txtRootSfx
                   then RootSuffix ptxt
                   else case L.find matchesParmValue parms of
                          Just (pn,_) -> RootParNm pn ptxt
                          Nothing -> RootText ptxt
                 else RootSep ptxt
        in fmap assignPart $ zip rnSplit [0..]

  -- want [prefix, sep, MATCHES, [suffix]]
  guard (length rnSplit > 2 + length txtRootSfx)

  let hasDupParNm =
        let getParNm = \case
              RootParNm pn _ -> Just pn
              _ -> Nothing
            parNms = catMaybes (getParNm <$> rnParts)
        in not $ length parNms == length (L.nub parNms)

  guard (not hasDupParNm)

  guard (not $ isRootParNm $ head rnParts) -- must have a prefix

  let rnChunks =
        --  pfx parms1 mid parms2 sfx
        --      r1-------------------
        --             r2------------
        --                 r3--------
        let (pfx,r1)     = L.span (not . isRootParNm) rnParts
            (parms1,r2)  = L.span paramPart r1
            (mid,r3)     = L.span (not . isRootParNm) r2
            (parms2,sfx) = L.span paramPart r3
            (_,extraprm) = L.span (not . isRootParNm) sfx
            paramPart x = isRootParNm x || isRootSep x
        in if null r3
           then Just $ Left (pfx, parms1, mid)
           else if null extraprm
                then Just $ Right (pfx, parms1, mid, parms2, sfx)
                else Nothing

      freeFirst Nothing = mzero
      freeFirst (Just (Right _)) = mzero
      freeFirst (Just (Left (allRP, [], []))) =
        -- There were no parameter value matches.  If there is
        -- a wildcard parameter, try it in all the possible
        -- positions.
        if length allRP < 3
        then mzero
        else case freeValueParm of
               Nothing -> mzero
               Just p ->
                 do idx <- eachFrom [i | i <- [2..length allRP], even i]
                    case drop idx allRP of
                      (RootText idxv:_) -> do
                        let free = RootParNm (fst p) idxv
                            start = take (idx - 1) allRP
                        return ( rpNPM [free]
                               , rpStr $ start
                               , rpStr $ drop (idx + 2) allRP )
                      _ -> mzero
      freeFirst (Just (Left (pfx, pl1, sfx))) =
        case freeValueParm of
          Nothing ->
            -- No wildcard param, so just try the observed
            -- pattern
            return ( rpNPM pl1, rpStr $ init pfx, rpStr sfx )
          Just p ->
            if length pfx < 3
            then
              -- not enough elements of pfx to support a wildcard, so there must
              -- be no expression of the wildcard and just the observed pattern.
              -- Also remove any separator from the prefix.
              return ( rpNPM pl1, rpStr $ take 1 pfx, rpStr sfx )
            else
              -- There is a wildcard parameter, try it at the end
              -- of pfx and before pl1
              case reverse pfx of
                (_:RootText lpv:_) ->
                  return ( rpNPM $ RootParNm (fst p) lpv : pl1
                         , rpStr $ reverse $ drop 3 $ reverse pfx
                         , rpStr sfx )
                _ -> mzero

      freeLast Nothing = mzero
      freeLast (Just (Right _)) = mzero
      freeLast (Just (Left (_, [], []))) = mzero -- handled by freeFirst
      freeLast (Just (Left (_, _, []))) = mzero
      freeLast (Just (Left (pfx, parms1, sfx))) =
        case freeValueParm of
          Nothing -> mzero  -- handled by freeFirst
          Just p ->
            -- There is a wildcard parameter, try it at the end
            -- of pfx and before pl1
            case sfx of
              (RootText fsv:_) ->
                return ( rpNPM $ parms1 <> [RootParNm (fst p) fsv]
                       , rpStr $ init pfx
                       , rpStr $ tail sfx )
              _ -> mzero

      freeMid Nothing = mzero
      freeMid (Just (Left _)) = mzero
      freeMid (Just (Right (pfx, parms1, mid, parms2, sfx))) =
        -- If there is a wildcard param and mid is a single
        -- element, then try converting the mid to the
        -- wildcard, otherwise this is an invalid name.
        if length mid /= 3
        then mzero
        else case freeValueParm of
               Nothing -> mzero
               Just p ->
                 case mid of
                   (ms1:RootText mv:ms2:[]) ->
                     return ( rpNPM ( parms1 <> [RootParNm (fst p) mv] <>
                                      parms2 )
                            , rpStr $ pfx <> [ms1]
                            , rpStr $ ms2 : sfx )
                   _ -> mzero

  (\(a,fn,b) -> (a, rootF { candidateFile = fn }, b))
    <$> ((freeFirst rnChunks)
         `mplus` (freeLast rnChunks)
         `mplus` (freeMid rnChunks))


-- If no separators, there are no "rnParts" identifiable, so fall
-- back on a cruder algorithm that simply attempts to find a
-- sequence of paramvals in the middle of the string and extract
-- the prefix and suffix (if any) around those paramvals.
rootParamMatchNoSeps :: CandidateFile -> Separators -> [ParameterPattern]
                     -> Logic ([NamedParamMatch], CandidateFile, String)
rootParamMatchNoSeps rootF seps' parms = do
  pseq <- eachFrom $ filter (not . null) $ L.permutations parms
  pvals <- getPVals pseq
  (pvset, _pvcnt, pvstr) <- pvalMatch seps' [] pvals
  -- _pvcnt can be ignored because each is a different root
  let explicit = filter (isExplicit . snd) pvset
  let rootNm = candidateFile rootF
  guard (and [ not $ null explicit
             , pvstr `L.isInfixOf` rootNm
             , not $ pvstr `L.isPrefixOf` rootNm
             ])
  let l1 = length rootNm
      l2 = length pvstr
      bslen = l1 - l2
      matches n = pvstr `L.isPrefixOf` (drop n rootNm)
  case L.find matches $ reverse [1..bslen] of
    Just pfxlen ->
      let basefname = take pfxlen rootNm
          basename = rootF { candidateFile = basefname }
          suffix = drop (pfxlen + l2) rootNm
      in return (explicit, basename, suffix)
    _ -> mzero

-- Return origRootName up to each sep-indicated point.
noRootParamMatch :: CandidateFile -> Separators
                 -> Logic ([NamedParamMatch], CandidateFile, String)
noRootParamMatch origRoot seps =
  return ([], origRoot, "") `mplus`
  do s <- eachFrom seps
     let origRootName = candidateFile origRoot
     i <- eachFrom [1..length origRootName - 1]
     let a = origRoot { candidateFile = take i origRootName }
     let b = drop i origRootName
     if null b
       then do return ([], a, "")
       else do guard (and [ not $ null b, head b == s ])
               return ([], a, tail b)
