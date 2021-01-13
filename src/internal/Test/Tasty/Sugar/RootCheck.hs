-- | Function and associated helpers to determine the matching root
-- name.  The root name may contain zero or more parameter values.

{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Sugar.RootCheck
  (
    rootMatch
  )
  where

import           Control.Monad.Logic
import qualified Data.List as L
import           Data.Maybe ( catMaybes, isNothing )

import           Test.Tasty.Sugar.ParamCheck
import           Test.Tasty.Sugar.Types


-- | Determine which parts of the input name form the basePrefix and any
-- parameter values for searching for related files (expected and
-- associated)
rootMatch :: FilePath -> Separators -> [ParameterPattern] -> String
          -> Logic ([NamedParamMatch], FilePath, FilePath)
rootMatch origRootName seps params rootCmp =
  ifte
  (rootParamMatch origRootName seps params rootCmp)
  return
  (noRootParamMatch origRootName seps)


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

isRootSuffix :: RootPart -> Bool
isRootSuffix (RootSuffix _) = True
isRootSuffix _ = False

rpStr :: [RootPart] -> String
rpStr = let s = \case
              RootSep x -> x
              RootParNm _ x -> x
              RootText x -> x
              RootSuffix x -> x
            bld a b = a <> s b
        in foldl bld ""

rpNPM :: [RootPart] -> [NamedParamMatch]
rpNPM = let bld (RootParNm n v) = Just [(n, Explicit v)]
            bld (RootSep _) = Nothing
            bld p = error ("Invalid RootPart for NamedParamMatch: "
                           <> show p)
        in concat . catMaybes . fmap bld


-- Return the prefix and suffix of the root name along with the
-- explicit parameter matches that comprise the central portion.
rootParamMatch :: FilePath -> Separators -> [ParameterPattern] -> String
               -> Logic ([NamedParamMatch], FilePath, FilePath)
rootParamMatch origRootName seps params rootCmp =
  if null seps
  then rootParamMatchNoSeps origRootName seps params
  else rootParamMatches origRootName seps params rootCmp

rootParamMatches :: FilePath -> Separators -> [ParameterPattern] -> String
                 -> Logic ([NamedParamMatch], FilePath, FilePath)
rootParamMatches rootNm seps parms rMatch = do
  let rnSplit = sepSplit rootNm
      sepSplit = L.groupBy sepPoint
      sepPoint a b = not $ or [a `elem` seps, b `elem` seps ]
      rnPartIndices = [ n | n <- [0 .. length rnParts - 1] , even n ]
      freeValueParm = L.find (isNothing . snd) parms

      txtRootSfx = sepSplit $ reverse $
                   -- Find the concrete extension in the
                   -- rootName. Somewhat crude, but basically stops at
                   -- any charcter that could be part of a filemanip
                   -- GlobPattern.
                   takeWhile (not . flip elem "[*]\\(|)") $ reverse rMatch

      -- if a part of the rootNm matches a known parameter value,
      -- that is the only way that part can be interpreted, and
      -- that anchors it.

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
                    let free = RootParNm (fst p) idxv
                        RootText idxv = head $ drop idx allRP
                        start = take (idx - 1) allRP
                    guard (not $ isRootSuffix $ head $ drop idx allRP)
                    return ( rpNPM [free]
                           , rpStr $ start
                           , rpStr $ drop (idx + 2) allRP )
      freeFirst (Just (Left (pfx, pl1, sfx))) =
        if length pfx < 3
        then mzero
        else case freeValueParm of
               Nothing ->
                 -- No wildcard param, so just try the observed
                 -- pattern
                 return ( rpNPM pl1, rpStr pfx, rpStr sfx )
               Just p ->
                 -- There is a wildcard parameter, try it at the end
                 -- of pfx and before pl1
                 let free = RootParNm (fst p) lpv
                     RootText lpv = last start
                     start = init pfx
                 in do guard (not . isRootSuffix $ last start)
                       return ( rpNPM $ free : pl1
                              , rpStr $ reverse $ drop 3 $ reverse pfx
                              , rpStr sfx )

      freeLast Nothing = mzero
      freeLast (Just (Right _)) = mzero
      freeLast (Just (Left (_, [], []))) = mzero -- handled by freeFirst
      freeLast (Just (Left (pfx, parms1, sfx))) =
        if null sfx
        then mzero
        else case freeValueParm of
               Nothing -> mzero  -- handled by freeFirst
               Just p ->
                 -- There is a wildcard parameter, try it at the end
                 -- of pfx and before pl1
                 let free = [RootParNm (fst p) fsv]
                     RootText fsv = head sfx
                 in do guard (not $ isRootSuffix $ head sfx)
                       return ( rpNPM $ parms1 <> free
                              , rpStr pfx
                              , rpStr $ tail sfx )

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
                 let free = [RootParNm (fst p) mv]
                     (ms1:RootText mv:ms2:[]) = mid
                 in return ( rpNPM $ parms1 <> free <> parms2
                           , rpStr $ pfx <> [ms1]
                           , rpStr $ ms2 : sfx )

  (freeFirst rnChunks)
    `mplus` (freeLast rnChunks)
    `mplus` (freeMid rnChunks)


-- If no separators, there are no "rnParts" identifiable, so fall
-- back on a cruder algorithm that simply attempts to find a
-- sequence of paramvals in the middle of the string and extract
-- the prefix and suffix (if any) around those paramvals.
rootParamMatchNoSeps :: FilePath -> Separators -> [ParameterPattern]
                     -> Logic ([NamedParamMatch], FilePath, FilePath)
rootParamMatchNoSeps rootNm seps' parms = do
  pseq <- eachFrom $ filter (not . null) $ L.permutations parms
  pvals <- getPVals pseq
  (pvset, _pvcnt, pvstr) <- pvalMatch seps' [] pvals
  -- _pvcnt can be ignored because each is a different root
  let explicit = filter (isExplicit . snd) pvset
  guard (and [ not $ null explicit
             , pvstr `L.isInfixOf` rootNm
             , not $ pvstr `L.isPrefixOf` rootNm
             ])
  let (basename, suffix) =
        let l1 = length rootNm
            l2 = length pvstr
            bslen = l1 - l2
            matches n = pvstr `L.isPrefixOf` (drop n rootNm)
            Just pfxlen = L.find matches $ reverse [1..bslen]
        in (take pfxlen rootNm, drop (pfxlen + l2) rootNm)
  return (explicit, basename, suffix)

-- Return origRootName up to each sep-indicated point.
noRootParamMatch :: FilePath -> Separators
                 -> Logic ([NamedParamMatch], FilePath, FilePath)
noRootParamMatch origRootName seps =
  return ([], origRootName, "") `mplus`
  do s <- eachFrom seps
     i <- eachFrom [1..length origRootName - 1]
     let a = take i origRootName
     let b = drop i origRootName
     if null b
       then do return ([], a, "")
       else do guard (and [ not $ null b, head b == s ])
               return ([], a, tail b)
