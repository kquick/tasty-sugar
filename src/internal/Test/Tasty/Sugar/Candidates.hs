{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.Tasty.Sugar.Candidates
  (
    candidateToPath
  , findCandidates
  , makeCandidate
  )
where

import           Control.Monad ( filterM, guard )
import           Data.Bifunctor ( first )
import qualified Data.List as DL
import           Data.Maybe ( fromMaybe, isNothing )
import           Numeric.Natural
import           System.Directory ( doesDirectoryExist, getCurrentDirectory
                                  , listDirectory, doesDirectoryExist )
import           System.FilePath ( (</>), isRelative, makeRelative
                                 , splitPath, takeDirectory, takeFileName)

import           Test.Tasty.Sugar.Iterations
import           Test.Tasty.Sugar.Types


findCandidates :: CUBE -> FilePath -> IO ([Either String CandidateFile])
findCandidates cube inDir =
  let collectDirEntries d =
        let recurse = takeFileName d == "*"
            top = if recurse then Just (takeDirectory d) else Nothing
            start = if recurse then takeDirectory d else d
        in dirListWithPaths top start
      dirListWithPaths topDir d =
        -- putStrLn ("Reading " <> show d) >>
        doesDirectoryExist d >>= \case
          True ->
            do dirContents <- listDirectory d
               case topDir of
                 Nothing -> do
                   let mkC = makeCandidate cube d []
                   return (Right . mkC <$> dirContents)
                 Just topdir -> do
                   let subs = filter (not . null)
                              (init
                               <$> init (splitPath
                                          $ makeRelative topdir (d </> "x")))
                   let mkC = makeCandidate cube topdir subs
                   subdirs <- filterM (doesDirectoryExist . (d </>)) dirContents
                   let here = Right . mkC <$> (filter (not . (`elem` subdirs)) dirContents)
                   subCandidates <- mapM (dirListWithPaths topDir)
                                    ((d </>) <$> subdirs)
                   return $ here <> (concat subCandidates)
          False -> do
            showD <- case isRelative d of
                       True -> do cwd <- getCurrentDirectory
                                  return $ "[" <> cwd <> "/]" <> d
                       False -> return d
            return [Left $ showD <> " does not exist"]
  in collectDirEntries inDir


-- | Create a CandidateFile entry for this top directory, sub-paths, and
-- filename.  In addition, any Explicit parameters with known values that appear
-- in the filename are captured.  Note that:
--
-- * There may be multiple possible matches for a single parameter (e.g. the
--   value is repeated in the name or path, or an undefind value (Nothing)
--   parameter could have multiple possible values extracted from the filename.
--
-- * File name matches are preferred over sub-path matches and will occlude the
--   latter.
--
-- * All possible filename portions and sub-paths will be suggested for non-value
-- * parameters (validParams with Nothing).
--
makeCandidate :: CUBE -> FilePath -> [String] -> FilePath -> CandidateFile
makeCandidate cube topDir subPath fName =
  let fl = DL.length fName
      isSep = (`elem` separators cube)
      firstSep = maybe fl (+1) $ DL.findIndex isSep fName
      fle = maybe fl (fl-) $ DL.findIndex isSep $ DL.reverse fName
      pmatches = fst $ observeIAll
                 $ do p <- eachFrom "param for candidate" $ validParams cube
                      v <- eachFrom "value for param" (fromMaybe [] (snd p))
                      -- Note: there maybe multiple v values for a single p that
                      -- are matched in the name.  This is accepted here (and
                      -- this file presumably satisfies either with an Explicit
                      -- match).
                      let vl = DL.length v
                      i <- eachFrom "param starts"
                           $ DL.findIndices (`elem` (separators cube)) fName
                      let vs = i + 1
                      let ve = vs + vl
                      if and [ ve + 1 < fl  -- v fits in fName[i..]
                             , v == DL.take vl (DL.drop vs fName)
                             , head (DL.drop ve fName) `elem` (separators cube)
                             ]
                         then return ((fst p, Explicit v), (toEnum vs, ve))
                        else do guard $ v `elem` subPath
                                return ((fst p, Explicit v), (0, 0))
      pmatchArbitrary =
        case DL.find (isNothing . snd) $ validParams cube of
          Nothing -> []
          Just (p,_) ->
            let chkRange = [(firstSep, fle)]
                arbs = holes chkRange (snd <$> pmatches)
                getRange (s,e) = let s' = fromEnum s in DL.take (e - s' - 1) $ DL.drop s' fName
                holeVals = (\r -> (getRange r, r)) <$> arbs
                pvals = getParamVal . snd . fst <$> pmatches
                dirVals = (, (0,0)) <$> filter (not . (`elem` pvals) . Just) subPath
            in (first ((p,) . Explicit)) <$> (holeVals <> dirVals)
      pAll = pmatches <> pmatchArbitrary
  in CandidateFile { candidateDir = topDir
                   , candidateSubdirs = subPath
                   , candidateFile = fName
                   -- nub the results in case a v value appears twice in a single
                   -- file.  Sort the results for stability in testing.
                   , candidatePMatch = DL.nub $ DL.sort $ (fst <$> pAll)
                   , candidateMatchIdx = minimum
                                         $ toEnum fle
                                         : filter (/= 0) (fst . snd <$> pAll)
                   }


-- Remove present from chkRange leaving holes.
holes :: [(Int,Int)] -> [(Natural,Int)] -> [(Natural,Int)]
holes chkRange present =
  let rmvKnown _ [] = []
      rmvKnown p@(ps,pe) ((s,e):rs) =
        if abs(ps-s) <= 1
        then if pe > e
             then rmvKnown (toEnum e,pe) rs
             else if abs(pe-e) <= 1
                  then rs
                  else (toEnum pe + 1, e) : rs -- KWQ 1
        else if ps >= s && fromEnum ps < e
             then if abs(pe - e) <= 1
                  then (s, fromEnum ps) : rs
                  else if pe < e
                       then (s, fromEnum ps) : (toEnum pe, e) : rs
                       else (s, fromEnum ps) : rmvKnown (toEnum e, pe) rs
             else rmvKnown p rs
      r' = filter (\x -> fst x /= snd x) chkRange
  in foldr rmvKnown (first toEnum <$> r') (DL.sort present)


-- | This converts a CandidatFile into a regular FilePath for access
candidateToPath :: CandidateFile -> FilePath
candidateToPath c =
  candidateDir c </> foldr (</>) (candidateFile c) (candidateSubdirs c)
