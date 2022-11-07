{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Sugar.Candidates
  (
    candidateToPath
  , findCandidates
  , makeCandidate
  )
where

import           Control.Monad ( filterM, guard )
import qualified Data.List as DL
import           Data.Maybe ( fromMaybe, isNothing )
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
-- in the filename are captured.  Note that a parameter without known values
-- (validParams with Nothing) will not be matched here and must be explicitly
-- checked later).
makeCandidate :: CUBE -> FilePath -> [String] -> FilePath -> CandidateFile
makeCandidate cube topDir subPath fName =
  let fl = DL.length fName
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
                         then return ((fst p, Explicit v), ve)
                        else do guard $ v `elem` subPath
                                return ((fst p, Explicit v), 0)
      pmatchArbitrary =
        case DL.find (isNothing . snd) $ validParams cube of
          Nothing -> []
          Just (p,_) ->
            case DL.reverse $ DL.sortOn snd pmatches of
              (((pl, Explicit vl), el):_) | el > 0 ->
                let remName = DL.drop (el + 1) fName
                in case DL.findIndex (`elem` separators cube) remName of
                     Just el' -> [ (p, Explicit (DL.take el' remName)) ]
                     Nothing -> []
              [] ->
                let isSep = (`elem` separators cube)
                    middle = DL.drop 1
                             $ DL.dropWhile (not . isSep)
                             $ DL.dropWhileEnd (not . isSep) fName
                in case middle of
                     [] -> []
                     m -> let m' = DL.init m
                          in case DL.findIndex isSep m' of
                               Nothing -> [ (p, Explicit m') ]
                               Just _ -> []  -- multiple parts; give up
              _ -> [] -- never happens
  in CandidateFile { candidateDir = topDir
                   , candidateSubdirs = subPath
                   , candidateFile = fName
                   -- nub the results in case a v value appears twice in a single
                   -- file.  Sort the results for stability in testing.
                   , candidatePMatch = DL.nub $ DL.sort
                                       $ (fst <$> pmatches) <> pmatchArbitrary
                   }


-- | This converts a CandidatFile into a regular FilePath for access
candidateToPath :: CandidateFile -> FilePath
candidateToPath c =
  candidateDir c </> foldr (</>) (candidateFile c) (candidateSubdirs c)
