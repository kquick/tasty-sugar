{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Sugar.Candidates
  (
    findCandidates
  , candidateToPath
  )
where

import Control.Monad ( filterM )
import System.Directory ( doesDirectoryExist, getCurrentDirectory
                        , listDirectory, doesDirectoryExist )
import System.FilePath ( (</>), isRelative, makeRelative
                       , splitPath, takeDirectory, takeFileName)

import Test.Tasty.Sugar.Types


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


makeCandidate :: CUBE -> FilePath -> [String] -> FilePath -> CandidateFile
makeCandidate cube topDir subPath fName =
  CandidateFile { candidateDir = topDir
                , candidateSubdirs = subPath
                , candidateFile = fName
                }


-- | This converts a CandidatFile into a regular FilePath for access
candidateToPath :: CandidateFile -> FilePath
candidateToPath c =
  candidateDir c </> foldr (</>) (candidateFile c) (candidateSubdirs c)
