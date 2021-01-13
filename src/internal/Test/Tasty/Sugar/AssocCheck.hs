-- | Function and implementation to find association files for an
-- identified test root file.

{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Sugar.AssocCheck
  (
    getAssoc
  )
  where

import           Control.Monad.Logic
import qualified Data.List as L
import           Data.Maybe ( catMaybes )

import           Test.Tasty.Sugar.ParamCheck
import           Test.Tasty.Sugar.Types




-- | For a specific NamedParamMatch, find all associated files having
-- the rootMatch plus the named parameter values (in the same order
-- but with any combination of separators) and the specified suffix
-- match.
getAssoc :: FilePath
         -> Separators
         -> [NamedParamMatch]
         -> [ (String, FileSuffix) ]
         -> [FilePath]
         -> Logic [(String, FilePath)]
getAssoc rootPrefix seps pmatch assocNames allNames = assocSet
  where
    assocSet = catMaybes <$> mapM fndAnAssoc assocNames

    fndAnAssoc assoc = ifte (fndAssoc assoc)
                       (return . Just)
                       (return Nothing)

    fndAssoc assoc =
      do pseq <- npseq pmatch
         (assocPfx, assocSfx) <- sepParams seps (fmap snd pseq)
         if null assocSfx
           then do let assocNm = rootPrefix <> assocPfx <> (snd assoc)
                   guard (assocNm `elem` allNames)
                   return (fst assoc, assocNm)
           else let assocStart = rootPrefix <> assocPfx
                    assocEnd = assocSfx <> snd assoc
                    aSL = length assocStart
                    aEL = length assocEnd
                    possible f =
                      and [ assocStart `L.isPrefixOf` f
                          , assocEnd `L.isSuffixOf` f
                          , length f > (aSL + aEL)
                          , let mid = drop aSL (take (length f - aEL) f)
                            in and $ fmap (not . flip elem mid) seps
                          ]
                    fnd = filter possible allNames
                in do f <- eachFrom fnd
                      return (fst assoc, f)

    sepParams :: Separators -> [ParamMatch] -> Logic (String, String)
    sepParams sl = \case
      [] -> if null sl
            then return ([], [])
            else do s <- eachFrom sl
                    return ([s], [])
      (NotSpecified:ps) -> do r <- sepParams sl ps
                              return ([], fst r)
      ((Explicit v):ps) -> do (l,r) <- sepParams sl ps
                              if null sl
                                then return (v <> l, r)
                                else do s <- eachFrom sl
                                        return ([s] <> v <> l, r)
      ((Assumed  v):ps) -> do (l,r) <- sepParams sl ps
                              if null sl
                                then return (v <> l, r)
                                else do s <- eachFrom sl
                                        return ([s] <> v <> l, r)

    npseq = eachFrom
            . ([]:)                -- consider no parameters just once
            . filter (not . null)  -- excluding multiple blanks in
            . concatMap L.inits    -- any number of the
            . L.permutations       -- parameters in each possible order
