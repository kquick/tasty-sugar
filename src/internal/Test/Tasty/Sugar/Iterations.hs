{-# LANGUAGE FlexibleContexts #-}

module Test.Tasty.Sugar.Iterations
where

import           Control.Monad ( mplus, mzero )
import           Control.Monad.Logic
import           Control.Monad.State ( StateT, runStateT, modify )
import           Data.Functor.Identity ( Identity, runIdentity )
import qualified Data.Map as Map
import           Data.Text ( Text )


type IterStat = Map.Map Text Int

emptyStats :: IterStat
emptyStats = mempty

joinStats :: IterStat -> IterStat -> IterStat
joinStats = Map.unionWith (+)


----------------------------------------------------------------------

type LogicI a = LogicT (StateT IterStat Identity) a

addSubLogicStats :: (a, IterStat) -> LogicI a
addSubLogicStats (r, stats) = do modify $ joinStats stats
                                 return r

observeIAll :: LogicI a -> ([a], IterStat)
observeIAll op = runIdentity $ runStateT (observeAllT op) emptyStats


----------------------------------------------------------------------

-- | Core Logic function to iteratively return elements of a list via
-- backtracking.
eachFrom :: Text -> [a] -> LogicI a
-- eachFrom location = foldr (mplus . return) mzero
eachFrom location =
  let attempt c a = do modify $ Map.insertWith (+) location 1
                       return c `mplus` a
  in foldr attempt mzero
