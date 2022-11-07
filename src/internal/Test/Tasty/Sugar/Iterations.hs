module Test.Tasty.Sugar.Iterations
where

import Control.Monad ( mplus, mzero )
import Control.Monad.Logic


-- | Core Logic function to iteratively return elements of a list via
-- backtracking.
eachFrom :: [a] -> Logic a
eachFrom = foldr (mplus . return) mzero
