-- | TSP.Cost: compute tour cost from distance matrix.
-- Tour format: closed [1, ..., 1], cities 1-indexed.
module TSP.Cost
  ( tourCost,
  )
where

import TSP.IO (Matrix)

-- | Cost of closed tour: sum of matrix[from][to] for adjacent pairs.
-- Matrix and tour are 0-indexed in implementation; conceptually 1-indexed.
tourCost :: Matrix -> [Int] -> Int
tourCost m tour =
  sum $ zipWith edgeCost tour (tail tour)
  where
    -- 1-indexed: city i is at index i-1
    edgeCost from to = (m !! (from - 1)) !! (to - 1)
