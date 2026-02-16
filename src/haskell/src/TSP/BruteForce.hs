-- | TSP.BruteForce: exact solution via permutation enumeration.
-- Start/finish at city 1, permutations of [2..N].
module TSP.BruteForce
  ( solveBruteForce,
  )
where

import Data.List (permutations, minimumBy)
import TSP.Cost (tourCost)
import TSP.IO (Matrix)

-- | Build closed tour: [1] ++ perm ++ [1]
buildTour :: [Int] -> [Int]
buildTour perm = 1 : perm ++ [1]

-- | Find optimal (cost, tour). Cities 1..N, start at 1.
solveBruteForce :: Matrix -> (Int, [Int])
solveBruteForce m
  | n == 1 = (0, [1, 1])
  | otherwise =
      minimumBy (\(c1, _) (c2, _) -> compare c1 c2) $
        [ (tourCost m tour, tour)
          | perm <- permutations [2 .. n],
            let tour = buildTour perm
        ]
  where
    n = length m
