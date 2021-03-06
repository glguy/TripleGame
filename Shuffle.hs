-- | Utility functions for providing random elements to the game.
module Shuffle where

import System.Random

-- | Select an element from a non-empty list with uniform distribution.
randomElement :: [a] -> IO a
randomElement xs
  | n == 0 = fail "randomElement: no elements"
  | otherwise = do r <- randomRIO (0,n-1)
                   return $! xs !! r
  where
  n = length xs

-- | Select an element from a non-empty list with custom distribution.
randomElementDist :: [(a,Int)] -> IO a
randomElementDist dist
  | total <= 0 = fail "randomElementDist: bad distribution"
  | otherwise  = do
       r <- randomRIO (1,total)
       return $! select r dist
  where
  total = sum (map snd dist)

  select r ((x,v):xs)
    | r <= v = x
    | otherwise = select (r-v) xs
  select _ _ = error "select: impossible"
