{-# LANGUAGE FlexibleContexts #-}
module Metrics where
import Data.Array.Repa as R (Array, U, DIM1, fromListUnboxed, Z (Z), (:.) ((:.)), (!), index, Shape (size), Source (extent), DIM0, zipWith, D, computeUnboxedS )

-- | manhattanDist calculates the total distance of the current state (cur) to the goal board with size (n), performing recurrsion using (idx)
manhattanDist :: Source r Int =>  Array r DIM1 Int -> Int -> Int-> Int
manhattanDist cur idx n | idx == R.size (R.extent cur) = 0
                        | otherwise = diff idx (cur ! ( Z :. idx)) + manhattanDist cur (idx+1) n
                        where diff x y = abs (x `mod` n - y `mod` n) + abs (x `div` n - y `div` n)

-- | hammingDist calculates the number of wrong tiles of the current state (cur) to the goal board with size (n), performing recursion using (idx)
hammingDist :: Source r Int =>  Array r DIM1 Int -> Int -> Int ->Int
hammingDist cur idx n | idx == R.size (R.extent cur) = 0
                      | otherwise = diff idx (cur!(Z :. idx)) + hammingDist cur (idx+1) n
                      where diff x y | x == y = 1
                                     | otherwise = 0