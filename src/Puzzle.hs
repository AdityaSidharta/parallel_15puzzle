{-# LANGUAGE FlexibleContexts #-}
module Puzzle where
import Data.Array.Repa as R (Array, U, DIM1, fromListUnboxed, Z (Z), (:.) ((:.)), (!), index, Shape (size), Source (extent), DIM0, zipWith, D, computeUnboxedS )
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')
import Metrics (manhattanDist)
import Data.Maybe (catMaybes)
import Control.Parallel.Strategies (runEval, rpar)

-- | PuzzleState contains the current moves (fn), distance to goal (gn), current position of blank tile (zeroPos), and the current board state (state)
data PuzzleState = PuzzleState {fn::Int,
                                gn::Int,
                                zeroPos::Int,
                                state::Array U DIM1 Int} deriving (Show, Eq)

-- | cmpUboxarray performs comparison between two different arrays, perfomed by doing pairwise comparison across the subsequent values in the two arrays
cmpUboxarray:: Array U DIM1 Int -> Array U DIM1 Int -> Ordering
cmpUboxarray a1 a2 = cmp a1 a2 0
    where cmp a1 a2 idx | idx ==  R.size (R.extent a1) = GT
                        | a1!(Z :. idx) == a2!(Z :. idx) = cmp a1 a2 (idx+1)
                        | otherwise = compare (a1!(Z :. idx)) (a2!(Z :. idx))

-- | PuzzleState is ordered by the total incurred cost and distance to goal (fn + gn). Else, it perform comparison between the two array
instance Ord PuzzleState where
    PuzzleState a b _ s1 `compare` PuzzleState c d _ s2 = if a+b /= c+d then (a+b) `compare` (c+d) else cmpUboxarray s1 s2

-- | generateArrays returns k number of shuffled matrix of size n for the input of 15-puzzle problem
generateArrays :: (Num a, Enum a) => Int -> a -> [[a]]
generateArrays 0 _ = []
generateArrays k n = let xs = [0..(n * n - 1)] in shuffle' xs (length xs) (mkStdGen k) : generateArrays (k -1) n

-- | formatArray takes the array (a) and the size of the puzzle (n) and return it as a string, according to the input text format of this program
formatArray :: [Int] -> Int -> String
formatArray [] n = ""
formatArray a n = unwords (map show (take n a)) ++ "\n" ++ formatArray (drop n a) n

-- | formatArrays takes the arrays (a:as) and return it as a string according to the input text format of this program
formatArrays :: [[Int]] -> String
formatArrays [] = ""
formatArrays (a:as) = show n ++ "\n" ++ formatArray a n ++ formatArrays as
    where
        n = floor(sqrt(fromIntegral(length a))) :: Int

-- | writeArrays takes the arrays and write it into the filename according to the input text format of this program
writeArrays :: [[Int]] -> FilePath -> IO ()
writeArrays arrays filename =
    writeFile filename (show n ++ "\n" ++ formatArrays arrays)
      where
        n = length arrays


-- | getZeroPos returns the idx within the given array (arr) where the blank tile is located. If fail, return -1
getZeroPos :: Source r Int => Array r DIM1 Int -> Int -> Int
getZeroPos arr idx | idx == R.size (R.extent arr) = -1
                   | arr!(Z :. idx) == 0 = idx
                   | otherwise = getZeroPos arr (idx+1)

-- | swapTwo perform swap between two elements in the array (arr), given two indexes, f and s in the array
swapTwo :: Source r Int => Int -> Int -> Array r DIM1 Int -> Array D DIM1 Int
swapTwo f s arr = R.zipWith (\x y->
    if      x == f then arr!(Z :. s)
    else if x == s then arr!(Z :. f)
    else y)  (fromListUnboxed sh [0..(R.size sh -1)]) arr
    where sh = R.extent arr


-- | getUpNeighbor return the subsequent PuzzleState by swapping the blank tile with the tile above it. If its impossible, return Nothing
getUpNeighbor :: PuzzleState -> Int -> Maybe PuzzleState
getUpNeighbor (PuzzleState f g ze reparray) n | row < 0 = Nothing
                                              | otherwise = Just $ PuzzleState (f+1) newg (row*n+col) newarray
  where oldrow = ze `div` n
        row = oldrow - 1
        col = ze `mod` n
        newarray = computeUnboxedS $ swapTwo (oldrow*n+col) (row*n+col) reparray
        newg = manhattanDist newarray 0 n

-- | getDownNeighbor return the subsequent PuzzleState by swapping the blank tile with the tile below it. If its impossible, return Nothing
getDownNeighbor :: PuzzleState -> Int -> Maybe PuzzleState
getDownNeighbor (PuzzleState f g ze reparray) n | row >= n = Nothing
                                                | otherwise = Just $ PuzzleState (f+1) newg (row*n+col) newarray
  where oldrow = ze `div` n
        row = oldrow + 1
        col = ze `mod` n
        newarray = computeUnboxedS $ swapTwo (oldrow*n+col) (row*n+col) reparray
        newg = manhattanDist newarray 0 n

-- | getLeftNeighbor return the subsequent PuzzleState by swapping the blank tile with the tile left to it. If its impossible, return Nothing
getLeftNeighbor :: PuzzleState -> Int -> Maybe PuzzleState
getLeftNeighbor (PuzzleState f g ze reparray) n | col < 0 = Nothing
                                                | otherwise = Just $ PuzzleState (f+1) newg (row*n+col) newarray
  where oldcol = ze `mod` n
        row = ze `div` n
        col = oldcol - 1
        newarray = computeUnboxedS $ swapTwo (row*n+oldcol) (row*n+col) reparray
        newg = manhattanDist newarray 0 n

-- | getRightNeighbor return the subsequent PuzzleState by swapping the blank tile with the tile right to it. If its impossible, return Nothing
getRightNeighbor :: PuzzleState -> Int -> Maybe PuzzleState
getRightNeighbor (PuzzleState f g ze reparray) n | col >=n = Nothing
                                                 | otherwise = Just $ PuzzleState (f+1) newg (row*n+col) newarray
  where oldcol = ze `mod` n
        row = ze `div` n
        col = oldcol + 1
        newarray = computeUnboxedS $ swapTwo (row*n+oldcol) (row*n+col) reparray
        newg = manhattanDist newarray 0 n

-- | getAllNeighbor return all of the neighboring state of the current PuzzleState
getAllNeighbor:: PuzzleState -> Int -> [PuzzleState]
getAllNeighbor p n = [x | Just x <- [getUpNeighbor p n, getDownNeighbor p n, getLeftNeighbor p n, getRightNeighbor p n]]


-- | getAllNeighborPar return all of the neighboring state of the current PuzzleState
getAllNeighborPar:: PuzzleState -> Int -> [PuzzleState]
getAllNeighborPar p n = catMaybes (runEval $ do
    a <- rpar (getUpNeighbor p n)
    b <- rpar (getDownNeighbor p n)
    c <- rpar (getLeftNeighbor p n)
    d <- rpar (getRightNeighbor p n)
    return [a, b, c, d])


-- | numinv check the number of inversions in the board (arr)
numinv :: Array U DIM1 Int -> Int
numinv arr = aux arr 0 1 0
    where aux arr i j r | i == R.size (R.extent arr) = r
                        | j == R.size (R.extent arr) = aux arr (i+1) (i+2) r
                        | arr!(Z:.i) == 0 || arr!(Z:.j) == 0 = aux arr i (j+1) r
                        | arr!(Z:.i) > arr!(Z:.j) = aux arr i (j+1) r
                        | arr!(Z:.i) < arr!(Z:.j) = aux arr i (j+1) (r+1)
                        | otherwise = error "inversion error!"

-- | solvability checks whether the given board (arr) with the current zero position (zeropos) is solvable 8-puzzle problem
solvability:: Array U DIM1 Int -> Int -> Int -> Bool
solvability arr zeropos n | odd n && even (numinv arr) = True
                          | even n && even (zeropos `div` n + 1) && even (numinv arr) = True
                          | even n && odd  (zeropos `div` n + 1) && odd  (numinv arr) = True
                          | otherwise  = False