{-# LANGUAGE FlexibleContexts #-}
module Lib where
import System.IO (hGetLine, Handle)
import Data.PSQueue as PQ (PSQ, singleton, size, findMin, deleteMin, key, insert, toList)
import Data.Maybe (fromJust)
import Data.HashMap.Strict as H (HashMap, singleton, member, lookup, insert)
import Data.Array.Repa as R (Array, U, DIM1, fromListUnboxed, Z (Z), (:.) ((:.)), (!), index, Shape (size), Source (extent), DIM0, zipWith, D, computeUnboxedS )
import System.Environment (getProgName, getArgs)
import Control.Monad ( forM )
import GHC.IOArray (IOArray)
import Data.List (sortOn, intercalate)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

-- | PuzzleState contains the current moves (fn), distance to goal (gn), current position of blank tile (zeroPos), and the current board state (state)
data PuzzleState = PuzzleState {fn::Int,
                                gn::Int,
                                zeroPos::Int,
                                state::Array U DIM1 Int} deriving (Show, Eq)

cmpUboxarray:: Array U DIM1 Int -> Array U DIM1 Int -> Ordering
cmpUboxarray a1 a2 = cmp a1 a2 0
    where cmp a1 a2 idx | idx ==  R.size (R.extent a1) = GT
                        | a1!(Z :. idx) == a2!(Z :. idx) = cmp a1 a2 (idx+1)
                        | otherwise = compare (a1!(Z :. idx)) (a2!(Z :. idx))

-- | PuzzleState is ordered by the total incurred cost and distance to goal (fn + gn) 
instance Ord PuzzleState where
    PuzzleState a b _ s1 `compare` PuzzleState c d _ s2 = if (a+b) /= (c+d) then (a+b) `compare` (c+d) else cmpUboxarray s1 s2

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

-- | manhattanDist calculates the total distance of the current state (cur) to the goal board with size (n), performing recurrsion using (idx)
manhattanDist :: Source r Int =>  Array r DIM1 Int -> Int -> Int-> Int
manhattanDist cur idx n | idx == R.size (R.extent cur) = 0
                        | otherwise = diff idx (cur ! ( Z :. idx)) + manhattanDist cur (idx+1) n
                        where diff x y = abs (x `mod` n - (y `mod` n)) + abs (x `div` n - (y `div` n))

-- | hammingDist calculates the number of wrong tiles of the current state (cur) to the goal board with size (n), performing recursion using (idx)
hammingDist :: Source r Int =>  Array r DIM1 Int -> Int -> Int ->Int
hammingDist cur idx n | idx == R.size (R.extent cur) = 0
                      | otherwise = diff idx (cur!(Z :. idx)) + hammingDist cur (idx+1) n
                      where diff x y | x == y = 1
                                     | otherwise = 0

-- | readInt parse the input handle and return an Integer from its first line
readInt :: Handle -> IO Int
readInt handle = do
    str <- hGetLine handle
    return (read str::Int)

-- | printList print a given list (l) into IO
printList::Show a =>[a] -> IO ()
printList l =
    print $ show l

-- | getStateVector parse the input handle and return lists of list of integer, which is the initial game board
getStateVector :: Handle -> Int -> Int -> IO [[Int]]
getStateVector handle n 0 = return []
getStateVector handle n cur = do
    line <- hGetLine handle
    let tokens = (\x -> read x::Int) <$> words line
    post <- getStateVector handle n (cur-1)
    return (tokens:post)


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

-- | getValidNeighbor filters all neighbor puzzles that improves (fn) or have not been discovered previously (not in mp)
getValidNeighbor::[PuzzleState] -> H.HashMap String Int-> [PuzzleState]
getValidNeighbor ps mp = filter (filterInMap mp) ps

-- | filterInMap returns True if the puzzle (puzzle) is not in the HashMap (mp) or if the puzzle can now be reached in less steps (fn)
filterInMap :: HashMap [Char] Int -> PuzzleState -> Bool
filterInMap mp puzzle = not (H.member key mp) || fromJust (H.lookup key mp) > fn puzzle
    where key = getHashKey $ state puzzle

-- | addMap add all of the puzzle states (ps) into the given HashMap (mp)
addMap :: Foldable t => t PuzzleState -> HashMap [Char] Int -> HashMap [Char] Int
addMap ps mp = foldr (\ p -> H.insert (getHashKey (state p)) (fn p)) mp ps

-- | addPSQ adds all of the given puzzle states (ps) into the PriorityQueue (psq)
addPSQ :: [PuzzleState] -> PSQ PuzzleState Int -> PSQ PuzzleState Int
addPSQ ps psq = foldr(\ p -> PQ.insert p (fn p + gn p)) psq ps

-- | getHashKey turns the hash result from the given array (li) and return string as the hash key. hash [0, 3, 1, 2] -> "00030102"
getHashKey:: Array U DIM1 Int -> [Char]
getHashKey li = show $ hash li 0

-- | hash perform simple hash function on the given array (l), using recursive function on idx. hash [0, 3, 1, 2] -> "00030102"
hash :: Integral a => Array U DIM1 Int -> Int -> a
hash l idx | (Z:.idx) == R.extent l = 0
             | otherwise = fromIntegral (l!(Z:.idx)) + 100 * hash l (idx+1)

-- | numinv check the number of inversions in the board (arr)
numinv :: Array U DIM1 Int -> Int
numinv arr = aux arr 0 1 0
    where aux arr i j r | i == R.size (R.extent arr) = r
                        | j == R.size (R.extent arr) = aux arr (i+1) (i+2) r
                        | arr!(Z:.j) == 0 = aux arr i (j+1) r
                        | arr!(Z:.i) < arr!(Z:.j) = aux arr i (j+1) r
                        | arr!(Z:.i) > arr!(Z:.j) = aux arr i (j+1) (r+1)
                        | otherwise = error "inversion error!"

-- | solvability checks whether the given board (arr) with the current zero position (zeropos) is solvable 8-puzzle problem
solvability:: Array U DIM1 Int -> Int -> Int -> Bool
solvability arr zeropos n | odd n && even (numinv arr) = True
                          | even n && even (zeropos `mod` n +1) && even (numinv arr) = True
                          | even n && odd  (zeropos `mod` n +1) && odd  (numinv arr) = True
                          | otherwise  = False

-- | solveKpuzzle perform solving on multiple 8-puzzles using A* algorithm
solveKpuzzle :: Handle -> Int -> IO ()
solveKpuzzle handle 0 =
    return ()
solveKpuzzle handle k = do
    n <- readInt handle
    matrix <- getStateVector handle n n
    let array  = fromListUnboxed (Z :. (n*n) :: DIM1) $ concat matrix
        target = fromListUnboxed (Z :. (n*n) :: DIM1) [0..(n*n-1)]
        gn     = manhattanDist array 0 n
        psq    = PQ.singleton (PuzzleState 0 gn (getZeroPos array 0) array) gn
        mp     = H.singleton (getHashKey array) 0 -- a hashmap storing visited states -> fn
    step  <- solve psq target n mp

    -- output step needed to solve the puzzle
    print step

    solveKpuzzle handle (k-1)

-- | solve perform sequential solving on 8-puzzle using A* algorithm
solve :: PSQ PuzzleState Int -> Array U DIM1 Int -> Int -> H.HashMap String Int-> IO Int
solve psq target n mp = do
    let top       = fromJust $ findMin psq
        npsq      = deleteMin psq
        depth     = fn $ key top
        curarray  = state $ key top

    -- if PQ.size psq == 0 then
    if not (solvability curarray (zeroPos $ key top) n) || PQ.size psq == 0 then
        return (-1)
    else if curarray == target then
        return depth
    else do
        -- get neighbors from top
        -- check if they are in the hashmap
        -- if so, compare mp[state] = fn and current fn
        -- if not, push them to psq and hashmap
        let neighborList = getAllNeighbor (key top) n
            validNeighborList = getValidNeighbor neighborList mp
            newmap = addMap validNeighborList mp
            newpsq = addPSQ validNeighborList npsq
        --print curarray
        --printList validNeighborList
        --print $ PQ.toList newpsq
        --print $ PQ.size newpsq
        solve newpsq target n newmap

