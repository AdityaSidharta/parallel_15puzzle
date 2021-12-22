{-# LANGUAGE FlexibleContexts #-}

module Solver where
import System.IO (hGetLine, Handle)
import Data.PSQueue as PQ (PSQ, singleton, prio, size, findMin, deleteMin, key, insert, toList)
import Data.Maybe ( fromJust, catMaybes )
import Data.HashMap.Strict as H (HashMap, singleton, member, lookup, insert)
import Data.Array.Repa as R (Array, U, DIM1, fromListUnboxed, Z (Z), (:.) ((:.)), (!), index, Shape (size), Source (extent), DIM0, zipWith, D, computeUnboxedS )
import Data.List ( zip4)
import Control.Monad ( forM, void )
import Control.Parallel.Strategies(rpar, using, parList, rseq, parBuffer)
import Control.Concurrent ( newEmptyMVar, newMVar, forkIO, tryPutMVar, takeMVar, putMVar, readMVar, killThread)
import GHC.IO (unsafePerformIO)

import Puzzle ( PuzzleState, PuzzleState(PuzzleState, gn, fn, state), getZeroPos, swapTwo, getAllNeighbor, getAllNeighborPar, solvability)
import Metrics ( manhattanDist )
import Parse ( readInt, getStateVector, getAllPuzzles)

-- | getValidNeighbor filters all neighbor puzzles that improves (fn) or have not been discovered previously (not in mp)
getValidNeighbor::[PuzzleState] -> H.HashMap String Int-> [PuzzleState]
getValidNeighbor ps mp = filter (filterInMap mp) ps

-- | filterInMap returns True if the puzzle (puzzle) is not in the HashMap (mp) or if the puzzle can now be reached in less steps (fn)
filterInMap :: HashMap String Int -> PuzzleState -> Bool
filterInMap mp puzzle = not (H.member key mp) || fromJust (H.lookup key mp) > fn puzzle
    where key = getHashKey $ state puzzle

-- | addMap add all of the puzzle states (ps) into the given HashMap (mp)
addMap :: Foldable t => t PuzzleState -> HashMap String Int -> HashMap String Int
addMap ps mp = foldr (\ p -> H.insert (getHashKey (state p)) (fn p)) mp ps

-- | addPSQ adds all of the given puzzle states (ps) into the PriorityQueue (psq)
addPSQ :: [PuzzleState] -> PSQ PuzzleState Int -> PSQ PuzzleState Int
addPSQ ps psq = foldr(\ p -> PQ.insert p (fn p + gn p)) psq ps

-- | getHashKey turns the hash result from the given array (li) and return string as the hash key. hash [0, 3, 1, 2] -> "00030102"
getHashKey:: Array U DIM1 Int -> String
getHashKey li = show $ hash li 0

-- | hash perform simple hash function on the given array (l), using recursive function on idx. hash [0, 3, 1, 2] -> "00030102"
hash :: Integral a => Array U DIM1 Int -> Int -> a
hash l idx | (Z:.idx) == R.extent l = 0
             | otherwise = fromIntegral (l!(Z:.idx)) + 100 * hash l (idx+1)

-- | solveBool perform sequential solving on 8-puzzle using A* algorithm, returning True if the puzzle is solvable
solveBool :: (PSQ PuzzleState Int,  Array U DIM1 Int, Int, H.HashMap String Int)-> IO Bool
solveBool (psq, target, n, mp) = do
    let top       = fromJust $ findMin psq
        npsq      = deleteMin psq
        depth     = fn $ key top
        curarray  = state $ key top

    -- if PQ.size psq == 0 then
    if PQ.size psq == 0 then
        return False
    else if curarray == target then
        return True
    else do
        let neighborList = getAllNeighbor (key top) n
            validNeighborList = getValidNeighbor neighborList mp
            newmap = addMap validNeighborList mp
            newpsq = addPSQ validNeighborList npsq
        solveBool (newpsq, target, n, newmap)

-- | solve perform sequential solving on 8-puzzle using A* algorithm
solve :: (PSQ PuzzleState Int,  Array U DIM1 Int, Int, H.HashMap String Int)-> IO Int
solve (psq, target, n, mp) = do
    let top       = fromJust $ findMin psq
        npsq      = deleteMin psq
        depth     = fn $ key top
        curarray  = state $ key top

    -- if PQ.size psq == 0 then
    if PQ.size psq == 0 then
        return (-1)
    else if curarray == target then
        return depth
    else do
        let neighborList = getAllNeighbor (key top) n
            validNeighborList = getValidNeighbor neighborList mp
            newmap = addMap validNeighborList mp
            newpsq = addPSQ validNeighborList npsq
        solve (newpsq, target, n, newmap)

-- | solveOnepuzzle perform solving on a single 8-puzzle
solveOnepuzzle :: (Int, [Int]) -> Int
solveOnepuzzle (n, state) | solvable = unsafePerformIO $ solve (psq, target, n, mp)
                    | otherwise = -1
  where array = fromListUnboxed (Z :. (n*n) :: DIM1) state
        target = fromListUnboxed (Z :. (n*n) :: DIM1) [0..(n*n-1)]
        gn     = manhattanDist array 0 n
        psq    = PQ.singleton (PuzzleState 0 gn (getZeroPos array 0) array) gn
        mp     = H.singleton (getHashKey array) 0 -- a hashmap storing visited states -> fn
        solvable = solvability array (getZeroPos array 0) n

-- | solveParNeighbor perform solving by parallelizing the calculation of GetAllNeighbor into 4 different threads
solveParNeighbor :: (PSQ PuzzleState Int , Array U DIM1 Int , Int , H.HashMap String Int)-> IO Int
solveParNeighbor (psq, target, n, mp) = do
    let top       = fromJust $ findMin psq
        npsq      = deleteMin psq
        depth     = fn $ key top
        curarray  = state $ key top

    -- if PQ.size psq == 0 then
    if PQ.size psq == 0 then
        return (-1)
    else if curarray == target then
        return depth
    else do
        let neighborList = getAllNeighborPar (key top) n
            validNeighborList = getValidNeighbor neighborList mp
            newmap = addMap validNeighborList mp
            newpsq = addPSQ validNeighborList npsq
        solveParNeighbor (newpsq, target, n, newmap)

-- | solveParPSQ perform solving by creating multiple priority queues and abort the other thread once we have solved the puzzle
solveParPSQ :: (PSQ PuzzleState Int, Array U DIM1 Int, Int, HashMap String Int) -> IO Int
solveParPSQ (psq, target, n, mp) = do
    let top      = fromJust $ findMin psq
        npsq     = deleteMin psq
        depth    = fn $ key top
        curarray = state $ key top
        k = 5

    -- if PQ.size psq == 0 then
    if PQ.size psq == 0 then
        return (-1)
    else if curarray == target then
        return 1
    else if PQ.size psq < k then do
        let neighborList = getAllNeighbor (key top) n
            validNeighborList = getValidNeighbor neighborList mp
            newmap = addMap validNeighborList mp
            newpsq = addPSQ validNeighborList npsq
        solveParPSQ (newpsq, target, n, newmap)
    else do
        let length = PQ.size psq
        resultV <- newEmptyMVar
        runningV <- newMVar length
        threads <- forM [PQ.singleton (key x) (prio x) | x <- PQ.toList psq] $ \ipsq -> forkIO $ do
            if unsafePerformIO(solveBool(ipsq, target, n, mp)) then void (tryPutMVar resultV 1) else (do m <- takeMVar runningV
                                                                                                         if m == 1
                                                                                                               then void (tryPutMVar resultV 0)
                                                                                                               else putMVar runningV (m-1))
        result <- readMVar resultV
        mapM_ killThread threads
        return result


-- | puzzleSolver is the base function for other solver
puzzleSolver :: (Num a, Show a, Num v) => Handle -> Int -> ((PSQ PuzzleState Int, Array U DIM1 Int, Int, HashMap String v) -> IO a) -> IO ()
puzzleSolver handle 0 solver = return ()
puzzleSolver handle k solver = do
    n <- readInt handle
    matrix <- getStateVector handle n n
    let array  = fromListUnboxed (Z :. (n*n) :: DIM1) $ concat matrix
        target = fromListUnboxed (Z :. (n*n) :: DIM1) [0..(n*n-1)]
        gn     = manhattanDist array 0 n
        psq    = PQ.singleton (PuzzleState 0 gn (getZeroPos array 0) array) gn
        mp     = H.singleton (getHashKey array) 0 -- a hashmap storing visited states -> fn
        solvable = solvability array (getZeroPos array 0) n

    step  <- if solvable then solver (psq, target, n, mp) else return (-1)
    print step

    puzzleSolver handle (k-1) solver


-- | solveKpuzzle perform solving on mutliple 8-puzzle in a sequential manner
solveKpuzzle :: Handle -> Int -> IO ()
solveKpuzzle handle k = puzzleSolver handle k solve

-- | parSolveKpuzzle perform solving on mutliple 8-puzzle in a parallel manner, by sparking different threads to solve different puzzles
parSolveKpuzzle:: Handle -> Int -> IO()
parSolveKpuzzle handle k = do
    allpuzzles <- getAllPuzzles handle k
    let result = map solveOnepuzzle allpuzzles `using` parBuffer 100 rseq -- `using` parList rseq
    print result

-- | parNeighborSolveKpuzzle perform solving on multiple 80puzzle in a parallel manner, by sparking different threads to calculate the valid Neighbors
parNeighborSolveKpuzzle :: Handle -> Int -> IO()
parNeighborSolveKpuzzle handle k = puzzleSolver handle k solveParNeighbor

-- | parPSQSolvePuzzle is an interface to parPSQ
parPSQSolvePuzzle :: Handle -> Int -> IO()
parPSQSolvePuzzle handle k = puzzleSolver handle k solveParPSQ


