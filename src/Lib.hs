{-# LANGUAGE FlexibleContexts #-}
module Lib
    (   PuzzleState (..),
        readInt,
        solveKpuzzle
    ) where
import System.IO (hGetLine, Handle)
import Data.PSQueue as PQ (PSQ, singleton, size, findMin, deleteMin, key, insert)
import Data.Maybe (fromJust)
import Data.HashMap.Strict as H (HashMap, singleton, member, lookup, insert)
import Data.Array.Repa as R (Array, U, DIM1, fromListUnboxed, Z (Z), (:.) ((:.)), (!), index, Shape (size), Source (extent), DIM0, zipWith, D, computeUnboxedS )

---------------------------
-- Puzzle data structure --
---------------------------

-- Puzzle State fn gn [status vector]
data PuzzleState = PuzzleState {fn::Int,
                                gn::Int,
                                zeorPos::Int,
                                state::Array U DIM1 Int} deriving (Show, Eq)

instance Ord PuzzleState where
    PuzzleState a b _ _ `compare` PuzzleState c d _ _ = (a+b) `compare` (c+d)

-- manhattan dist
manhattandist:: Source r Int =>  Array r DIM1 Int -> Int -> Int-> Int
manhattandist cur idx n | idx == R.size (R.extent cur) = 0
                        | otherwise = diff idx (cur ! ( Z :. idx)) + manhattandist cur (idx+1) n
                        where diff x y = abs (x `mod` n - (y `mod` n)) + abs (x `div` n - (y `div` n))

-- hamming dist
hammingdist:: Source r Int =>  Array r DIM1 Int -> Int -> Int ->Int
hammingdist cur idx n | idx == R.size (R.extent cur) = 0
                      | otherwise = diff idx (cur!(Z :. idx)) + hammingdist cur (idx+1) n
                      where diff x y | x == y = 1
                                     | otherwise = 0

-----------------------------------------
-- helper functions for reading inputs --
-----------------------------------------

-- parse an integer from handle
readInt :: Handle -> IO Int
readInt handle = do
    str <- hGetLine handle
    return (read str::Int)

-- Reading an n*n array from handle
getStateVector :: Handle -> Int -> Int -> IO [[Int]]
getStateVector handle n 0 = return []
getStateVector handle n cur = do
    line <- hGetLine handle
    let tokens = (\x -> read x::Int) <$> words line
    post <- getStateVector handle n (cur-1)
    return (tokens:post)


-- helper function for finding neighbors:
getZeroPos::Source r Int => Array r DIM1 Int -> Int -> Int
getZeroPos arr idx | idx == R.size (R.extent arr) = -1
                   | arr!(Z :. idx) == 0 = idx
                   | otherwise = getZeroPos arr (idx+1)

swaptwo:: Source r Int => Int -> Int -> Array r DIM1 Int -> Array D DIM1 Int
swaptwo f s arr = R.zipWith (\x y->
    if      x == f then arr!(Z :. s)
    else if x == s then arr!(Z :. f)
    else y)  (fromListUnboxed sh [0..(R.size sh -1)]) arr
    where sh = R.extent arr

getUpNeighbor :: PuzzleState -> Int -> Maybe PuzzleState
getUpNeighbor (PuzzleState f g ze reparray) n | row < 0 = Nothing
                                              | otherwise = Just $ PuzzleState (f+1) newg (row*n+col) newarray
  where oldrow = ze `div` n
        row = oldrow - 1
        col = ze `mod` n
        newarray = computeUnboxedS $ swaptwo (oldrow*n+col) (row*n+col) reparray
        newg = manhattandist newarray 0 n

getDownNeighbor :: PuzzleState -> Int -> Maybe PuzzleState
getDownNeighbor (PuzzleState f g ze reparray) n | row >= n = Nothing
                                                | otherwise = Just $ PuzzleState (f+1) newg (row*n+col) newarray
  where oldrow = ze `div` n
        row = oldrow + 1
        col = ze `mod` n
        newarray = computeUnboxedS $ swaptwo (oldrow*n+col) (row*n+col) reparray
        newg = manhattandist newarray 0 n

getLeftNeighbor :: PuzzleState -> Int -> Maybe PuzzleState
getLeftNeighbor (PuzzleState f g ze reparray) n | col < 0 = Nothing
                                                | otherwise = Just $ PuzzleState (f+1) newg (row*n+col) newarray
  where oldcol = ze `mod` n
        row = ze `div` n
        col = oldcol - 1
        newarray = computeUnboxedS $ swaptwo (row*n+oldcol) (row*n+col) reparray
        newg = manhattandist newarray 0 n

getRightNeighbor :: PuzzleState -> Int -> Maybe PuzzleState
getRightNeighbor (PuzzleState f g ze reparray) n | col >=n = Nothing
                                                 | otherwise = Just $ PuzzleState (f+1) newg (row*n+col) newarray
  where oldcol = ze `mod` n
        row = ze `div` n
        col = oldcol + 1
        newarray = computeUnboxedS $ swaptwo (row*n+oldcol) (row*n+col) reparray
        newg = manhattandist newarray 0 n

getAllNeighbor:: PuzzleState -> Int -> [PuzzleState]
getAllNeighbor p n = [x | Just x <- [getUpNeighbor p n, getDownNeighbor p n, getLeftNeighbor p n, getRightNeighbor p n]]


getValidNeighbor::[PuzzleState] -> H.HashMap String Int-> [PuzzleState]
getValidNeighbor plist mp = filter (filterInMap mp) plist

filterInMap :: HashMap [Char] Int -> PuzzleState -> Bool
filterInMap mp puzzle = not (H.member key mp) || fromJust (H.lookup key mp) > fn puzzle
    where key = getHashKey $ state puzzle

-- if 
addMap :: Foldable t => t PuzzleState -> HashMap [Char] Int -> HashMap [Char] Int
addMap ps mp = foldr (\ p -> H.insert (getHashKey (state p)) (fn p)) mp ps

addPSQ :: Foldable t => t PuzzleState -> PSQ PuzzleState Int -> PSQ PuzzleState Int
addPSQ ps psq = foldr(\ p -> PQ.insert p (fn p + gn p)) psq ps

-- for debug...
printList::Show a =>[a] -> IO ()
printList l =
    print $ show l

---------------------------------
-- helper function for Hashing --
---------------------------------
-- naive version
-- get hashkey [0, 3, 1, 2] -> "00030102"
getHashKey:: Array U DIM1 Int -> [Char]
getHashKey li = show $ helper li 0

helper:: Integral a => Array U DIM1 Int -> Int -> a
helper l idx | (Z:.idx) == R.extent l = 0
             | otherwise = fromIntegral (l!(Z:.idx)) + 100 * helper l (idx+1)

----------------------------------------
-- functions for checking solvability --
----------------------------------------
numinv:: Array U DIM1 Int -> Int
numinv pstate = 0 -- TODO: finish this part!!!

solvability:: Array U DIM1 Int -> Int -> Int -> Bool
solvability pstate zeropos n | odd  n && even (numinv pstate) = True
                             | even n && even (n - (zeropos `mod` n)) && even (numinv pstate) = True
                             | even n && odd  (n - (zeropos `mod` n)) && odd  (numinv pstate) = True
                             | otherwise  = False


-----------------------------------------
-- helper function for solving puzzles --
-----------------------------------------

solveKpuzzle :: Handle -> Int -> IO ()
solveKpuzzle handle 0 =
    return ()
solveKpuzzle handle k = do
    n <- readInt handle
    matrix <- getStateVector handle n n
    let array  = fromListUnboxed (Z :. (n*n) :: DIM1) $ concat matrix
        target = fromListUnboxed (Z :. (n*n) :: DIM1) [0..(n*n-1)]
        gn     = manhattandist array 0 n
        psq    = PQ.singleton (PuzzleState 0 gn (getZeroPos array 0) array) gn
        mp     = H.singleton (getHashKey array) 0 -- a hashmap storing visited states -> fn
    step  <- solve psq target n mp

    -- output step needed to solve the puzzle
    print step

    solveKpuzzle handle (k-1)

-- should do parallel algorithm at this level
solve :: PSQ PuzzleState Int -> Array U DIM1 Int -> Int -> H.HashMap String Int-> IO Int
solve psq target n mp = do
    let top       = fromJust $ findMin psq
        npsq      = deleteMin psq
        depth     = fn $ key top
        curarray  = state $ key top

    if PQ.size psq == 0 then
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
        -- print curarray
        -- printList validNeighborList
        solve newpsq target n newmap



