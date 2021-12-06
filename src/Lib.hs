module Lib
    (   PuzzleState (..),
        parseK,
        solveKpuzzle
    ) where
import System.IO (hGetLine, Handle)
import Data.PSQueue (PSQ, singleton, size, findMin, deleteMin, key)
import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap)

-- Puzzle State fn gn [status vector]
data PuzzleState = PuzzleState {fn::Int, 
                                gn::Int,
                                state::[Int]} deriving (Show, Eq)

instance Ord PuzzleState where
    PuzzleState a b _ `compare` PuzzleState c d _ = (a+b) `compare` (c+d) 

parseK :: Handle -> IO Int
parseK handle = do
    str <- hGetLine handle
    return (read str::Int)

getStateVector :: Handle -> Int -> Int -> IO [[Int]]
getStateVector handle n 0 = return []
getStateVector handle n cur = do
    line <- hGetLine handle
    let tokens = (\x -> read x::Int) <$> words line
    post <- getStateVector handle n (cur-1)
    return (tokens:post)


solveKpuzzle :: (Num t, Eq t) => Handle -> t -> IO ()
solveKpuzzle handle 0 = do
    return ()
solveKpuzzle handle k = do
    n <- parseK handle
    matrix <- getStateVector handle n n
    let array  = concat matrix
        target = [0..(n*n-1)]
        gn     = costg array target n
        psq    = singleton (PuzzleState 0 gn array) gn
        -- mp     = singleton array 0 -- a hashmap storing visited states -> fn
        step   = fromJust $ solve psq target n
    print step
    solveKpuzzle handle (k-1)

costg :: Integral a => [a] -> [a] -> a -> a
costg cur target n = sum $ fmap diff (zip cur target)
    where diff (x,y) = abs (x `mod` n - y `mod` n) + abs (x `div` n - y `div` n)


solve :: Ord p1 => PSQ PuzzleState p1 -> [Int] -> p2 -> Maybe Int
solve psq target n = do
    top <- findMin psq
    let npsq = deleteMin psq
        depth = fn $ key top
        curarray = state $ key top 
    if curarray == target then
        return depth
    else do
        -- get neighbors from top
        -- check if they are in the hashmap
        -- if so, compare mp[state] = fn and current fn
        -- if not, push them to psq and hashmap
        return 0



