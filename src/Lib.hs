module Lib
    (   PuzzleState (..),
        parseK,
        solveKpuzzle
    ) where
import System.IO (hGetLine, Handle)
import Data.PSQueue (PSQ)

data PuzzleState = PuzzleState Int Int [Int] deriving (Show, Eq)

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
solveKpuzzle handle 0 = return ()
solveKpuzzle handle k = do
    n <- parseK handle
    matrix <- getStateVector handle n n
    let array = concat matrix
    solve array n
    solveKpuzzle handle (k-1)


solve :: [Int] -> Int -> IO ()
solve myinput n = do
    -- do solving here
    print myinput



