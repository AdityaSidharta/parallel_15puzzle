module Parse where

import System.IO (hGetLine, Handle)

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

-- | GetAllPuzzles read all of the matrices in the handle and return a list of (n, array) where n is the size of the puzzle and array is the initial state of puzzle
getAllPuzzles :: Handle -> Int -> IO [(Int, [Int])]
getAllPuzzles handle 0 = return []
getAllPuzzles handle k = do
    n <- readInt handle
    matrix <- getStateVector handle n n
    latter <- getAllPuzzles handle (k-1)
    return ((n, concat matrix): latter)