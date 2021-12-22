module ParallelPuzzle where

import Lib (parSolveKpuzzle)
import Parse (readInt)
import System.Exit(die)
import System.Environment(getArgs, getProgName)
import System.IO(openFile, IOMode(ReadMode))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            handle <- openFile filename ReadMode
            k <- readInt handle
            parSolveKpuzzle handle k
        _ -> do
            pn <- getProgName
            die $ "Usage: "++pn++" <filename>"