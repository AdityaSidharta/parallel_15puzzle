module Main where

import Lib (readInt, solveKpuzzle)
import System.Exit(die)
import System.Environment(getArgs, getProgName)
import System.IO(readFile, openFile, IOMode(ReadMode))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            handle <- openFile filename ReadMode
            k <- readInt handle
            solveKpuzzle handle k
        _ -> do
            pn <- getProgName
            error $ "Usage: "++pn++" <filename>"