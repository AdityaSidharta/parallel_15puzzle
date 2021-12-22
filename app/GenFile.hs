module GenFile where

import System.Environment (getProgName, getArgs)
import Puzzle (generateArrays, writeArrays)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [k, n, filename] -> do
            let arrays = generateArrays (read k) (read n)
            writeArrays arrays filename
        _ -> do
            pn <- getProgName
            error $ "Usage: "++pn++" <filename>"