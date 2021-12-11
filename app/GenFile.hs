module GenFile where
import System.Environment (getProgName, getArgs)
import Control.Monad ( forM )
import GHC.IOArray (IOArray)
import Data.List (sortOn, intercalate)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

generate :: (Num a, Enum a) => Int -> a -> [[a]]
generate 0 _ = []
generate k n = let xs = [0..(n * n - 1)] in shuffle' xs (length xs) (mkStdGen k) : generate (k -1) n

formatArray :: [Int] -> Int -> String
formatArray [] n = ""
formatArray a n = unwords (map show (take n a)) ++ "\n" ++ formatArray (drop n a) n

formatArrays :: [[Int]] -> String
formatArrays [] = ""
formatArrays (a:as) = show n ++ "\n" ++ formatArray a n ++ formatArrays as
    where
        n = floor(sqrt(fromIntegral(length a))) :: Int

writeArrays :: [[Int]] -> FilePath -> IO ()
writeArrays arrays filename =
    writeFile filename (show n ++ "\n" ++ formatArrays arrays)
      where
        n = length arrays

main :: IO ()
main = do
    args <- getArgs
    case args of
        [k, n, filename] -> do
            let arrays = generate (read k) (read n)
            writeArrays arrays filename
        _ -> do
            pn <- getProgName
            error $ "Usage: "++pn++" <filename>"