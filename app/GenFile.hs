module GenFile where
import System.Environment (getProgName, getArgs)
import Control.Monad ( forM )
import GHC.IOArray (IOArray)

sortsnd = map swap . ssort . map swap 

shuffle xs = do
    g <- getStdGen
    let ys = take 10 (randoms g :: [Double])
    let sortedxs = sortsnd zip (xs ys)
    return sortedxs

generate 0 _ = []
generate k n = shuffle([0..(n * n - 1)]) ++ (generate (k -1 ) n)

formatArray 0 _ = ""
formatArray a n = intercalate " " (fst(splitAt n a)) : (formatArray (snd(splitAt n a)) n)

formatArrays [] = ""
formatArrays (a:as) = "n/n" + (formatArray a n) + "/n" + (formatArrays as)
    where 
        n = (floor(sqrt(fromIntegral(length a)))) 

writeArrays arrays filename = do 
    writeFile filename (formatArrays arrays)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [k, n, filename] -> do
            let arrays = generate k n
            print "Hello"
            -- writeArrays arrays filename
        _ -> do
            pn <- getProgName
            error $ "Usage: "++pn++" <filename>"