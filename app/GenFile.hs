module GenFile where

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