import Test.Tasty
import Test.Tasty.HUnit

import Lib (getStateVector, parseK, solveKpuzzle)
import System.IO (openFile, IOMode (ReadMode))

main :: IO ()
main = do
  defaultMain (testGroup "Library Test" [getStateVectorTest])

fn2 x = do put

fn :: FilePath -> IO [[Int]]
fn filename = do 
                handle <- openFile filename ReadMode
                matrix <- do
                    n <- parseK handle
                    matrix <- getStateVector handle n n
                    return matrix
                return matrix

getStateVectorTest :: TestTree
getStateVectorTest = testCase "Testing sayYo"
  (assertEqual [[1,2,3,4]] (fn "test.txt"))
  where 