import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, assertEqual, Assertion, (@?=) )

import Lib (getStateVector, parseK, solveKpuzzle)
import System.IO (openFile, IOMode (ReadMode))

main :: IO ()
main = do
  defaultMain (testCase "getStateVectorTest" getStateVectorTest)

fn :: FilePath -> IO [Int]
fn filename = do
                handle <- openFile filename ReadMode
                do
                    k <- parseK handle
                    n <- parseK handle
                    print n
                    matrix <- getStateVector handle n n
                    let array = concat matrix
                    return array

getStateVectorTest :: Assertion
getStateVectorTest = do
  x <- fn "test/test.txt"
  x @?= [1,3,0,2]