import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, assertEqual, Assertion, (@?=) )
import Lib (numinv, solvability, getStateVector, getValidNeighbor, readInt, solveKpuzzle, generateArrays, formatArray, formatArrays, manhattanDist, hammingDist, getZeroPos, swapTwo, getUpNeighbor, PuzzleState (PuzzleState), getRightNeighbor, getLeftNeighbor, getDownNeighbor, getAllNeighbor, hash, getHashKey, addMap)
import System.IO (openFile, IOMode (ReadMode))
import Data.Array.Repa (DIM1, fromListUnboxed, Z (Z), (:.) ((:.)), Array, U, computeS)
import Data.HashMap.Strict as H ( fromList, singleton )
import Data.PSQueue as PQ (fromList, singleton)

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit Tests" [
  testCase "getStateVectorTest" getStateVectorTest,
  testCase "generateArraysTest" generateArraysTest,
  testCase "formatArrayTest" formatArrayTest,
  testCase "formatArraysTest" formatArraysTest,
  testCase "manhattanDistTest" manhattanDistTest,
  testCase "hammingDistTest" hammingDistTest,
  testCase "getZeroPosTest" getZeroPosTest,
  testCase "swapTwoTest" swapTwoTest,
  testCase "getUpNeighborTest" getUpNeighborTest,
  testCase "getDownNeighborTest" getDownNeighborTest,
  testCase "getLeftNeighborTest" getLeftNeighborTest,
  testCase "getRightNeighborTest" getRightNeighborTest,
  testCase "getAllNeighborTest" getAllNeighborTest,
  testCase "hashTest" hashTest,
  testCase "getHashKeyTest" getHashKeyTest,
  testCase "addMapTest" addMapTest,
  testCase "getValidNeighborTest" getValidNeighborTest,
  testCase "numinvTest" numinvTest,
  testCase "solvabilityTest" solvabilityTest]

getStateVectorTest :: Assertion
getStateVectorTest = do
  x <- fn "test/test.txt"
  x @?= [0,4,2,1,3,8,6,5,7]
    where fn filename = do
                          handle <- openFile filename ReadMode
                          do
                              k <- readInt handle
                              n <- readInt handle
                              print n
                              matrix <- getStateVector handle n n
                              let array = concat matrix
                              return array


generateArraysTest :: Assertion
generateArraysTest = do
  generateArrays 3 3 @?= [[6,8,1,7,2,5,3,0,4],[7,0,8,1,4,3,2,5,6],[5,3,2,7,6,8,0,1,4]]
  generateArrays 2 2 @?= [[3,2,1,0],[1,3,0,2]]

formatArrayTest :: Assertion
formatArrayTest = do
  formatArray [1,2,3,4] 2 @?= "1 2\n3 4\n"
  formatArray [1,2,3,4] 4 @?= formatArray [1,2,3,4] 6

formatArraysTest :: Assertion
formatArraysTest =
  formatArrays [[1,2,3,4],[1,2,3,4,5,6,7,8,9]] @?=  "2\n1 2\n3 4\n3\n1 2 3\n4 5 6\n7 8 9\n"

manhattanDistTest :: Assertion
manhattanDistTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  manhattanDist x 0 2 @?= 4

hammingDistTest :: Assertion
hammingDistTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  hammingDist x 0 2 @?= 2

getZeroPosTest :: Assertion
getZeroPosTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  getZeroPos x 0 @?= 3
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,0,1,2]
  getZeroPos y 0 @?= 1

swapTwoTest :: Assertion
swapTwoTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  computeS (swapTwo 0 3 x) @?= y

getUpNeighborTest :: Assertion
getUpNeighborTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let puzx = PuzzleState 0 0 0 x
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  let puzy = PuzzleState 0 0 3 y
  let res = fromListUnboxed (Z :. (2*2) :: DIM1) [3,0,2,1]
  let puzres = PuzzleState 1 4 1 res

  getUpNeighbor puzx 2 @?= Nothing
  getUpNeighbor puzy 2 @?= Just puzres

getDownNeighborTest :: Assertion
getDownNeighborTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let puzx = PuzzleState 0 0 0 x
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  let puzy = PuzzleState 0 0 3 y
  let res = fromListUnboxed (Z :. (2*2) :: DIM1) [2,1,0,3]
  let puzres = PuzzleState 1 2 2 res

  getDownNeighbor puzx 2 @?= Just puzres
  getDownNeighbor puzy 2 @?= Nothing

getLeftNeighborTest :: Assertion
getLeftNeighborTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let puzx = PuzzleState 0 0 0 x
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  let puzy = PuzzleState 0 0 3 y
  let res = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,0,2]
  let puzres = PuzzleState 1 4 2 res

  getLeftNeighbor puzx 2 @?= Nothing
  getLeftNeighbor puzy 2 @?= Just puzres


getRightNeighborTest :: Assertion
getRightNeighborTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let puzx = PuzzleState 0 0 0 x
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  let puzy = PuzzleState 0 0 3 y
  let res = fromListUnboxed (Z :. (2*2) :: DIM1) [1,0,2,3]
  let puzres = PuzzleState 1 2 1 res

  getRightNeighbor puzx 2 @?= Just puzres
  getRightNeighbor puzy 2 @?= Nothing

getAllNeighborTest :: Assertion
getAllNeighborTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let puzx = PuzzleState 0 0 0 x
  let res1 = fromListUnboxed (Z :. (2*2) :: DIM1) [2,1,0,3]
  let puzres1 = PuzzleState 1 2 2 res1
  let res2 = fromListUnboxed (Z :. (2*2) :: DIM1) [1,0,2,3]
  let puzres2 = PuzzleState 1 2 1 res2

  getAllNeighbor puzx 2 @?= [puzres1, puzres2]

hashTest :: Assertion
hashTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  hash x 0 @?= 3020100
  hash y 0 @?= 20103

getHashKeyTest :: Assertion
getHashKeyTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  getHashKey x @?= "3020100"
  getHashKey y @?= "20103"

addMapTest :: Assertion
addMapTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  let z = fromListUnboxed (Z :. (2*2) :: DIM1) [1,0,3,2]
  let puzx = PuzzleState 0 0 0 x
  let puzy = PuzzleState 0 0 0 y
  let puzz = PuzzleState 0 0 0 z
  let mp = H.singleton (getHashKey x) 0
  let resmp = H.fromList [(getHashKey x, 0), (getHashKey y, 0), (getHashKey z, 0)]
  addMap [puzy, puzz] mp @?= resmp

getValidNeighborTest :: Assertion
getValidNeighborTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,2,0]
  let z = fromListUnboxed (Z :. (2*2) :: DIM1) [1,0,3,2]
  let puzx = PuzzleState 1 0 0 x
  let puzy = PuzzleState 1 0 0 y
  let puzz = PuzzleState 1 0 0 z
  let mp = H.singleton (getHashKey x) 0
  getValidNeighbor [puzx, puzy, puzz] mp @?= [puzy, puzz]

numinvTest :: Assertion
numinvTest = do
  let x = fromListUnboxed (Z :. (2*2) :: DIM1) [3,1,0,2]
  numinv x @?= 4
  let y = fromListUnboxed (Z :. (2*2) :: DIM1) [0,1,2,3]
  numinv y @?= 0

solvabilityTest :: Assertion
solvabilityTest = do
  let x = fromListUnboxed (Z :. (3*3) :: DIM1) [1,8,2,0,4,3,7,6,5]
  solvability x 3 3 @?= False
  let y = fromListUnboxed (Z :. (3*3) :: DIM1) [8,1,2,0,4,3,7,6,5]
  solvability y 3 3 @?= True