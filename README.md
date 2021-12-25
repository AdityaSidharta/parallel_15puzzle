# parallel_fifteen_puzzle

Parallel Functional Programming Fall 2021 Project – Parallel Fifteen Puzzle

[![Documentation](https://img.shields.io/badge/Documentation-Haddock-brightgreen)](https://adityasidharta.com/parallel_15puzzle/)

## Problem Formulation
15 Puzzle is a sliding puzzle, which consists of (N ×N ) square tiles, where each squared
tile is numbered from 1 to (N 2−1), leaving a single square tile empty. Tiles that are located
adjacent to the empty tile can be moved by sliding them horizontally, or vertically. The goal
of the puzzle is to place the tiles in numerical order, leaving the last tile to be located at the
bottom right corner of the frame.

## Algorithm
For the 15 puzzle problem, we adapted the A∗algorithm to help us minimize the effort to
backtrack all of the possible steps. A∗algorithm is an informed search algorithm, which aims
to find a path to a given goal node having the smallest cost f (n)
f (n) = g(n) + h(n)
Where g(n) is defined as the cost of the current state starting from the start node, and
h(n) is the heuristic function that estimates the cost of the cheapest path, attainable or not,
from the current state to the goal state. In the 15 puzzle problem, the heuristic function
can be defined as the sum of distances between the current entry and the target entry of
the digits in that entry. The distance metrics can be chosen to be Manhattan distance or
Hamming distance.
A priority queue of the possible configurations prioritizing minimal cost functions is kept
during solving. We iteratively pop the most probable configuration, compute possible next
steps and push them to the priority queue. The algorithm will stop when the configuration
reaches the goal state.

## Parallelism Strategy
The first parallelism strategy that comes into our mind for the A∗algorithm is to perform
a parallel concurrent neighbor expansion, where the calculation of 3 possible neighbors are
parallelized. Nevertheless, as the Manhattan distance calculation might not be expensive,
this will more likely create a massive overhead. Thus, a more effective solution is to perform
parallelization by creating multiple sparks on expansion on the openSet, the list of potential
path candidates that has f (n) calculated from the starting node at least once.

## Installation
```
stack install
stack build
```

## Testing
```
stack test
```

## Running the Script
```
stack exec 15puzzle-exe "input.txt"
stack exec sequential-exe "input.txt"
stack exec parneighbor-exe "input.txt"
stack exec parpq-exe "input.txt"
stack exec parpuzzle-exe "input.txt"
```

## Docs Generation
```
stack exec -- haddock --html app/GenFile.hs app/Main.hs app/ParallelNeighbor.hs app/ParallelPriorityQueue.hs app/ParallelPuzzle.hs app/Sequential.hs src/Lib.hs --hyperlinked-source --odir=docs
```

## Example Generation

100 denote the number of puzzle that should be randomly generated, each with size 3

```
stack exec 15puzzle-generate 100 3 "input.txt"
```

## Building the main module
```
stack exec ghc-pkg unregister libiserv
stack ghc -- -threaded -rtsopts -eventlog app/Main.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is ParallelNeighbor app/ParallelNeighbor.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is ParallelPriorityQueue app/ParallelPriorityQueue.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is ParallelPuzzle app/ParallelPuzzle.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is Sequential app/Sequential.hs
```

## Running all experiment on our testcase input.txt
```
./test.sh
```

```
./app/ParallelNeighbor "input.txt" +RTS -ls -N5
./app/ParallelPriorityQueue "input.txt" +RTS -ls -N5
./app/ParallelPuzzle "input.txt" +RTS -ls -N5
./app/Sequential "input.txt" +RTS -ls -N5
```
