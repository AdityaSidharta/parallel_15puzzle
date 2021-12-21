stack build
stack exec ghc-pkg unregister libiserv
stack ghc -- -threaded -rtsopts -eventlog app/Main.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is ParallelNeighbor app/ParallelNeighbor.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is ParallelPriorityQueue app/ParallelPriorityQueue.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is ParallelPuzzle app/ParallelPuzzle.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is Sequential app/Sequential.hs

