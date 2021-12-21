for i in 1 2 3 4 5
do
  for name in "ParallelNeighbor" "ParallelPriorityQueue" "Sequential" "ParallelPuzzle"
  do
    time ./app/$name input.txt +RTS -ls -N$i
  done
if [ ! -d "eventlog/n$i/" ] 
then
  mkdir "eventlog/n$i/"
fi
mv *.eventlog "eventlog/n$i/"
done
