import Data.List

go = do
  strings <- fmap words $ readFile "eggnog.txt"
  let sizes = map (read::String->Int) strings
      sets = subsequences sizes
      sums = map sum sets
  return (length (filter (==150) sums))

--part 2
go2 = do
  strings <- fmap words $ readFile "eggnog.txt"
  let sizes = map (read::String->Int) strings
      sets = subsequences sizes
      validSets = filter ((==150) . sum) sets
      sorted = sortBy (\a b -> length a `compare` length b) validSets
      smallLength = length $ head sorted
      smallSets = filter ((== smallLength) . length) validSets
  return (length smallSets)
