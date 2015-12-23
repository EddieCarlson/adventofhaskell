import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split
import Debug.Trace(trace)
import Data.Maybe
import Data.List


splitStr s = case words s of [s, _, f, _, d] -> [(s, [(f, (read::String->Int) d)]), (f, [(s, (read::String->Int) d)])]

makeMap = Map.fromListWith (\a b -> a ++ b) . concatMap splitStr

data Path = Path Int [String] deriving Show

instance Eq Path where
  (Path dist1 ps1) == (Path dist2 ps2) = dist1 == dist2 && ps1 == ps2

instance Ord Path where
  (Path dist1 _) `compare` (Path dist2 _) = dist1 `compare` dist2


--remove min path from set
--look up adjacent nodes in graph
--add new nodes to paths
--add new paths back into set
--repeat until min path has 8 elements

findPath :: Map.Map String [(String, Int)] -> Set.Set Path -> Path
findPath m curPaths =
  let p@(Path i ps) = Set.findMin curPaths
  in
  if trace (show p) $ length ps == Map.size m then p
  else
    let x = if length ps < 3 then
              trace (show curPaths) 5
            else
              5
        (Just adj1) = Map.lookup (head ps) m
        unvisited1 = filter (\(f, d) -> not (f `elem` ps)) adj1
        newPs1 = map (\(f, d) -> Path (i + d) (f : ps)) unvisited1
        newPaths = (Set.delete p curPaths) `Set.union` Set.fromList newPs1
    in
    if any (\(Path dist pp) -> length pp == Map.size m) newPs1
    then fromJust $ find (\(Path dist pp) -> length pp == Map.size m) newPs1
    else findPath m newPaths



bestPathFromStart m start =
  let (Just fromStart) = Map.lookup start m
      startingPaths = trace (show . length $ fromStart) $ Set.fromList $ map (\(f, d) -> Path d [f, start]) fromStart
  in findPath m startingPaths

go = do
  text <- fmap lines $ readFile "stops.txt"
  let m = makeMap text
      keys = Map.keys m
      fastestPath = Set.findMin $ Set.fromList $ map (bestPathFromStart m) ["Tristram"]
  return (fastestPath)
