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
--repeat until min path contains all nodes

findPath :: Map.Map String [(String, Int)] -> Set.Set Path -> Path                                    
findPath m curPaths =
  let p@(Path i ps) = Set.findMin curPaths                                                            
  in
  if length ps == Map.size m then p                                                                   
  else
    let (Just adj1) = Map.lookup (head ps) m                                                          
        unvisited1 = filter (\(f, d) -> not (f `elem` ps)) adj1                                       
        newPs1 = map (\(f, d) -> Path (i + d) (f : ps)) unvisited1                                    
        newPaths = (Set.delete p curPaths) `Set.union` Set.fromList newPs1                            
    in findPath m newPaths
    
                                                                                                      
                                                                                                      
bestPathFromStart m start =                                                                           
  let (Just fromStart) = Map.lookup start m                                                           
      startingPaths = Set.fromList $ map (\(f, d) -> Path d [f, start]) fromStart
  in findPath m startingPaths

go file = do
  text <- fmap lines $ readFile file                                                           
  let m = makeMap text                                                                                
      keys = Map.keys m
      fastestPath = Set.findMin $ Set.fromList $ map (bestPathFromStart m) keys                       
  return (fastestPath)

-- brute

addPath (Path dist ps) (newP, d2) = Path (dist + d2) (newP : ps)

bestPathFromStartBrute m start =                                                                           
  let (Just fromStart) = Map.lookup start m                                                           
      startingPaths = Set.fromList $ map (\(f, d) -> Path d [f, start]) fromStart
  in allPaths m startingPaths

goBrute file = do
  text <- fmap lines $ readFile file                                                           
  let m = makeMap text                                                                                
      keys = Map.keys m
      fastestPath = Set.foldl (Set.union) Set.empty $ Set.fromList $ map (bestPathFromStartBrute m) keys                       
  return (fastestPath)

allPaths m curPaths = 
  case Set.filter (\(Path dist _) -> dist < 270) $ Set.foldl (Set.union) Set.empty $ Set.map f curPaths of x | Set.size x == 0 -> curPaths
                                                                                                           x -> allPaths m x
    where f (Path i ps) = let (Just adj1) = Map.lookup (head ps) m                                                          
                              unvisited1 = filter (\(f, d) -> not (f `elem` ps)) adj1                                       
                          in Set.fromList $ map (\(f, d) -> Path (i + d) (f : ps)) unvisited1                                    


-- part 2

data Path2 = Path2 Float [String] deriving Show                                                           

instance Eq Path2 where                                                                                
  (Path2 dist1 ps1) == (Path2 dist2 ps2) = dist1 == dist2 && ps1 == ps2                                 
  
instance Ord Path2 where                                                                               
  (Path2 dist1 _) `compare` (Path2 dist2 _) = dist1 `compare` dist2                                     

splitStr2 s = case words s of [s, _, f, _, d] -> [(s, [(f, 1.0 / ((read::String->Float) d))]), (f, [(s, 1.0 / ((read::String->Float) d))])]

makeMap2 = Map.fromListWith (\a b -> a ++ b) . concatMap splitStr2

goBrute2 file = do
  text <- fmap lines $ readFile file                                                           
  let m = makeMap2 text                                                                                
      keys = Map.keys m
      fastestPath2 = Set.foldl (Set.union) Set.empty $ Set.fromList $ map (bestPath2FromStartBrute2 m) keys                       
  return (fastestPath2, (Set.findMin fastestPath2))

bestPath2FromStartBrute2 m start =                                                                           
  let (Just fromStart) = Map.lookup start m                                                           
      startingPath = Set.fromList $ map (\(f, d) -> Path2 d [f, start]) fromStart
  in allPaths2 m startingPath

allPaths2 m curPath2s = 
  case Set.filter (\(Path2 dist _) -> dist < 0.1) $ Set.foldl (Set.union) Set.empty $ Set.map f curPath2s of x | Set.size x == 0 -> curPath2s
                                                                                                             x -> allPaths2 m x
    where f (Path2 i ps) = let (Just adj1) = Map.lookup (head ps) m                                                          
                               unvisited1 = filter (\(f, d) -> not (f `elem` ps)) adj1                                       
                           in Set.fromList $ map (\(f, d) -> Path2 (i + d) (f : ps)) unvisited1                                    
