import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Data.List

neighbors l@(x:xs) = ns
  where flipPairs = map (\(a, b) -> (b, a))
        ns = l `zip` (xs ++ [x])

happiness :: (Num a) => [(String, String)] -> Map.Map String (Map.Map String a) -> [a]
happiness ns m = map aToB ns
  where aToB (a, b) = fromJust $ Map.lookup a m >>= Map.lookup b

parse [a, _, loseOrGain, hap, _, _, _, _, _, _, b] | loseOrGain == "gain" = toMapTuple (a, b, read hap)
                                                   | loseOrGain == "lose" = toMapTuple (a, b, -1 * read hap)
  where toMapTuple (a, b, c) = (a, Map.fromList [(filter (/='.') b, c)])


combinations xs = filter ((==n) . length . nub) $ mapM (const xs) [1..n]
  where n = length xs

getMap = do
  text <- fmap lines $ readFile "seating.txt"
  let mapList = (map parse $ map words text)
      smap = Map.fromListWith (Map.union) mapList
  return smap

go = do
  text <- fmap lines $ readFile "seating.txt"
  let mapList = (map parse $ map words text)
      smap = Map.fromListWith (Map.union) mapList
      combs = combinations $ Map.keys smap
      ns = map neighbors combs
      haps = map (\x -> sum $ happiness x smap) ns
  return (maximum haps)


-- part 2

neighbors2 l@(x:xs) = l `zip` (xs ++ [x])

happiness2 ns m = sum pairHappiness - minimum pairHappiness
  where aToB (a, b) = fromJust $ (+) <$> (Map.lookup a m >>= Map.lookup b) <*> (Map.lookup b m >>= Map.lookup a)
        pairHappiness = map aToB ns

go2 = do
  text <- fmap lines $ readFile "seating.txt"
  let mapList = (map parse $ map words text)
      smap = Map.fromListWith (Map.union) mapList
      combs = combinations $ Map.keys smap
      ns = map neighbors2 combs
      haps = map (\x -> happiness2 x smap) ns
  return (maximum haps)
