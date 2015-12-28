import Data.List
import Data.List.Split

choose xs n = filter ((==n) . length) $ subsequences xs

options n xs = concatMap (\n -> choose xs n) [1..(length xs - n + 1)]

sets :: (Eq a) => Int -> [a] -> [[[a]]]
sets n xs | n == 1 = [[xs]]
          | otherwise = concatMap (\s -> map (\x -> s:x) (nextSets s)) curOpts
  where curOpts = options n xs
        nextSets s = sets (n-1) (filter (\z -> not $ z `elem` s) xs)

go = do
  strAry <- map (read::String->Int) <$> lines <$> readFile "weights.txt"
  let third = (sum strAry) `div` 3
  return third
  

subsetSum :: Int -> [Int] -> [Int] -> [[Int]]
subsetSum target cur remaining
  | curScore == target = [cur]
  | curScore > target = []
  | remaining == [] = []
  | otherwise = concatMap (\n -> subsetSum target (n:cur) (filter (not . (==n)) options)) options
  where curScore = sum cur
        options = filter ((<= target) . (+ curScore)) remaining


chooseN n xs | n == 1 = map (:[]) xs
             | otherwise = concatMap (\(choice, rest) -> map (\r -> choice:r) (chooseN (n-1) rest)) choices
  where choices = map (\(x:xs) -> (x, xs)) $ map snd $ map (\x -> splitAt x xs) [0..length xs - 2]

array = [1,3,5,11,13,17,19,23,29,31,37,41,43,47,53,59,67,71,73,79,83,89,97,101,103,107,109,113]
thirdSum = sum array `div` 3
quarterSum = sum array `div` 4

zz = sortOn product $ filter ((==thirdSum) . sum) $ chooseN 6 array
zz4 = sortOn product $ filter ((==quarterSum) . sum) $ chooseN 5 array

z1 = head zz
z41 = head zz4

rest = array \\ z1

testAry = [1..5] ++ [7..11]
testThirdSum = (sum testAry) `div` 3

testZz = sortOn product $ filter ((==testThirdSum) . sum) $ chooseN 2 testAry

