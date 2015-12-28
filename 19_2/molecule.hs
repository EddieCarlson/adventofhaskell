import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Map
import Debug.Trace

parse all@(x:xs) | all == "e" = ["e"]
                 | isUpper x = (x:lower):parse rest
  where (lower, rest) = span isLower xs
parse _ = []

parseTrans :: String -> (String, String)
parseTrans t = toTuple $ splitOn " => " t
  where toTuple :: [String] -> (String, String)
        toTuple [x, y] = (x, y)

replacementCount m = map (\x -> Map.lookup x m)

replacements :: (Eq a) => a -> a -> [a] -> [a] -> [[a]]
replacements _ _ _ [] = []
replacements a b prev (x:xs) | x == a = (reverse prev ++ (b:xs)) : r
                             | otherwise = r
  where r = replacements a b (x:prev) xs

allReplacements rs list = nub $ map concat $ concatMap (\(a, b) -> replacements a b [] list) rs

go = do
  text <- readFile "molecule.txt"
  transText <- lines <$> readFile "transforms.txt"
  let mList = parse text
      transList = map parseTrans transText
      all = allReplacements transList mList
  return (length all)

--part 2

revTuple (x, y) = (y, x)

splitAtFirst s arr@(x:xs) pre | Just p <- stripPrefix s arr = (reverse pre, drop (length s) arr)
                              | otherwise = splitAtFirst s xs (x:pre)

makeReplacement replacements count all
  | all == "e" = count
  | otherwise = makeReplacement replacements (count + 1) (h ++ b ++ tl)
    where Just (a, b) = find ((`isInfixOf` all) . fst) replacements
          (h, tl) = splitAtFirst a all []

go3 = do
  text <- readFile "molecule.txt"
  transText <- lines <$> readFile "transforms.txt"
  let mList = parse text
      transList = sortOn (negate . length . parse . fst) $ map (revTuple . parseTrans) transText
      count = makeReplacement transList 0 $ concat mList
  return count
