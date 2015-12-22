import qualified Data.Set as Set
import Debug.Trace

numbers = Set.fromList $ map show [0..9]

value [] cur = cur

takeObject xs acc count | count == 0 = (reverse acc, xs)
takeObject rest@(x:xs) acc count | x == '{' = takeObject xs (x:acc) (count + 1)
                                 | x == '}' = takeObject xs (x:acc) (count - 1)
                                 | otherwise = let (val, restRest) = takeTokenVal rest in 

takeTokenVal (x:xs) | x == '-' = (-1 * num, rest)
                    | x `elem` numbers = (num, rest)
                    | otherwise = (0, xs)
  where (num, rest) = span (`elem` numbers) xs

--value (x:xs) cur | x == '-' = value rest (cur + read num)
--                 | x `elem` numbers = value rest (cur + (read (x:num)))
 --                | x == '{'
--
 -- where (num, rest) = span (`elem` numbers) xs
