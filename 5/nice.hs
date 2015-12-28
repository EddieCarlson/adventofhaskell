

import qualified Data.Set as Set
import Data.List

vowles = Set.fromList ['a', 'e', 'i', 'o', 'u']
badPairs = ["ab", "cd", "pq", "xy"]

has3Vowels x = length (vowles `Set.intersection` (Set.fromList x)) >= 3

hasDuplicate s = case s of [] -> False
                           x : [] -> False
                           x : y : _ | x == y -> True
                           x : y : _ -> hasDuplicate (drop 1 s)

hasBadPair s = any (\x -> x `isInfixOf` s) badPairs

isNice strings = length (filter (\x -> has3Vowels x && hasDuplicate x && not (hasBadPair x)) strings)

numNiceStrings = do words <- readFile "words.txt"
                    let strings = lines words
                    return (isNice strings)
