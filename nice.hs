import Data.List
import qualified Data.Set as Set

vowels = Set.fromList ['a', 'e', 'i', 'o', 'u']

has3Vowels = (<=) 3 . length . filter (`Set.member` vowels)

hasDuplicate input = case input of [] -> False 
                                   [x] -> False
                                   x:y:_ | x == y -> True
                                   x:xs -> hasDuplicate xs

badWords = ["ab", "cd", "pq", "xy"]

hasBadWord x = any (`isInfixOf` x) badWords

isNice x = all ($x) [has3Vowels, hasDuplicate, not . hasBadWord]

numNice = do
  text <- readFile "words.txt"
  let words = lines text
      niceWords = filter isNice words
  return $ length niceWords


-- part 2:

hasSandwich input = case input of [] -> False 
                                  [x] -> False
                                  [x, y] -> False
                                  x:y:z:_ | x == z -> True
                                  x:xs -> hasSandwich xs


hasTwoDuplicates input = case input of xs | length xs < 4 -> False 
                                       x:y:zs -> [x,y] `isInfixOf` zs || hasTwoDuplicates (y:zs)

isNice2 x = all ($x) [hasSandwich, hasTwoDuplicates]

numNice2 = do
  text <- readFile "words.txt"
  let words = lines text
      niceWords = filter isNice2 words
  return $ length niceWords
