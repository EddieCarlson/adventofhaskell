
import Data.List
import Data.List.Split
import Text.Regex


subs new old = concat . intersperse new . splitOn old

replaceS [] = []
replaceS x | Just rest <- stripPrefix "\\\"" x = '.' : replaceS rest
replaceS x | Just rest <- stripPrefix "\\\\" x = '.' : replaceS rest
replaceS x | Just rest <- stripPrefix "\\x" x = '.' : replaceS (drop 2 rest)
replaceS x = head x : replaceS (tail x)

fixedLength x = (length $ replaceS x) - 2

getStrings = do 
  textWithQuotes <- readFile "strings.txt"
  let lsWith = lines textWithQuotes
  return lsWith

go = do
  textWithQuotes <- readFile "strings.txt"
  let lsWith = lines textWithQuotes
  return ((sum (map length lsWith)) - (sum (map fixedLength lsWith)))


-- part 2


encodeS [] = []
encodeS x | Just rest <- stripPrefix "\\\"" x = "...." ++ encodeS rest
encodeS x | Just rest <- stripPrefix "\\\\" x = "...." ++ encodeS rest
encodeS x | Just rest <- stripPrefix "\\x" x = "....." ++ encodeS (drop 2 rest)
encodeS x = head x : encodeS (tail x)

encodedLength x = (length $ encodeS x) + 4

go2 = do
  textWithQuotes <- readFile "strings.txt"
  let lsWith = lines textWithQuotes
  return ((sum (map encodedLength lsWith)) - (sum (map length lsWith)))
