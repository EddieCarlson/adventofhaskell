import Data.Char
import Data.List
import Debug.Trace

parse [] total = total
parse (x:xs) total | x == '-' = case (getNumber xs) of (num, rest) -> parse rest (total - read num)
                   | isNumber x = case (getNumber (x:xs)) of (num, rest) -> parse rest (total + read num)
                   | otherwise = parse xs total
  where getNumber = span isNumber


go = do
  text <- readFile "json.txt"
  return (parse text 0)


-- part 2


removeRedObjs prev [] = reverse prev
removeRedObjs prev rest | ":\"red\"" `isPrefixOf` rest = removeRedObjs (dropObjLeft1 prev) (dropObjRight1 rest)
removeRedObjs prev (x:xs) | otherwise = removeRedObjs (x:prev) xs

dropObjLeft1 = dropTilEven '{' '}' 1
dropObjRight1 = dropTilEven '}' '{' 1

dropTilEven minusChar plusChar count arr | count == 0 = arr
dropTilEven minusChar plusChar count (x:xs) | x == minusChar = dropIt (count - 1) xs
                                            | x == plusChar = dropIt (count + 1) xs
                                            | otherwise = dropIt count xs
  where dropIt = dropTilEven minusChar plusChar

go3 = do
  text <- readFile "json.txt"
  let redless = removeRedObjs [] text
  return (parse redless 0)
