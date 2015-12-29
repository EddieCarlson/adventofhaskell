import qualified Data.Map as Map
import Data.List
import Data.Maybe

parse n l | Just x <- stripPrefix "jio a " l = \(a, b) -> (if a == 1 then (n + (read::String->Int) x) else n + 1, a, b) 
          | Just x <- stripPrefix "jie a " l = \(a, b) -> (if even a then (n + (read::String->Int) x) else n + 1, a, b) 
          | Just x <- stripPrefix "jmp " l = \(a, b) -> ((n + (read::String->Int) x), a, b) 
          | l == "inc a" = \(a, b) -> (n + 1, a + 1, b) 
          | l == "tpl a" = \(a, b) -> (n + 1, a * 3, b) 
          | l == "hlf a" = \(a, b) -> (n + 1, a `div` 2, b) 
          | l == "inc b" = \(a, b) -> (n + 1, a, b + 1) 

parseTuple n l = (n, parse n l)

badChars = ",+"

go startA startB = do
  ins <- lines <$> filter (not . (`elem` badChars)) <$> readFile "instructions.txt"
  let m = Map.fromList $ map (\(n, l) -> parseTuple n l) $ zip [1..] ins
  return (travel 1 startA startB m)

travel n a b m = fromMaybe (a, b) $ z
  where z = recurse <$> ($ (a, b)) <$> mFun
        mFun = Map.lookup n m
        recurse (newN, newA, newB) = travel newN newA newB m
