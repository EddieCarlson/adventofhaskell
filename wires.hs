import Data.Word
import Data.Bits
import Data.List
import Data.List.Split
import Data.String.Utils
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace

stripSplit p s = filter (/= "") $ map strip (splitOn p s)
parts = stripSplit "->" 

data Op = Set Word16 | And String String | Or String String | Not String | Lshift Int String | Rshift Int String deriving (Eq, Show, Ord)

andS = "AND"
orS = "OR"
notS = "NOT"
lshiftS = "LSHIFT"
rshiftS = "RSHIFT"

readWord x = ((read :: String -> Word16) x)
readInt = read :: String -> Int

parseLeft x | andS `isInfixOf` x = case stripSplit andS x of [a, b] -> And a b
parseLeft x | orS `isInfixOf` x = case stripSplit orS x of [a, b] -> Or a b
parseLeft x | notS `isInfixOf` x = case stripSplit notS x of [a] -> Not a
parseLeft x | lshiftS `isInfixOf` x = case stripSplit lshiftS x of [n, a] -> Lshift (readInt n) a
parseLeft x | rshiftS `isInfixOf` x = case stripSplit rshiftS x of [n, a] -> Rshift (readInt n) a


stringIsNumber s = all isDigit s

stripSplitApply2 p str f m = case stripSplit p str of [a, b] -> let (av, newMa) = getVal a m
                                                                    (bv, newMb) = getVal b newMa
                                                                in (f av bv, newMb)

stripSplitApply p str f m = case stripSplit p str of [a] -> let (av, newMa) = getVal a m
                                                            in (f av, newMa)

stripSplitApplyShift p str f m = case stripSplit p str of [a, b] -> let (av, newMa) = getVal a m
                                                                    in (f av (readInt b), newMa)

getVal :: String -> Map.Map String String -> (Word16, Map.Map String String)
getVal x m | stringIsNumber x = (readWord x, m)
getVal x m | andS `isInfixOf` x = stripSplitApply2 andS x (.&.) m
getVal x m | orS `isInfixOf` x = stripSplitApply2 orS x (.|.) m
getVal x m | lshiftS `isInfixOf` x = stripSplitApplyShift lshiftS x shiftL m
getVal x m | rshiftS `isInfixOf` x = stripSplitApplyShift rshiftS x shiftR m
getVal x m | notS `isInfixOf` x = stripSplitApply notS x complement m
getVal x m = let (v, newM) = fromJust $ fmap (\x -> getVal x m) (Map.lookup x m)
             in (v, (Map.insert x (show v) newM))


toTuple [x, y] = (x, y)

aVal x wireFile = do
  text <- readFile wireFile
  let inputs = lines text
      m = Map.fromList $ map (toTuple . reverse . stripSplit "->") inputs
  return (fst $ getVal x m)
