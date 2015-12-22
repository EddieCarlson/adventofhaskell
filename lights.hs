import qualified Data.Map as Map
import Data.List
import Data.List.Split
import Data.String.Utils

makeGrid n = replicate n (replicate n False)

turnOn = "turn on"
through = "through"
turnOff = "turn off"
toggle = "toggle"

data Cmd = On | Off | Toggle deriving (Show, Eq, Ord)
data Point = Point Int Int deriving (Eq)
data Instruction = Instruction Cmd Point Point deriving (Show, Eq)

instance Show Point where
  show (Point a b) = show (a, b)

boolFun On x = True
boolFun Off x = False
boolFun Toggle x = not x

changeMiddle array start stop f =
  let (pre, rest) = splitAt start array
      span = stop - start + 1
      (middle, end) = splitAt span rest
      newMiddle = map f middle
  in pre ++ newMiddle ++ end

changeRow :: Int -> Int -> Cmd -> [Bool] -> [Bool]
changeRow start stop cmd row = changeMiddle row start stop (boolFun cmd)

changeGrid grid (Instruction cmd (Point x1 y1) (Point x2 y2)) = changeMiddle grid y1 y2 $ changeRow x1 x2 cmd

throughSplit = map (splitOn ",") . map strip . splitOn through
toInstr [[x1, y1], [x2, y2]] cmd = Instruction cmd (Point (read x1) (read y1)) (Point (read x2) (read y2))

parseInstr string cmd = toInstr (throughSplit string) cmd

parse string | Just rest <- stripPrefix turnOn string = parseInstr rest On
parse string | Just rest <- stripPrefix turnOff string = parseInstr rest Off
parse string | Just rest <- stripPrefix toggle string = parseInstr rest Toggle

numLit = do
  text <- readFile "instructions.txt"
  let instructionStrings = lines text
      instructions = map parse instructionStrings
      grid = makeGrid 1000
      finalGrid = foldl changeGrid grid instructions
      numTrue = length $ filter id $ concat finalGrid
  return numTrue
             

-- part 2
 
intFun On x = x + 1
intFun Off x | x > 0 = x - 1
intFun Off x = 0
intFun Toggle x = x + 2

changeRow2 start stop cmd row = changeMiddle row start stop $ intFun cmd
changeGrid2 grid (Instruction cmd (Point x1 y1) (Point x2 y2)) = changeMiddle grid y1 y2 $ changeRow2 x1 x2 cmd

makeIntGrid n = replicate n (replicate n 0)

intensity = do
  text <- readFile "instructions.txt"
  let instructionStrings = lines text
      instructions = map parse instructionStrings
      grid = makeIntGrid 1000
      finalGrid = foldl changeGrid2 grid instructions
      numTrue = sum $ concat finalGrid
  return numTrue
