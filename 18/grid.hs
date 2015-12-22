import Data.List
import System.Posix.Unistd

parse [] = []
parse (x:xs) | x == '#' = 1:(parse xs)
             | x == '.' = 0:(parse xs)

stayOn :: (Eq a, Num a) => [a]
stayOn = [2, 3]

change :: (Eq a, Num a) => a -> a -> a
change cur nCount | cur == 1 && (nCount `elem` stayOn) = 1
                  | cur == 1 = 0
                  | nCount == 3 = 1
                  | otherwise = 0

output :: (Eq a, Num a) => [[a]] -> a
output w = change cur ((sum $ map sum w) - cur)
  where cur = (!!1) $ (!!1) w

groupThree x | length x < 3 = []
groupThree (a:b:c:ds) = [a, b, c] : groupThree (b:c:ds)

padArray n = (n:).(++[n])

padAB :: (Num a) => [[a]] -> [[a]]
padAB g = padArray padRow g
  where padRow = map (const 0) (head g)

padGrid = padAB . map (padArray 0)
          
windows = map (transpose . (map groupThree)) . groupThree . padGrid

transform = map (map output) . windows

go = do
  text <- lines <$> readFile "grid.txt"
  let initialGrid = map parse text
      finalGrid = (!!100) $ iterate transform initialGrid
  return (sum $ map sum finalGrid)

-- part 2

go2 = do
  text <- lines <$> readFile "grid.txt"
  let initialGrid = map parse text
      finalGrid = (!!100) $ iterate (setCorners1 . transform) (setCorners1 initialGrid)
  return (sum $ map sum finalGrid)

setArrayEnds1 = padArray 1 . middle

middle = drop 1 . init

setCorners1 g = ((setArrayEnds1 (head g)) : middle g) ++ [setArrayEnds1 (last g)]

--animation

goShow = do
  text <- lines <$> readFile "grid.txt"
  putStrLn "hello there"
  let initialGrid = map parse text
      gridList = take 100 $ iterate transform initialGrid
      gridListStrings = map showGrid gridList
  do
    mapM_ (mapM_ putStrLn) $ gridListStrings
    sleep 1
    return ""
  --sequence $ map (mapM_ (putStrLn) gridListStrings
  return (sum $ map sum $ last gridList)

toHashDot x | x == 1 = '#'
            | x == 0 = '.'

brackets = ['[', ']', ',']

showGrid :: (Num a, Eq a) => [[a]] -> [String]
showGrid = map (show . map toHashDot) 

