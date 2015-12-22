import Data.List

inputFile = "directions.txt"

move (a, b) '^' = (a, b + 1)
move (a, b) '>' = (a + 1, b)
move (a, b) 'v' = (a, b - 1)
move (a, b) '<' = (a - 1, b)
move (a, b) _   = (a, b)

-- how to refer to correct foldr? (i.e. not from Data.Set?)
-- unique = foldr p []
--          where p x acc | x `elem` acc = acc
--                        | otherwise = x : acc

numUniqueHouses = do
  text <- readFile inputFile
  let distinctHouses = nub $ scanl move (0, 0) text
  return (length distinctHouses)



-- part 2:

split2 x = reverseTuple $ splitReverse ([], []) x
             where reverseTuple (a, b) = (reverse a, reverse b)
splitReverse (a, b) x = case take 2 x of [] -> (a, b)
                                         [e1] -> (e1:a, b)
                                         [e1, e2] -> splitReverse (e1:a, e2:b) $ drop 2 x

doubleSanta = do
  text <- readFile inputFile
  let (santa, robo) = split2 text
      distinctHouses = nub $ concatMap (scanl move (0, 0)) [santa, robo]
  return (length distinctHouses)
