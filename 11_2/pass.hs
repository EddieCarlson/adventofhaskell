import qualified Data.Set as Set

badLetters = Set.fromList $ map ((subtract 96) . ord) ['i', 'o', 'l']

incr [] = [1]
incr (x:xs) | x == 26 = 1 : incr xs
incr (x:xs) | x `elem` xs = (x + 2):xs
incr (x:xs) = (x + 1):xs
