import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Map as Map


sqrtI n = floor $ sqrt (fromIntegral n)

modIsZero n x = n `mod` x == 0

lowest_denom n = fromJust $ find (modIsZero n) [2..n]

m_all_factors = (map all_factors [0..] !!)
  where all_factors 1 = [1]
        all_factors n = let lowest = lowest_denom n in nub $ n:lowest:(m_all_factors (n `div` lowest))

score :: [Int] -> Int
score = sum . map (*10)

go = find ((>33100000) . score . m_all_factors) [1..]

go2 n = find ((>n) . score . m_all_factors) [1..]

update :: (Int, Int, Map.Map Int [Int]) -> Int -> (Int, Int, Map.Map Int [Int])
update _ 1 = (10, 1, Map.fromList [(1, [1])])
update (_, _, m) n = ((score newList), n, (Map.insert n newList m))
  where lowest = lowest_denom n
        newList = nub $ n:lowest:(fromJust $ Map.lookup (n `div` lowest) m)

startTuple :: (Int, Int, Map.Map Int [Int])
startTuple = (0, 0, Map.empty)

findScore n = find ((>= n) . fst) $ map (\(a, b, _) -> (a, b)) $ scanl update startTuple [1..]
