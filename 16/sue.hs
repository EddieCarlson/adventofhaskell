import qualified Data.Map as Map
import qualified Data.Set as Set

data Sue = Sue { num::Int, kids::Maybe Int, cats::Maybe Int, sams::Maybe Int, poms::Maybe Int, aks::Maybe Int, vizs::Maybe Int, goldfish::Maybe Int, trees::Maybe Int, cars::Maybe Int, perfumes::Maybe Int } deriving Show

theSue = Sue 0 (Just 3) (Just 7) (Just 2) (Just 3) (Just 0) (Just 0) (Just 5) (Just 3) (Just 2) (Just 1)
testSue = Sue 0 (Nothing) (Just 7) (Just 2) (Just 3) (Nothing) (Just 0) (Just 5) (Just 3) (Just 2) (Just 1)

sueArray (Sue _ k cat s pom a v g t c per) = [k, cat, s, pom, a, v, g, t, c, per]
sueMaybeEq m1 m2 = ((==) <$> m1 <*> m2) /= Just False

instance Eq Sue where
  sue1 == sue2 = and $ map (\(a, b) -> sueMaybeEq a b) $ zip (sueArray sue1) (sueArray sue2)

tuplify [] = []
tuplify (x:y:ys) = (x, (read::String->Int) y):(tuplify ys)

mapToSueAry m = map (\k -> Map.lookup k m) ["Sue", "children", "cats", "samoyeds", "pomeranians", "akitas", "vizslas", "goldfish", "trees", "cars", "perfumes"]

mapToSue m = case mapToSueAry m of [Just n, ch, cats, sams, poms, aks, visz, golds, trees, cars, pers] -> Sue n ch cats sams poms aks visz golds trees cars pers

go = do
  text <- fmap lines $ readFile "sue.txt"
  let ms = map (Map.fromList . tuplify . words . filter (not . (`elem` ":,"))) text
      sues = map mapToSue ms
  return (filter (== theSue) sues)

--part 2

sueMaybeLt m1 m2 = not $ (compare <$> m1 <*> m2) `elem` [Just GT, Just EQ]
sueMaybeGt m1 m2 = not $ (compare <$> m1 <*> m2) `elem` [Just LT, Just EQ]

mustBeGt sue = [cats sue, trees sue]
mustBeEq sue = [kids sue, sams sue, aks sue, vizs sue, cars sue, perfumes sue]
mustBeLt sue = [poms sue, goldfish sue]

matchingSue sue2 = and [gt, lt, eq]
  where f comp gen = and $ map (\(a, b) -> comp a b) $ zip (gen theSue) (gen sue2) 
        gt = f sueMaybeLt mustBeGt
        lt = f sueMaybeGt mustBeLt
        eq = f sueMaybeEq mustBeEq

go2 = do
  text <- fmap lines $ readFile "sue.txt"
  let ms = map (Map.fromList . tuplify . words . filter (not . (`elem` ":,"))) text
      sues = map mapToSue ms
  return (filter (matchingSue) sues)
