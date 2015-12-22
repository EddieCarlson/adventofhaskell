import Data.List

data Ing = Ing { cap::Int, dur::Int, flav::Int, tex::Int, cal::Int } deriving Show

addIng (Ing cap dur flav tex cal) (Ing cap2 dur2 flav2 tex2 cal2) = Ing (cap + cap2) (dur + dur2) (flav + flav2) (tex + tex2) (cal + cal2)

ingScore (Ing cap dur flav tex cal) = product $ map minZero [cap, dur, flav, tex]
  where minZero x | x < 0 = 0
                  | otherwise = x

nextIng baseIngs i = head $ sortBy ingCompare $ map (addIng i) baseIngs 
  where ingCompare a b = ingScore b `compare` ingScore a


parse [_, _, cap, _, dur, _, flav, _, tex, _, cal] = Ing (read cap) (read dur) (read flav) (read tex) (read cal)

go = do
  ls <- fmap lines $ readFile "cookies.txt"
  let baseIngs = map (parse . words . filter (/=',')) ls
      oneOfEach = foldl1 addIng baseIngs
      ultimateCookie = (!! (100 - length baseIngs)) $ iterate (nextIng baseIngs) oneOfEach
  return (ingScore ultimateCookie, ultimateCookie)



-- part 2

multIng x (Ing cap dur flav tex cal) = Ing (cap*x) (dur*x) (flav*x) (tex*x) (cal*x)

combinations = [[a, b, c, d] | a <- [1..100], b <- [1..100], a + b < 100, c <- [1..100], a + b + c < 100, d <- [1..100], a + b + c + d == 100, (3*a) + (3*b) + (8*c) + (8*d) == 500]

score baseIngs cs = ingScore $ foldl1 addIng $ zipWith (multIng) cs baseIngs

go2 = do
  ls <- fmap lines $ readFile "cookies.txt"
  let baseIngs = map (parse . words . filter (/=',')) ls
      scores = map (score baseIngs) combinations
  return (maximum scores)
