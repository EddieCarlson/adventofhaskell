import Debug.Trace
import Data.List

data Chr = Chr { d::Int, a::Int, hp::Int, items::[String] }

-- returns true if first Chr wins, false otherwise
fight (Chr d1 a1 hp1 i1) (Chr d2 a2 hp2 i2) | hp2 <= 0 = (True, i1)
                                            | hp1 <= 0 = (False, i1)
                                            | otherwise = fight (Chr d1 a1 (hp1 - damage d2 a1) i1) (Chr d2 a2 (hp2 - damage d1 a2) i2)
  where damage d a = max (d - a) 1

data Item = Item { cost::Int, di::Int, ai::Int, n::String }

instance Show Item where
  show (Item c di ai n) = show (c, n)

addItem (Chr d a hp i) (Item c di ai n) = Chr (d + di) (a + ai) hp (n:i)

choose xs n = filter ((==n) . length) $ subsequences xs
chooseMult xs ns = concatMap (choose xs) ns

weapons = [ Item 8  4 0 "dagger", Item 10 5 0 "Shortsword", Item 25 6 0 "Warhammer", Item 40 7 0 "Longsword", Item 74 8 0 "Greataxe" ]

armor = [ Item 13  0 1 "Leather", Item 31  0 2 "Chainmail", Item 53  0 3 "Splintmail", Item 75  0 4 "Bandedmail", Item 102 0 5 "Platemail" ]

rings = [ Item  25 1 0 "Damage +1", Item  50 2 0 "Damage +2", Item 100 3 0 "Damage +3", Item  20 0 1 "Defense +1", Item  40 0 2 "Defense +2", Item  80 0 3 "Defense +3" ]

possibleWeapons = choose weapons 1
possibleArmor = chooseMult armor [0, 1]
possibleRings = chooseMult rings [0, 1, 2]

sortedItemSets = sortOn (sum . map cost) $ map concat $ sequence [possibleWeapons, possibleArmor, possibleRings]
baseChr = Chr 0 0 100 []
boss = Chr 9 2 103 ["boss items"]

createChr items = foldl addItem baseChr items

go = find win sortedItemSets
  where win is = fst $ fight (createChr is) boss 

--part 2

go2 = find lose $ reverse sortedItemSets
  where lose is = not $ fst $ fight (createChr is) boss
