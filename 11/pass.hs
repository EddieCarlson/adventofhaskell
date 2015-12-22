import qualified Data.Set as Set
import Data.List

badLetters = Set.fromList ['i', 'o', 'l']

incr [] = ['a']
incr (x:xs) | x == 'z' = 'a' : incr xs
            | succ x `Set.member` badLetters = (succ . succ $ x):xs
            | otherwise = (succ x):xs

has3Decreasing x | length x < 3 = False
has3Decreasing (x:y:z:zs)       = (x == succ y && y == succ z) || has3Decreasing (y:z:zs)

has2Doubles x | length x < 4 = False
has2Doubles (x:y:zs)         = x == y && hasDouble zs x
  where hasDouble arr _ | length arr < 2 = False
        hasDouble (x:y:zs) prev          = x == y && x /= prev || hasDouble (y:zs) prev

hasNoBadLetters = Set.null . Set.intersection badLetters . Set.fromList

nextPassword = fmap reverse . find suitable . iterate incr . reverse
  where suitable = and . sequenceA [has3Decreasing, has2Doubles, hasNoBadLetters]
