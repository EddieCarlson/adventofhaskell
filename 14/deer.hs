import Data.List
import Control.Applicative

data Deer = Deer { name::String, speed::Int, dur::Int, restDur::Int } deriving Show

deerAry (Deer _ speed dur restDur) = concat $ repeat speedAndRest
  where speedAndRest = (replicate dur speed) ++ (replicate restDur 0)

distAfterSecs s d = sum $ take s $ deerAry d

parseLine [name, _, _, speed, _, _, dur, _, _, _, _, _, _, restDur, _] = Deer name (read speed) (read dur) (read restDur)

go = do
  text <- fmap lines $ readFile "deer.txt"
  let deer = (map parseLine (map words text))
      speed = map (\d -> (distAfterSecs 2503 d, d)) deer
  return (sortOn fst speed)
  
-- part 2

distancesBySecond s d = tail $ scanl (+) 0 $ take s $ deerAry d

zipDeers = getZipList . sequenceA . map (ZipList . distancesBySecond 2503)

deerScores distances = map (\x -> if x == bestD then 1 else 0) distances
  where bestD = maximum distances

getTotalScores = map sum . getZipList . sequenceA . map ZipList

go2 = do
  text <- fmap lines $ readFile "deer.txt"
  let deer = (map parseLine (map words text))
      scores = getTotalScores $ map deerScores $ zipDeers deer
  return (sort scores)

