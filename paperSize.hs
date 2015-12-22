import Data.List
import Data.List.Split

sortAndApply3 f x@[a, b, c] = case sort x of sorted -> f sorted

paperSize = sortAndApply3 paperSizePreSorted
ribbonSize = sortAndApply3 ribbonSizePreSorted

paperSizePreSorted [s, m, l] = (2 * (s * m + s * l + m * l)) + (s * m)
ribbonSizePreSorted [s, m, l] =  (2 * (s + m)) + (s * m * l)

presentSizes = map $ map (read::String->Int) . splitOn "x"

calculatePaperSize = do
  text <- readFile "input.txt"
  let xStrings = filter (/="") (lines text)
      presents = presentSizes xStrings
      sortedPresents = map sort presents
      paper = sum $ map paperSizePreSorted sortedPresents
      ribbon = sum $ map ribbonSizePreSorted sortedPresents
  return (paper, ribbon)
