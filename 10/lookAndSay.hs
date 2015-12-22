groupLike [] = []
groupLike (x:xs) = show (length same + 1) ++ [x] ++ groupLike rest
  where (same, rest) = span (==x) xs

lookAndSay n = (!!n). iterate groupLike
