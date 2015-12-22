split :: String -> Char -> [String]
split str char = takeWhile (/=char) str 
