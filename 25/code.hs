
divisor = 33554393
mult = 252533
oneKmult = mult ^ 1000 `mod` divisor
oneMmult = oneKmult ^ 1000 `mod` divisor

num row col | col == 1 = 1 + sum [1..row - 1]
            | otherwise = col - 1 + num (row + col - 1) 1 

index = num 2978 3083

ms = index `div` 1000000
ks = (index `mod` 1000000) `div` 1000
ones = (index `mod` 1000) - 1 -- starts at 1

firstCode = 20151125

code = firstCode * (oneMmult ^ ms) * (oneKmult ^ ks) * (mult ^ ones) `mod` divisor
 
