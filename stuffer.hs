import Data.Hash.MD5
import Data.List
import Debug.Trace(trace)

input = "bgvyzdsv"

hashConcat = md5s . Str . (++) input

findLowest n = find hashHasNLeadingZeroes $ map show [1..]
                 where prefix = replicate n '0'
                       hashHasNLeadingZeroes x = prefix `isPrefixOf` hashConcat x
