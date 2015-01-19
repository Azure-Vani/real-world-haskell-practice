import Data.Char
import Data.List

asInt_fold :: String -> Int
asInt_fold "" = 0
asInt_fold all@(x:xs)
    | x == '-' = negate $ toInt xs
    | otherwise = toInt all
        where toInt = foldl' step 0
              step acc x = acc * 10 + digitToInt x
