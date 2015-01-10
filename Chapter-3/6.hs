import Data.List

cmp :: [a] -> [a] -> Ordering
cmp x y | length x < length y = LT
        | length x > length y = GT
        | otherwise = EQ

mySort :: [[a]] -> [[a]]
mySort xs = sortBy cmp xs
