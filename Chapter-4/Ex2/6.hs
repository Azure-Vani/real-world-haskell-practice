groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xs = foldr step [] xs
    where step x [] = [[x]]
          step x all@(a:acc)
              | f x (head a) = (x : a) : acc
              | otherwise = [x] : all
