takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) 
    | f x = x : takeWhile f xs
    | otherwise = takeWhile f xs

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr step [] xs
                       where step x acc 
                                 | f x = x : acc
                                 | otherwise = []
