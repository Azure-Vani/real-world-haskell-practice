splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f xs
    | f (head xs) = 
        let (cur, res) = break (not.f) xs 
        in splitWith f res
    | otherwise = 
        let (cur, res) = break f xs 
        in cur : splitWith f res
