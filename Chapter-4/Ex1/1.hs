applySafe :: ([a] -> b) -> [a] -> Maybe b
applySafe f x 
    | null x = Nothing
    | otherwise = Just (f x)

safeHead :: [a] -> Maybe a
safeHead xs = applySafe head xs

safeTail :: [a] -> Maybe [a]
safeTail xs = applySafe tail xs

safeLast :: [a] -> Maybe a
safeLast xs = applySafe last xs

safeInit :: [a] -> Maybe [a]
safeInit xs = applySafe init xs
