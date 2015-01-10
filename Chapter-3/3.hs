mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)
