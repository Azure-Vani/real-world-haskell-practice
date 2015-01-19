import Data.Char
import Data.List

type ErrorMessage = String

asInt_fold :: String -> Either ErrorMessage Int
asInt_fold "" = Right 0
asInt_fold all@(x:xs)
    | x == '-' = case xs of
                     "" -> Left "Invalid input"
                     otherwise -> fmap negate (toInt xs)
    | otherwise = toInt all
        where toInt = foldl' step (Right 0)
              step acc x
                  | x `elem` ['0'..'9'] = fmap ((+ (digitToInt x)) . (* 10)) acc
                  | otherwise = Left $ "Non-digit " ++ [x]
