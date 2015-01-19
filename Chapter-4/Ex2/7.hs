import Data.List
import Data.Char

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldr step False xs
    where step x acc = f x || acc

cycle' :: [a] -> [a]
cycle' xs = foldr step [] [1..]
    where step x acc = xs ++ acc

words' :: String -> [String]
words' xs = strip $ foldr step ("", []) xs
    where step x (last, acc)
              | isAlpha x = (x:last, acc)
              | otherwise = case last of
                                "" -> (last, acc)
                                otherwise -> ("", last:acc)
          strip ("", acc) = acc
          strip (str, acc) = str:acc

unlines' :: [String] -> String
unlines' xs = foldr step "" xs
    where step x acc = x ++ "\n" ++ acc
