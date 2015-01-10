data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

getHeight Empty = 0
getHeight (Node now l r) = max (getHeight l) (getHeight r) + 1
