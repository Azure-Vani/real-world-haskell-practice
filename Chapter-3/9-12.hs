import Data.List
inf = 1000000000

data Direction = Clockwise
               | CounterClockwise
               | Straight deriving(Eq)

direction :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Direction
direction (x1, y1) (x2, y2) (x3, y3) = 
    if prod < 0 
        then CounterClockwise 
        else if prod > 0 
                 then Clockwise 
                 else Straight 
                     where prod = dx1 * dy2 - dx2 * dy1 
                               where dx1 = x1 - x2 
                                     dy1 = y1 - y2 
                                     dx2 = x3 - x2 
                                     dy2 = y3 - y2

-- construct a compare function according the polar order
consCmp :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Ordering
consCmp (x0, y0) (x1, y1) (x2, y2) = 
    case dir of Clockwise -> LT
                CounterClockwise -> GT
                Straight -> EQ
    where dir = direction (x1, y1) (x0, y0) (x2, y2)

-- find the left-most point as the polar and sort the points
reArrange :: [(Int, Int)] -> [(Int, Int)]
reArrange xs = 
    polar : (sortBy (consCmp polar) $ filter (/= polar) xs)
    where
        polar = minimum xs

-- attemp to push a point into the existing convex
pushTo :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
pushTo (x, y) convex 
    | length convex <= 1 = (x, y):convex
    | otherwise = if direction (x, y) mid oth /= Clockwise
                      then pushTo (x, y) $ tail convex
                      else (x, y):convex
                  where
                      mid = head convex
                      oth = head $ tail convex

-- generate the convex using graham algorithm
genConvex :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
genConvex convex [] = convex
genConvex convex (x:xs) = genConvex (pushTo x convex) xs

graham :: [(Int, Int)] -> [(Int, Int)]
graham xs = (head rest) : (genConvex [] (tail rest)) where rest = reArrange xs

