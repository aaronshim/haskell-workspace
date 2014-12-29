-- Selected exercises from Chapter 3 of Real World Haskell
import Data.List

meanFromList :: [Integer] -> Double
meanFromList xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))
-- this will automatically computer NaN for empty lists

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
-- I don't think this is very efficient

palindromify :: [a] -> [a]
palindromify xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs = (head xs == last xs) && ((isPalindrome . tail . init) xs)
-- but this is at least squared time because finding the last element
-- (and dropping it) requires us to transverse the whole list.
fastPalindromeCheck :: Eq a => [a] -> Bool
fastPalindromeCheck xs = xs == reverse xs
-- (this runs in linear time with linear memory because we only need to
--  transverse the list twice, once to reverse it and another to check)
-- experimental results confirm this! Compare
--  isPalindrome ([1..1000000] ++ reverse [1..1000000])
-- with
--  fastPalindromeCheck ([1..1000000] ++ reverse [1..1000000])
-- (the latter runs in a couple seconds, the former takes forever)

sublistOrdering :: [a] -> [a] -> Ordering
sublistOrdering xs ys = compare (length xs) (length ys)
-- this is a Haskell comparator so that we can feed it into sortBy

sortBySublistLength :: [[a]] -> [[a]]
sortBySublistLength xs = sortBy sublistOrdering xs

joinSublists :: [[a]] -> [a]
joinSublists [] = []
joinSublists (x:xs) = x ++ (joinSublists xs)

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = x
myIntersperse s (x:xs) = x ++ (s:(myIntersperse s xs))
-- a more general case version of joining sublists

-- TREES

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

heightTree :: Tree a -> Integer
heightTree Empty = 0
heightTree (Node _ left right) = 1 + max (heightTree left) (heightTree right)
-- exhaustive search. This is the most recursive and clean solution I can
--  think of, but is there a smarter way?

-- GRAHAM SCAN CONVEX HULL IMPLEMENTATION

data Direction = LeftTurn | RightTurn | Straight deriving (Show, Eq)
type Point = (Double, Double)

whichTurn :: Point -> Point -> Point -> Direction
whichTurn (x1,y1) (x2,y2) (x3, y3)
  | crossProduct > 0 = LeftTurn
  | crossProduct < 0 = RightTurn
  | otherwise = Straight
 where crossProduct = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
-- we could have considered multiple cases of approaching slopes from positive
--  versus negative slopes and then deal with the zero/infinity slope edge
--  case as well, which makes much more nasty code than this

turnList :: [Point] -> [Direction]
turnList (x:(y:(z:xs))) = whichTurn x y z : turnList (y:(z:xs))
turnList _ = [] -- anything with fewer than three points
-- and this is a higher order function turning a list of points into a list
-- of succesive directions that must be taken

grahamPointOrdering :: Point -> Point -> Ordering
grahamPointOrdering (x1,y1) (x2,y2)
   | y1 < y2             = LT
   | y1 > y2             = GT
   | y1 == y2 && x1 < x2 = LT -- find bottommost, leftmost
   | y1 == y2 && x1 > x2 = GT
   | otherwise           = EQ
-- this lets us find our starting point for the algorithm

anglePointOrdering :: Point -> Point -> Point -> Ordering
anglePointOrdering (px,py) (x1,y1) (x2,y2)
   | a1 < a2             = LT
   | a2 > a1             = GT
   | a1 == a2 && d1 < d2 = LT -- measure distance to p if angles equal
   | a1 == a2 && d1 > d2 = GT
   | otherwise           = EQ
  where a1 = atan ((y1 - py) / (x1 - px))
        a2 = atan ((y2 - py) / (x2 - px))
        d1 = (x1 - px)*(x1 - px) + (y1 - py)*(y1 - py)
        d2 = (x2 - px)*(x2 - px) + (y2 - py)*(y2 - py)

acceptPoint :: [Point] -> [Point]
acceptPoint (a:(b:(c:xs)))
  | whichTurn a b c == RightTurn = acceptPoint (b:(c:xs))
  | otherwise                    = b:(acceptPoint (b:(c:xs)))
acceptPoint _ = []
-- (this is the filtering mechanism for the Graham Scan algorithm)
-- take if point considered (which is second point in list passed) is left or
--  straight turn, skip over otherwise. Anything fewer than 3 points do not
--  have to be considered for inclusion.

grahamScan :: [Point] -> [Point]
grahamScan ps = p:(acceptPoint (p:angleSorted)) --p is always sorted last
  where p = minimumBy grahamPointOrdering ps
        angleSorted = sortBy (anglePointOrdering p) ps
-- main algorithm wrapper that combines the previous parts together

