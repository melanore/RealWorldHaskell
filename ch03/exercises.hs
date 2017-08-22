import Data.List

-- exercises
--1-2.
count :: Num t => [a] -> t
count []       = 0
count (_:xs)   = 1 + (count xs)

--3.
mean :: Fractional t => [t] -> t
mean []        = 0
mean xs        = mySum / myLen
    where 
        mySum = sum xs
        myLen = fromIntegral $ length xs
        
--4.
palindromize []     = []
palindromize list   = list ++ (reverse list)
        where 
            reverse []      = []
            reverse (x:xs)  = (reverse xs) ++ [x]

--5.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = (head xs) == (last xs) && (isPalindrome (tail xs))

--6.
-- in case of empty list in ghci type should be explicit for print (e.q. listSorter ([] :: String))
compareLists :: (Foldable t, Foldable t1) => t1 a1 -> t a -> Ordering
compareLists s1 s2
    | length s1 > length s2 = GT
    | length s1 < length s2 = LT
    | otherwise             = EQ

listSorter :: Foldable t => [t a] -> [t a]
listSorter a = case a of
    []  -> []
    x   -> sortBy compareLists x

--7-8.
f :: a -> [[a]] -> [a]
f _ []     = []
f _ [x]    = x
f s (x:xs) = let f' x1 x2 = (x1 ++ [s] ++ x2) in f' x (f s xs)

--9.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
    deriving (Show)

height :: (Ord t, Num t) => Tree t1 -> t
height Empty                    = 0
height (Node _ Empty Empty)     = 1
height (Node _ a b)   = 1 + max (height a) (height b)

--10
data Direction = Left
               | Right
               | Straight
    deriving (Show)

type Point = (Integer, Integer)
