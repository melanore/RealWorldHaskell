import Data.Char

asInt :: [Char] -> Int
asInt xs = loop 0 xs where 
    loop acc []     = acc
    loop acc (x:xs) = loop acc' xs where
        acc' = acc * 10 + digitToInt x

square :: Num t => [t] -> [t]
square []       = []
square (x:xs)   = x^2 : square xs

square' :: Num t => [t] -> [t]
square' = map (^2)

upperCase :: [Char] -> [Char]
upperCase []        = []
upperCase (x:xs)    = toUpper x : upperCase xs

upperCase' :: [Char] -> [Char]
upperCase'  = map toUpper

myMap :: (t -> a) -> [t] -> [a]
myMap f (x:xs)    = f x : myMap f xs
myMap _ _         = []

oddList :: Integral a => [a] -> [a]
oddList (x:xs)  | odd x     = x : oddList xs
                | otherwise = oddList xs
oddList _                   = []

mySum :: Num t => [t] -> t
mySum xs = aggr 0 xs where
    aggr acc (x:xs) = aggr (acc + x) xs
    aggr acc _      = acc

niceSum :: (Num b, Foldable t) => t b -> b
niceSum = foldl (+) 0

myFoldl :: Foldable t => (t2 -> t1 -> t2) -> t2 -> t t1 -> t2
myFoldl f z xs = foldr step id xs z 
    where step x g a = g (f a x)
