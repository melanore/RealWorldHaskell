import Data.Char

--1.
asInt :: Foldable t => t Char -> Int
asInt = foldl f 0 where 
    f x y   = x * 10 + digitToInt y


g xs = foldr f "0" xs where 
    f x y   = "(" ++ [x] ++ "* 10" ++ "+" ++ y ++ ")"

--2.

acc :: Int -> Char -> Int
acc x y = digitToInt y +  x * 10 

asInt' :: [Char] -> Int
asInt' ('-':xs) = -1 * foldl acc 0 xs
asInt' xs       = foldl acc 0 xs

--3.

int :: [Char] -> Int
int xs@(x:xs') 
    | x == '-'  = -1 * foldl f 0 xs'
    | otherwise = foldl f 0 xs
    where f x y = digitToInt y +  x * 10 

capCount :: String -> Int
capCount = length . filter (isUpper . head) . words
