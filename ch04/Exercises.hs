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