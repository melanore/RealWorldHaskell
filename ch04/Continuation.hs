import Data.List

plus :: Num a => a -> a -> a
a `plus` b = a + b

data a `Pair` b = a `Pair` b
    deriving(Show)

foo :: Pair Integer Integer
foo = Pair 1 2

bar :: Pair Bool [Char]
bar = True `Pair` "bar"

(|>) x f = f x

helloFsharp     = "Hello" |> (++) " F#"
helloHaskell    =  (++) " Haskell" $ "Hello" 

safeHead :: [a] -> Maybe a
safeHead l 
    | not $ null $ l   = Just $ head l
    | otherwise        = Nothing