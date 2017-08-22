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

fsharpYoda list     = if list |> null |> not then head list else "Yoda people me call"
haskellNormail list = if not $ null $ list then head list else "I'm normal"