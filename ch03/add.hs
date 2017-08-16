


add :: Num a => a -> a -> a
add a b = a + b

-- my comment
myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
    then xs
    else myDrop (n - 1) (tail xs)

isOdd :: Integral a => a -> Bool
isOdd n = mod n 2 == 1

lastButOne :: [a] -> Maybe a
lastButOne xs = if null xs || null (tail xs) 
    then Nothing
    else if null (tail (tail xs)) 
        then Just (head xs) 
        else lastButOne (tail xs)

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo :: BookInfo
myInfo = Book 9780135072455 "Algebra of programming" ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String
data Review = Review CustomerID ReviewBody deriving (Show)

