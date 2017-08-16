data List a = Cons a (List a)
            | Nil
    deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
    deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- exercises
toList :: List a -> [a]
toList (Cons x xs) = x : toList xs
toList Nil = [] 


data Tree' a = Node' (Maybe a) (Maybe (Tree' a)) (Maybe (Tree' a))
    deriving (Show)