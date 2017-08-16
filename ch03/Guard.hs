fromMaybe :: t -> Maybe t -> t
fromMaybe defval wrapped =
    case wrapped of
        Nothing     -> defval
        Just value  -> value

skip n xs 
    | n <= 0    = xs
    | xs == []  = []
skip n (_:xs)       = skip (n - 1) xs

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