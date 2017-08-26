import Data.List

--1.
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (h:_)  = Just h

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:t)  = Just t

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [h]    = Just h
safeLast (_:t)  = safeLast t

safeInit :: [t] -> Maybe [t]
safeInit []     = Nothing
safeInit [_]    = Just []
safeInit (h:t)  = Just (h : (initImpl t)) where 
    initImpl []     = []
    initImpl [_]    = []
    initImpl (x:y)  = x : (initImpl y)


--2. Write a function splitWith that acts similarly to words but takes a predicate and a list of any type, 
-- and then splits its input list on every element for which the predicate returns False:

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ []              = []
splitWith predicate (x:xs)
    | predicate x           = let (f, s) = span predicate (x:xs)
                              in  f : (splitWith predicate s)
    | otherwise             = splitWith predicate xs

--3. Write a program that prints the first word of each line of its input.
fstWordOfEachLine :: String -> String
fstWordOfEachLine = unlines . concat . map (take 1 . words) . lines

--4.Write a program that transposes the text in a file. For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n"
transposeFstWordEachLine :: String -> String
transposeFstWordEachLine = unlines . concat .  map transpose . transpose . map words . lines