fromMaybe :: t -> Maybe t -> t
fromMaybe defval wrapped =
    case wrapped of
        Nothing     -> defval
        Just value  -> value

skip n xs 
    | n <= 0    = xs
    | xs == []  = []
skip n (_:xs)       = skip (n - 1) xs

