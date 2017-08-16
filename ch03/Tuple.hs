third :: (t1, t, t2) -> t2
third (_, _, c)   = c

complicated :: (t2, [t3], t1, [t]) -> t3
complicated (_, _:(y:_), _, []) = y


