-- x and y length
data Cartesian2D = Cartesian2D Double Double
    deriving (Eq, Show)

data Polar2D = Polar2D Double Double
    deriving (Eq, Show)


--x = Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (sqrt 2) (sqrt 2)