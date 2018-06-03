data Shape = Triangle Double Double
  | Rectangle Double Double
  | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

--1
Eq Shape where
  (==) (Triangle x z) (Triangle a b) = x == a && z == b
  (==) (Rectangle x z) (Rectangle a b) = x == a && z == b
  (==) (Circle x) (Circle y) = x == y
  (==) _ _ = False

--2
Ord Shape where
  compare x y = compare (area x) (area y)
