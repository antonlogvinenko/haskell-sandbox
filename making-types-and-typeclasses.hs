-- *** Algebraic data types

--the type and type constructors
--type constructors are functions, type is a concrete type
data Bool = False | True
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- :t Circle produces:
-- Circle :: Float -> Float -> Float -> Shape
-- :t Rectangle produces:
-- Rectangle :: Float -> Float -> Float -> Float -> Shape

--types can be used in type declarations, constructors in desctructuring pattern matching
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- surface $ Circle 10 20 10
-- surface $ Rectangle 0 0 100 100
-- pattern matching against [] or Bool is just the same

-- automagically made the type part of the show typeclass
data Shape2 = Circle2 Float Float Float | Rectangle2 Float Float Float Float deriving (Show)
-- printable:
-- Circle2 10 20 5
-- Rectangle2 50 230 60 90

--type constructors are functions, and the result is printable:
mapping = map (Circle2 10 20) [4, 5, 6]