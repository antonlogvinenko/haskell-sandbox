-- **** Algebraic data types

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

--just the same name for the type and type constructor
data Point = Point Float Float deriving (Show)
data Shape3 = Circle3 Point Float | Rectangle3 Point Point deriving (Show)
surface2 :: Shape3 -> Float
surface2 (Circle3 _ r) = pi * 2 * r
surface2 (Rectangle3 (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape3 -> Float -> Float -> Shape3
nudge (Circle3 (Point x y) r) a b = Circle3 (Point (x + a) (y + b)) r
nudge (Rectangle3 (Point x1 y1) (Point x2 y2)) a b = Rectangle3 (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

--and now, auxilliary functions:
baseCircle :: Float -> Shape
baseCircle r = Circle3 (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

--importing types
--module Shapes ( Point(..), Shape(..), baseCircle, baseRectangle) where
--also possible ot skip importing type constructors so that only aux function are accessible outside




-- **** Record syntax