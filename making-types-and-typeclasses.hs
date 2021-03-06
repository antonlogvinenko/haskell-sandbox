-- 1. types (with or without parameters), synonyms, deriving typeclasses; partial application
-- 2. typeclasses, subclassing, instantiation; partial application in definition
-- 3. relations in definition and with (partial) parent




-- **** Algebraic data types

--the type and value constructors
--value constructors are functions, type is a concrete type
data Bool1 = False1 | True1
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

--value constructors are functions, and the result is printable:
mapping = map (Circle2 10 20) [4, 5, 6]

--just the same name for the type and value constructor
data Point = Point Float Float deriving (Show)
data Shape3 = Circle3 Point Float | Rectangle3 Point Point deriving (Show)
surface2 :: Shape3 -> Float
surface2 (Circle3 _ r) = pi * 2 * r
surface2 (Rectangle3 (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape3 -> Float -> Float -> Shape3
nudge (Circle3 (Point x y) r) a b = Circle3 (Point (x + a) (y + b)) r
nudge (Rectangle3 (Point x1 y1) (Point x2 y2)) a b = Rectangle3 (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

--and now, auxilliary functions:
baseCircle :: Float -> Shape3
baseCircle r = Circle3 (Point 0 0) r

baseRect :: Float -> Float -> Shape3
baseRect width height = Rectangle3 (Point 0 0) (Point width height)

--importing types
--module Shapes ( Point(..), Shape(..), baseCircle, baseRectangle) where
--also possible ot skip importing value constructors so that only aux function are accessible outside




-- **** Record syntax
data Person1 = Person1 String String Int Float String String deriving (Show)
guy1 = Person1 "Buddy" "Frank" 43 184.2 "532" "Chocolate"

data Person = Person {firstName :: String, lastName :: String, age :: Int, height :: Float, phoneNumber :: String, flavor :: String} deriving (Show)
--field names are real functions
-- :t firstName is:
-- firstName :: Person -> String

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
a = Car {company = "Ford", model = "Mustang", year = 1967}
-- printed in a different manner




-- **** Type parameters
--Maybe a - type constructor
data Maybe1 a = Nothing1 | Just1 a

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)




-- **** Derived instances
data Person3 = Person3 {firstName3 :: String, lastName3 :: String, age3 :: Int} deriving (Eq, Show, Read)
-- person is now printable, readable, comparable
mikeD = Person3 {firstName3 = "Michael", lastName3 = "Diamond", age3 = 43}
showMikeD = show mikeD
--also compare, read

--data Bool2 = False2 | True2 deriving (Ord)
--comparable, the first is lesser

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)
showMonday = show Monday
readMonday = read "Monday" :: Day
equality = Saturday == Sunday
minBoundDay = minBound :: Day
succDay = succ Monday
daysRange = [Thursday .. Sunday]
allDaysRange = [minBound .. maxBound] :: [Day]




-- **** Type synonyms
type String2 = [Char]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]

--partially applied!
data Map1 k v = Empty1 | Map1 {head :: (k, v), tail :: Map1 k v}
type IntMap1 v = Map1 Int v
type IntMap2 = Map1 Int




-- **** Recursive data structures




-- **** Typeclasses 102
class Eq1 a where
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    x === y = not (x /== y)
    x /== y = not (x === y)

data TrafficLight = Red | Yellow | Green
-- implementing minimal subset
instance Eq1 TrafficLight where
    Red === Red = True
    Green === Green = True
    Yellow === Yellow = True
    _ === _ = False

-- subclassing typeclasses:
--class (Eq a) => Num a where

--typeclasses instantiation: making all (Maybe m) of typeclass Eq, considering m is of Eq:
instance (Eq m) => Eq (Maybe1 m) where
    Just1 x == Just1 y = x == y
    Nothing1 == Nothing1 = True
    _ == _ = False




-- **** Yes-no typeclass
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo TrafficLight where 
    yesno Red = False
    yesno _ = True




-- **** The Functor typeclass
class MyFunctor f where
    myFmap :: (a -> b) -> f a -> f b

instance MyFunctor [] where
    myFmap = map

instance MyFunctor Maybe where
    myFmap f (Just x) = Just (f x)
    myFmap f Nothing = Nothing

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
instance MyFunctor Tree where
    myFmap f EmptyTree = EmptyTree
    myFmap f (Node x leftsub rightsub) = Node (f x) (myFmap f leftsub) (myFmap f rightsub)

data MyEither a b = MyLeft a | MyRight b
instance MyFunctor (MyEither a) where
    myFmap f (MyRight x) = MyRight (f x)
    myFmap f (MyLeft x) = MyLeft x




-- **** Kinds
class ToFu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance ToFu Frank where
    tofu x = Frank x

-- tofu (Just 'a') :: Frank Char Maybe

data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}


