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
data Maybe a = Nothing | Just a

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
type IntMap1 v = Map Int v
type IntMap2 = Map Int





-- **** Recursive data structures
-- **** Typeclasses 102
-- **** Yes-no typeclass
-- **** The Functor typeclass
-- **** Kinds