--pattern matching
--guards
--where
--let
--case expressions


lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

--failing pattern matching
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Brospeh"
charName 'c' = "Cecil"

--structural pattern matching
addVectors1 :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors1 a b = (fst a + fst b, snd a + snd b)

addVectors2 :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

second :: (a, b, c) -> b
second (_, y, _) = y


--structural pattern matching in list comprehensions
xs = [(3,4), (5, 6), (8, 9)]
compr = [a+b | (a, b) <- xs]


--list matching: [], x:xs, x:y:z:xs
head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "This list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is very long! The first two elements are " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


--as pattern: all@(x:xs)
capital :: String -> String
capital "" = "Empty string!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


--guards
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
        | bmi <= 18.5 = "You're underweight"
	| bmi <= 25.0 = "You're mornal"
	| bmi <= 30.0 = "You're fat"
	| otherwise = "Congratulations, you're a whale"

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
	| weight / height ^ 2 <= 18.5 = "Underweight"
	| weight / height ^ 2 <= 25.0 = "Normal"
	| weight / height ^ 2 <= 30.0 = "Fat"
	| otherwise = "Congratulations, you're a whale"

max' :: (Ord a) => a -> a -> a
max' a b
	| a < b = b
	| otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b = GT
	| a == b = EQ
	| otherwise = LT



--where
bmiTell3 :: (RealFloat a) => a -> a -> String
bmiTell3 weight height
	| bmi <= skinny = "You are skinny"
	| bmi <= normal = "You are normal"
	| bmi <= fat = "You are fat"
	| otherwise = "You are a whale"
	where bmi = weight / height ^ 2
	      (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials first second = [f] ++ ". " ++ [s] ++ "."
	where (f:_) = first
              (s:_) = second

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs ]
              where bmi weight height = weight / height ^ 2

describeList :: [a] -> String
describeList xs = "This list is " ++ what xs
                  where what [] = "empty."
                        what [x] = "a singleton list."
                        what xs = "a longer list."


--let
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let sideArea = 2 * pi * r * h
            topArea = pi * r ^ 2
        in sideArea + 2 * topArea
test1 = 4 * (let a = 9 in a + 1) + 2
test2 = let square x = x * x in (square 2, square 3, square 4)
test3 = (let a = 100; b = 2; c = 3 in a * b * c, let a = "cake is a "; b = "lie" in a ++ b)
--pattern mathing in let bindings:
test4 = (let (a, b, c) = (1, 2, 3) in a + b + c) * 100
--let in list comprehensions
calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
calcBmis3 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25]


--case expressions
head'' :: [a] -> a
head'' xs = case xs of [] -> error "Head of an empy list? Kinda kiddin me?"
                       (x:_) -> x

describeList' :: [a] -> String
describeList' xs  = "This list is " ++ case xs of [] -> "empty."
   					          [x] -> "a singleton list."
                                                  xs -> "a longer list."


--summary:
--function: pattern matching level; guards level; where for all guards in a matched pattern
--let is similar to where but is an expression
--case expression is similar to pattern matching but is an expression
