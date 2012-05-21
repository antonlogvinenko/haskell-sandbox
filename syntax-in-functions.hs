
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