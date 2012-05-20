-- types
-- use :t predicate in console
-- types examples: Bool, Char, (Bool, Char), [Char]
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

--String is a synonym for [Char]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + x

addTwo :: Int -> Int -> Int
addTwo x y = x + y




--type variables and polymorphic functions
head' :: [a] -> a
head' xs = head xs

fst' :: (a, b) -> a
fst' pair = fst pair




--typeclass, type constraint
equals :: (Eq a) => a -> a -> Bool
equals a b = a == b

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = x `elem` xs

--Type classes
--Eq; ==, /=; Bool
--Ord; >, <, <=, >=; Ordering, GT, LT, EQ
--Show; show
--Read; read
read' :: (Read a) => String -> a
read' xs = read xs
read1 = read "[1,2,3,4,5]" :: String
read2 = read "[1,2,3,4,5]" :: [Int]
--Enum; succ, pred;
succ' :: Enum a => a -> a
succ' e = succ e
--Bounded; maxBound, minBound; kinda crazy
bounded1 = maxBound :: Int
bounded2 = maxBound :: Char
maxBound' :: (Bounded a) => a
maxBound' = maxBound