--curried functions. space is sort of operator and has the highest precedence
max2 = max 5

multiplyThree :: (Num a) => a -> a -> a -> a
multiplyThree x y z = x * y * z

multiplyByNine :: (Num a) => a -> a -> a
multiplyByNine = multiplyThree 9

compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlfanum :: Char -> Bool
isUpperAlfanum = (`elem` ['A'..'Z'])

--higher order functions in haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f $ f x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
      where g x y = f y x

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f x y = f y x

--maps and filters
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x = x : others
	| otherwise = others
	where others = filter' f xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
	let lessThanX = filter' (<= x) xs
	    moreThanX = [y | y <- xs, y > x ]
	in lessThanX ++ [x] ++ moreThanX

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
                   where p x = x `mod` 3829 == 0

sick = sum $ takeWhile (< 1000) $ filter odd $ map (^2) [1..]




--lambdas, single pattern matching
zipper = zipWith (\a b -> (a * 30 + 3) / b) [5, 6, 9, 4] [54, 2, 8, 9]
mapper = map (\(a, b) -> a + b) [(1, 2), (3, 4), (5, 6)]
curryingLambdaHeyHey = \x -> \y -> \z -> x + y + z

flip3 :: (a -> b -> c) -> (b -> a -> c)
flip3 f = \x y -> f y x




--folds
sum1 :: (Num a) => [a] -> a
sum1 xs = foldl (\acc x -> acc + x) 0 xs

--currying foldl
sum2 :: (Num a) => [a] -> a
sum2 = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldl (\acc y -> if x == y then True else acc) False xs

--map2 to be implemented with right fold as : is less expensive than ++
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\x acc -> f x : acc) [] xs

--foldl1 and foldr 1 use first element as accumulator initial value
product' :: (Num a) => [a] -> a
product' = foldr1 (*)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

--scanl, scanr report all accumulator values; similar scanl1, scanr1





--function applicaton with $
($$) :: (a -> b) -> a -> b
f $$ x = f x

--this thing makes me mad, guys
crazyStuff = map ($ 3) [(4+), (10*), (^2), sqrt]
crazyStuff2 = map ($$ 3) [(6+), (9*), (^3), sqrt]