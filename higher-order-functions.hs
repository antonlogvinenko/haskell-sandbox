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