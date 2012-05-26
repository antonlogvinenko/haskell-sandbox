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
isUpperAlfanum = `elem` ['A'..'Z'])
