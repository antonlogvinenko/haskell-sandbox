maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of an empty list undefined"
maximum' [x] = x
maximum' (x:xs)
         | x > maxTail = x
         | otherwise = maxTail
         where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of an empty list undefined"
maximum'' [x] = x
maximum'' (x:xs) = max x $ maximum'' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
           | n <= 0 = []
           | otherwise = x:replicate' (n-1) x

