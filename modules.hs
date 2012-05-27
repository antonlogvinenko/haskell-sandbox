import Data.List
--import Data.List (nub, sort)
--import Data.List hiding (nub)
--import qualified Data.List
--import qualified Data.List as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


--Data.List
intersperse1 = intersperse '.' "monkeys"
intercalate1 = intercalate " " ["hey", "there", "gals"]
transpose1 = transpose [[1,2,3], [5,6,7], [8,9,0]]
--foldl', foldl1' strict versions of their lazy incarnations
concat1 = concat ["cake ", "is ", "a ", "lie"]
concatMap1 = concatMap (replicate 4) [1..3]

--I'll try to implement it
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f xs = concat $ map f xs
--concatMap' f = concat . map f
--concatMap' f xs = concat (map f xs)

and1 = and $ map (>4) [4, 6]
and2 = and $ map (>4) [9, 9]
--or is analogous
--any, all - same with predicate



