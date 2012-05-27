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

iterate1 = take 10 $ iterate (*2) 1
iterate2 = take 3 $ iterate (++ "haha") "haha"

splitAt1 = splitAt 3 "heyyou"

takeWhile1 = takeWhile (>2) [6, 5, 4, 2, 10]
--dropWhile, span, break, sort, group, inits, tails
--isInfixOf, isSuffixOf, elem, notElem, elemIndex, find, elemIndices
--findIndex, findIndices
--zip3, zip4
--lines, unlines
--words, unwords
--nub



--maps
findKey1 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey1 key [] = Nothing
findKey1 key ((k,v):xs) = if key == k
                         then Just v
                              else findKey1 key xs

findKey2 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey2 key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing