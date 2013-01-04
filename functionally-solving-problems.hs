-- **** Reverse Polish Notation Calculator
-- 10 4 3 + 2 * - evaluates to -4, steps:
-- [10] [10, 4] [10, 4, 3] [10, 4, 3, +] [10, 7] [10, 7, 2] [10, 7, 2, *] [10, 14] [10, 14, -] [-4]

-- implementaion tips:
-- function type will be solveRPN :: (Num a) => String -> a
-- String should be split with space into [String]: ["10", "4", "3", "+", "2", "*"]
-- we'll use left fold, using a stack as an accumulator, the result is a single element stack
-- we'll implement a stack as a list, with its top at the head (performance, appropriate use of lists)

import Data.List

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl folding [] . words
                      where folding (x:y:ys) "*" = (x * y) : ys
                            folding (x:y:ys) "+" = (x + y) : ys
                            folding (x:y:ys) "-" = (x - y) : ys
                            folding xs number = read number : xs




-- **** Heathrow to London
data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

-- should return [(B, 10), (C, 30), (A, 5), (C, 20), (B, 2), (B, 8)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPathToA = priceA + a
        crossPathToA = priceB + b + c
        forwardPathToB = priceB + b
        crossPathToB = priceA + a + c
        newPathToA = if forwardPathToA <= crossPathToA
                     then (A, a) : pathA
                     else (C, c) : (B, b) : pathB
        newPathToB = if forwardPathToB <= crossPathToB
                     then (B, b) : pathB
                     else (C, c) : (A, a) : pathA
    in (newPathToA, newPathToB)


optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
        a = sum $ map snd bestAPath
        b = sum $ map snd bestBPath
    in if a <= b
       then reverse bestAPath
       else reverse bestBPath

findOptimalPath = optimalPath heathrowToLondon