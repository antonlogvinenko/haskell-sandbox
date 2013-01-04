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