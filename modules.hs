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
