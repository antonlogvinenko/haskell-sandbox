import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Instances
import Control.Monad.State

-- **** Writer
-- Writer: logging context
isBigBang :: Int -> (Bool, String)
isBigBang x = (x > 9, "Compared gang size to 9.")

applyLog' :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog' (x, log) f  = let (y, newLog) = f x in (y, log ++ newLog)

main1 = ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))

-- hence we append things let's use Monoid type class
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

main2 = ("beans", Sum 20) `applyLog` addDrink

-- the Writer type
newtype Writer' w a = Writer' { runWriter' :: (a, w) }

instance (Monoid w) => Monad (Writer' w) where
    return x = Writer' (x, mempty)
    (Writer' (x, v)) >>= f = let (Writer' (y, v')) = f x in Writer' (y, v `mappend` v')

main3 = runWriter' (return 3 :: Writer' (Product Int) Int)

-- do notation with Writer
logNumber' :: Int -> Writer' [String] Int
logNumber' x = Writer' (x, ["GotNumber: " ++ show x])

multWithLog :: Writer' [String] Int
multWithLog = do
  a <- logNumber' 3
  b <- logNumber' 5
  return (a * b)
main4 = runWriter' multWithLog

-- tell function is used to add logs
-- be careful with monoid to use - lists can be very slow

-- difference lists:
-- prepending create another external lambda
-- any call calculates it all
-- \xs -> "dog" ++ ("meat" ++ xs)  
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs ++)  

fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))




-- **** Reader 3
-- instance Monad ((->) r) where
--    return x = \_ -> x
--    h >>= f = \w -> f (h w) w

addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)
main5 = addStuff 3
-- (*2) >>= (\a -> (+10) >>= (\b -> return (a+b)))   applied to 3

-- (+10) >>= (\b -> return (6+b))   applied to 3
-- return (6+13)   applied to 3
-- (\_ -> 19)   applied to 3
-- 19




-- **** Stateful computations 10
-- stateful computation: s -> (a, s), where s it the type of the state and a - the result of
-- stateful computations
type Stack = [Int]

pop1 :: Stack -> (Int, Stack)
pop1 (x : xs) = (x, xs)

push1 :: Int -> Stack -> ((), Stack)
push1 a xs = ((), a : xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push1 3 stack
    (a, newStack2) = pop1 newStack1
    in pop1 newStack2

main6 = stackManip [5, 8, 2, 1]

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a
                                    in  g newState  

pop :: State Stack Int  
pop = State $ \(x : xs) -> (x, xs)  
  
push :: Int -> State Stack ()  
push a = State $ \xs -> ((), a : xs) 

stackManip2 :: State Stack Int  
stackManip2 = do  
    push 3  
    a <- pop
    pop

-- push 3 >>= (\_ -> pop >>= (\a -> (\_ - pop)))
-- 

main7 = runState stackManip2 [5, 8, 2, 1]

stackManip3 :: State Stack Int  
stackManip3 = do  
    push 3  
    pop  
    pop  

main8 = runState stackManip3 [5, 8, 2, 1]

-- **** Errors 3

-- **** Useful monadic functions 18

-- **** Making monads 6
