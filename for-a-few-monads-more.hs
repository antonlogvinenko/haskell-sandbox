import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Instances
import Control.Monad.Error
import Control.Monad
import Data.Ratio
--import Control.Monad.State

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

main7 = runState stackManip2 [5, 8, 2, 1]

stackManip3 :: State Stack Int  
stackManip3 = do  
    push 3  
    pop  
    pop  

main8 = runState stackManip3 [5, 8, 2, 1]

stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a== 5
     then push 5
     else do
       push 3
       push 8

main9 = runState stackStuff [9, 0, 2, 1, 0]

moreStack :: State Stack ()
moreStack = do
  a <- stackManip3
  if a == 100
     then stackStuff
     else return ()

get = State $ \s -> (s, s)
put newState = State $ \s -> ((), newState)

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1, 2, 3]
     then put [8, 3, 1]
     else put [9, 2, 1]




-- **** Errors 3
-- instance (Error e) => Monad (Either e) where
--     return x = Right x
--     Right x >>= f = f x
--     Left err >>= f = Left err
--     fail msg = Left (strMsg msg)
main10 = Right 3 >>= \x -> return (x + 100)
main11 = Left "boom" >>= \x -> return (x + 1)




-- **** Useful monadic functions 18
-- liftM
-- Applicative Functors are Functors
-- Monads are Applicative Functors, thought not definied explicitly

-- fmap :: (Monad m) => (a -> b) -> m a -> m b
liftM :: (Monad f) => (a -> b) -> f a -> f b
liftM f m = m >>= (\x -> return (f x))

liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f m = do
  x <- m
  return (f x)

-- <*> :: (Applicative f) => f (a -> b) -> f a -> f b
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
  f <- mf
  x <- m
  return (f x)

-- mf >>= (\f -> (\x -> return (f x)))

-- join
join' :: (Monad m) => m (m a) -> m a
join' mm = do
  m <- mm
  m

main12 = join' (Just (Just 9))
main13 = join' [[1, 2, 3], [4, 5, 6]]

-- filterM
-- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

-- foldM
-- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

-- safer RPN calculator
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = Main.liftM (:xs) (readMaybe numberString)  

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

main14 = solveRPN "1 2 * 4 + 5 *"

-- composing monadic functions

-- **** Making monads
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)  
    ]

flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)  
  
coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  
  
loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]
  
--flipThree :: Prob [Prob]
-- note: fmap in >>= definition applies will aply a function to Head or Tails, that's important
-- a, b, c take different values one by one
-- functional to imperative representation of computation
flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin
    return (all (== Tail) [a, b, c])

--main15 = getProb flipThree