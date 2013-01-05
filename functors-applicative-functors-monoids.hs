import Data.Char  
import Data.List 
--import Control.Monad.Instances




-- **** Functors redux
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- IO as a Functor
-- instance Functor IO where
--    fmap f action = do
--      result <- action
--      return (f result)

main1 = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

main2 = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

-- Function as a Functor
-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))

instance Functor ((->) r) where
    fmap = (.)

main3 = fmap (*3) (+100)

--even more weird, with currying
main4 :: (Functor f, Num a) => f a -> f a
main4 = fmap (*2)

-- first functor law
-- if we map the id function over the functor
-- then the functor we get back should be the same as the original functor
-- i.e., fmap id = id
mustHold = fmap id (Just 3) == (Just 3)

-- second functor law
-- composing two functions and mapping the result over a functor
-- should be the same as first mapping one function over the functor
-- and then mapping the other one
-- i.e., fmap (f . g) = fmap f . fmap g
-- or fmap (f . g) F = fmap f (fmap g F)


-- Pathological example of a type constructor
-- being an instance of Functor typeclass
-- but not really being a Functor
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- breaking the law
firstLaw = case fmap id (CJust 0 "cake") of
             (CJust 0 "cake") -> True
             otherwise -> False
secondLaw = case fmap ((++ "blah") . (++ "cake")) (CJust 0 "test") of
              (CJust 2 "testcakeblah") -> True
              otherwise -> False




-- **** Applicative functors
-- what about mapping an f over a functor for f with more than one argument?
-- we get functors containing functions
functorWithFunction :: Maybe (String -> String)
functorWithFunction = fmap (++) (Just "hey ")
functorWithFunctionUsed = fmap (\f -> f "you") functorWithFunction

-- what if want to apply (Just (*8)) to (Just 8)?
-- no magic here, quire clear intentions:
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
-- so we can write:
main5 = Just (+3) <*> Just 9
main6 = Just (++"haha") <*> Nothing
main7 = pure (*3) <*> Just 9
-- or even like that:
main8 = pure (+) <*> Just 3 <*> Just 5

-- now, further:
-- "pure f <*> x" is the same as "fmap f x"
-- "pure f <*> x <*> y <*> ..." is the same as "fmap f x <*> y <*> ..."
-- so, let's define an operator:
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
-- now we can:
main9 = (++) <$> Just "John" <*> Just "Reeze"
-- so, if we want to apply f, we just write "f <$> x <*> y <*> ..."

-- so, now we have applicative functor typeclass and pure, <*>, <$> functions
-- let's try with types other than Maybe
-- [], IO, (->) r, ZipList

-- for []
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
main10 = [(*0), (+100), (^2)] <*> [1, 2, 3]
-- [0, 0, 0, 101, 102, 103, 1, 4, 9]
main11 = [(+), (*)] <*> [1, 2] <*> [3, 4]
main12 = (++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "."]

-- for IO
instance Applicative IO where
    pure = return
    a <*> b = do
      f <- a
      x <- b
      return (f x)

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

main13 = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The two lines contcatenated to be: " ++ a

-- for (->) r
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
main14 = pure (+) <*> (+3)





-- **** The newtype keyword
-- **** Monoids