import Data.Char  
import Data.List 
--import qualified Control.Applicative as Appl
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
main14 = (+) <$> (+3) <*> (*100) $ 5
main15 = (\x y z -> [x, y, z]) <$> (+3) <*> (*2) <*> (/2) $ 5

-- k <$> f <*> g    -   k will be called on eventual results from f and g
-- k <$> Just a <$> Just b   -   k will be called on values that may or may not be there

-- for ZipList, another way for lists to be applicative functors
--instance Applicative ZipList where
--    pure x = ZipList (repeat x)
--    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
--main16 = getZipList $ ZipList [(+), (+), (+)] <*> ZipList [100, 100, 100] <*> ZipList [1, 2, 3]
--main17 = getZipList $ (+) <$> ZipList [100, 100, 100] <*> ZipList [1, 2, 3]
-- ,, is (\x y z -> (x, y, z))
-- , is (\x y -> (x, y))
-- standard library contains functions: zipWithN, N in [1..7]
-- applicative approach allows us to build n-arity zipWith

-- and now, for something completely different, liftA2 function
liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$> a <*> b
main18 = liftA2' (:) (Just 3) (Just [4])

-- sequenceA'
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x : xs) = (:) <$> x <*> sequenceA' xs

-- hmm, let's use liftA2!
sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' [] = pure []
sequenceA'' (x : xs) = liftA2' (:) x $ sequenceA'' xs

-- or let's use foldr
sequenceA''' :: (Applicative f) => [f a] -> f [a]
sequenceA''' = foldr (liftA2' (:)) (pure [])
main19 = sequenceA''' [Just 3, Just 2, Just 1]
main20 = sequenceA''' [Just 1, Nothing, Just 5]
main21 = sequenceA''' [(+3), (+2), (+1)] 1
main22 = sequenceA''' [[1, 2, 3], [4, 5, 6]]
main23 = sequenceA''' [(>4), (<10), odd] 7
main24 = and $ sequenceA''' [(>4), (<10), odd] 7

-- rules for applicative functors:
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u




-- **** The newtype keyword
-- newtype for creation faster types based on existing types
-- but with only one constructor
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
-- CharList :: [Char] -> CharList
-- getCharList ::CharList -> [Char]

-- newtype for making new class instances
-- to make a functor over the first parameter of something, not the first one
newtype Pair b a = Pair { getPair :: (a, b) }
instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

-- newtype laziness
-- evaluation of undefined leads to an exception, we'll use it later
main25 = undefined
newtype CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "Hello"
-- no exception here:
main26 = helloMe undefined

-- type vs newtype vs data
-- type - for just synonyms
-- newtype - wrapping an existing type into a new one
-- data - making new data types




-- **** Monoids
-- a typeclass with associative binary function and an identity function
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

-- rules for monoids:
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- Lists are monoids
instance Monoid [a] where
    mempty = []
    mappend = (++)

-- Product and Sum
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
main27 = getProduct . mconcat . map Product $ [3, 4, 2]

-- Any and All
newtype Any  = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)
main28 = getAny. mconcat . map Any $ [False, False, True, False]

newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)
main29 = getAll . mconcat . map All $ [True, True, False]

-- Ordering
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

-- Maybe
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
main30 = Nothing `mappend` Just "andy"

newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x

main31 = getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]

-- Using to fold data structure 

