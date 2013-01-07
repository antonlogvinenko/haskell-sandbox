import Control.Applicative

-- If you have a value with a context, `m a`, how do you apply to it
-- a function that takes a normal `a` and returns a value with a context?

-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- Monads are applicative funcotrs that support >>= operation (bind)

-- **** Getting out feet with Monad
-- Maybe as a functor
main1 = fmap (++"!") (Just "wisdom")
-- Maybe as an applicative functor
main2 = (Just (+)) <*> (Just 5) <*> (Just 6)
main3 = (+) <$> (Just 5) <*> (Just 6)
-- liftA2, sequenceA

-- now, what would >>= do for a Maybe?
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- now, some examples:
main4 = Just 3 `applyMaybe` \x -> Just (x + 1)
main5 = Just "smile" `applyMaybe` \x -> Just (x ++ " :)")
main6 = Nothing `applyMaybe` \x -> Just (x + 1)
main7 = Just 2 `applyMaybe` \x -> if x > 2 then Just x else Nothing




-- **** The Monad type class
class Monad' m where
    return' :: a -> m a

    (>>==) :: m a -> (a -> m b) -> m b

    (>>>) :: m a -> m b -> m b
    x >>> y = x >>== \_ -> y

    fail' :: String -> m a
    fail' msg = error msg

instance Monad' Maybe where
    return' x = Just x
    Nothing >>== f = Nothing
    Just x >>== f = f x
    fail' _ = Nothing

main8 = return "Hat" :: Maybe String
main9 = Just 9 >>= \x -> return (x * 10)
main10 = Nothing >>= \x -> return (x * 10)




-- **** Walk the line (example)
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

main11 = landLeft 2 (0, 0)
main12 = landRight 1 (1, 2)
main13 = landLeft 2 (landRight 1 (landLeft 1 (0, 0)))

-- defining a function that changes the order
x -: f = f x

main14 = (0, 0) -: landLeft 2
main15 = (0, 0) -: landLeft 2 -: landRight 1 -: landLeft 2

-- making function fail:
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left, right)
         | abs ((left + n) - right) < 4 = Just (left + n, right)
         | otherwise = Nothing
landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left, right)
          | abs (left - (right + n)) < 4 = Just (left, right + n)
          | otherwise = Nothing
main16 = landLeft 2 (0, 0)
main17 = landLeft 10 (0, 3)
main18 = landRight 1 (0, 0) -: landLeft 2

main19 = landRight' 1 (0, 0) >>== landLeft' 2 >>== landLeft' 1 >>== landRight' 5 >>== landRight' 1
main20 = return (0, 0) >>== landRight' 2 >>== landLeft' 2 >>== landRight' 2

banana :: Pole -> Maybe Pole
banana _ = Nothing

main21 = return (0, 0) >>== landLeft' 1 >>== banana >>== landRight' 2

-- let's use >>
main22 = return (0, 0) >>== landLeft' 1 >>> Nothing >>== landRight' 2

-- >>= is about "desctructiring" a container, using its value with a given function
-- to build a new container




-- **** do notation
-- do is a special syntax for monads
main23 = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))) 
main24 = Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))

foo :: Maybe String 
foo = do
  x <- Just 3
  y <- Just "!"
  return (show x ++ y)

-- main20 = return (0, 0) >>== landRight' 2 >>== landLeft' 2 >>== landRight' 2
foo2 :: Maybe Pole
foo2 = do
  x <- return (0, 0)
  y <- landRight' 2 x
  z <- landLeft' 2 y
  r <- landRight' 2 z
  return r

-- do syntax for more common >>= application order
-- it's clear why do result is the result of the last line

marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)

-- not writing a <- is equivalent to using >>

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft' 2 start
  Nothing
  second <- landRight' 2 first
  landLeft' 1 second

-- pattern matching
justH :: Maybe Char
justH = do
  (x : xs) <- Just "hello"
  return x
-- when matching falis, the next pattern is matched
-- when pattern mathching fails in a do expression, the fail function is called

-- monads that incorporate a context of possible failure implement their own fail function
-- fail _ = Nothing
-- example:
wopwop :: Maybe Char
wopwop = do
  (x : xs) <- Just ""
  return x




-- **** The list monad




-- **** Monad laws



