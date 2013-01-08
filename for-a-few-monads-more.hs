import Data.Monoid

-- **** Writer 18
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
newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

main3 = runWriter (return 3 :: Writer (Product Int) Int)

-- do notation with Writer
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["GotNumber: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)
main4 = runWriter multWithLog


-- **** Reader 3

-- **** Stateful computations 10

-- **** Errors 3

-- **** Useful monadic functions 18

-- **** Making monads 6
