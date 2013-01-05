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


-- Applicative functors
-- The newtype keyword
-- Monoids