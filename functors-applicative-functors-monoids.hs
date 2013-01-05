import Data.Char  
import Data.List 

-- **** Functors redux

-- IO is a Functor
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



-- Applicative functors
-- The newtype keyword
-- Monoids