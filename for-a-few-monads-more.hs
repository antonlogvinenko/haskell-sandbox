-- **** Writer 18
-- Writer: logging context
isBigBang :: Int -> (Bool, String)
isBigBang x = (x > 9, "Compared gang size to 9.")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f  = let (y, newLog) = f x in (y, log ++ newLog)

main1 = ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))


-- **** Reader 3

-- **** Stateful computations 10

-- **** Errors 3

-- **** Useful monadic functions 18

-- **** Making monads 6
