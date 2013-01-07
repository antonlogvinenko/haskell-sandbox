-- If you have a value with a context, `m a`, how do you apply to it
-- a function that takes a normal `a` and returns a value with a context?

-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- Monads are applicative funcotrs that support >>= operation (bind)

-- **** Getting out feet with Monad
-- Maybe as a functor
main1 = fmap (++"!") (Just "wisdom")


-- **** The Monad type class


-- **** Walk the line (example)


-- **** do notation

-- **** The list monad

-- **** Monad laws
