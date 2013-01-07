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
class Monad m where
    return :: a -> m a

    (>==) :: m a -> a -> m b -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg


-- **** Walk the line (example)


-- **** do notation

-- **** The list monad

-- **** Monad laws
