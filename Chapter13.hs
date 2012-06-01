-- Monads are beefed up Applicative Functors.
--
-- Some prep: Values that reside inside of types are like values with a
-- context.  There is more to their meaning than just the value itself.  Maybe
-- values represent a value from a computation that may have failed.  [] values
-- represent values from computations that have several results (perhaps you
-- could call them nondeterministic computations).  IO values represent values
-- that have side effects.
--
-- Monads are a natural extension of applicative functors.  They aim to address
-- the following question:  How can one take a value with a context and apply a
-- function to it that takes the value inside of that context and produces a
-- value of another type in that context and use all of that to produce a value
-- of the second type in that context?
--
-- The bind function type signature:
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
--
-- (>>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Nothing >>>= _ = Nothing
-- (Just x) >>>= f = f x
--
-- class Monad m where
--     return :: a -> m a
--
--     (>>=) :: m a -> (a -> m b) -> b
--
--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y
--
--     fail :: String -> m a
--     fail msg = error msg

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise = Nothing

-- Will equal Nothing
testLanding = return (0,0) >>= landLeft 2 >>= landRight 4 >>= landLeft (-1) >>= landLeft (-1)
-- Will equal Just (1,4)
testLanding2 = return (0,0) >>= landLeft 2 >>= landRight 4 >>= landLeft (-1)

-- Awesome!  Monads allow us to use functions which take normal values as
-- arguments and return other values wrapped in types (which are members of the
-- Monad typeclass).  You don't have to modify the arguments of the function to
-- account for the output of it.  In this way, the contexts of Monads are
-- preserved through chains of operations and failures are propagated down the
-- chain.

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- Will equal Nothing
testLanding3 = return (0,0) >>= landLeft 2 >>= landRight 4 >>= banana >>= landLeft 2

-- Remember the (>>) function?
testLanding4 = return (0,0) >>= landLeft 2 >>= landRight 4 >> Nothing >>= landLeft 2

-- Do syntax is just another way of binding monads
testLanding5 = do
    x <- return (0,0)
    y <- landLeft 2 x
    z <- landRight 4 y
    landLeft 2 z

-- Putting a nomadic value on a line without binding it is the same as (>>).  This causes the value bound to y to now be `Nothing`.
testLanding6 = do
    x <- return (0,0)
    y <- landLeft 2 x
    Nothing
    z <- landRight 4 y
    landLeft 2 z

-- Do syntax looks like imperative programming, but it really just builds a
-- sequential expression in which each new line depends on the value of the
-- last one.
