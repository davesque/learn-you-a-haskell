import Data.Char
import Data.List

-- Applicative functors refer to functors which have been mapped over with fmap?

-- IO actions can be mapped over as well, since they are members of the Functor
-- class
mapFunctorAction = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line

-- Functions are members of the functor class.  Their instance definition is as
-- follows.
--
-- Instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))
--
--     -or-
--
-- Instance Functor ((->) r) where
--     fmap = (.)
--
-- Functions act as contexts in this way.  The argument that is passed to them
-- is the eventual value they will produce, but with the added context that they
-- must have an operation performed on them before we can get that value.  They
-- are like boxes that hold values but stipulate certain conditions should you
-- want to access their contents.
--
-- fmap's type signature can also be thought of in this way way:
-- fmap :: (Functor f) => (a -> b) -> (f a -> f b)
--
-- So, it can be said that fmap takes a function which takes a value of type `a`
-- and produces a values of type `b`.  It then returns a functions that is the
-- same, but it operates on functors which contain those kinds of values.  In
-- this way, it is said that fmap "lifts" a function to operate on functors.
--
-- Functor laws:
-- 1. Mapping id over a functor should equal the functor...
--    >>> fmap id (func x) == func x
-- 2. Mapping a composed function over a functor should equal the functions
--    successively mapped over the functor...
--    >>> fmap (f . g) (func x) == fmap f (fmap g (func x))
--
-- Here is a counter type that doesn't obey functor laws, even though it seems
-- like it might:
data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

-- In that anti-example, part of the data in the box is hidden from the
-- mapping function (the counter int).  Part of the functor's mutation, when it
-- is mapped, is not due to the application of the mapping function.  This makes
-- the functor instance impure, in a sense, since it may produce different
-- results given the same conditions.  And that's not okay in Haskell.
--
-- So, applicative functors are functors which contain a function?
--
-- Applicative type class definition:
-- class (Functor f) => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b
--
-- So, members of the Applicative class define two functions.  pure returns a
-- value wrapped in a functor of the instance type.  <*> takes a functor which
-- contains a function from `a` to `b` and takes a functor which contains an `a`
-- and returns a functor which contains a `b`.  Or, it takes a functor which
-- contains a function from `a` to `b` and returns a function from a functor of
-- type `a` to a functor of type `b`.  It's kind of like it extracts the
-- function from the functor and allows it to act on values in other functors.
