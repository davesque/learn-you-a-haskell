import Control.Applicative
-- The `newtype` keyword is used to make types whose only function is to wrap
-- another type.  So, even though it may contain exactly the same data, it will
-- be considered a different type for the purposes of checking type integrity.
-- Since it is only used for this one purpose, Haskell is able to optimize its
-- usage, which makes it faster than just declaring another type with the `data`
-- keyword.
--
-- Newtypes can only have one value constructor.  That value constructor can
-- only takes one field as well.  Consider this newtype which wraps a boolean
-- value:
newtype CoolBool = CoolBool { getCoolBool :: Bool } deriving (Show)
--
--  instance Functor CoolBool where
--      fmap f (CoolBool x) = CoolBool (f x)
--  instance Applicative CoolBool where
--      pure x = CoolBool x
--      (<*>) (CoolBool f) (CoolBool x) = CoolBool (f x)

-- An implementation of the monoid type class:
class MyMonoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

-- Monoid laws:
-- mempty is the identity value...
-- mempty `mappend` x = x
-- x `mappend` mempty = x
--
-- mappend is associative (it doesn't matter what order the operations run
-- in)...
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
--
-- note that the following is not required as a law:
-- x `mappend` y = y `mappend` x
--
-- Common Monoids:
-- Lists
-- Nums (through Product and Sum newtypes)
-- Bools (through Any and All newtypes)
