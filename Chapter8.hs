import qualified Data.Map as Map

-- Define a data type with a self-named value constructor
data Point = Point Float Float
    deriving (Show)
-- Define a shape data type.  `deriving` makes Shape part of the Show typeclass.
-- Two `value constructor functions` are defined here: Circle and Rectangle
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

-- Define a function to find the surfance area of a shape
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

-- Demonstrates how value constructors are functions too
concentricCircles = map (Circle $ Point 0 0) [1,2,3,4]

-- Data type defined with record syntax
data Person = Person
              { firstName :: String
              , lastName :: String
              , age :: Int
              , height :: Float
              , phoneNumber :: String
              , flavor :: String
              } deriving (Show)
me = Person
     { firstName="David"
     , lastName="Sanders"
     , age=31
     , height=5.90
     , phoneNumber="303-000-0000"
     , flavor="Pistachio"
     }

-- Define a vector type
data Vector a = Vector a a a deriving (Show)

vectorPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vectorPlus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectorMult :: (Num t) => t -> Vector t -> Vector t
m `vectorMult` (Vector i j k) = Vector (m*i) (m*j) (m*k)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- Type synonyms allow for easy "aliasing" of types under different names
type MyString  = [Char]
type PhoneBook = [(String,String)]

-- The Either type constructor allows potentially more than one type to be returned
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerNumber = Int
type Response = String
type LockerMap = Map.Map LockerNumber (LockerState, Code)

lockerLookup :: LockerNumber -> LockerMap -> Either Response Code
lockerLookup lockerNumber lockerMap =
    case Map.lookup lockerNumber lockerMap of
        Nothing            -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                              then Right code
                              else Left $ "Locker number " ++ show lockerNumber ++ " is already taken!"

-- Recursive data types can be used.  The built-in list type is one.
infixr 5 :-:
data MyList a = Empty | a :-: (MyList a) deriving (Show, Read, Ord, Eq)

-- Infix function that adds two of our lists together
infixr 5 .++
(.++) :: MyList a -> MyList a -> MyList a
Empty .++ ys    = ys
(x:-:xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = (Node a left right)
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

-- Implementation of Eq typeclass:
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)
--
-- Defining == and /= by mutual recursion (in terms of each other) allows the
-- minimal completion definition for the typeclass to be smaller.  When
-- defining an instance of this typeclass later on, we only have to explicitly
-- define one of those functions.  The one left undefined will be evaluated in
-- terms of the one that was defined.

data TrafficLight = Red | Yellow | Green

-- Create an instance of Eq.  Defining == alone is fine because of the
-- recursive definition inside of the class.
instance Eq TrafficLight where
    Red == Red       = True
    Yellow == Yellow = True
    Green == Green   = True
    _ == _           = False

instance Show TrafficLight where
    show Red    = "Red"
    show Yellow = "Yellow"
    show Green  = "Green"

-- Implementation of the Maybe instance for the Eq typeclass:
-- instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y   = (x == y)
--     Nothing == Nothing = True
--     _ == _             = False

-- Implementation of Functor typeclass:
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b
--
-- Here, we see a single function signature defined: fmap.  Fmap takes a
-- function which converts an `a` to a `b` and then takes an enclosing type or
-- type constructor which contains an `a` and produces a type constructor which
-- contains `b`.  So fmap takes an `a` to `b` converter function as well as a
-- type constructor containing `a` and produces a type constructor containing
-- `b`.  I just said that twice!!  So, in the most basic sense, any type
-- constructor which is an instance of the Functor typeclass is something that
-- contains things.  And it requires one function to be implemented which
-- converts a container of one kind of thing into a container of another kind
-- of thing.

data MyMaybe a = MyNothing | MyJust a deriving (Show, Read, Eq)

class MyFunctor f where
    fmap' :: (a -> b) -> f a -> f b

-- instance Functor Maybe where
--     fmap _ Nothing = Nothing
--     fmap f (Just x) = Just (f x)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- instance Functor (Either a) where
--     fmap f (Right x) = Right (f x)
--     fmap f (Left x) = Left x

-- Kinds: kinds are sort of like the types of types.  typing :k Int in ghci
-- will yield "Int :: *", which means that Int is a concrete type.  Anything
-- that is a value (5, "Hello", takeWhile) must have a concrete type.  Typing
-- :k Maybe will yield "Maybe :: * -> *", which means that Maybe is a type
-- constructor that takes one concrete type and returns another concrete type.
--
-- Prelude> :k Maybe Int  -- Maybe Int is a concrete type
-- Maybe Int :: *
-- Prelude> :k Either     -- Either takes two concrete types and returns a concrete type
-- Either :: * -> * -> *
