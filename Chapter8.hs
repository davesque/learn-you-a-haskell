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
     , phoneNumber="303-819-6083"
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
