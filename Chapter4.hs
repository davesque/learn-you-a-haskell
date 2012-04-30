import Tools

--
-- PATTERN MATCHING
--

lucky :: Integral a => a -> String
lucky 7 = "Hey, it's lucky!"
lucky _ = "Ehh...try again."

-- Patterns match top to bottom
sayMe :: Integral a => a -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe 3 = "three"
sayMe 4 = "four"
sayMe 5 = "five"
sayMe _ = "some other number"

-- Recursive factorial
factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Without pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
-- With pattern matching
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Functions for triples
first :: (Num a) => (a, a, a) -> a
first (x, _, _) = x
second :: (Num a) => (a, a, a) -> a
second (_, y, _) = y
third :: (Num a) => (a, a, a) -> a
third (_, _, z) = z

-- Pattern matching in a list comprehension
addPairs :: (Num a) => [(a, a)] -> [a]
addPairs xs = [(a + b) | (a, b) <- xs]

-- Reimplement head
head' :: [a] -> a
head' [] = error "Can't get head of empty list."
head' (x:_) = x

-- Recursive re-implementations
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

firstLetter :: String -> String
firstLetter "" = "No first letter of an empty string."
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is '" ++ [x] ++ "'."

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "thin"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "overweight"
    | otherwise   = "obese"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | bmi <= thin       = "thin"
    | bmi <= normal     = "normal"
    | bmi <= overweight = "overweight"
    | otherwise         = "obese"
    where bmi = weight / height ^ 2
          (thin, normal, overweight) = (18.5, 25.0, 30.0)

-- Define initials with pattern matching in where clause.  This is not the best
-- way to do it, but should demonstrate the uses of where bindings.
initials :: String -> String -> String
initials fname lname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = fname
          (l:_) = lname

-- Here's how it probably should be defined.
initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ "." ++ [l] ++ "."

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi w h = w / h ^ 2

-- Where bindings are visible in the whole function, including any guards.  Let
-- bindings are only visible in the expression defined after their 'in'
-- keyword.  Let also defines an expression as opposed to where which is merely
-- a 'syntactic construct'.
testLet =
    let a = 100
        b = 200
        c = 300
    in a + b + c
testLet2 = let a = 100; b = 200; c = 300 in a + b + c

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi w h | (w, h) <- xs, let bmi w h = w / h ^ 2]

-- "Overloaded" function definitions are syntactic sugar for case expressions.
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs  -> "a many-valued list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what []  = "empty."
          what [x] = "a singleton list."
          what xs  = "a many-valued list."

describeList'' :: [a] -> String
describeList'' []  = "The list is empty."
describeList'' [x] = "The list is a singleton list."
describeList'' xs  = "The list is a many-valued list."
