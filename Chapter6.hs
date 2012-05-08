import Tools

-- Function application and currying
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multByTwo = multThree 2
multBySix = multByTwo 3

divideByTen = (/10)
multiplyByTen = (*10)
multiplyByTen' = (10*)

-- Takes a function as an argument (`f`).  Function represented by `f` must
-- take one argument of type `a` and return a value of type `a`.
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<=x) xs) ++ [x] ++ quicksort (filter (>x) xs)

-- A bit slower since it first filters all the numbers in that range
largestBy3829 = maximum (filter f [1..100000])
    where f x = (mod x 3829) == 0

-- Way faster since it quits as soon as it gets the head of the list
largestBy3829' = head (filter f [100000,99999..])
    where f x = (mod x 3829) == 0

sumOfSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
sumOfSquares' = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | odd n  = n : collatz (n * 3 + 1)

partialMultipliers :: [Integer -> Integer]
partialMultipliers = map (*) [0..]

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

-- Lambdas: (\args -> expression with args)
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

-- Fun gimmick with lambdas to illustrate currying
addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

-- Folds
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\a x -> a + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

factorial :: (Num a, Enum a) => a -> a
factorial 0 = 1
factorial x = foldl (*) 1 [1..x]

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldl (\acc i -> acc || x == i) False

-- Function application operator.  $ has lowest precedence.
testApplication  = sqrt 3 + 4 + 9 == (sqrt 3) + 4 + 9
testApplication2 = (sqrt $ 3 + 4 + 9) == sqrt (3 + 4 + 9)

-- Make expressions more readable
testApplication3 = sum (filter (>10) (map (2*) [2..10]))
testApplication4 = sum $ filter (>10) $ map (2*) [2..10]

-- Use function application operator to call functions
testApplication5 = map ($ 3) [(4+), (10*), (^2), sqrt]

-- Function composition operator.
testComposition = map (negate . abs) [1,2,3,4,5,-3,-2,-2]

-- Earlier function written in so-called `point-free` style.  This means the
-- right-most argument is omitted because of function currying.
sum''' :: (Num a) => [a] -> a
sum''' = foldl1 (+)
