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
