import Tools

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Can't get maximum of empty list."
maximum' [x] = x
maximum' (x:xs)
    | x > maximum' xs = x
    | otherwise = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Can't get maximum of empty list."
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:(replicate' (n - 1) x)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n xs
    | n <= 0 = []
    | otherwise = let y:ys = xs in y:(take' (n - 1) ys)

take'' :: (Num i, Ord i) => i -> [a] -> [a]
take'' n _
    | n <= 0    = []
take'' _ []     = []
take'' n (x:xs) = x:(take'' (n - 1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' i (x:xs) = if i == x
                 then True
                 else elem' i xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' _ [] = False
elem'' a (x:xs)
    | a == x    = True
    | otherwise = elem'' a xs

-- A sorted list is composed of: a sorted list of all elements smaller than or
-- equal to the head, then the head, then a sorted list of all elements larger
-- than the head.
quicksort :: (Ord a) => [a] -> [a]
quicksort []      = []
quicksort (x:xs)  = smaller ++ [x] ++ larger
    where smaller = quicksort [a | a <- xs, a <= x]
          larger  = quicksort [a | a <- xs, a > x]
