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
take'' _ [] = []
take'' n _
    | n <= 0 = []
take'' n (x:xs) = x:(take'' (n - 1) xs)
