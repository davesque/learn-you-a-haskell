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
