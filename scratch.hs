import Data.List

{-
 - Splits a list by a certain list element.
 -
 - Taken from: http://www.haskell.org/haskellwiki/Data.List.Split
 -}
-- A list splitted by an certain element is...
splitOn :: Eq a => a -> [a] -> [[a]]
-- If an empty list was provided, a splitted list is an empty list.
splitOn _ [] = []
-- If a list with one or more elements was provided...
splitOn splitter list@(x:xs)
    -- and the first element is the splitter, it's the rest of the list
    -- splitted.
    | splitter == x = splitOn splitter xs
    -- otherwise, return the first part of the list broken over the splitter
    -- consed onto the second half of the list after `splitOn` has been applied
    -- to it.
    | otherwise = let (firstPart,secondPart) = break (==splitter) list
                  in firstPart:(splitOn splitter secondPart)

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == head xs = compress xs
    | otherwise    = x:compress xs

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs) = reverse $ foldl (\acc x -> if x == head acc then acc else x:acc) [x] xs

compress'' :: (Eq a) => [a] -> [a]
compress'' = (map head) . group
