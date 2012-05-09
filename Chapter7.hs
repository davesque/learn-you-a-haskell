import Tools
import qualified Data.List as L
import qualified Data.Function as F
import qualified Data.Char as C
import qualified Data.Map as M

-- Zip an item into a list
testIntersperse = L.intersperse '.' "MONKEY" === "M.O.N.K.E.Y"

-- Take a list and a list of lists, zip in the single list and flatten the result
testIntercalate = L.intercalate "--" ["YO", "BUDDY"] === "YO--BUDDY"

-- Transpose changes columns to rows and vice versa
testTranspose = L.transpose [[1,2,3],[4,5,6],[7,8,9]] === [[1,4,7],[2,5,8],[3,6,9]]

-- Iterate repeatedly applies a function
testIterate = (take 3 $ L.iterate (+2) 1) === [1,3,5]

testTails = L.tails "Hello!" === ["Hello!", "ello!", "llo!", "lo!", "o!", "!", ""]

-- Implement substring search using tails
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack        = foldl checkEquality False subStrings
    where needleSizedString   = take (length needle)
          subStrings          = L.tails haystack
          checkEquality acc t = if needleSizedString t == needle then True else acc

testIsInfixOf  = "asdf" `L.isInfixOf` "Hello! asdf Hello!" === True
testIsPrefixOf = "asdf" `L.isPrefixOf` "asdf Hello! Hello!" === True
testIsSuffixOf = "asdf" `L.isSuffixOf` "Hello! Hello! asdf" === True

-- On is a little confusing :P.  (f `on` g) x y = f (g x) (g y).
testList = [[4,3,2],[1,2],[],[1],[2,5,2,2]]
testOn   = L.sortBy (compare `F.on` length) testList === [[],[1],[1,2],[4,3,2],[2,5,2,2]]
testOn'  = L.sortBy compareLength testList === [[],[1],[1,2],[4,3,2],[2,5,2,2]]
    where compareLength = (\x y -> if length x > length y then GT else if length x == length y then EQ else LT)

-- Simulate a map
phoneBook =
    [("test1", "111-111-1111")
    ,("test2", "222-222-2222")
    ,("test3", "333-333-3333")
    ]

-- Basic find key function
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key map = snd . head . filter (\(k,_) -> key == k) $ map

-- More robust find key function
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' _ []           = Nothing
findKey' key ((k,v):xs) = if key == k
                          then Just v
                          else findKey' key xs

-- Find function defined with a fold
findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key = foldl (\acc (k,v) -> if key == k then Just v else acc) Nothing

-- The proper way to use maps
testMap    = M.fromList phoneBook
testInsert = M.insert "test4" "444-444-4444" testMap
testLookup = M.lookup "test1" testMap === Just "111-111-1111"
testMember = M.member "test1" testMap === True
