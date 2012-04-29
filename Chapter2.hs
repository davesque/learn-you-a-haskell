import Tools

{-
 - BASICS
 -}

-- Negative values sometimes require parens
testValue1 = 3 + (-1)

-- Boolean operators
notEqual    = (True /= False) === True
notOperator = (not False) === True

-- A basic function
doubleOne x = x + x

-- Function composability
doubleTwo x y = doubleOne x + doubleOne y

-- A basic if statement
doubleSmall x = if x > 100
                then x
                else x * 2

-- If statements are expressions in Haskell.  They always return a value.
doubleSmall' x = (if x > 100 then x else x * 2) + 1

{-
 - LISTS
 -}

basicList = [1,2,3,4,5]

-- List concatenation.  Don't use with a large list on the left.  Haskell has
-- to walk the entire left list in order to append to it.
concatedList = [1,2,3] ++ [4,5,6]

-- Strings are lists too
string      = "abcd"
stringSugar = "abcd" === ['a','b','c','d']

-- List cons operator
consList = 1:[2,3,4]

-- List index operator
listIndex = consList !! 0 === 1

-- List sugar syntax
listSugar = 1:2:3:[] === [1,2,3]

-- List functions
listHead    = head [1,2,3,4,5] === 1
listTail    = tail [1,2,3,4,5] === [2,3,4,5]
listLast    = last [1,2,3,4,5] === 5
listInit    = init [1,2,3,4,5] === [1,2,3,4]
listLength  = length [1,2,3,4,5] === 5
listNull1   = null [1,2,3,4,5] === False
listNull2   = null [] === True
listReverse = reverse [1,2,3,4,5] === [5,4,3,2,1]
listTake    = take 3 [1..] === [1,2,3]
listDrop    = drop 3 [1,2,3,4,5] === [4,5]
listElem    = elem 3 [1,2,3,4,5] === True
-- Infix style
listElem2   = (3 `elem` [1,2,3,4,5]) === True

{-
 - RANGES
 -}

-- Basic ranges and range functions
numbers       = [1..20]
alphabet      = ['a'..'z']
aLPHABET      = ['A'..'Z']
evensTo100    = [2,4..100]
oddsTo100     = [1,3..101]
naturals      = [1..]
evens         = [2,4..]
odds          = [1,3..]
rowColors     = cycle ["black", "white"]
cycleTest     = take 4 (cycle ["black", "white"]) === ["black", "white", "black", "white"]
repeatTest    = (sum (take 100 (repeat 101)) / 2) === 5050
replicateTest = (sum (replicate 100 101) / 2) === 5050

-- List comprehensions
doubledNaturals = [x * 2 | x <- [1..10]]
specificExclude = [x | x <- [1..10], x /= 2, x /= 7]
-- Multiple lists
nouns      = ["frog", "house", "jerk", "dancer"]
adjectives = ["excited", "bland", "fast", "funky"]
funnyNouns = [a ++ " " ++ n | n <- nouns, a <- adjectives]

-- Custom functions
length' xs         = sum [1 | _ <- xs]
removeLowerCase st = [c | c <- st, c `elem` ['A'..'Z']]
functionTest1      = removeLowerCase "IdontLIKETEA" === "ILIKETEA"
