import Tools

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

-- Lists
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
