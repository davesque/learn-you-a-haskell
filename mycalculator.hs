import System.IO
import Data.List

--main = do
--    stuff <- getLine
--    putStrLn stuff
--    main

data Op = Add | Subtract | Multiply deriving (Show, Eq)
data (Num a) => RPNTree a = Value a | Node (RPNTree a) (RPNTree a) Op deriving (Show, Eq)

evaluateTree :: (Num a) => RPNTree a -> a
evaluateTree (Value a) = a
evaluateTree (Node left right op) = evaluateOp (evaluateTree left) (evaluateTree right) op

evaluateOp :: (Num a) => a -> a -> Op -> a
evaluateOp a b op
    | op == Add      = a + b
    | op == Subtract = a - b
    | op == Multiply = a * b

testString = "3 4 +"
testString2 = "3 4 + 4 *"
testString3 = "3 4 + 4 2 * *"

testParseItems = ["+","4","3"]
testParseItems2 = ["*","4","+","4","3"]
testParseItems3 = ["*","*","2","4","+","4","3"]

testTree = Node (Value 3) (Value 4) Add
testTree2 = Node (Node (Value 3) (Value 4) Add) (Value 4) Multiply
testTree3 = Node (Node (Value 3) (Value 4) Add) (Node (Value 4) (Value 2) Multiply) Multiply

parseTree :: (Num a) => [String] -> (RPNTree a, [String])
parseTree [v] = (Value (readNumber v), [])
parseTree (thisItem:rest)
    | thisOpResult /= Nothing     = let (left, leftover) = parseTree rest
                                        (right, end) = parseTree leftover
                                    in Node left right thisOpResult
    | thisNumberResult /= Nothing = Value thisNumberResult
    where thisOpResult     = readOp thisItem
          thisNumberResult = readNumber thisItem

readOp :: String -> Maybe Op
readOp "+" = Just Add
readOp "-" = Just Subtract
readOp "*" = Just Multiply
readOp _   = Nothing

readNumber :: String -> Maybe Int
readNumber s
    | parsedNumber == [] = Nothing
    | otherwise          = Just (fst . head $ parsedNumber)
    where parsedNumber   = reads s