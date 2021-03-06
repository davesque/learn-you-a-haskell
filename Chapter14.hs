import Control.Monad.Writer

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

data Gang a = Gang a String deriving (Show, Eq)

instance Monad Gang where
    return x = Gang x ""

    (Gang x oldLog) >>= f = Gang y (oldLog ++ newLog)
        where (Gang y newLog) = f x

-- instance Monad Writer where
--     return x = writer (x, mempty)
--
--     (Writer oldMonoid x) >>= f = writer (y, oldMonoid ++ newMonoid)
--         where (Writer newMonoid y) = f x

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multNumber :: Int -> Int -> Writer [String] Int
multNumber x y = writer (x * y, ["Multiplying " ++ show y ++ " by " ++ show x])

testLogNumber = do
    a <- logNumber 2
    b <- logNumber 3
    logNumber (a * b)

testLogNumber2 = logNumber 2 >>= (\a -> logNumber 3 >>= (\b -> logNumber (a * b)))

testLogNumber3 = do
    logNumber 2
    logNumber 3
    logNumber 4
    tell ["Hello!"]

testLogNumber4 = logNumber 2 >> logNumber 3 >> logNumber 4 >> tell ["Hello!"]

type Stack = [Int]

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

pop :: Stack -> (Int, Stack)
pop (a:xs) = (a, xs)

stackStuff :: Stack -> (Int, Stack)
stackStuff stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2)  = pop newStack1
    in pop newStack2
