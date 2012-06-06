import Control.Monad.Writer

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

data Gang a = Gang a String deriving (Show, Eq)

instance Monad Gang where
    return x = Gang x ""

    (Gang x oldLog) >>= f = Gang y (oldLog ++ newLog)
        where (Gang y newLog) = f x

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

testLogNumber = do
    a <- logNumber 2
    b <- logNumber 3
    return (a * b)

data MultWriter m = MultWriter [String] m

instance Monad MultWriter where
    return x = MultWriter x

    (MultWriter oldLog x) >>= f = MultWriter (oldLog ++ newLog) (x * y)
        where (MultWriter newLog y) = f x
