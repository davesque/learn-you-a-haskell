import Data.Char
import Control.Monad

helloWorldAction = do
    putStrLn "Hello, what's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let capitalFirst = (toUpper.head $ firstName) : tail firstName
        capitalLast  = (toUpper.head $ lastName) : tail lastName
    putStrLn $ "Hello, " ++ capitalFirst ++ " " ++ capitalLast ++ "!"

reverseWordsAction = do
    ourWords <- getLine
    if null ourWords
        then return ()
        else do
            putStrLn $ reverseWords ourWords
            reverseWordsAction

reverseWords :: String -> String
reverseWords = unwords . reverse . words

-- Reimplement the putStr function.  Notice how functions that output IO
-- values use recursion as well.
myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
    putChar x
    myPutStr xs

-- Simple use of when.  When is a function that returns the supplied IO action
-- if the Boolean argument is true.  Otherwise, it returns an empty IO action
-- `return ()`.
myPutCharAction = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        myPutCharAction

-- sequence takes a list of IO actions and returns a list of their results.
-- This list can then be bound to an identifier.
mySequenceAction = do
    xs <- sequence [getLine, getLine, getLine]
    print xs

-- mapM makes that a bit more easy
myMapMAction = do
    mapM print [1,2,3,4,5]

-- mapM_ throws away the results
myMapM_Action = do
    mapM_ print [1,2,3,4,5]

-- forM reverses the arguments of mapM
colorNumbersAction = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn ("What color do you associate with the number " ++ show a ++ "?")
        color <- getLine
        return color)
    putStrLn "The colors you associate with 1, 2, 3, and 4 are:"
    mapM_ putStrLn colors

funAction = mapM_ print [1..10]
funAction2 = do
    forM [1..10] print
    return ()

-- IO actions are values like everything else.  Functions like putStrLn should
-- not be thought of as performing an action with a value.  Rather, they take a
-- value and return an IO action that is performed when it either falls into
-- the main function or into a ghci prompt.
