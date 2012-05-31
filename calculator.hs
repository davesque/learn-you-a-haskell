import System.IO
import Data.List

parseRPN :: String -> Double
parseRPN = head . foldl foldingFunction [] . words
    where foldingFunction (y:x:xs) "+" = (x + y):xs
          foldingFunction (y:x:xs) "-" = (x - y):xs
          foldingFunction (y:x:xs) "*" = (x * y):xs
          foldingFunction (y:x:xs) "/" = (x / y):xs
          foldingFunction (y:x:xs) "^" = (x ** y):xs
          foldingFunction (x:xs) "ln"  = (log x):xs
          foldingFunction xs "sum"     = [sum xs]
          foldingFunction xs numberString = (read numberString):xs
