import Data.List
import Control.Monad

main = do
    contents <- getContents
    if allArePalindromes $ lines contents
    then putStrLn "All are palindromes."
    else putStrLn "Not all are palindromes."

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

allArePalindromes :: [String] -> Bool
allArePalindromes xs = and $ map isPalindrome xs