import Control.Exception
import Data.Char
import System.IO

-- Use openFile to get a file handle
--main = do
--    handle <- openFile "girlfriend.txt" ReadMode
--    contents <- hGetContents handle
--    putStr contents
--    hClose handle

---- Or, use withFile
--main = do
--    myWithFile "girlfriend.txt" ReadMode (\handle -> do
--        contents <- hGetContents handle
--        putStr contents)

--myWithFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
--myWithFile name mode f = bracket (openFile name mode)
--    (\handle -> hClose handle)
--    (\handle -> f handle)

---- Or, just do it the easy way
--main = do
--    contents <- readFile "girlfriend.txt"
--    putStr contents

-- Convert girlfriend to caps
main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriend.txt" (map toUpper contents)
