import Control.Exception
import Data.Char
import System.IO
import Data.List.Utils

{-
 - A SubRip file is:
 - * A collection of sub rip entries
 -
 - A collection of sub rip entries is:
 - * Sub rip entries in a file separated by a newline character
 -
 - A sub rip entry is:
 - * A number then a newline, then
 - * Two time stamps separated by '-->' then a newline, then
 - * Any number of lines of text ending with newline characters
 -}


{-
 - Splits a list by a certain list element.
 -
 - Taken from: http://www.haskell.org/haskellwiki/Data.List.Split
 -}
-- A list splitted by an certain element is...
splitOn :: Eq a => a -> [a] -> [[a]]
-- If an empty list was provided, a splitted list is an empty list.
splitOn _ [] = []
-- If a list with one or more elements was provided...
splitOn splitter list@(x:xs)
    -- and the first element is the splitter, it's the rest of the list
    -- splitted.
    | splitter == x = splitOn splitter xs
    -- otherwise, return the first part of the list broken over the splitter
    -- consed onto the second half of the list after `splitOn` has been applied
    -- to it.
    | otherwise = let (firstPart,secondPart) = break (==splitter) list
                  in firstPart:(splitOn splitter secondPart)

slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from + 1) . drop from

type SubNumber = Int
data SubTime = SubTime { hour :: Int
                       , minute :: Int
                       , second :: Float
                       } deriving (Show)
type SubText = String

data SubEntry = SubEntry { number :: SubNumber
                         , startTime :: SubTime
                         , endTime :: SubTime
                         , text :: SubText
                         } deriving (Show)

testEntry = ["607"
            ,"01:41:23,377 --> 01:41:27,871"
            ,"<i>because I turned away"
            ,"and didn 't watch them go.</i>"
            ]

getSubNumber :: String -> SubNumber
getSubNumber = read

getSubTime :: String -> SubTime
getSubTime s = SubTime hours minutes seconds
    where items   = split ":" s
          hours   = read $ items !! 0
          minutes = read $ items !! 1
          seconds = read $ replace "," "." (items !! 2)

getSubText :: [String] -> SubText
getSubText = unlines

getSubEntry :: [String] -> SubEntry
getSubEntry xs = SubEntry number startTime endTime text
    where number    = getSubNumber $ xs !! 0
          times     = split " --> " $ xs !! 1
          startTime = getSubTime $ times !! 0
          endTime   = getSubTime $ times !! 1
          text      = getSubText $ drop 2 xs

--  entryFromStrings :: [String] -> SubEntry
--  entryFromStrings [rNumber:rTimes:rText] = SubEntry number startTime endTime text
--      where number = read rNumber
--            startTime = read r

--  getEntries :: String -> [SubEntry]
--  getEntries s = 
--      where fLines = lines s

main = do
    contents <- readFile "never_cry_wolf.srt"
    putStr contents
