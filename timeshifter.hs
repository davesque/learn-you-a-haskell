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

import Control.Exception
import Data.Char
import Data.List.Utils
import System.Directory
import System.Environment
import System.IO
import Text.Printf

{-
 - Data Types
 -}
type SubNumber = Int
data SubTime = SubTime { hour :: Int
                       , minute :: Int
                       , second :: Float
                       }
type SubText = String
data SubEntry = SubEntry { number :: SubNumber
                         , startTime :: SubTime
                         , endTime :: SubTime
                         , text :: SubText
                         }

{-
 - Class Instances
 -}
instance Show SubTime where
    show (SubTime hour minute second) =
        fHour   ++ ":" ++
        fMinute ++ ":" ++
        replace "." "," fSecond
        where fHour   = printf "%02d" hour
              fMinute = printf "%02d" minute
              fSecond = printf "%06.3f" second

instance Show SubEntry where
    show (SubEntry number startTime endTime text) =
        fNumber ++ "\n" ++
        fTime   ++ "\n" ++
        text    ++ "\n"
        where fNumber = show number
              fTime   = show startTime ++ " --> " ++ show endTime

slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from + 1) . drop from

strip :: (Eq a) => a -> [a] -> [a]
strip _ [] = []
strip x (y:ys)
    | x == y    = strip x ys
    | otherwise = y:strip x ys

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

shiftSubTime :: Float -> SubTime -> SubTime
shiftSubTime interval (SubTime hours minutes seconds) =
    SubTime hours minutes (seconds + interval)

shiftSubEntry :: Float -> SubEntry -> SubEntry
shiftSubEntry interval (SubEntry number startTime endTime text) =
    SubEntry number newStartTime newEndTime text
    where newStartTime = shiftSubTime interval startTime
          newEndTime   = shiftSubTime interval endTime

shiftEntries :: Float -> String -> IO ()
shiftEntries interval filename = do
    contents <- readFile filename

    let rawLines        = lines contents
        cleanLines      = map (replace "\r" "") rawLines
        rawLineGroups   = split [""] cleanLines
        cleanLineGroups = strip [] rawLineGroups
        entries         = map getSubEntry cleanLineGroups
        newEntries      = map (shiftSubEntry interval) entries

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            mapM_ (hPutStr tempHandle . show) newEntries
            hClose tempHandle
            removeFile filename
            renameFile tempName filename)

    putStrLn "Done!"

printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "timeshifter interval filename"

dispatch :: [String] -> IO ()
dispatch (interval:filename:[]) = shiftEntries (read interval) filename
dispatch _ = printUsage

main = do
    args <- getArgs
    dispatch args
