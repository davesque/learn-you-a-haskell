import Data.Char

-- Long version...
--main = do
--    contents <- getContents
--    putStr $ shortLines contents

-- Short version...
--main = interact shortLines

-- Self-contained (and less readable) version...
main = interact $ unlines . filter ((<10) . length) . lines

shortLines :: String -> String
shortLines text = unlines $ filter isShort allLines
    where allLines = lines text
          isShort line = length line < 10