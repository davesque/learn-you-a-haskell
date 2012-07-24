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
