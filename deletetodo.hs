import System.IO
import System.Directory
import Data.List

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                            [0..] todoTasks

    putStrLn "Here are your todo tasks:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine

    let number = read numberString
        newTodoList = unlines $ delete (todoTasks !! number) todoTasks

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoList
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")