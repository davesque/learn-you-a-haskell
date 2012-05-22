import Control.Exception
import System.Environment
import System.Directory
import System.IO
import Data.List

main = do
    (command:argList) <- getArgs
    dispatch command argList

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

add :: [String] -> IO ()
add [fileName, todoItem] = do
    appendFile fileName (todoItem ++ "\n")
    putStrLn $ "Added item `" ++ todoItem ++ "`"

view :: [String] -> IO ()
view [] = return ()
view [fileName] = do
    contents <- readFile fileName

    let todoTasks = lines contents
        numberedTasks = zipWith (\n task -> show n ++ " - " ++ task)
                            [0..] todoTasks

    putStr "\n"
    putStrLn "List contents:"
    putStr $ unlines numberedTasks
    putStr "\n"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName

    let todoTasks = lines contents
        number = read numberString
        deletedItem = todoTasks !! number
        newTodoList = unlines $ delete (todoTasks !! number) todoTasks

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoList
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)

    newContents <- readFile fileName

    let todoTasks = lines newContents
        numberedTasks = zipWith (\n task -> show n ++ " - " ++ task)
                            [0..] todoTasks

    putStr "\n"
    putStrLn $ "Deleted item #" ++ show number ++ " (" ++ deletedItem ++ ")\n"
    putStrLn "New list contents:"
    putStr $ unlines numberedTasks
    putStr "\n"
