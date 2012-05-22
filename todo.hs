import Control.Exception
import System.Environment
import System.Directory
import System.IO
import Data.List

main = do
    args <- getArgs

    let command = if length args > 0
                  then Just $ head args
                  else Nothing
        argList = if length args > 0
                  then Just $ tail args
                  else Nothing

    dispatch command argList

dispatch :: Maybe String -> Maybe [String] -> IO ()
dispatch (Just "add") = add
dispatch (Just "view") = view
dispatch (Just "remove") = remove
dispatch _ = printUsage

printUsage :: Maybe [String] -> IO ()
printUsage _ = do
    putStrLn "Usage:"
    putStrLn "todo add|view|remove filename [content|index]"

add :: Maybe [String] -> IO ()
add (Just [fileName, todoItem]) = do
    appendFile fileName (todoItem ++ "\n")
    putStrLn $ "Added item `" ++ todoItem ++ "`"
add _ = printUsage Nothing

view :: Maybe [String] -> IO ()
view (Just [fileName]) = do
    contents <- readFile fileName

    let todoTasks = lines contents
        numberedTasks = zipWith (\n task -> show n ++ " - " ++ task)
                            [0..] todoTasks

    putStr "\n"
    putStrLn "List contents:"
    putStr $ unlines numberedTasks
    putStr "\n"
view _ = printUsage Nothing

remove :: Maybe [String] -> IO ()
remove (Just [fileName, numberString]) = do
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
remove _ = printUsage Nothing
