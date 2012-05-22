import System.Environment

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    putStrLn $ show args
    putStrLn "The program name is:"
    putStrLn progName