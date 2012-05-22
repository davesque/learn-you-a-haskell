import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randomNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "I'm thinking of a number between 1 and 10...what is it?"
    numberString <- getLine
    when (not $ null numberString) $ do
        let numberList = reads numberString
        if numberList /= [] && randomNumber == (fst . head $ numberList)
            then putStrLn "You are correct!"
            else putStrLn "Try again :P"
        askForNumber newGen