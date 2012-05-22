import System.Environment
import System.Directory
import System.IO
import Control.Exception
import qualified Data.ByteString.Lazy as B

main = do
    (sourceFile:destFile:_) <- getArgs
    copy sourceFile destFile

copy :: String -> String -> IO ()
copy source dest = do
    contents <- B.readFile source

    bracketOnError
        (openTempFile "." "temp")

        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)

        (\(tempName, tempHandle) -> do
            B.hPutStr tempHandle contents
            hClose tempHandle
            renameFile tempName dest)