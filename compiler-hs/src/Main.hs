import Compiler
import Parser
import System.IO hiding (writeFile)
import System.Environment
import Prelude hiding (writeFile, concat)
import Data.ByteString.Lazy (writeFile, concat)
import Data.Binary (encode)
import Serialize

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if (length args > 1) then do
        txt <- readFile (head args)
        let target = args !! 1
        case parseSrc txt of
            Left err -> putStrLn $ "Error in parsing: " ++ show err
            Right src -> do
                putStrLn "Source code:"
                putStrLn $ showSource src
                case runCompiler src of
                    (Right (), s, w) -> do
                        putStrLn $ "Final State: \n" ++ show s
                        putStrLn $ "\nSCode:\n"
                        mapM_ (putStrLn . show) w
                        writeFile target (concat $ map (encode . serialize) w)
                    (Left errMsg, s, w) -> do
                        putStrLn $ "Error: " ++ errMsg
        else putStrLn "usage: rusc <source> <target>"

