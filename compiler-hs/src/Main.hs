import Compiler
import Parser
import System.IO
import System.Environment

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if (length args > 0) then do
        txt <- readFile (head args)
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
                    (Left errMsg, s, w) -> do
                        putStrLn $ "Error: " ++ errMsg
        else putStrLn "usage: rusc <path>"

