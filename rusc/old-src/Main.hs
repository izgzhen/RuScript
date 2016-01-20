import System.IO hiding (writeFile)
import System.Environment
import Prelude hiding (writeFile, concat)
import Data.ByteString.Lazy (writeFile, concat)
import Data.Binary (encode)

import Language.RuScript.Serialize
import Language.RuScript.Compiler
import Language.RuScript.Parser
import Language.RuScript.AST (showSource)

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
                    (Right (), s) -> do
                        putStrLn $ "\nSCode:\n"
                        mapM_ (putStrLn . show) $ emittedCode s
                        writeFile target (concat $ map (encode . serialize) $ emittedCode s)
                    (Left errMsg, s) -> do
                        putStrLn $ "Error: " ++ errMsg
        else putStrLn "usage: rusc <source> <target>"

