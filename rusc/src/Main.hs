import System.IO hiding (writeFile)
import System.Environment
import Prelude hiding (writeFile, concat)
import Data.ByteString.Lazy (writeFile, concat)
import Data.Binary (encode)
import Control.Monad (when)

import Language.RuScript.Serialize
import Language.RuScript.Codegen
import Language.RuScript.Parser
import Language.RuScript.StaticCheck
import Language.RuScript.Option

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if (length args > 1) then do
        txt <- readFile (head args)
        let opt = parseOpt $ tail args
        let target = args !! 1
        case parseProgram txt of
            Left err -> putStrLn $ "Error in parsing: " ++ show err
            Right program -> do
                case checkProgram program of
                    Left err -> putStrLn err
                    Right _  -> do
                        let bytecode = runCodegen program
                        when (_debugOpt opt) $ printCode bytecode
                        writeFile target (concat $ map (encode . serialize) bytecode)
        else putStrLn "usage: rusc <source> <target>"

