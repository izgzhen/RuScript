import System.IO hiding (writeFile)
import System.Environment
import Prelude hiding (writeFile, concat)
import Data.ByteString.Lazy (writeFile, concat)
import Data.Binary (encode)
import Control.Monad (when)
import System.Exit (exitFailure)

import Language.RuScript.Serialize
import Language.RuScript.Codegen
import Language.RuScript.Parser
import Language.RuScript.StaticCheck
import Language.RuScript.Option
import Language.RuScript.Optimize
import Language.RuScript.Import

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if (length args > 1) then do
        txt <- readFile (head args)
        let opt = parseOpt $ drop 2 args
        let target = args !! 1
        case parseProgram txt of
            Left err -> exitError $ "Error in parsing: " ++ show err
            Right program -> do
                case checkProgram program of
                    Left err -> exitError $ "Error in checking: " ++ err
                    Right _  -> do
                        program' <- resolveDeps program
                        let bytecode = if (_optimizeOpt opt)
                                        then optimize $ runCodegen program'
                                        else runCodegen program'
                        when (_debugOpt opt) $ printCode bytecode
                        writeFile target (concat $ map (encode . serialize) bytecode)
        else putStrLn "usage: rusc <source> <target>"

    where
        exitError s = hPutStrLn stderr s >> exitFailure
