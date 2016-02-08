import System.IO hiding (writeFile)
import System.Environment
import Prelude hiding (writeFile, concat)
import Data.ByteString.Lazy (writeFile, concat)
import Data.Binary (encode)
import Control.Monad (when)
import System.Exit (exitFailure)
import System.FilePath ((</>))

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
        let opt = parseOpt $ drop 2 args
        let includeDir = _includeOpt opt
        txt <- readFile (includeDir </> head args)
        let target = includeDir </> (args !! 1)
        case parseProgram txt of
            Left err -> exitError $ "Error in parsing: " ++ show err
            Right program -> do
                program' <- resolveDeps includeDir program
                when (_debugOpt opt) $ print program'
                case checkProgram program' of
                    Left err -> exitError $ "Error in checking: " ++ err
                    Right _  -> do
                        let bytecode = if (_optimizeOpt opt)
                                        then optimize $ runCodegen program'
                                        else runCodegen program'
                        when (_debugOpt opt) $ printCode bytecode
                        writeFile target (concat $ map (encode . serialize) bytecode)
        else putStrLn "usage: rusc <source> <target>"

    where
        exitError s = hPutStrLn stderr s >> exitFailure
