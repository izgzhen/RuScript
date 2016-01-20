import System.IO hiding (writeFile)
import System.Environment
import Prelude hiding (writeFile, concat)
import Data.ByteString.Lazy (writeFile, concat)
import Data.Binary (encode)

import Language.RuScript.Serialize
import Language.RuScript.Codegen
import Language.RuScript.Parser

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if (length args > 1) then do
        txt <- readFile (head args)
        let target = args !! 1
        case parseProgram txt of
            Left err -> putStrLn $ "Error in parsing: " ++ show err
            Right src -> printCode $ runCodegen src
        else putStrLn "usage: rusc <source> <target>"

