-- test script, which should be run by `runghc` under the `rusc` directory

module Test where

import System.FilePath ((</>))
import System.Process
import System.Exit

data Result = NormalStdOut String
            | CompilerErrorOut String

ruscExecutable :: String
ruscExecutable = "./.stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/rusc/rusc"

demoDir :: String
demoDir = "examples"

rvmExecutable :: String
rvmExecutable = "../target/debug/main"

testList :: [(String, String, Result)] -- Name, stdin, expected stdout
testList = [ ("inheritance", "", NormalStdOut "1")
           , ("add",         "", NormalStdOut "3")
           , ("control",     "", NormalStdOut "2")
           , ("vis",         "", CompilerErrorOut "Error in checking: accessing private attribute pri\n")
           , ("nil",         "", NormalStdOut "nil")
           , ("list",        "", NormalStdOut "[1]")
           , ("invoke",      "", NormalStdOut "6") ]

main = do
    putStrLn $ "Building artifacts..."
    -- Compile VM
    callProcess "cargo" ["build", "-q"]
    -- Compile Compiler
    callProcess "stack" ["build"]

    putStrLn $ "Testing " ++ show (length testList) ++ " cases..."
    flip mapM_ testList $ \(name, stdin, expected) -> do
        let source = demoDir </> name ++ ".rus"
        let binary = demoDir </> name ++ ".rusb"
        -- Compile
        (ecode, out, err) <- readProcessWithExitCode ruscExecutable [source, binary] ""
        case ecode of
            ExitSuccess -> do
                -- Run
                stdout <- readProcess rvmExecutable [binary] stdin
                case expected of
                    NormalStdOut eout ->
                        if eout == stdout
                            then putStrLn $ "+ Test passed: " ++ name
                            else putStrLn $ "+ Test failed: " ++ name ++
                                            ", because \"" ++ stdout ++
                                            "\" is not the expected \"" ++ eout ++ "\""
                    CompilerErrorOut s ->
                        putStrLn $ "+ Test failed: " ++ name ++
                                   ", because failure \"" ++ s ++ "\" is expected"
            ExitFailure i -> case expected of
                CompilerErrorOut s ->
                    if s == err
                        then putStrLn $ "+ Test passed: " ++ name
                        else putStrLn $ "+ Test failed: " ++ name ++
                                        ", because \"" ++ err ++
                                        "\" is not the expected failure \"" ++ s ++ "\""
                _ -> putStrLn $ "+ Test failed: " ++ name ++
                                ", because it is expected to success, but fail with code \"" ++
                                show i ++ ": " ++ err ++ "\""
