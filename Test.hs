-- test script, which should be run by `runghc` under the `rusc` directory

module Test where

import System.FilePath ((</>))
import System.Process
import System.Exit

data Result = NormalStdOut String
            | CompilerErrorOut String

ruscExecutable :: String
ruscExecutable = "rusc/.stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/rusc/rusc"

testsDir :: String
testsDir = "tests"

rvmExecutable :: String
rvmExecutable = "rvm/target/debug/main"

testList :: [(String, String, Result)] -- Name, stdin, expected stdout
testList = [ ("inheritance", "", NormalStdOut "1")
           , ("add",         "", NormalStdOut "3")
           , ("control",     "", NormalStdOut "2")
           , ("nil",         "", NormalStdOut "nil")
           , ("list",        "", NormalStdOut "[1]")
           , ("invoke",      "", NormalStdOut "6")
           , ("mod",         "", NormalStdOut "1")
           , ("vis",         "", CompilerErrorOut "Error in checking: accessing private attribute pri\n") ]

main :: IO ()
main = do
    putStrLn $ "Building artifacts..."
    -- Compile VM
    _ <- readCreateProcess ((shell "cargo build -q") { cwd = Just "rvm" }) ""
    -- Compile Compiler
    _ <- readCreateProcess ((shell "stack build") { cwd = Just "rusc" }) ""

    putStrLn $ "Testing " ++ show (length testList) ++ " cases..."
    flip mapM_ testList $ \(name, stdin, expected) -> do
        let source = name ++ ".rus"
        let binary = name ++ ".rusb"
        -- Compile
        (ecode, out, err) <- readProcessWithExitCode
                                ruscExecutable
                                [source, binary, "-i", testsDir]
                                ""
        case ecode of
            ExitSuccess -> do
                -- Run
                stdout <- readProcess rvmExecutable [testsDir </> binary] stdin
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
