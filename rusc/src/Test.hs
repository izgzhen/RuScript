module Test where

import System.FilePath ((</>))
import System.Process (readProcess, callProcess)

ruscExecutable :: String
ruscExecutable = "./.stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/rusc/rusc"

demoDir :: String
demoDir = "examples"

rvmExecutable :: String
rvmExecutable = "../target/debug/main"

testList :: [(String, String, String)] -- Name, stdin, expected stdout
testList = [("inheritance", "", "2")]

main = do
    putStrLn $ "Build artifacts..."
    -- Compile VM
    callProcess "cargo" ["build", "-q"]
    -- Compile Compiler
    callProcess "stack" ["build"]

    putStrLn $ "Testing " ++ show (length testList) ++ " cases..."
    flip mapM_ testList $ \(name, stdin, expected) -> do
        let source = demoDir </> name ++ ".rus"
        let binary = demoDir </> name ++ ".rusb"
        -- Compile
        callProcess ruscExecutable [source, binary]
        -- Run
        stdout <- readProcess rvmExecutable [binary] stdin
        if stdout == expected
            then putStrLn $ "+ Test passed: " ++ name
            else putStrLn $ "+ Test failed: " ++ name ++
                            ", because " ++ stdout ++
                            " is not expected " ++ expected
