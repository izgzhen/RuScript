module Language.RuScript.Optimize where


import Language.RuScript.ByteCode
import Language.RuScript.Traversal


-- POP after PUSH optimization
optimize :: [ByteCode] -> [ByteCode]
optimize bs@(PUSH i : POP i' : bs')
    | i == i'   = bs'
    | otherwise = bs
optimize bs = bs

