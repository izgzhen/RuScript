module Compiler where

-- Bytecode compiler for ruscript language

data SCode = SPush Int
           | SAdd
           | SCall Int String Int
           | SRet
           | SNew Int
           | SPushInt Int
           | SPushStr String
           | SFrameStart
           | SFrameEnd Int
           | SClass Int Int
           deriving (Show, Eq)

serialize :: [SCode] -> ByteString

compile :: Source -> Except String [SCode]
compile = undefined

