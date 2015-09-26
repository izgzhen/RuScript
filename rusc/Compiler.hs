module Compiler where

import Parser
import Control.Monad.RWS
import Control.Monad.Except

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

type Config = ()
type State = ()

type Compiler = ExceptT String (RWS Config [SCode] State)

compile :: Source -> Compiler ()
compile = undefined


