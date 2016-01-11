module Language.RuScript.SCode where

data SCode = SPushL Int
           | SPushG Int
           | SPopG Int
           | SAdd
           | SCallL Int String Int
           | SCallG Int String Int
           | SRet
           | SNew Int Int
           | SPushInt Int
           | SPushStr String
           | SFrameEnd
           | SClass Int Int
           | SPrint
           | SPopL Int
           | SPushA Int
           | SPushSelf
           | SPushAStr String
           | SJumpRelF Int -- Both JUMPs will check the TOS's value to decide action, if False, do it
           | SJumpRel  Int -- Jump now!
           | SNop
           deriving (Show, Eq)


