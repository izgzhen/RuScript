module SCode where

data SCode = SPushL Int
           | SPushG Int
           | SPopG Int
           | SAdd
           | SCallL Int String Int
           | SCallG Int String Int
           | SRet
           | SNew Int
           | SPushInt Int
           | SPushStr String
           | SFrameEnd
           | SClass Int Int
           | SPrint
           | SPopL Int
           | SPopA Int
           | SPushA Int
           | SPushSelf
           | SPushAStr String
           deriving (Show, Eq)


