module Serialize where

import Data.Int
import Data.Binary
import Control.Monad
import Compiler(SCode(..))

data Segment = Segment Int8 [Int32]

inBytes :: Segment -> Int8
inBytes (Segment opcode operands) = fromIntegral $ 1 + length operands * 4

instance Binary Segment where
    put seg@(Segment opcode operands) = do
        put $ inBytes seg
        put opcode
        forM_ operands put

    get = do
        bytes <- get :: Get Int8
        opcode <- get :: Get Int8
        operands <- mapM id $ take ((fromIntegral bytes - 1) `div` 4) $ repeat (get :: Get Int32)
        return $ Segment opcode operands

serialize :: SCode -> Segment
serialize (SPushL i) = segify 0 [i]
serialize (SPushG i) = segify 1 [i]
serialize (SPopG  i) = segify 2 [i]
serialize SAdd       = segify 3 []
-- serialize SCall
serialize SRet       = segify 5 []
serialize (SNew   i) = segify 6 [i]
serialize (SPushInt i)   = segify 7 [i]
-- serialize (SPushStr)
serialize SFrameEnd      = segify 9 []
serialize (SClass i1 i2) = segify 10 [i1, i2]
serialize SPrint         = segify 11 []
serialize x = error $ "unimplelemented serialization of: " ++ show x


segify :: Int -> [Int] -> Segment
segify i is = Segment (fromIntegral i) (map fromIntegral is)