module Serialize where

import Data.Int
import Data.Binary
import Control.Monad

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
        return $ Segment opcode(operands
