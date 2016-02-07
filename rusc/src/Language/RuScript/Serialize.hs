module Language.RuScript.Serialize where

import Language.RuScript.ByteCode

-- import Data.Word (Word8, Word32)
import Data.Binary
import Control.Monad
import Data.ByteString.UTF8 (fromString)
import Data.ByteString (unpack)
import Data.List.Split (chunksOf)
import Data.Bits (shiftL)

data Segment = Segment Word8 [Word32] deriving Show

-- 32 bit per operand
inBytes :: Segment -> Word8
inBytes (Segment _ operands) = fromIntegral (1 + length operands * 4)

instance Binary Segment where
    put seg@(Segment opcode operands) = do
        put $ inBytes seg   -- One byte: length of following payload in bytes
        put opcode          -- One byte: opcode
        forM_ operands put  -- 4 * number of operands bytes

    get = do
        bytes <- get :: Get Word8
        opcode <- get :: Get Word8
        operands <- mapM id $ take ((fromIntegral bytes - 1) `div` 4) $ repeat (get :: Get Word32)
        return $ Segment opcode operands

serialize :: ByteCode -> Segment
serialize (CALL i)          = segify 0 [i]
serialize (INVOKE s)        = Segment (fromIntegral 1) $ strToWord32Arr s
serialize RET               = segify 2 []
serialize (JUMP (Left i))   = segify 3 [i]
serialize (JUMPT (Left i))  = segify 4 [i]
serialize (JUMPF (Left i))  = segify 5 [i]
serialize (PUSH i)          = segify 6 [i]
serialize (POP i)           = segify 7 [i]
serialize (NEW i)           = segify 8 [i]
serialize (PUSHA s)         = Segment (fromIntegral 9) $ strToWord32Arr s
serialize (POPA s)          = Segment (fromIntegral 10) $ strToWord32Arr s
serialize (PUSHSTR s)       = Segment (fromIntegral 11) $ strToWord32Arr s
serialize (PUSHINT i)       = segify 12 [i]
serialize (PUSHBOOL i)      = segify 13 [i]
serialize (CLASS i1 i2 i3)  = segify 14 [i1, i2, i3]
serialize SFUNC             = segify 15 []
serialize (EBODY i)         = segify 16 [i]
serialize PUSHLIST          = segify 17 []
serialize other             = error $ "can't serialize" ++ show other

-- serialize x = error $ "unimplelemented serialization of: " ++ show x

segify :: Int -> [Int] -> Segment
segify i is = Segment (fromIntegral i) (map fromIntegral is)

-- UTF-8 encoding
strToWord32Arr :: String -> [Word32]
strToWord32Arr str = map (f . reverse) . chunksOf 4 $ unpack (fromString str)
    where
        f :: [Word8] -> Word32
        f [] = 0
        f (l:hs) = fromIntegral l + f hs `shiftL` 8

