module Serialize where

import Data.Word (Word8, Word32)
import Data.Binary
import Control.Monad
import SCode
import Data.ByteString.UTF8 (fromString)
import Data.ByteString (unpack)
import Data.List.Split (chunksOf)
import Data.Bits (shiftL)

data Segment = Segment Word8 [Word32] deriving Show

inBytes :: Segment -> Word8
inBytes (Segment opcode operands) = fromIntegral (1 + length operands * 4)

instance Binary Segment where
    put seg@(Segment opcode operands) = do
        put $ inBytes seg
        put opcode
        forM_ operands put

    get = do
        bytes <- get :: Get Word8
        opcode <- get :: Get Word8
        operands <- mapM id $ take ((fromIntegral bytes - 1) `div` 4) $ repeat (get :: Get Word32)
        return $ Segment opcode operands

serialize :: SCode -> Segment
serialize (SPushL i)     = segify 0 [i]
serialize (SPushG i)     = segify 1 [i]
serialize (SPopG  i)     = segify 2 [i]
serialize SAdd           = segify 3 []
serialize (SCallL i name narg) = Segment (4 :: Word8) $ map fromIntegral [i, narg] ++ strToWord32Arr name
serialize SRet           = segify 5 []
serialize (SNew   i)     = segify 6 [i]
serialize (SPushInt i)   = segify 7 [i]
serialize (SPushStr str) = Segment (fromIntegral 8) $ strToWord32Arr str
serialize SFrameEnd      = segify 9 []
serialize (SClass i1 i2) = segify 10 [i1, i2]
serialize SPrint         = segify 11 []
serialize (SPopL i)      = segify 12 [i]
serialize (SPushA i)     = segify 14 [i]
serialize SPushSelf      = segify 15 []
serialize (SPushAStr s)  = Segment (fromIntegral 16) $ strToWord32Arr s
serialize (SCallG i name narg) = Segment (17 :: Word8) $ map fromIntegral [i, narg] ++ strToWord32Arr name
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

