module WebAssembly.Encode.Leb128
  ( Uleb128
  , uleb128
  , uleb128Fixed
  , Sleb128
  , sleb128
  , sleb128Fixed
  )
  where

import Data.Bits (Bits (..))
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)

class (Bits a, Integral a) => Leb128 a
instance Leb128 Word
instance Leb128 Word8
instance Leb128 Word16
instance Leb128 Word32
instance Leb128 Word64
instance Leb128 Int
instance Leb128 Int8
instance Leb128 Int16
instance Leb128 Int32
instance Leb128 Int64

least7 :: Leb128 a => a -> Word8
least7 value = fromIntegral value .&. 0b01111111

shift7 :: Leb128 a => a -> a
shift7 value = shiftR value 7

finish :: [Word8] -> [Word8]
finish = \case
  [] -> []
  [byte] -> [byte]
  byte : rest -> (byte .|. 0b10000000) : finish rest

finishFixed :: Int -> [Word8] -> [Word8]
finishFixed width bytes =
  if count > width
    then errorWithoutStackTrace "byte count is larger than width"
    else finish (bytes ++ replicate (width - count) 0)
  where
    count = length bytes

class (Leb128 a, Eq a) => Uleb128 a
instance Uleb128 Word
instance Uleb128 Word8
instance Uleb128 Word16
instance Uleb128 Word32
instance Uleb128 Word64

uleb128Bytes :: Uleb128 a => a -> [Word8]
uleb128Bytes value = byte : rest where
  byte = least7 value
  next = shift7 value
  rest = if next /= 0 then uleb128Bytes next else []

uleb128 :: Uleb128 a => a -> [Word8]
uleb128 = finish . uleb128Bytes

uleb128Fixed :: Uleb128 a => Int -> a -> [Word8]
uleb128Fixed width = finishFixed width . uleb128Bytes

class (Leb128 a, Eq a) => Sleb128 a
instance Sleb128 Int
instance Sleb128 Int8
instance Sleb128 Int16
instance Sleb128 Int32
instance Sleb128 Int64

sleb128Bytes :: Sleb128 a => a -> [Word8]
sleb128Bytes value = byte : rest where
  byte = least7 value
  next = shift7 value
  sign = byte .&. 0b01000000
  finished = next == 0 && sign == 0 || next == -1 && sign /= 0
  rest = if not finished then sleb128Bytes next else []

sleb128 :: Sleb128 a => a -> [Word8]
sleb128 = finish . sleb128Bytes

sleb128Fixed :: Sleb128 a => Int -> a -> [Word8]
sleb128Fixed width = finishFixed width . sleb128Bytes
