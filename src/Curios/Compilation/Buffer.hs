module Curios.Compilation.Buffer
  ( Buffer (..)
  , byte
  , unsigned
  , unsignedFixed
  , signed
  , signedFixed
  , floatingSingle
  , floatingDouble
  , prependSize
  )
  where

import Data.Word (Word8, Word32)
import Data.ByteString.Builder (Builder, word8, word32LE, word64LE)

import Curios.Compilation.Leb128
  ( Uleb128
  , Sleb128
  , uleb128
  , uleb128Fixed
  , sleb128
  , sleb128Fixed
  )

import Curios.Compilation.Ieee754
  ( ieee754Single
  , ieee754Double
  )

data Buffer =
  Buffer Word32 Builder

instance Semigroup Buffer where
  Buffer size builder <> Buffer size' builder' =
    Buffer (size + size') (builder <> builder')

instance Monoid Buffer where
  mempty = Buffer 0 mempty

byte :: Word8 -> Buffer
byte value =
  Buffer 1 (word8 value)

unsigned :: Uleb128 a => a -> Buffer
unsigned value =
  mconcat (map byte $ uleb128 value)

unsignedFixed :: Uleb128 a => Int -> a -> Buffer
unsignedFixed size value =
  mconcat (map byte $ uleb128Fixed size value)

signed :: Sleb128 a => a -> Buffer
signed value =
  mconcat (map byte $ sleb128 value)

signedFixed :: Sleb128 a => Int -> a -> Buffer
signedFixed size value =
  mconcat (map byte $ sleb128Fixed size value)

floatingSingle :: Float -> Buffer
floatingSingle value =
  Buffer 4 (word32LE $ ieee754Single value)

floatingDouble :: Double -> Buffer
floatingDouble value =
  Buffer 8 (word64LE $ ieee754Double value)

prependSize :: Buffer -> Buffer
prependSize buffer =
  let Buffer size _ = buffer in unsigned size <> buffer
