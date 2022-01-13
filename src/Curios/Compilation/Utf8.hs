{-# LANGUAGE BinaryLiterals #-}

module Curios.Compilation.Utf8
  ( utf8
  )
  where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.Word (Word8)

least6 :: Int -> Word8
least6 value =
  fromIntegral value .&. 0b00111111

encode :: Char -> [Word8]
encode character =
  if value <= 0x7F then
    [ fromIntegral value
    ]
  else if value <= 0x7FF then
    [ fromIntegral (shiftR value 6) .|. 0b11000000
    , least6 value .|. 0b10000000
    ]
  else if value <= 0xFFFF then
    [ fromIntegral (shiftR value 12) .|. 0b11100000
    , least6 (shiftR value 6) .|. 0b10000000
    , least6 value .|. 0b10000000
    ]
  else if value <= 0x10FFFF then
    [ fromIntegral (shiftR value 18) .|. 0b11110000
    , least6 (shiftR value 12) .|. 0b10000000
    , least6 (shiftR value 6) .|. 0b10000000
    , least6 value .|. 0b10000000
    ]
  else
    error "character can't be represented in utf-8"
  where
    value = fromEnum character

utf8 :: String -> [Word8]
utf8 =
  concatMap encode
