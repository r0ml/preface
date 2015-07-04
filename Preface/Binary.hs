
module Preface.Binary (
  getWord32be, getWord16be, putWord32be, putWord16be
) where

import Data.ByteString (ByteString, pack, index)
import Data.Bits (shiftL, shiftR, (.|.) )
import Data.Int (Int32, Int16)
import GHC.Word

nth :: ByteString -> Int -> Word8
nth = index

getWord32be :: Int -> ByteString -> Int32
getWord32be n s = (fromIntegral (nth s n) `shiftL` 24) .|.
              (fromIntegral (nth s (n+1) ) `shiftL` 16) .|.
              (fromIntegral (nth s (n+2) ) `shiftL`  8) .|.
              (fromIntegral (nth s (n+3) ))

getWord16be :: Int -> ByteString -> Int16
getWord16be n s = (fromIntegral (nth s n) `shiftL` 8) .|.
              (fromIntegral (nth s (n+1) ))

putWord32be :: Int32 -> ByteString
putWord32be w = pack [(fromIntegral (shiftR w 24) ) ,
                      (fromIntegral (shiftR w 16) ) ,
                      (fromIntegral (shiftR w  8) ) ,
                      (fromIntegral w) ]

putWord16be :: Int16 -> ByteString
putWord16be w = pack [(fromIntegral (shiftR w 8)), fromIntegral w]


