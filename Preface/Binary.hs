{-# LANGUAGE MagicHash #-}

module Preface.Binary (
    getWord32be, getWord16be, putWord32be, putWord16be
  , getWord64be, putWord64be
  , roll, unroll
  , lowLevel
) where

import Preface.Imports
import Preface.Stringy
import Unsafe.Coerce (unsafeCoerce)

getWord64be :: Int -> ByteString -> Int64
getWord64be n s = do (fromIntegral (nth s n) `shiftL` 56) .|.
                      (fromIntegral (nth s (n+1)) `shiftL` 48) .|.
                      (fromIntegral (nth s (n+2)) `shiftL` 40) .|.
                      (fromIntegral (nth s (n+3)) `shiftL` 32) .|.
                      (fromIntegral (nth s (n+4)) `shiftL` 24) .|.
                      (fromIntegral (nth s (n+5)) `shiftL` 16) .|.
                      (fromIntegral (nth s (n+6)) `shiftL`  8) .|.
                      (fromIntegral (nth s (n+7)) )

getWord32be :: Int -> ByteString -> Int32
getWord32be n s = (fromIntegral (nth s n) `shiftL` 24) .|.
              (fromIntegral (nth s (n+1) ) `shiftL` 16) .|.
              (fromIntegral (nth s (n+2) ) `shiftL`  8) .|.
              (fromIntegral (nth s (n+3) ))

getWord16be :: Int -> ByteString -> Int16
getWord16be n s = (fromIntegral (nth s n) `shiftL` 8) .|.
              (fromIntegral (nth s (n+1) ))


putWord64be :: Int64 -> ByteString
putWord64be w = pack [(fromIntegral (shiftR w 56) ) ,
                      (fromIntegral (shiftR w 48) ) ,
                      (fromIntegral (shiftR w 40) ) ,
                      (fromIntegral (shiftR w 32) ) ,
                      (fromIntegral (shiftR w 24) ) ,
                      (fromIntegral (shiftR w 16) ) ,
                      (fromIntegral (shiftR w  8) ) ,
                      (fromIntegral w) ]

putWord32be :: Int32 -> ByteString
putWord32be w = pack [(fromIntegral (shiftR w 24) ) ,
                      (fromIntegral (shiftR w 16) ) ,
                      (fromIntegral (shiftR w  8) ) ,
                      (fromIntegral w) ]

putWord16be :: Int16 -> ByteString
putWord16be w = pack [(fromIntegral (shiftR w 8)), fromIntegral w]

unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step (-1) = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

-- let t x = x == (roll . unroll) x
-- let t = ap (==) (roll . unroll)
-- Q.quickCheck t
--
--
-- newtype Positive = Positive Integer deriving Show
-- instance Q.Arbitrary Positive  where arbitrary = fmap (Positive . abs) Q.arbitrary 
-- let t (Positive x) = x == (roll . unroll) x
-- Q.quickCheck t
--
--
--
lowLevel :: Int32 -> ByteString
lowLevel h = unsafePerformIO $ do 
  p <- mallocByteString 4
  withForeignPtr p (\pp -> poke (castPtr pp) (byteSwap32 (unsafeCoerce h)))
  return (fromForeignPtr p 0 4)

-- import Data.Binary.Put 
-- timings' (map (\x -> runPut (Data.Binary.Put.putWord32be x)) ) (map (lowLevel . fromIntegral) ) =<< randomList 1000 1 1000000000

