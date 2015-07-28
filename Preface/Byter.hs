
module Preface.Byter
  ( wordsToBytes
  , bytesToWords
  , longsToBytes
  , bytesToLongs
  , swapEndian32
  , swapEndian64
) where

import Preface.Imports

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

-- import Foreign.Ptr (castPtr, plusPtr)
-- import Foreign.ForeignPtr (withForeignPtr)
-- import Foreign.Marshal.Array
-- import System.IO.Unsafe
-- import Foreign.Storable
-- import Data.Word
-- import Data.Bits

-- | An array of Word32 is copied byte by byte into a ByteString
wordsToBytes :: [Word32] -> B.ByteString
wordsToBytes a = let n = length a
                     d = undefined :: Word32
                  in B.unsafeCreate (sizeOf d * n) $ \p -> pokeArray (castPtr p) a

-- | A ByteString is copied byte for byte where every 4 bytes is treated as a Word32
bytesToWords :: B.ByteString -> [Word32]
bytesToWords a = let (p, o, n) = B.toForeignPtr a
                     d = undefined :: Word32
                     c = n `div` (sizeOf d) :: Int
                  in unsafeDupablePerformIO $ withForeignPtr p $ \px -> peekArray c (plusPtr px o)

-- | An array of Word64 is copied byte by byte into a ByteString
longsToBytes :: [Word64] -> B.ByteString
longsToBytes a = let n = length a
                     d = undefined :: Word64
                  in B.unsafeCreate (sizeOf d * n) $ \p -> pokeArray (castPtr p) a

-- | A ByteString is copied byte for byte where every 8 bytes is treated as a Word64
bytesToLongs :: B.ByteString -> [Word64]
bytesToLongs a = let (p, o, n) = B.toForeignPtr a
                     d = undefined :: Word64
                     c = n `div` (sizeOf d) :: Int
                  in unsafeDupablePerformIO $ withForeignPtr p $ \px -> peekArray c (plusPtr px o)

-- | A Word32 has its bytes swapped resulting in the Word32 which would have been
-- represented by the opposite endian-ness.
swapEndian32 :: Word32 -> Word32
swapEndian32 x  =  (shiftR x 24) .|. (shiftL x 24) 
                   .|. (shiftR (x .&. 0x00FF0000) 8) .|. (shiftL (x .&. 0x0000FF00) 8)
-- | A Word64 has its bytes swapped resulting in the Word64 which would have been
-- represented by the opposite endian-ness.
swapEndian64 :: Word64 -> Word64
swapEndian64 x = (shiftR x 56) .|. ((x .&. 0x00FF000000000000) `shiftR` 40)
   .|. ((x .&. 0x0000FF0000000000) `shiftR` 24) .|. ((x .&. 0x000000FF00000000) `shiftR` 8)
   .|. ((x .&. 0x00000000FF000000) `shiftL` 8) .|. ((x .&. 0x0000000000FF0000) `shiftL` 24)
   .|. ((x .&. 0x000000000000FF00) `shiftL` 40) .|. (shiftL x 56)

