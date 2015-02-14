
module Byter
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array
import System.IO.Unsafe
import Foreign.Storable
import Data.Word
import Data.Bits

wordsToBytes :: [Word32] -> B.ByteString
wordsToBytes a = let n = length a
                     d = undefined :: Word32
                  in B.unsafeCreate (sizeOf d * n) $ \p -> pokeArray (castPtr p) a

bytesToWords :: B.ByteString -> [Word32]
bytesToWords a = let (p, o, n) = B.toForeignPtr a
                     d = undefined :: Word32
                     c = n `div` (sizeOf d) :: Int
                     r = unsafeDupablePerformIO $ withForeignPtr p $ \px -> peekArray c (plusPtr px o)
                 in r




longsToBytes :: [Word64] -> B.ByteString
longsToBytes a = let n = length a
                     d = undefined :: Word64
                  in B.unsafeCreate (sizeOf d * n) $ \p -> pokeArray (castPtr p) a

bytesToLongs :: B.ByteString -> [Word64]
bytesToLongs a = let (p, o, n) = B.toForeignPtr a
                     d = undefined :: Word64
                     c = n `div` (sizeOf d) :: Int
                     r = unsafeDupablePerformIO $ withForeignPtr p $ \px -> peekArray c (plusPtr px o)
                 in r

swapEndian32 :: Word32 -> Word32
swapEndian32 x  =  (shiftR x 24) .|. (shiftL x 24) 
                   .|. (shiftR (x .&. 0x00FF0000) 8) .|. (shiftL (x .&. 0x0000FF00) 8)

swapEndian64 :: Word64 -> Word64
swapEndian64 x = (shiftR x 56) .|. ((x .&. 0x00FF000000000000) `shiftR` 40)
   .|. ((x .&. 0x0000FF0000000000) `shiftR` 24) .|. ((x .&. 0x000000FF00000000) `shiftR` 8)
   .|. ((x .&. 0x00000000FF000000) `shiftL` 8) .|. ((x .&. 0x0000000000FF0000) `shiftL` 24)
   .|. ((x .&. 0x000000000000FF00) `shiftL` 40) .|. (shiftL x 56)

