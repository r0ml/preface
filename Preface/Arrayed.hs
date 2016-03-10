
module Preface.Arrayed ( Arrayed(..)
 ) where

import Preface.Imports
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC

class Arrayed a where
  aDrop :: Int -> a -> a
  aTake :: Int -> a -> a
  aSplitAt :: Int -> a -> (a,a)

instance Arrayed [a] where
  aDrop = drop . fromIntegral
  aTake = take . fromIntegral 
  aSplitAt = splitAt . fromIntegral

instance Arrayed T.Text where
  aDrop = T.drop . fromIntegral
  aTake = T.take . fromIntegral
  aSplitAt = T.splitAt . fromIntegral

instance Arrayed TL.Text where
  aDrop = TL.drop . fromIntegral
  aTake = TL.take . fromIntegral
  aSplitAt = TL.splitAt . fromIntegral

instance Arrayed B.ByteString where
  aDrop = B.drop . fromIntegral
  aTake = B.take . fromIntegral
  aSplitAt = B.splitAt . fromIntegral

instance Arrayed LC.ByteString where
  aDrop = LC.drop . fromIntegral
  aTake = LC.take . fromIntegral
  aSplitAt = LC.splitAt . fromIntegral


{-
 - let a = pack [0,0,14,44] ::ByteString
 - x <- unsafeForeignPtrToStorableArray (castForeignPtr b) (0,0) :: IO (StorableArray Int Int32)
 - y <- unsafeRead x 0 
 - showHex y ""
 -}


