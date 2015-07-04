
module Preface.Arrayed ( Arrayed(..)
 ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC

class Arrayed a where
  aDrop :: Int -> a -> a
  aTake :: Int -> a -> a
  aSplitAt :: Int -> a -> (a,a)

instance Arrayed [a] where
  aDrop = drop
  aTake = take
  aSplitAt = splitAt

instance Arrayed T.Text where
  aDrop = T.drop
  aTake = T.take
  aSplitAt = T.splitAt

instance Arrayed TL.Text where
  aDrop = TL.drop . fromIntegral
  aTake = TL.take . fromIntegral
  aSplitAt = TL.splitAt . fromIntegral

instance Arrayed B.ByteString where
  aDrop = B.drop
  aTake = B.take
  aSplitAt = B.splitAt

instance Arrayed LC.ByteString where
  aDrop = LC.drop . fromIntegral
  aTake = LC.take . fromIntegral
  aSplitAt = LC.splitAt . fromIntegral

