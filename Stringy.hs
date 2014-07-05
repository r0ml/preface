{-# LANGUAGE FlexibleInstances #-}

module Stringy where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word (Word8)
import Data.Char (chr, ord)
-- import Data.List ((!!))
import System.IO (Handle, hGetContents)

class Chary a where
    asChar :: a -> Char
    asByte :: a -> Word8

instance Chary Char where
	asChar = id
	asByte = fromIntegral . ord
instance Chary Word8 where
	asChar = chr . fromEnum
	asByte = id

class Stringy a where
    strCat :: [a] -> a
    zilde :: a
    strNull :: a -> Bool
    strLen :: a -> Int
    strDrop :: Int -> a -> a
    strTake :: Int -> a -> a
    strBrk :: (Char -> Bool) -> a -> (a,a)
    strSplitAt :: Int -> a -> (a,a)
    strChar :: Chary b => b -> a
    strRepl :: Chary b => Int -> b -> a
    strCons :: Chary b => b -> a -> a
    strHGetContents :: Handle -> IO a
    nthChar :: a -> Int -> Char
    
{-
  strReplace
  strHead
  strLast
  strTail
  strInit
  strSplit
  strSplitAll
  strToLower
  strToUpper
  strCapitalize
  strMap
  strJoin
  strAppend
  strTrim
  strPadLeft
  strPadRight
  strPadBoth
  strReverse
  strStartsWith
  strEndsWith
  strEq
-}

-- Text, Lazy.Text, ByteString, Lazy.ByteString, String 

instance Stringy T.Text where
  strCat = T.concat
  zilde = T.empty
  strNull = T.null
  strLen = T.length
  strDrop = T.drop
  strTake = T.take
  strBrk = T.break 
  strSplitAt = T.splitAt
  strChar = T.singleton . asChar
  strRepl n x = T.replicate n (strChar (asChar x))
  strCons a b = T.cons (asChar a) b
  strHGetContents = T.hGetContents
  nthChar = T.index
    
instance Stringy B.ByteString where
  strCat = B.concat
  zilde = B.empty
  strNull = B.null
  strLen = B.length
  strDrop = B.drop
  strTake = B.take
  strBrk f = B.break (f . asChar)
  strSplitAt = B.splitAt
  strChar = B.singleton . asByte
  strRepl n w = B.replicate n (asByte w)
  strCons a b = B.cons (asByte a) b
  strHGetContents = B.hGetContents
  nthChar a b = asChar (B.index a b)
  
instance Stringy [Char] where
  strCat = concat
  zilde = ""
  strNull = null
  strLen = length
  strDrop = drop
  strTake = take
  strBrk = break 
  strSplitAt = splitAt
  strChar = (:[]) . asChar
  strRepl n x = replicate n (asChar x)
  strCons a b = asChar a : b
  strHGetContents = hGetContents
  nthChar = (!!)

nthByte :: B.ByteString -> Int -> Word8
nthByte = B.index

packBytes :: [Word8] -> B.ByteString
packBytes = B.pack
