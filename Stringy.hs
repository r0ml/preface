{-# LANGUAGE FlexibleInstances #-}

module Stringy where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Bits
import Data.Word (Word8)
import Data.Char (chr, ord)
-- import Data.List ((!!))
import System.IO (Handle, hGetContents)
import Data.List (isPrefixOf, isSuffixOf)

class Scalary a where
    asChar :: a -> Char
    asByte :: a -> Word8

instance Scalary Char where
	asChar = id
	asByte = fromIntegral . ord
instance Scalary Word8 where
	asChar = chr . fromEnum
	asByte = id

type Length = Integer

class Stringy a where
    strCat :: [a] -> a
    zilde :: a
    strNull :: a -> Bool
    strLen :: a -> Length
    strDrop :: Length -> a -> a
    strTake :: Length -> a -> a
    strBrk :: (Char -> Bool) -> a -> (a,a)
    strSplitAt :: Length -> a -> (a,a)
    strChar :: Scalary b => b -> a
    strReplicate :: Scalary b => Length -> b -> a
    strCons :: Scalary b => b -> a -> a
    strHGetContents :: Handle -> IO a
    nthChar :: a -> Length -> Char
    
    asByteString :: a -> ByteString
    asString :: a -> String
    asText :: a -> Text
    
    asLazyByteString :: a -> L.ByteString
    asLazyByteString x = L.fromChunks [asByteString x]

    strTail :: a -> a
    strTail = strDrop 1

    strHead :: a -> Char
    strHead = flip nthChar 0

    strLast :: a -> Char
    strLast x = nthChar x (strLen x - 1)
      
    strIsPrefixOf :: a -> a -> Bool
    strStartsWith :: a -> a -> Bool
    strStartsWith = flip strIsPrefixOf
    
    strIsSuffixOf :: a -> a -> Bool
    strEndsWith :: a -> a -> Bool
    strEndsWith = flip strIsSuffixOf
    
    strReverse :: a -> a
    
    strInit :: a -> a
    
{-
  strReplace
  strLast
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
  strEq
-}

-- Text, Lazy.Text, ByteString, Lazy.ByteString, String 

instance Stringy T.Text where
  strCat = T.concat
  zilde = T.empty
  strNull = T.null
  strLen = toInteger . T.length
  strDrop = T.drop . fromInteger
  strTake = T.take . fromInteger
  strBrk = T.break 
  strSplitAt = T.splitAt . fromInteger
  strChar = T.singleton . asChar
  strReplicate n x = T.replicate (fromInteger n) (strChar (asChar x))
  strCons a b = T.cons (asChar a) b
  strHGetContents = T.hGetContents
  nthChar t n = T.index t (fromInteger n)
  asByteString = T.encodeUtf8
  asString = T.unpack
  asText = id
  
  strIsPrefixOf = T.isPrefixOf
  strIsSuffixOf = T.isSuffixOf

  strReverse = T.reverse
  strInit = T.init
  
instance Stringy B.ByteString where
  strCat = B.concat
  zilde = B.empty
  strNull = B.null
  strLen = toInteger . B.length
  strDrop = B.drop . fromInteger
  strTake = B.take . fromInteger
  strBrk f = B.break (f . asChar)
  strSplitAt = B.splitAt . fromInteger
  strChar = B.singleton . asByte
  strReplicate n w = B.replicate (fromInteger n) (asByte w)
  strCons a b = B.cons (asByte a) b
  strHGetContents = B.hGetContents
  nthChar a b = asChar (B.index a (fromInteger b))
  asByteString = id
  asString = T.unpack . T.decodeUtf8
  asText = T.decodeUtf8
  
  strIsPrefixOf = B.isPrefixOf
  strIsSuffixOf = B.isSuffixOf

  strReverse = B.reverse
  strInit = B.init

instance Stringy L.ByteString where
  strCat = L.concat
  zilde = L.empty
  strNull = L.null
  strLen = toInteger . L.length
  strDrop = L.drop . fromInteger
  strTake = L.take . fromInteger
  strBrk f = L.break (f . asChar)
  strSplitAt = L.splitAt . fromInteger 
  strChar = L.singleton . asByte
  strReplicate n w = L.replicate (fromInteger n) (asByte w)
  strCons a b = L.cons (asByte a) b
  strHGetContents = L.hGetContents
  nthChar a b = asChar (L.index a (fromInteger b))
  asByteString = L.toStrict
  asString = T.unpack . T.decodeUtf8 . asByteString
  asText = T.decodeUtf8 . asByteString
  
  strIsPrefixOf = L.isPrefixOf
  strIsSuffixOf = L.isSuffixOf

  strReverse = L.reverse
  strInit = L.init
  
instance Stringy [Char] where
  strCat = concat
  zilde = ""
  strNull = null
  strLen = toInteger . length
  strDrop = drop . fromInteger
  strTake = take . fromInteger
  strBrk = break 
  strSplitAt = splitAt . fromInteger
  strChar = (:[]) . asChar
  strReplicate n x = replicate (fromInteger n) (asChar x)
  strCons a b = asChar a : b
  strHGetContents = hGetContents
  nthChar a b = (!!) a (fromInteger b)
  asByteString = T.encodeUtf8 . T.pack
  asString = id
  asText = T.pack
  
  strIsPrefixOf = isPrefixOf
  strIsSuffixOf = isSuffixOf
  
  strReverse = reverse
  strInit = init

nthByte :: B.ByteString -> Length -> Word8
nthByte b n = B.index b (fromInteger n)

packBytes :: [Word8] -> B.ByteString
packBytes = B.pack

{-
byteStringToString :: ByteString -> String
byteStringToString = T.unpack . T.decodeUtf8

stringFromByteString :: ByteString -> String
stringFromByteString = byteStringToString


stringToByteString :: String -> ByteString
stringToByteString = T.encodeUtf8 . T.pack

byteStringFromString :: String -> ByteString
byteStringFromString = stringToByteString

stringToText :: String -> Text
stringToText = T.pack

textFromString :: String -> Text
textFromString = T.pack
-}

{-
byteConcat :: [ByteString] -> ByteString
byteConcat = B.concat

byteNull :: ByteString -> Bool
byteNull = B.null

byteEmpty :: ByteString 
byteEmpty = B.empty
-}

byteReadFile :: FilePath -> IO ByteString
byteReadFile = B.readFile

-- ------------------------------------------------------------------------------

base64 :: Stringy a => a -> String
base64 a = let fe = fromIntegral . fromEnum
               q' = fe (strHead a)
               a' = strTail a
               (r',a'') = if strNull a' then  (0,a') else (fe (strHead a'),strTail a')
               (s',rest) = if strNull a'' then (0,a'') else (fe (strHead a''),strTail a'')
               alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
               q = (shiftR q' 2) .&. 63
               r = (((shiftR r' 4) .&. 15) .|. (shiftL (q' .&. 3) 4))
               s = if strLen a < 2 then 64 else ((shiftL (r' .&. 15) 2) .|. ((shiftR s' 6) .&. 3))
               t = if strLen a < 3 then 64 else (s' .&. 63)
               ss = map (alphabet !!) [q,r,s,t]
           in if strNull a then "" else ss ++ (base64 rest)

-- | Percent-encoding for URLs.
urlEncode' :: String -> B.ByteString -> String
urlEncode' exch s = concatMap  (encodeChar . fromEnum) (B.unpack s)
    where
      encodeChar ch 
        | ch >= 65 && ch <= 90  = [chr ch]
        | ch >= 97 && ch <= 122 = [chr ch]
        | ch >= 48 && ch <= 57  = [chr ch]
        | elem (chr ch) exch = [chr ch]
        | otherwise = let (a, b) = ch `divMod` 16 in '%' : hex a : hex b : []
      hex i = chr $ if i < 10 then 48 + i else 65 + i - 10 

urlEncode
    :: Bool -- ^ Whether input is in query string. True: Query string, False: Path element
    -> B.ByteString
    -> String
urlEncode True  = urlEncode' "-_.~"
urlEncode False = urlEncode' ":@&=+$,"
