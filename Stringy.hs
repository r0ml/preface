{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

-- MultiParamTypeClasses ?
-- TypeFamilies ?

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
import Data.List (isPrefixOf, isSuffixOf, mapAccumL)

class Scalary a where
    asChar :: a -> Char
    asByte :: a -> Word8

instance Scalary Char where
	asChar = id
	asByte = fromIntegral . ord
instance Scalary Word8 where
	asChar = chr . fromEnum
	asByte = id

class Stringy a b | a -> b where
    strCat :: [a] -> a
    strAppend :: a -> a -> a
    strAppend x y = strCat [x,y]
    
    strEmpty :: a
    strEmpty = zilde
    zilde :: a
    strNull :: a -> Bool
    strLen :: a -> Integer
    strDrop :: Integral c => c -> a -> a
    strTake :: Integral c => c -> a -> a
    strBrk :: (b -> Bool) -> a -> (a,a)
    strBreak :: (b -> Bool) -> a -> (a,a)
    strBreak = strBrk
    
    strBreakSubstring :: a -> a -> (a,a)
    
    strSplitAt :: Integral c => c -> a -> (a,a)
    strChar :: b -> a
    strReplicate :: (Integral c) => c -> b -> a
    strCons :: b -> a -> a
    strSnoc :: a -> b -> a
    strHGetContents :: Handle -> IO a
    nthChar :: Integral c => a -> c -> Char
    
    asByteString :: a -> ByteString
    asString :: a -> String
    asText :: a -> Text
    
    asLazyByteString :: a -> L.ByteString
    asLazyByteString x = L.fromChunks [asByteString x]

    strTail :: a -> a
    strTail = strDrop (1 :: Int)

    strHead :: a -> Char
    strHead = flip nthChar ( 0 :: Int)

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
    strPut :: a -> IO ()
    
    strMapAccumL :: (d -> b -> (d, b)) -> d -> a -> (d, a)
    
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
  strTrim
  strPadLeft
  strPadRight
  strPadBoth
  strEq
-}

-- Text, Lazy.Text, ByteString, Lazy.ByteString, String 

instance Stringy T.Text Char where
  strCat = T.concat
  zilde = T.empty
  strNull = T.null
  strLen = toInteger . T.length
  strDrop = T.drop . fromIntegral
  strTake = T.take . fromIntegral
  strBrk = T.break 
  strBreakSubstring = T.breakOn
    
  strSplitAt = T.splitAt . fromIntegral
  strChar = T.singleton . asChar
  strReplicate n x = T.replicate (fromIntegral n) (strChar (asChar x))
  strCons a b = T.cons a b
  strSnoc a b = T.snoc a b
  
  strHGetContents = T.hGetContents
  nthChar t n = T.index t (fromIntegral n)
  asByteString = T.encodeUtf8
  asString = T.unpack
  asText = id
  
  strIsPrefixOf = T.isPrefixOf
  strIsSuffixOf = T.isSuffixOf

  strReverse = T.reverse
  strInit = T.init
  strPut = T.putStr
  
  strMapAccumL = T.mapAccumL
  
instance Stringy B.ByteString Word8 where
  strCat = B.concat
  zilde = B.empty
  strNull = B.null
  strLen = toInteger . B.length
  strDrop = B.drop . fromIntegral
  strTake = B.take . fromIntegral
  strBrk f = B.break f -- (f . asChar)
  strBreakSubstring = B.breakSubstring
  
  strSplitAt = B.splitAt . fromIntegral
  strChar = B.singleton . asByte
  strReplicate n w = B.replicate (fromIntegral n) (asByte w)
  strCons a b = B.cons a b
  strSnoc a b = B.snoc a b
  
  strHGetContents = B.hGetContents
  nthChar a b = asChar (B.index a (fromIntegral b))
  asByteString = id
  asString = T.unpack . T.decodeUtf8
  asText = T.decodeUtf8
  
  strIsPrefixOf = B.isPrefixOf
  strIsSuffixOf = B.isSuffixOf

  strReverse = B.reverse
  strInit = B.init
  strPut = B.putStr
  
  strMapAccumL = B.mapAccumL
  
instance Stringy L.ByteString Word8 where
  strCat = L.concat
  zilde = L.empty
  strNull = L.null
  strLen = toInteger . L.length
  strDrop = L.drop . fromIntegral
  strTake = L.take . fromIntegral
  strBrk f = L.break f -- (f . asChar)
  strBreakSubstring pat src = search 0 src
    where search n s
            | strNull s             = (src, zilde :: L.ByteString )
            | pat `L.isPrefixOf` s = (L.take n src,s)
            | otherwise          = search (n+1) (L.tail s)
  
  strSplitAt = L.splitAt . fromIntegral
  strChar = L.singleton . asByte
  strReplicate n w = L.replicate (fromIntegral n) (asByte w)
  strCons a b = L.cons a b
  strSnoc a b = L.snoc a b
  strHGetContents = L.hGetContents
  nthChar a b = asChar (L.index a (fromIntegral b))
  asByteString = L.toStrict
  asString = T.unpack . T.decodeUtf8 . asByteString
  asText = T.decodeUtf8 . asByteString
  
  strIsPrefixOf = L.isPrefixOf
  strIsSuffixOf = L.isSuffixOf

  strReverse = L.reverse
  strInit = L.init
  strPut = L.putStr
  
  strMapAccumL = L.mapAccumL
  
instance Stringy [Char] Char where
  strCat = concat
  zilde = ""
  strNull = null
  strLen = toInteger . length
  strDrop = drop . fromIntegral
  strTake = take . fromIntegral
  strBrk = break 
  strBreakSubstring pat src = search 0 src
    where search n s
            | null s             = (src,[])
            | pat `isPrefixOf` s = (take n src,s)
            | otherwise          = search (n+1) (tail s)

  strSplitAt = splitAt . fromIntegral
  strChar = (:[]) . asChar
  strReplicate n x = replicate (fromIntegral n) (asChar x)
  strCons a b = a : b
  strSnoc a b = a ++ [b]
  strHGetContents = hGetContents
  nthChar a b = (!!) a (fromIntegral b)
  asByteString = T.encodeUtf8 . T.pack
  asString = id
  asText = T.pack
  
  strIsPrefixOf = isPrefixOf
  strIsSuffixOf = isSuffixOf
  
  strReverse = reverse
  strInit = init
  strPut = putStr
  
  strMapAccumL = mapAccumL

nthByte :: Integral a => B.ByteString -> a -> Word8
nthByte b n = B.index b (fromIntegral n)

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

base64 :: Stringy a b => a -> String
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

