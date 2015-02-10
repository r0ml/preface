{-# LANGUAGE FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}

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
import qualified Data.Char as C
import System.IO (Handle, hGetContents)
import qualified Data.List as DL

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC

-- instance Stringy a b => Show a where
--   show x = asString x

class Enum a => Chary a where
    asChar :: a -> Char
    asByte :: a -> Word8
    isSpace :: a -> Bool

instance Chary Char where
        asChar = id
        asByte = fromIntegral . C.ord
        isSpace = C.isSpace

instance Chary Word8 where
        asChar = C.chr . fromEnum
        asByte = id
        isSpace = C.isSpace . asChar

class Chary b => Stringy a b | a -> b where
    strCat :: [a] -> a
    strConcat :: [a] -> a
    strConcat = strCat
    strConcatMap :: (b -> a) -> a -> a
    strAppend :: a -> a -> a
    strAppend x y = strCat [x,y]
    
    strEmpty :: a
    strEmpty = zilde
    zilde :: a
    strNull :: a -> Bool
    strLen :: a -> Integer
    strDrop :: (Num c, Integral c, Eq c) => c -> a -> a
    strDropWhile :: (b->Bool) -> a -> a

    strTake :: (Num c, Integral c, Eq c) => c -> a -> a
    strTakeWhile :: (b->Bool) -> a -> a

    strBrk :: (b -> Bool) -> a -> (a,a)
    strBreak :: (b -> Bool) -> a -> (a,a)
    strBreak = strBrk
    
    strBreakSubstring :: a -> a -> (a,a)
    
    strSplitAt :: (Num c, Integral c, Eq c) => c -> a -> (a,a)
    stringleton :: b -> a
    strReplicate :: (Num c, Integral c) => c -> b -> a
    strCons :: b -> a -> a
    strSnoc :: a -> b -> a
    strHGetContents :: Handle -> IO a
    nth :: Integral c => a -> c -> b

    asByteString :: a -> ByteString
    asString :: a -> String
    asText :: a -> Text
    
    asLazyByteString :: a -> L.ByteString
    asLazyByteString x = L.fromChunks [asByteString x]

    strHead :: a -> b
    strHead = flip nth ( 0 :: Int)

    strTail :: a -> a
    strTail = strDrop ( 1 :: Int)

    strLast :: a -> b
    strLast x = nth x (strLen x - 1)
      
    isPrefixOf :: a -> a -> Bool
    startsWith :: a -> a -> Bool
    startsWith = flip isPrefixOf
    
    isSuffixOf :: a -> a -> Bool
    endsWith :: a -> a -> Bool
    endsWith = flip isSuffixOf
    
    strReverse :: a -> a
    
    strInit :: a -> a
    strPut :: a -> IO ()
    strPutLn :: a -> IO ()

    strMapAccumL :: (d -> b -> (d, b)) -> d -> a -> (d, a)

    pack :: [b] -> a

    stripStart :: a -> a
    stripStart = strDropWhile isSpace
    
    split :: b -> a -> [a]
--    splitOn :: a -> a -> [a]
    splitWith :: (b->Bool) -> a -> [a]

    intercalate :: a -> [a] -> a

    strReadFile :: FilePath -> IO a
--    strReads :: Read a => ReadS a

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
  strConcatMap = T.concatMap
  zilde = T.empty
  strNull = T.null
  strLen = toInteger . T.length
  strDrop = T.drop . fromIntegral
  strDropWhile = T.dropWhile
  strTake = T.take . fromIntegral
  strTakeWhile = T.takeWhile

  strBrk = T.break 
  strBreakSubstring = T.breakOn
    
  strSplitAt = T.splitAt . fromIntegral
  stringleton = T.singleton
  strReplicate n x = T.replicate (fromIntegral n) (stringleton x)
  strCons a b = T.cons a b
  strSnoc a b = T.snoc a b
  
  strHGetContents = T.hGetContents
  nth t n = T.index t (fromIntegral n)
  asByteString = T.encodeUtf8
  asString = T.unpack
  asText = id
  
  isPrefixOf = T.isPrefixOf
  isSuffixOf = T.isSuffixOf

  strReverse = T.reverse
  strInit = T.init
  strPut = T.putStr
  strPutLn = T.putStrLn
  
  strMapAccumL = T.mapAccumL
  pack = T.pack

  stripStart = T.stripStart

  split = T.splitOn . stringleton
--  splitOn = T.splitOn
  splitWith = T.split

  intercalate = T.intercalate
  strReadFile = T.readFile
  
--  strReads = TR.reads

instance Stringy B.ByteString Word8 where
  strCat = B.concat
  strConcatMap = B.concatMap
  zilde = B.empty
  strNull = B.null
  strLen = toInteger . B.length
  strDrop = B.drop . fromIntegral
  strDropWhile = B.dropWhile
  strTake = B.take . fromIntegral
  strTakeWhile = B.takeWhile
  strBrk f = B.break f -- (f . asChar)
  strBreakSubstring = B.breakSubstring
  
  strSplitAt = B.splitAt . fromIntegral
  stringleton = B.singleton
  strReplicate n w = B.replicate (fromIntegral n) (asByte w)
  strCons a b = B.cons a b
  strSnoc a b = B.snoc a b
  
  strHGetContents = B.hGetContents
  nth a b = (B.index a (fromIntegral b))
  asByteString = id
  asString = T.unpack . T.decodeUtf8
  asText = T.decodeUtf8
  
  isPrefixOf = B.isPrefixOf
  isSuffixOf = B.isSuffixOf

  strReverse = B.reverse
  strInit = B.init
  strPut = B.putStr
  strPutLn = BC.putStrLn
  
  strMapAccumL = B.mapAccumL
  
  pack = B.pack
  
  split = B.split
--  splitOn = B.splitOn
  splitWith = B.splitWith

  intercalate = B.intercalate
  strReadFile = B.readFile

instance Stringy L.ByteString Word8 where
  strCat = L.concat
  strConcatMap = L.concatMap
  zilde = L.empty
  strNull = L.null
  strLen = toInteger . L.length
  strDrop = L.drop . fromIntegral
  strDropWhile = L.dropWhile
  strTake = L.take . fromIntegral
  strTakeWhile = L.takeWhile
  strBrk f = L.break f -- (f . asChar)
  strBreakSubstring pat src = search 0 src
    where search n s
            | strNull s             = (src, zilde :: L.ByteString )
            | pat `L.isPrefixOf` s = (L.take n src,s)
            | otherwise          = search (n+1) (L.tail s)
  
  strSplitAt = L.splitAt . fromIntegral
  stringleton = L.singleton
  strReplicate n w = L.replicate (fromIntegral n) (asByte w)
  strCons a b = L.cons a b
  strSnoc a b = L.snoc a b
  strHGetContents = L.hGetContents
  nth a b = L.index a (fromIntegral b)
  asByteString = L.toStrict
  asString = T.unpack . T.decodeUtf8 . asByteString
  asText = T.decodeUtf8 . asByteString
  
  isPrefixOf = L.isPrefixOf
  isSuffixOf = L.isSuffixOf

  strReverse = L.reverse
  strInit = L.init
  strPut = L.putStr
  strPutLn = LC.putStrLn
  
  strMapAccumL = L.mapAccumL
  pack = L.pack

  split = L.split
  splitWith = L.splitWith

  intercalate = L.intercalate
  strReadFile = L.readFile
  
instance Stringy [Char] Char where
  strCat = concat
  strConcatMap = concatMap
  zilde = ""
  strNull = null
  strLen = toInteger . length
  strDrop = drop . fromIntegral
  strDropWhile = dropWhile
  strTake = take . fromIntegral
  strTakeWhile = takeWhile
  strBrk = break 
  strBreakSubstring pat src = search 0 src
    where search n s
            | null s             = (src,[])
            | pat `DL.isPrefixOf` s = (take n src,s)
            | otherwise          = search (n+1) (tail s)

  strSplitAt = splitAt . fromIntegral
  stringleton = (:[]) 
  strReplicate n x = replicate (fromIntegral n) (asChar x)
  strCons a b = a : b
  strSnoc a b = a ++ [b]
  strHGetContents = hGetContents
  nth a b = (!!) a (fromIntegral b)
  asByteString = T.encodeUtf8 . T.pack
  asString = id
  asText = T.pack
  
  isPrefixOf = DL.isPrefixOf
  isSuffixOf = DL.isSuffixOf
  
  strReverse = reverse
  strInit = init
  strPut = putStr
  strPutLn = putStrLn
  
  strMapAccumL = DL.mapAccumL

  pack = id

  split = split
  splitWith = splitWith

  intercalate = DL.intercalate
  strReadFile = Prelude.readFile

-- packBytes :: [Word8] -> B.ByteString
-- packBytes = B.pack


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

-- byteReadFile :: FilePath -> IO ByteString
-- byteReadFile = B.readFile

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
        | ch >= 65 && ch <= 90  = [C.chr ch]
        | ch >= 97 && ch <= 122 = [C.chr ch]
        | ch >= 48 && ch <= 57  = [C.chr ch]
        | elem (C.chr ch) exch = [C.chr ch]
        | otherwise = let (a, b) = ch `divMod` 16 in '%' : hex a : hex b : []
      hex i = C.chr $ if i < 10 then 48 + i else 65 + i - 10 

urlEncode
    :: Bool -- ^ Whether input is in query string. True: Query string, False: Path element
    -> B.ByteString
    -> String
urlEncode True  = urlEncode' "-_.~"
urlEncode False = urlEncode' ":@&=+$,"

