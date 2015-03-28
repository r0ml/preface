{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

module Stringy where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.String (IsString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Lazy as TL
import Data.Bits
import Data.Word (Word8)
import qualified Data.Char as C
import System.IO (Handle, hGetContents)
import qualified Data.List as DL

import Data.Array.Unboxed (UArray, listArray, (!))

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI
import Foreign.ForeignPtr

import System.Random (newStdGen, randomRs)

-- instance Stringy a b => Show a where
--   show x = asString x

type LazyByteString = LC.ByteString
type LazyText = TL.Text

default (Integer, Int)

class (Eq a, Enum a) => Chary a where
    asChar :: a -> Char
    asByte :: a -> Word8
    isSpace :: a -> Bool
    fromChar :: Char -> a

instance Chary Char where
        asChar = id
        asByte = fromIntegral . C.ord
        isSpace = C.isSpace
        fromChar = id

instance Chary Word8 where
        asChar = C.chr . fromEnum
        asByte = id
        isSpace = C.isSpace . asChar
        fromChar = fromIntegral . C.ord 

type ErrorMessage = String

class (IsString a, Eq a, Chary (Char_y a)) => Stringy a where 
    type Char_y a

-- can I change this definition to define a type for Chary?
    strCat :: [a] -> a
    strConcat :: [a] -> a
    strConcat = strCat
    strConcatMap :: ( (Char_y a) -> a ) -> a -> a
    strAppend :: a -> a -> a
    strAppend x y = strCat [x,y]
    
    strEmpty :: a
    strEmpty = zilde
    zilde :: a
    strNull :: a -> Bool
    strLen :: a -> Integer
    strDrop :: (Integral c, Eq c) => c -> a -> a
    strDropWhile :: ( (Char_y a) -> Bool) -> a -> a

    strTake :: (Integral c, Eq c) => c -> a -> a
    strTakeWhile :: ( (Char_y a)->Bool) -> a -> a

    strBrk :: ((Char_y a) -> Bool) -> a -> (a,a)
    strBreak :: ((Char_y a) -> Bool) -> a -> (a,a)
    strBreak = strBrk
    
    strBreakSubstring :: a -> a -> (a,a)
    
    strLines :: a -> [a]

    strSplitAt :: (Integral c, Eq c) => c -> a -> (a,a)
    stringleton :: (Char_y a) -> a
    strReplicate :: (Integral c) => c -> (Char_y a) -> a
    strCons :: (Char_y a) -> a -> a
    strSnoc :: a -> (Char_y a) -> a
    strHGetContents :: Handle -> IO a
    nth :: Integral c => a -> c -> (Char_y a)

    asByteString :: a -> ByteString
    asString :: a -> String
    asText :: a -> Text
    asLazyText :: a-> TL.Text
    asLazyText x = TL.fromChunks [asText x]
 
    asLazyByteString :: a -> L.ByteString
    asLazyByteString x = L.fromChunks [asByteString x]

    strHead :: a -> (Char_y a)
    strHead = flip nth ( 0 :: Int)

    strTail :: a -> a
    strTail = strDrop ( 1 :: Int)

    strLast :: a -> (Char_y a)
    strLast x = nth x (strLen x - 1)
      
    isPrefixOf :: a -> a -> Bool
    startsWith :: a -> a -> Bool
    startsWith = flip isPrefixOf
    
    isSuffixOf :: a -> a -> Bool
    endsWith :: a -> a -> Bool
    endsWith = flip isSuffixOf
    
    strElemIndex :: (Integral c) => (Char_y a) -> a -> Maybe c

    strReverse :: a -> a
    
    strInit :: a -> a
    strPut :: a -> IO ()
    strPutLn :: a -> IO ()

    strMap :: ((Char_y a)->(Char_y a)) -> a -> a
    strMapAccumL :: (d -> (Char_y a) -> (d, (Char_y a))) -> d -> a -> (d, a)

    pack :: [(Char_y a)] -> a
    unpack :: a -> [(Char_y a)]

    stripStart :: a -> a
    
    splitChar :: (Char_y a) -> a -> [a]
    splitStr :: a -> a -> [a]
--    splitOn :: a -> a -> [a]
    splitWith :: ((Char_y a)->Bool) -> a -> [a]

    intercalate :: a -> [a] -> a

    strReadFile :: FilePath -> IO a
    strWriteFile :: FilePath -> a -> IO ()

    stringy :: a -> a
    stringy = id
--    strReads :: Read a => ReadS a

    str2decimal :: (Read c, Integral c) => a -> Either ErrorMessage (c, a)
    signed :: (Read c, Num c) => (a -> Either ErrorMessage (c, a)) -> (a -> Either ErrorMessage (c, a))
    str2hexadecimal :: (Read c, Integral c) => a -> Either ErrorMessage (c, a)
    str2rational :: (Read c, Fractional c) => a-> Either ErrorMessage (c, a)
    str2double :: a -> Either ErrorMessage (Double, a)

    strToLower :: a -> a
    strToUpper :: a -> a

    strReplace :: a -> a -> a -> a
    strReplace orig repl text = if strNull text then strEmpty
                                else if strLen text >= strLen orig && orig == strTake (strLen orig) text then strConcat [repl, strReplace orig repl (strDrop (strLen orig) text)]
                                   else strCons ( strHead text)  (strReplace orig repl (strTail text))
   
{-
  strSplit
  strSplitAll
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

instance Stringy T.Text where
  type Char_y T.Text = Char
  strCat = T.concat
  strConcatMap = T.concatMap
  zilde = T.empty
  strNull = T.null
  strLen = fromIntegral . T.length
  strDrop = T.drop . fromIntegral
  strDropWhile = T.dropWhile
  strTake = T.take . fromIntegral
  strTakeWhile = T.takeWhile

  strBrk = T.break 
  strBreakSubstring = T.breakOn
   
  strLines = T.lines 
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

  strElemIndex a b = fmap fromIntegral (T.findIndex (==a) b)

  strReverse = T.reverse
  strInit = T.init
  strPut = T.putStr
  strPutLn = T.putStrLn
  
  strMap = T.map
  strMapAccumL = T.mapAccumL
  pack = T.pack
  unpack = T.unpack

  stripStart = T.stripStart

  splitChar = T.splitOn . stringleton
  splitStr = T.splitOn
--  splitOn = T.splitOn
  splitWith = T.split

  intercalate = T.intercalate
  strReadFile = T.readFile
  strWriteFile = T.writeFile
  
  str2decimal = T.decimal
  signed = T.signed
  str2hexadecimal = T.hexadecimal
  str2rational = T.rational
  str2double = T.double

  strToLower = T.toLower
  strToUpper = T.toUpper

  strReplace = T.replace

--  strReads = TR.reads

instance Stringy TL.Text where
  type Char_y TL.Text = Char
  strCat = TL.concat
  strConcatMap = TL.concatMap
  zilde = TL.empty
  strNull = TL.null
  strLen = fromIntegral . TL.length
  strDrop = TL.drop . fromIntegral
  strDropWhile = TL.dropWhile
  strTake = TL.take . fromIntegral
  strTakeWhile = TL.takeWhile

  strBrk = TL.break 
  strBreakSubstring = TL.breakOn
   
  strLines = TL.lines 
  strSplitAt = TL.splitAt . fromIntegral
  stringleton = TL.singleton
  strReplicate n x = TL.replicate (fromIntegral n) (stringleton x)
  strCons a b = TL.cons a b
  strSnoc a b = TL.snoc a b
  
  -- strHGetContents = TL.hGetContents
  nth t n = TL.index t (fromIntegral n)
  asByteString = T.encodeUtf8 . TL.toStrict
  asString = TL.unpack
  asText = TL.toStrict 
  
  isPrefixOf = TL.isPrefixOf
  isSuffixOf = TL.isSuffixOf

  -- strElemIndex a b = fmap fromIntegral (T.findIndex (==a) b)

  strReverse = TL.reverse
  strInit = TL.init
  strPut = T.putStr . TL.toStrict
  strPutLn = T.putStrLn . TL.toStrict
  
  strMap = TL.map
  strMapAccumL = TL.mapAccumL
  pack = TL.pack
  unpack = TL.unpack

  stripStart = TL.stripStart

  splitChar = TL.splitOn . stringleton
  splitStr = TL.splitOn
--  splitOn = TL.splitOn
  splitWith = TL.split

  intercalate = TL.intercalate
  -- strReadFile = T.readFile 
  -- strWriteFile = T.writeFile
  
  -- str2decimal x = T.decimal TL.toStrict
  -- signed = T.signed . TL.toStrict 
  -- str2hexadecimal = T.hexadecimal . TL.toStrict
  -- str2rational = T.rational . TL.toStrict
  -- str2double = T.double . TL.toStrict
  str2decimal a = case str2decimal (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyText z)

  str2double a = case str2double (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyText z)

  str2hexadecimal a = case str2hexadecimal (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyText z)

  str2rational a = case str2rational (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyText z)

  signed f a = case signed (\x2 -> case f (asLazyText x2) of { Left x -> Left x; Right (y, z) -> Right (y, asText z) }) (asText a) of
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyText z)


  strToLower = TL.toLower
  strToUpper = TL.toUpper

  strReplace = TL.replace


instance Stringy B.ByteString where
  type Char_y B.ByteString = Word8
  strCat = B.concat
  strConcatMap = B.concatMap
  zilde = B.empty
  strNull = B.null
  strLen = fromIntegral . B.length
  strDrop = B.drop . fromIntegral
  strDropWhile = B.dropWhile
  strTake = B.take . fromIntegral
  strTakeWhile = B.takeWhile
  strBrk f = B.break f -- (f . asChar)
  strBreakSubstring = B.breakSubstring
  
  strLines = BC.lines
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

  strElemIndex a b = fromIntegral <$> B.elemIndex a b

  strReverse = B.reverse
  strInit = B.init
  strPut = B.putStr
  strPutLn = BC.putStrLn
  
  strMap = B.map
  strMapAccumL = B.mapAccumL
  
  pack = B.pack
  unpack = B.unpack
  
  stripStart = strDropWhile isSpace

  splitChar = B.split
  splitStr d x = let (a,b) = B.breakSubstring d x in if strNull b then [a] else a : splitStr d (strDrop (strLen d) b) 

--  splitOn = B.splitOn
  splitWith = B.splitWith

  intercalate = B.intercalate
  strReadFile = B.readFile
  strWriteFile = B.writeFile

  str2decimal a = case str2decimal (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asByteString z)

  str2double a = case str2double (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asByteString z)

  str2hexadecimal a = case str2hexadecimal (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asByteString z)

  str2rational a = case str2rational (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asByteString z)

  signed f a = case signed (\x2 -> case f (asByteString x2) of { Left x -> Left x; Right (y, z) -> Right (y, asText z) }) (asText a) of
      Left x -> Left x
      Right (y,z) -> Right (y, asByteString z)

  strToLower = B.map (\x -> ctype_lower!x)
  strToUpper = B.map (\x -> ctype_upper!x)
  
ctype_lower :: UArray Word8 Word8
ctype_lower = listArray (0,255) (map (BI.c2w . C.toLower) ['\0'..'\255'])

ctype_upper :: UArray Word8 Word8
ctype_upper = listArray (0,255) (map (BI.c2w . C.toUpper) ['\0'..'\255'])

instance Stringy L.ByteString where
  type Char_y L.ByteString = Word8
  strCat = L.concat
  strConcatMap = L.concatMap
  zilde = L.empty
  strNull = L.null
  strLen = fromIntegral . L.length
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
  
  strLines = LC.lines
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

  strElemIndex a b = fromIntegral <$> L.elemIndex a b

  strReverse = L.reverse
  strInit = L.init
  strPut = L.putStr
  strPutLn = LC.putStrLn
  
  strMap = L.map
  strMapAccumL = L.mapAccumL
  pack = L.pack
  unpack = L.unpack

  stripStart = strDropWhile isSpace

  splitChar = L.split
  splitStr d x = let (a,b) = strBreakSubstring d x in if strNull b then [a] else a : splitStr d (strDrop (strLen d) b) 
  splitWith = L.splitWith

  intercalate = L.intercalate
  strReadFile = L.readFile
  strWriteFile = L.writeFile
  
  str2decimal a = case str2decimal (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyByteString z)

  str2double a = case str2double (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyByteString z)

  str2hexadecimal a = case str2hexadecimal (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyByteString z)

  str2rational a = case str2rational (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyByteString z)

  signed f a = case signed (\x2 -> case f (asLazyByteString x2) of { Left x -> Left x; Right (y, z) -> Right (y, asText z) }) (asText a) of
      Left x -> Left x
      Right (y,z) -> Right (y, asLazyByteString z)

  strToLower = L.map (\x -> ctype_lower!x)
  strToUpper = L.map (\x -> ctype_upper!x)

instance Stringy [Char] where
  type Char_y [Char] = Char
  strCat = concat
  strConcatMap = concatMap
  zilde = ""
  strNull = null
  strLen = fromIntegral . length
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

  strLines = lines
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
  
  strElemIndex a b = fromIntegral <$> DL.elemIndex a b

  strReverse = reverse
  strInit = init
  strPut = putStr
  strPutLn = putStrLn
  
  strMap = DL.map
  strMapAccumL = DL.mapAccumL

  pack = id
  unpack = id

  stripStart = strDropWhile isSpace 

  splitChar d x = let (_,b) = split_ ([],[]) x in b where
     split_ (a,b) [] = ([], reverse (reverse a : b))
     split_ (a,b) (f:xs) = if d == f then split_ ([], reverse a : b) xs else split_ (f:a, b) xs
  splitStr d x = let (_,b) = split_ ([],[]) x in b where
     split_ (a,b) [] = ([], reverse (reverse a : b))
     split_ (a,b) xs = if d `isPrefixOf` xs then split_ ([], reverse a : b) (strDrop (strLen d) xs) else split_ ((head xs):a, b) (tail xs)
 
  splitWith = splitWith

  intercalate = DL.intercalate
  strReadFile = Prelude.readFile
  strWriteFile = Prelude.writeFile

-- packBytes :: [Word8] -> B.ByteString
-- packBytes = B.pack
  str2decimal x = if (null a) then Left "Parsing Error" else Right (head a)
        where a = reads x
  str2double x = if (null a) then Left "Parsing Error" else Right (head a)
        where a = reads x
  str2hexadecimal a = case str2hexadecimal (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asString z)

  str2rational a = case str2rational (asText a) of 
      Left x -> Left x
      Right (y,z) -> Right (y, asString z)

  signed f a = case signed (\x2 -> case f (asString x2) of { Left x -> Left x; Right (y, z) -> Right (y, asText z) }) (asText a) of
      Left x -> Left x
      Right (y,z) -> Right (y, asString z)

  strToLower = map C.toLower
  strToUpper = map C.toUpper

  -- strReplace orig repl text = if null text then ""
  --                            else if startsWith orig text then repl ++ strReplace orig repl (drop (length orig) text)
  --                                 else head text : strReplace orig repl (tail text)

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

byteStringFromForeignPtr :: ForeignPtr Word8 -> Int -> Int -> ByteString
byteStringFromForeignPtr = BI.fromForeignPtr

byteStringToForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int)
byteStringToForeignPtr = BI.toForeignPtr
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

base16 :: Stringy a => a -> String
base16 a = let d = strHead a
               e = 0x0f .&. shiftR (fromEnum d) 4
               f = 0x0f .&. (fromEnum d)
               xs = "0123456789abcdef"
               b = xs !! e
               c = xs !! f 
            in if strNull a then [] else b : c : base16 (strTail a)

-- | Percent-encoding for URLs.
urlEncode' :: String -> B.ByteString -> String
urlEncode' exch s = strConcat $ map  (encodeChar . fromEnum) (asString s)
    where
      encodeChar ch 
        | ch >= 65 && ch <= 90  = [C.chr ch]
        | ch >= 97 && ch <= 122 = [C.chr ch]
        | ch >= 48 && ch <= 57  = [C.chr ch]
        | elem (C.chr ch) exch = [C.chr ch]
        | otherwise = let (a, b) = ch `divMod` 16 in '%' : hex a : hex b : []
      hex i = C.chr $ if i < 10 then 48 + i else 65 + i - 10 

urlEncode
    :: (Stringy a) => Bool -- ^ Whether input is in query string. True: Query string, False: Path element
    -> a
    -> String 
urlEncode True  = urlEncode' "-_.~" . asByteString
urlEncode False = urlEncode' ":@&=+$," . asByteString 


randomString :: Int -> [Char] -> IO String
randomString n a = do { rs <- fmap (take n . randomRs (0, length a - 1)) newStdGen ; return [a !! z | z <- rs] }

