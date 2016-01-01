{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

{- | String support in Haskell is a mess.  The String class represents a string as a List of characters.
 - There are also ByteString classes (Lazy and Strict) which represent a bytestring as an array of bytes.
 - And lastly, there are Text classes (Lazy and Strict) which represent a String as a Unicode encoded array of bytes.
 - However, the functions which operate on these different string representations are different, and each representation
 - implements functions which are not available in the others.
 -
 - The Stringy class attempts to normalize the String problem by providing a class which implements a uniform set
 - of string functions on all representations.  
 -
 - Currently, there is a dependent type (Chary) which is the "type of character" composing the Stringy.  This is because
 - the Text and String representations are built from Char elements, whereas a ByteString consists (logically) of bytes 
 - (Word8 in Haskell).  One could possibly assert that all these Stringy implementations are based on Char,  thus obviating
 - the need for Chary -- but for now, the distinction between strings made up of bytes vs. characters remains.
 -
 - Implementation of the string functions for Stringy is a precursor to implementing the APL functions for arrays.
 - In APL, many of the string functions are the array functions -- so Stringy should probably be an subclass of 
 - APL Array.  This has not yet been done.  However, when that happens, it is not unlikely that a Data.Sequence.Seq of 
 - characters would be another instance of Stringy.
 -}
module Preface.Stringy 
  ( Stringy(..)
  , Chary(..)
  , LazyByteString
  , LazyText
  , ErrorMessage
  , byteStringToForeignPtr
  , byteStringFromForeignPtr
  , base16
  , base64
  , urlEncode
  , randomString
) where


import Preface.Imports

import qualified Data.ByteString.Lazy as L (ByteString, fromChunks)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Char as C
import qualified Data.List as DL

-- import Data.Array.Unboxed (UArray, listArray, (!))

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI

import Preface.Arrayed

type LazyByteString = LC.ByteString
type LazyText = TL.Text

-- default (Integer, Int)

-- | A class for the element of a Stringy
class (Eq a, Enum a) => Chary a where
    asChar :: a -> Char
    asByte :: a -> Word8
    isSpace :: a -> Bool
    fromChar :: Char -> a

-- | Most strings are made of characters
instance Chary Char where
        asChar = id
        asByte = fromIntegral . C.ord
        isSpace = C.isSpace
        fromChar = id

-- | ByteStrings are made of Word8
instance Chary Word8 where
        asChar = C.chr . fromEnum
        asByte = id
        isSpace = C.isSpace . asChar
        fromChar = fromIntegral . C.ord 

-- | ErrorMessages are Strings
type ErrorMessage = String

-- | A class to define generic Strings
class (IsString a, Eq a, Chary (Char_y a), Arrayed a) => Stringy a where 
    type Char_y a

    -- | Concatenation of multiple Stringy's into a single Stringy
    strCat :: [a] -> a

    -- | A synonym for @strCat
    strConcat :: [a] -> a
    strConcat = strCat

    -- | append a Stringy to the end of another
    strAppend :: a -> a -> a
    strAppend x y = strCat [x,y]
    
    -- | make an empty Stringy
    strEmpty :: a
    strEmpty = zilde

    -- | make an empty Stringy (synonym for @strEmpty )
    zilde :: a

    -- | is the Stringy empty?
    strNull :: a -> Bool

    -- | the length of the Stringy.  The length of Stringy is limited to an Int
    strLen :: a -> Int

    -- | Drop the first n elements of a Stringy
    strDrop :: Integral b => b -> a -> a
    strDrop = aDrop 

    -- | Drop the first n characters which test True on the given function
    strDropWhile :: ( (Char_y a) -> Bool) -> a -> a

    -- | Take the first n elements of a Stringy.  If n is greater than the length of the Stringy,
    -- only take the n elements.
    strTake :: Integral b => b -> a -> a
    strTake = aTake 

    -- | Take the first n characters which test True on the given function
    strTakeWhile :: ( (Char_y a)->Bool) -> a -> a

    -- | Return a tuple which is the first n characters which test True, and then the remainder
    -- This would be equivalent to (strTakeWhile f s, strDropWhile f s)
    strUntil :: ((Char_y a) -> Bool) -> a -> Maybe (a,a)

    -- | strBrkSubstring breaks a string by a the first occurence of the given substring    
    strUntilStr :: a -> a -> Maybe (a,a)

    strBrk :: ((Char_y a) -> Bool) -> a -> (a,a)

    -- | strSplit breaks a string into every substring between occurences of the supplied char
    strSplit :: (Char_y a) -> a -> [a]
    strSplit = strSplitWith . (==)

    -- | strSplitStr breaks a string into every substring between occurences of the supplied
    --   delimiter string
    strSplitStr :: (Stringy b) => b -> a -> [a]

    -- | strSplitWith breaks a string into every substring between occurences of the supplied
    --   function.  The function is applied to each character of the given string
    strSplitWith :: ((Char_y a)->Bool) -> a -> [a]

    -- | strSplitWhen breaks a string into every substring between occurences of the supplied
    --   function.  The function is applied to the remaining string after each character
    --   to locate whether this is a split location.  The function returns the number of
    --   characters to drop if it is a match.  If the number is 0 or negative, that is
    --   interpreted as "not a match" -- since a result of 0 would mean that the same
    --   function would be applied again to the same string.
    strSplitWhen :: (a -> Int) -> a -> [a]


    -- | split the given Stringy into a list of lines (split on newline)
    strLines :: a -> [a]

    -- | split the Arrayed at the given index
    strSplitAt :: Int -> a -> (a,a)
    strSplitAt = aSplitAt

    stringleton :: (Char_y a) -> a
    strReplicate :: Int -> (Char_y a) -> a
    strCons :: (Char_y a) -> a -> a
    strSnoc :: a -> (Char_y a) -> a
    strHGetContents :: Handle -> IO a
    strHGetLine :: Handle -> IO a
    strHGet :: Handle -> Int -> IO a
    strHPut :: Handle -> a -> IO ()
    nth :: Integral b => a -> b -> (Char_y a)

    asByteString :: a -> ByteString
    asString :: a -> String
    asText :: a -> Text
    asLazyText :: a-> TL.Text
    asLazyText x = TL.fromChunks [asText x]
 
    asLazyByteString :: a -> L.ByteString
    asLazyByteString x = L.fromChunks [asByteString x]

    strHead :: a -> (Char_y a)
    strHead = flip nth 0 

    strTail :: a -> a
    strTail = aDrop 1

    strLast :: a -> (Char_y a)
    strLast x = nth x (strLen x - 1)
     
    -- | strPrefixOf returns true if the first string is a prefix of the second string 
    strPrefixOf :: a -> a -> Bool

    -- startsWith :: a -> a -> Bool
    -- startsWith = flip strPrefixOf
    
    -- | strSuffixOf returns true if the first string is a suffix of the second string
    strSuffixOf :: a -> a -> Bool
    -- endsWith :: a -> a -> Bool
    -- endsWith = flip strSuffixOf

        

    strElemIndex :: (Char_y a) -> a -> Maybe Int 

    strReverse :: a -> a
    
    strInit :: a -> a
    strPut :: a -> IO ()
    strPutLn :: a -> IO ()
    strPutH :: Handle -> a -> IO ()
    strPutHLn :: Handle -> a -> IO ()

    strMap :: ((Char_y a)->(Char_y a)) -> a -> a
    strMapAccumL :: (d -> (Char_y a) -> (d, (Char_y a))) -> d -> a -> (d, a)

    pack :: [(Char_y a)] -> a
    unpack :: a -> [(Char_y a)]

    trimL :: a -> a
    trimR :: a -> a
    trimR = strReverse . trimL . strReverse . trimL

    trim :: a -> a
    trim = trimR . trimL 
   
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
  zilde = T.empty
  strNull = T.null
  strLen = fromIntegral . T.length
  strDropWhile = T.dropWhile
  strTakeWhile = T.takeWhile

  strUntil x y = let (a,b) = T.break x y in if strNull b then Nothing else Just (a,strTail b)
  strUntilStr x y = let (a,b) = T.breakOn x y in if strNull b then Nothing else Just (a,strDrop (strLen a) b)
  
  strBrk = T.break
 
  strSplit = T.splitOn . stringleton
  strSplitStr = T.splitOn . asText
  strSplitWith = T.split

  strLines = T.lines 
  stringleton = T.singleton
  strReplicate n x = T.replicate (fromIntegral n) (stringleton x)
  strCons a b = T.cons a b
  strSnoc a b = T.snoc a b
  
  strHGetContents = T.hGetContents
  strHGetLine = T.hGetLine
  strHGet = undefined
  strHPut = T.hPutStr
  nth t n = T.index t (fromIntegral n)
  asByteString = T.encodeUtf8
  asString = T.unpack
  asText = id
  
  strPrefixOf = T.isPrefixOf
  strSuffixOf = T.isSuffixOf

  strElemIndex a b = fmap fromIntegral (T.findIndex (==a) b)

  strReverse = T.reverse
  strInit = T.init
  strPut = T.putStr
  strPutLn = T.putStrLn
  strPutH = T.hPutStr
  strPutHLn = T.hPutStrLn
 
  strMap = T.map
  strMapAccumL = T.mapAccumL
  pack = T.pack
  unpack = T.unpack

  trimL = T.stripStart

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
  zilde = TL.empty
  strNull = TL.null
  strLen = fromIntegral . TL.length
  strDropWhile = TL.dropWhile
  strTakeWhile = TL.takeWhile

  strUntil x y = let (a,b) = TL.break x y in if strNull b then Nothing else Just (a, strTail b)
  strUntilStr x y = let (a,b) = TL.breakOn x y in if strNull b then Nothing else Just (a, strDrop (strLen x) b)
  
  strBrk = TL.break
 
  strSplit = TL.splitOn . stringleton
  strSplitStr = TL.splitOn . asLazyText
--  splitOn = TL.splitOn
  strSplitWith = TL.split

  strLines = TL.lines 
  stringleton = TL.singleton
  strReplicate n x = TL.replicate (fromIntegral n) (stringleton x)
  strCons a b = TL.cons a b
  strSnoc a b = TL.snoc a b
  
  strHGetContents = TL.hGetContents
  strHGetLine = TL.hGetLine
  strHGet = undefined
  strHPut = TL.hPutStr
  nth t n = TL.index t (fromIntegral n)
  asByteString = T.encodeUtf8 . TL.toStrict
  asString = TL.unpack
  asText = TL.toStrict 
  
  strPrefixOf = TL.isPrefixOf
  strSuffixOf = TL.isSuffixOf

  strElemIndex a b = let c = TL.length ( fst (TL.span (/=a) b)) in if c == TL.length b then Nothing else Just (fromIntegral c)

  strReverse = TL.reverse
  strInit = TL.init
  strPut = T.putStr . TL.toStrict
  strPutLn = T.putStrLn . TL.toStrict
  strPutH h = T.hPutStr h . TL.toStrict
  strPutHLn h = T.hPutStrLn h . TL.toStrict

  strMap = TL.map
  strMapAccumL = TL.mapAccumL
  pack = TL.pack
  unpack = TL.unpack

  trimL = TL.stripStart

  intercalate = TL.intercalate
  strReadFile = TL.readFile 
  strWriteFile = TL.writeFile
  
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
  zilde = B.empty
  strNull = B.null
  strLen = fromIntegral . B.length
  strDropWhile = B.dropWhile
  strTakeWhile = B.takeWhile
  strUntil f x = let (a,b) = B.break f x in if strNull b then Nothing else Just (a,strTail b)
  strUntilStr f x = let (a,b) = B.breakSubstring f x in if strNull b then Nothing else Just (a, strDrop (strLen a) b)
 
  strBrk = B.break
 
  strSplit = B.split
  strSplitStr d x = let (a,b) = B.breakSubstring (asByteString d) x in if strNull b then [a] else a : strSplitStr d (strDrop (strLen d) b) 
  strSplitWith = B.splitWith

  strLines = BC.lines
  stringleton = B.singleton
  strReplicate n w = B.replicate (fromIntegral n) (asByte w)
  strCons a b = B.cons a b
  strSnoc a b = B.snoc a b
  
  strHGetContents = B.hGetContents
  strHGetLine = B.hGetLine
  strHGet = B.hGet
  strHPut = B.hPut
  nth a b = (B.index a (fromIntegral b))
  asByteString = id
  asString = T.unpack . T.decodeUtf8
  asText = T.decodeUtf8
  
  strPrefixOf = B.isPrefixOf
  strSuffixOf = B.isSuffixOf

  strElemIndex a b = B.elemIndex a b

  strReverse = B.reverse
  strInit = B.init
  strPut = B.putStr
  strPutLn = BC.putStrLn
  strPutH = B.hPutStr
  strPutHLn = BC.hPutStrLn

  strMap = B.map
  strMapAccumL = B.mapAccumL
  
  pack = B.pack
  unpack = B.unpack
  
  trimL = strDropWhile isSpace

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

  strToLower = B.map (BI.c2w . C.toLower . chr . fromEnum )
  strToUpper = B.map (BI.c2w . C.toUpper . chr . fromEnum )
  
-- ctype_lower :: UArray Word8 Word8
-- ctype_lower = listArray (0,255) (map (BI.c2w . C.toLower) ['\0'..'\255'])

-- ctype_upper :: UArray Word8 Word8
-- ctype_upper = listArray (0,255) (map (BI.c2w . C.toUpper) ['\0'..'\255'])

{-
instance Stringy L.ByteString where
  type Char_y L.ByteString = Word8
  strCat = L.concat
  zilde = L.empty
  strNull = L.null
  strLen = fromIntegral . L.length
  strDropWhile = L.dropWhile
  strTakeWhile = L.takeWhile
  strBrk f = L.break f -- (f . asChar)
  strBrkSubstring pat src = search 0 src
    where search n s
            | strNull s             = (src, zilde :: L.ByteString )
            | pat `L.isPrefixOf` s = (L.take n src,s)
  strTake = take . fromIntegral
            | otherwise          = search (n+1) (L.tail s)
  
  strLines = LC.lines
  stringleton = L.singleton
  strReplicate n w = L.replicate (fromIntegral n) (asByte w)
  strCons a b = L.cons a b
  strSnoc a b = L.snoc a b
  strHGetContents = L.hGetContents
  strHGetLine = L.hGetLine
  strHGet = L.hGet
  strHPut = L.hPut
  nth a b = L.index a (fromIntegral b)
  asByteString = L.toStrict
  asString = T.unpack . T.decodeUtf8 . asByteString
  asText = T.decodeUtf8 . asByteString
  
  strPrefixOf = L.isPrefixOf
  strSuffixOf = L.isSuffixOf

  strElemIndex a b = L.elemIndex a b

  strReverse = L.reverse
  strInit = L.init
  strPut = L.putStr
  strPutLn = LC.putStrLn
  strPutH = L.hPutStr
  strPutHLn = LC.hPutStrLn

  strMap = L.map
  strMapAccumL = L.mapAccumL
  pack = L.pack
  unpack = L.unpack

  trimL = strDropWhile isSpace

  splitChar = L.split
  splitStr d x = let (a,b) = strBrkStr d x in if strNull b then [a] else a : splitStr d (strDrop (strLen d) b) 
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
-}

instance Stringy [Char] where
  type Char_y [Char] = Char
  strCat = concat
  zilde = ""
  strNull = null
  strLen = fromIntegral . length
  strDropWhile = dropWhile
  strTakeWhile = takeWhile
  strUntil x y = let (a,b) = break x y in if null b then Nothing else Just (a, strTail b)
  strUntilStr pat src = search 0 src
    where search n s
            | null s             = Nothing 
            | pat `strPrefixOf` s = Just (take n src, drop n s)
            | otherwise          = search (n+1) (tail s)

  strBrk = break

  strSplit d x = let (_,b) = split_ ([],[]) x in b where
     split_ (a,b) [] = ([], reverse (reverse a : b))
     split_ (a,b) (f:xs) = if d == f then split_ ([], reverse a : b) xs else split_ (f:a, b) xs
  strSplitStr d x = let (_,b) = split_ ([],[]) x in b where
     dx = asString d
     dn = strLen d
     split_ (a,b) [] = ([], reverse (reverse a : b))
     split_ (a,b) xs = if dx `strPrefixOf` xs then split_ ([], reverse a : b) (strDrop dn xs) else split_ ((head xs):a, b) (tail xs)
 
  strLines = lines
  stringleton = (:[]) 
  strReplicate n x = replicate (fromIntegral n) (asChar x)
  strCons a b = a : b
  strSnoc a b = a ++ [b]
  strHGetContents = hGetContents
  strHGetLine = hGetLine
  strHGet h n = if n <= 0 then return "" else do
        a <- hGetChar h
        b <- strHGet h (n-1)
        return (a:b)
  strHPut = hPutStr
  nth a b = (!!) a (fromIntegral b)
  asByteString = T.encodeUtf8 . T.pack
  asString = id
  asText = T.pack
  
  strPrefixOf = DL.isPrefixOf
  strSuffixOf = DL.isSuffixOf
  
  strElemIndex a b = DL.elemIndex a b

  strReverse = reverse
  strInit = init
  strPut = putStr
  strPutLn = putStrLn
  strPutH = hPutStr
  strPutHLn = hPutStrLn
 
  strMap = DL.map
  strMapAccumL = DL.mapAccumL

  pack = id
  unpack = id

  trimL = strDropWhile isSpace 

  intercalate = DL.intercalate
  strReadFile = readFile
  strWriteFile = writeFile

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

