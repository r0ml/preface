{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Preface.JSONic
    ( JSON(..)
    , JSONic(..)
    , readJSON
    , showsJSON
    -- * Convenience types

    , Numb(..)

    , (.=)
    , (.:)
    , (.?)
    , jsonElemQ
    , mfl
    , mlookup

    , JsonicOptions(..), JsonicSumEncoding(..), defaultJsonicOptions, defaultTaggedObject, camelTo
    , deriveJSONic
    , deriveJSONicT
    ) where

import Data.Bits ((.|.), shiftL)
import Data.Char (chr, ord, isSpace, isUpper, toLower)

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M

import Data.Either (partitionEithers)

import Numeric (showHex)

import Debug.Trace

import Control.Monad       ( liftM2 )
import Data.List           ( isPrefixOf, foldl', intercalate, genericLength, partition )
import Data.Maybe          ( catMaybes )
import Text.Printf         ( printf )

-- from template-haskell:
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( VarStrictType )


-- The most common way to use the library is to define a data type,
-- corresponding to some JSON data you want to work with, and then
-- write either a 'FromJSON' instance, a to 'ToJSON' instance, or both
-- for that type. For example, given this JSON data:
--
-- > { "name": "Joe", "age": 12 }
--
-- we create a matching data type:
--
-- > data Person = Person
-- >     { name :: Text
-- >     , age  :: Int
-- >     } deriving Show
--
-- To convert data to/from JSON we need to define a 'JSONic' instance:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > instance JSONic Person where
-- >     fromJSON (JsonObject v) = Person <$> v .: "name" <*> v .: "age"
-- >     -- A non-JsonObject value is of the wrong type, so fail.
-- >     fromJSON _          = mzero
-- >     toJSON (Person name age) = JsonObject (M.fromList ["name" .= name, "age" .= age ]
-- We can now parse the JSON data like so:
--
-- > >>> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
-- > Just (Person {name = "Joe", age = 12})
--
-- We can now encode a value like so:
--
-- > >>> encode (Person {name = "Joe", age = 12})
-- > "{\"name\":\"Joe\",\"age\":12}"
--
-- There are predefined 'JSONic' instances for many
-- types. Here's an example using lists and 'Int's:
--
-- > >>> decode "[1,2,3]" :: Maybe [Int]
-- > Just [1,2,3]
--
-- And here's an example using the 'Data.Map.Map' type to get a map of
-- 'Int's.
--
-- > >>> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
-- > Just (fromList [("bar",2),("foo",1)])

-- While the notes below focus on decoding, you can apply almost the
-- same techniques to /encoding/ data. (The main difference is that
-- encoding always succeeds, but decoding has to handle the
-- possibility of failure, where an input doesn't match our
-- expectations.)
--
-- Sometimes you want to work with JSON data directly, without first
-- converting it to a custom data type. This can be useful if you want
-- to e.g. convert JSON data to YAML data, without knowing what the
-- contents of the original JSON data was. The 'JSON' type, which is
-- an instance of 'JSONic', is used to represent an arbitrary JSON
-- AST (abstract syntax tree). Example usage:
--
-- > >>> decode "{\"foo\": 123}" :: Maybe JSON
-- > Just (JsonObject (fromList [("foo",JsonNumber 123)]))
--
-- > >>> decode "{\"foo\": [\"abc\",\"def\"]}" :: Maybe JSON
-- > Just (JsonObject (fromList [("foo",JsonArray (fromList [JsonString "abc",JsonString "def"]))]))
--
-- Once you have a 'JSON' you can write functions to traverse it and
-- make arbitrary transformations.

-- Any instance of 'JSONic' can be specified (but see the
-- \"Pitfalls\" section here&#8212;"Data.Aeson#pitfalls"):
--
-- > λ> decode "[1,2,3]" :: Maybe [Int]
-- > Just [1,2,3]
--
-- Alternatively, there are instances for standard data types, so you
-- can use them directly. For example, use the 'Data.Map.Map' type to
-- get a map of 'Int's.
--
-- > λ> :m + Data.Map
-- > λ> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
-- > Just (fromList [("bar",2),("foo",1)])

-- The above approach with maps of course will not work for mixed-type
-- objects that don't follow a strict schema, but there are a couple
-- of approaches available for these.
--
-- The 'JsonObject' type contains JSON objects:
--
-- > λ> decode "{\"name\":\"Dave\",\"age\":2}" :: Maybe Object
-- > Just (fromList) [("name",JsonString "Dave"),("age",JsonNumber 2)]
--
-- You can extract values from it with a parser using 'parse',
-- 'parseEither' or, in this example, 'parseMaybe':
--
-- > λ> do result <- decode "{\"name\":\"Dave\",\"age\":2}"
-- >       flip parseMaybe result $ \obj -> do
-- >         age <- obj .: "age"
-- >         name <- obj .: "name"
-- >         return (name ++ ": " ++ show (age*2))
-- >
-- > Just "Dave: 4"
--
-- Considering that any type that implements 'JSONic' can be used
-- here, this is quite a powerful way to parse JSON. 
--
-- The downside is that you have to write the parser yourself; the
-- upside is that you have complete control over the way the JSON is
-- parsed.

-- $pitfalls
-- #pitfalls#
--
-- Note that the JSON standard requires that the top-level value be
-- either an array or an object. If you try to use 'decode' with a
-- result type that is /not/ represented in JSON as an array or
-- object, your code will typecheck, but it will always \"fail\" at
-- runtime:
--
-- > >>> decode "1" :: Maybe Int
-- > Nothing
-- > >>> decode "1" :: Maybe String
-- > Nothing
--
-- So stick to objects (e.g. maps in Haskell) or arrays (lists or
-- vectors in Haskell):
--
-- > >>> decode "[1,2,3]" :: Maybe [Int]
-- > Just [1,2,3]
--
-- When encoding to JSON you can encode anything that's an instance of
-- 'JSONic', and this may include simple types. So beware that this
-- aspect of the API is not isomorphic. You can round-trip arrays and
-- maps, but not simple values:
--
-- > >>> encode [1,2,3]
-- > "[1,2,3]"
-- > >>> decode (encode [1]) :: Maybe [Int]
-- > Just [1]
-- > >>> encode 1
-- > "1"
-- > >>> decode (encode (1 :: Int)) :: Maybe Int
-- > Nothing
--
-- Encoding and decoding are each two-step processes.
--
-- * To encode a value, it is first converted to an abstract syntax
--   tree (AST), using 'toJSON' :: 'JSON'. This generic representation is then
--   encoded as bytes.
--
-- * When decoding a value, the process is reversed: the bytes are
--   converted to an AST, then the 'fromJSON' function is used to convert
--   to the desired type.
--
-- For convenience, the 'encode' and 'decode' functions combine both
-- steps.
--
--

tpack :: a->a
trpack :: String -> Text
tunpack :: Text -> String
trunpack :: a->a

mktu ::  a->a
mktp ::  M.Map String a -> M.Map Text a

mfl ::  a->a
mtl ::  a->a

mlookup :: Jstr -> [(Jstr, b)] -> Maybe b

#if defined(STRTEX)
type Jstr = Text
tpack = T.pack
trpack = id
tunpack = id
trunpack = T.unpack
mktu = M.mapKeys trunpack
mktp = id
#else 
type Jstr = String
tpack = id
trpack = T.pack
tunpack = T.unpack
trunpack = id
mktu = id
mktp = M.mapKeys trpack
#endif

type Errm = String
type Jinp = String

#if defined (MAPOBJ)
type Jmap = M.Map Jstr JSON
mfl = M.fromList
mtl = M.toList
mlookup = M.lookup
#else
type Jmap = [(Jstr, JSON)]
mfl = id
mtl = id
mlookup = lookup
#endif

instance JSONic JSON where
  toJSON = id
  fromJSON = Right

instance (JSONic a) => JSONic (Maybe a) where
    toJSON (Just a) = toJSON a
    toJSON Nothing  = JsonNull
    fromJSON JsonNull   = Right Nothing
    fromJSON a      = either Left (Right . Just) $ fromJSON a

instance JSONic Bool where
    toJSON = JsonBool
    fromJSON (JsonBool a) = Right a
    fromJSON _ = Left "expecting JSONic JsonBool"

-- should the empty tuple map be JSONic ?
instance {-# OVERLAPPING #-} JSONic [Char] where
    toJSON = JsonString . tpack
    fromJSON (JsonString x) = Right $ trunpack x
    fromJSON _ = Left "expecting JSONic JsonString"

instance JSONic Double where
    toJSON = realFloatToJSON
    fromJSON (JsonNumber (DD x ) ) = Right x
    fromJSON (JsonNumber (II x ) ) = Right $ fromIntegral x
    fromJSON _ = Left "expecting JSONic Double"

instance JSONic Integer where
    toJSON = JsonNumber . II . fromIntegral
    fromJSON (JsonNumber (II x)) = Right $ fromIntegral x
    fromJSON (JsonNumber (DD x)) = Right $ floor x
    fromJSON _ = Left "expecting JSONic Integer"

instance JSONic Int where
    toJSON = JsonNumber . II . fromIntegral
    fromJSON (JsonNumber (II x)) = Right $ fromIntegral x
    fromJSON (JsonNumber (DD x)) = Right $ floor x
    fromJSON _ = Left "expecting JSONic Int"

{-
instance Integral a => JSONic a where
    toJSON = JsonNumber . II . fromIntegral
    fromJSON (JsonNumber (II x)) = Right $ fromIntegral x
    fromJSON (JsonNumber (DD x)) = Right $ floor x
    fromJSON _ = Left "expecting JSONic Integral"
-}

instance JSONic Text where
    toJSON = JsonString . tunpack
    fromJSON (JsonString x) = Right $ trpack x
    fromJSON _ = Left "expecting JSONic Text"

instance {-# OVERLAPPABLE #-} JSONic [JSON] where
    toJSON = JsonArray
    fromJSON (JsonArray x) =  Right x {- let d = map fromJSON x 
                                  (c, b) = partitionEithers d 
                               in if null c then Right b else Left (head c)
-}
    fromJSON _ = Left "expecting JSONic JsonArray"

instance {-# OVERLAPPABLE #-} (JSONic a) => JSONic [a] where
    toJSON = JsonArray . map toJSON
    fromJSON (JsonArray x) = let d = map fromJSON x :: JSONic a => [Either Errm a] 
                                 (c, b) = partitionEithers d 
                              in if null c then Right b else Left (head c)
    fromJSON _ = Left "expecting JSONic JsonArray"

instance (JSONic v3) => JSONic (M.Map String v3) where
#if defined(MAPOB)
    toJSON = JsonObject . M.foldrWithKey (\k -> M.insert (tpack k) . toJSON) M.empty
    fromJSON (JsonObject o) = let t = M.mapWithKey (\k va -> fromJSON va) o :: JSONic v => M.Map Jstr (Either Errm v)
                                  (l,r) = M.partition isLeft t 
                               in if M.null l then Right $ mktu $ M.map (\(Right rr) -> rr) r else Left $ concat $ intersperse "\n" $ lefts (M.elems l)
#else
    toJSON = JsonObject . map (\(k,va) -> (tpack k, toJSON va) ) . M.assocs
 {-   fromJSON (JsonObject o) = let t = map (\(k,va) -> (k :: String, fromJSON va :: JSONic v => Either Errm v)) o -- :: JSONic v => [(Jstr, Either Errm v )]
                                  (r, l) = break (isLeft . snd) t :: JSONic v => ([(Jstr, Either Errm v)], [(Jstr, Either Errm v)])
                               in if null l then Right ( M.fromList ( map (\(k,Right rr) -> (k,rr)) r) :: JSONic v1 => M.Map String v1 )  else snd ( head l ) -}
#endif

    fromJSON _ = trace "expecting JSONic JsonObject for M.Map String v" $ Left "expecting JSONic JsonObject"
{-
instance (JSONic v) => JSONic (M.Map Text v) where
    toJSON = JsonObject . M.foldrWithKey (\k -> M.insert (tunpack k) . toJSON) M.empty
    fromJSON (JsonObject o) = let t = M.mapWithKey (\k va -> fromJSON va) o :: JSONic v => M.Map Jstr (Either Errm v) 
                                  (l,r) = M.partition isLeft t
                               in if M.null l then Right $ mktp $ M.map (\(Right rr) -> rr) r else Left $ concat $ intersperse "\n" $ lefts (M.elems l)
    fromJSON _ = Left "expecting JSONic JsonObject"
-}


{-
instance (ToJSON v) => ToJSON (M.Map String v) where
    toJSON = JsonObject . M.foldrWithKey (\k v -> M.insert (pack k) (toJSON v)) M.empty

instance (FromJSON v) => FromJSON (M.Map String v) where
    parseJSON = fmap (M.foldrWithKey (M.insert . unpack) M.empty) . parseJSON
-}

{-
instance ToJSON ZonedTime where
    toJSON t = JsonString $ pack $ formatTime defaultTimeLocale format t
      where
        format = "%FT%T." ++ formatMillis t ++ tzFormat
        tzFormat
          | 0 == timeZoneMinutes (zonedTimeZone t) = "Z"
          | otherwise = "%z"

formatMillis :: (FormatTime t) => t -> String
formatMillis t = take 3 . formatTime defaultTimeLocale "%q" $ t

instance FromJSON ZonedTime where
    parseJSON (JsonString t) =
      tryFormats alternateFormats
      <|> fail "could not parse ECMA-262 ISO-8601 date"
      where
        tryFormat f =
          case parseTime defaultTimeLocale f (unpack t) of
            Just d -> pure d
            Nothing -> empty
        tryFormats = foldr1 (<|>) . map tryFormat
        alternateFormats =
          dateTimeFmt defaultTimeLocale :
          distributeList ["%Y", "%Y-%m", "%F"]
                         ["T%R", "T%T", "T%T%Q", "T%T%QZ", "T%T%Q%z"]

        distributeList xs ys =
          foldr (\x acc -> acc ++ distribute x ys) [] xs
        distribute x = map (mappend x)

    parseJSON v = typeMismatch "ZonedTime" v

instance ToJSON UTCTime where
    toJSON t = JsonString $ pack $ formatTime defaultTimeLocale format t
      where
        format = "%FT%T." ++ formatMillis t ++ "Z"
    {-# INLINE toJSON #-}

instance FromJSON UTCTime where
    parseJSON = withText "UTCTime" $ \t ->
        case parseTime defaultTimeLocale "%FT%T%QZ" (unpack t) of
          Just d -> pure d
          _      -> fail "could not parse ISO-8601 date"
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (a,b) = JsonArray $ V.create $ do
                     mv <- VM.unsafeNew 2
                     VM.unsafeWrite mv 0 (toJSON a)
                     VM.unsafeWrite mv 1 (toJSON b)
                     return mv
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
    parseJSON = withArray "(a,b)" $ \ab ->
        let n = V.length ab
        in if n == 2
             then (,) <$> parseJSON (V.unsafeIndex ab 0)
                      <*> parseJSON (V.unsafeIndex ab 1)
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a pair"
    {-# INLINE parseJSON #-}
-}

realFloatToJSON :: Double -> JSON
realFloatToJSON d
    | isNaN d || isInfinite d = JsonNull
    | otherwise = JsonNumber (DD d)

{-
toRealFloat :: RealFloat a => Numb -> a
toRealFloat (II a) = realToFrac a
toRealFloat (DD a) = realToFrac a

toIntegral :: Integral a => Numb -> a
toIntegral (II a ) = fromIntegral a
toIntegral (DD a ) = floor a
-}

----------------------------------------------------------------

-- | A JSON \"object\" (key\/JSON map).
data Numb = II Integer | DD Double deriving (Eq, Show) 

-- | A JSON represented as a Haskell JSON.
data JSON = JsonObject Jmap 
           | JsonArray [JSON]
           | JsonString Jstr
           | JsonNumber !Numb
           | JsonBool !Bool
           | JsonNull
             deriving (Eq)


-- | A type that can be converted to/from JSON.
--
-- An example type and instance:
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance JSONic Coord where
--   toJSON (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y] 
--   fromJSON ('JsonObject' v) = Right $ Coord '<$>' v '.:' \"x\" '<*>' v '.:' \"y\"
--
--   \-- A non-'JsonObject' JSON is of the wrong type
--   fromJSON _ = Left "not convertible to Coord"
-- @
--
-- * "Data.Aeson.TH" provides template-haskell functions which will derive an
-- instance at compile-time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
class JSONic a where
    toJSON   :: a -> JSON
    fromJSON :: JSON -> Either Errm a


-- GOOD ONE!
(.=) :: JSONic a => Jstr -> a -> (Jstr, JSON)
-- name .= value = (name, toJSON value)
(.=) = (. toJSON) . (,)

(.:) :: JSONic a => JSON -> Jstr -> a
obj .: key | JsonObject v <- obj = trace "lookup " $  case mlookup key v of
                                Nothing -> trace "first ud" undefined
                                Just r -> case fromJSON r of
                                             Left _rr -> trace "second ud" undefined
                                             Right rr -> rr
           | otherwise = trace "third ud" undefined

-- GOOD ONE!
(.?) :: JSONic a => JSON -> Jstr -> Maybe a
obj .? key | JsonObject v <- obj = let m = mlookup key v :: Maybe JSON in maybe Nothing (either (const Nothing) Just . fromJSON) m
           | otherwise = trace "fourth ud" undefined

-- \u2208 
jsonElemQ :: Jstr -> JSON -> Bool
jsonElemQ key obj | JsonObject v <- obj = maybe False (const True) (mlookup key v)
          | otherwise = False

type StringReader' a = String -> Either Errm (a, String)
type StringReader a = String -> Either Errm a

-- GOOD ONE!

-- | Parse a top-level JSON value.  This must be either an object or an array ( RFC 4627 )
readJSON :: StringReader JSON
readJSON = either Left xs . readJSON'
  where xs (a,b) = case dropWhile isSpace b of { [] -> Right a; z -> (Left . ("excess input: " ++) . (take 10) ) z }

readJSON' :: StringReader' JSON
readJSON' [] = Left "attempting to parse empty string"
readJSON' x@(fc : tbs) | isSpace fc = readJSON' (dropWhile isSpace tbs)
                       | '"' == fc = jstring_ tbs
                       | '{' == fc = object_ tbs
                       | '[' == fc = array_ tbs
                       | 'f' == fc && isPrefixOf "alse" tbs = Right (JsonBool False, drop 4 tbs)
                       | 't' == fc && isPrefixOf "rue" tbs = Right (JsonBool True, drop 3 tbs)
                       | 'n' == fc && isPrefixOf "ull" tbs = Right (JsonNull, drop 3 tbs)
                       | fc >= '0' && fc <= '9' || fc == '.' || fc == '-' = number_ x
                       | otherwise = Left ( "failed to match input: "++take 10 x )

object_ :: StringReader' JSON
object_ x = either Left jf (aas [] x)
  where jf (p,q) = Right (JsonObject (mfl p), q)
        aas ac r = case dropWhile isSpace r of 
                     [] -> Left ("unterminated object: " ++ take 10 x)
                     ('}':rs) -> Right (ac, rs)
                     bs -> case readJSON' bs of
                             Left x2 -> Left x2
                             Right (JsonString m,r1) -> case dropWhile isSpace r1 of
                               [] -> Left ("unterminated key/value pair: " ++ take 10 bs)
                               (':':r4) -> case readJSON' r4 of
                                             Left x3 -> Left x3
                                             Right (vv, r5) -> case dropWhile isSpace r5 of
                                                                  [] -> Left ("unterminated object: " ++ take 10 bs)
                                                                  ('}':r8) -> Right ((m, vv) : ac, r8)
                                                                  (',':r8) -> aas ((m, vv) : ac) r8
                                                                  _ -> Left ("object expecting ',' or '}': " ++ take 10 r5)
                               _ -> Left ("key/value pair expecting ':': "++take 10 r1)
                             Right _m -> Left ("expecting a JsonString, received a "++ show _m)

-- GOOD ONE!
-- this used to be all nested if/then/else
-- switch to case
array_ :: StringReader' JSON
array_ x = either Left jf (aas [] x)
  where jf (p,q) = Right (JsonArray p, q)
        aas ac r = case dropWhile isSpace r of
                     [] -> Left ("unterminated array: " ++ take 10 x)  
                     (']':rs) -> Right (reverse ac, rs)
                     bs ->case readJSON' bs of
                            Left x4 -> Left x4
                            Right (p,q) -> case dropWhile isSpace q of
                                             [] -> Left ("array terminates prematurely: " ++ take 10 x)
                                             (',':rs) -> aas (p:ac) rs
                                             (']':rs) -> Right (reverse (p:ac), rs)
                                             _ -> Left ("array expected ',' or ']' at " ++ take 10 x)

-- | Parse a string without a leading quote.
jstring_ :: StringReader' JSON
jstring_ x = let res = jjs "" x in either Left jf res
  where jf (p,q) = Right (JsonString p, q)
        jjs ac ('"':r) = Right (tpack (reverse ac),r)
        jjs _ac [] = Left "unterminated string"
        jjs ac r = let (a,b) = unescape r
                   in maybe (Left ("invalid character escape sequence: " ++ take 10 r)) (flip jjs b . (:ac)) a

number_ :: StringReader' JSON
number_ x = let res = reads x :: [(Double,Jinp)]
             in if null res then Left ("failed to read number: " ++ take 10 x) 
                else let [(a,b)] = res in Right (JsonNumber (DD a), b)

unescape :: Jinp -> (Maybe Char, Jinp)
unescape [] = (Nothing, [])
unescape ('\\' : []) = (Nothing , "\n")
unescape ('\\' : '\\' : r) = (Just '\\', r)
unescape ('\\' : '/' : r) = (Just '/', r)
unescape ('\\' : 'n' : r) = (Just '\n', r)
unescape ('\\' : 't' : r) = (Just '\t', r)
unescape ('\\' : 'b' : r) = (Just '\b', r)
unescape ('\\' : 'r' : r) = (Just '\r', r)
unescape ('\\' : 'f' : r) = (Just '\f', r)
unescape ('\\' : '"' : r) = (Just '\"', r)
unescape ('\\' : 'u' : r) = hexQuad r
unescape ('\\' : i : r) = (Nothing, i:r)
unescape (a:b) = (Just a, b)

hexQuad :: Jinp -> (Maybe Char, Jinp)
hexQuad t@(a:b:c:d:r) = let aa = hex a
                            bb = hex b 
                            cc = hex c
                            dd = hex d
                         in if aa == 255 || bb == 255 || cc == 255 || dd==255 then (Nothing, t) -- invalid hex escape
                            else let x = aa `shiftL` 12 .|. bb `shiftL` 8 .|. cc `shiftL` 4 .|. dd
                                  in if x < 0xd800 || x > 0xdfff then (Just (chr x), r)
                                     else if x >= 0xdc00 || null r || '\\' /= head r || null (tail r) || 'u' /= head (tail r)
                                          then (Nothing, t)
                                          else let (e,ff) = hexQuad (tail (tail r))
                                                in case e of 
                                                     Nothing -> (Nothing, t)
                                                     Just ee -> if ord ee >= 0xdc00 && ord ee <= 0xdfff 
                                                                   then (Just $ chr $ ((x - 0xd800) `shiftL` 10) + (ord ee - 0xdc00) + 0x10000, ff)
                                                                   else (Nothing, t) -- invalid UTF-16 surrogate
   where hex w | w >= '0' && w <= '9' = ord w - 48
               | w >= 'a' && w <= 'f' = ord w - 87
               | w >= 'A' && w <= 'F' = ord w - 55
               | otherwise = 255

hexQuad n = (Nothing, n)

-- -----------------------------------------------------------------
-- | Writing JSON

instance Show JSON where
  show = flip showsJSON' ""

-- | Show strict JSON top level types. Values not permitted
-- at the top level are wrapped in a singleton array.
showsJSON :: JSON -> ShowS
showsJSON x | JsonArray a <- x = showsJsonArray a
            | JsonObject o <- x = showsJsonObject o
            | otherwise = showsJsonArray [x]

-- | Show JSON values
showsJSON' :: JSON -> ShowS
showsJSON' jv 
  | JsonNull <- jv = showString "null" 
  | JsonBool b <- jv = showString (if b then "true" else "false") 
  | JsonNumber n <- jv = case n of 
                            II a -> shows a 
                            DD a -> if isNaN a || isInfinite a then showString "null"  else shows a 
  | JsonArray a <- jv = showsJsonArray a 
  | JsonObject a <- jv = showsJsonObject a 
  | JsonString a <- jv = showsJsonString a 
  | otherwise = trace "showsJSON' impossible pattern" undefined

-- | Show a string in JSON format
showsJsonString :: Jstr -> ShowS
showsJsonString s = showChar '"' . encJSString s . showChar '"' 
  where encJSString :: Jstr -> ShowS
        encJSString jss = go (trunpack jss)
          where
          go :: Jstr -> ShowS 
          go s1 =
            case s1 of
              (x   :xs) | x < '\x20' || x > '\xfe' -> showChar '\\' . encControl x . go xs
              ('"' :xs)              -> showChar '\\' . showChar '"'  . go xs
              ('\\':xs)              -> showChar '\\' . showChar '\\' . go xs
              (x   :xs)              -> showChar x . go xs
              ""                     -> id 

        encControl x = case x of
                          '\b' -> showChar 'b' 
                          '\f' -> showChar 'f' 
                          '\n' -> showChar 'n' 
                          '\r' -> showChar 'r'
                          '\t' -> showChar 't' 
                          _ | x < '\x10'   -> showString "u000" . hexxs
                            | x < '\x100'  -> showString "u00" . hexxs
                            | x < '\x1000' -> showString "u0" . hexxs
                            | otherwise    -> showChar 'u' . hexxs
                            where hexxs = showHex (fromEnum x)


-- | Show a list in JSON format
showsJsonArray :: [JSON] -> ShowS
showsJsonArray arg = showChar '[' .  tail . showsJsonArray' arg 
  where showsJsonArray' z  | (y:[]) <- z = showChar ',' . showsJSON' y . showChar ']'
                           | (y:ys) <- z = showChar ',' . showsJSON' y . showsJsonArray' ys 
                           | [] <- z = showChar ',' . showChar ']' 
                           | otherwise = trace "showsJsonArray' impossible patter" undefined

-- | Show an association list in JSON format
showsJsonObject :: Jmap -> ShowS
showsJsonObject m = showChar '{' . tail . showsJsonObject' (mtl m) 
  where showsJsonObject' z | ( (k,v):[]) <- z = showsJsonPair k v . showChar '}'
                           | ( (k,v):ys) <- z = showsJsonPair k v . showsJsonObject' ys
                           | [] <- z = showChar ',' . showChar '}'
                           | otherwise = trace "showsJsonObject' impossible pattern" undefined

        showsJsonPair :: Jstr -> JSON -> ShowS
        showsJsonPair k v = showChar ',' . showsJsonString k . showChar ':' . showsJSON' v

-- ===================================================
--
{-|
Functions to mechanically derive 'JSONic' instances. Note that
you need to enable the @TemplateHaskell@ language extension in order to use this
module.

An example shows how instances are generated for arbitrary data types. First we
define a data type:

@
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq
@

Next we derive the necessary instances. Note that we make use of the
feature to change record field names. In this case we drop the first 4
characters of every field name. We also modify constructor names by
lower-casing them:

@
$('deriveJSON' 'defaultJsonicOptions'{'jsonicFieldLabelModifier' = 'drop' 4, 'jsonicConstructorTagModifier' = map toLower} ''D)
@

Now we can use the newly created instances.

@
d :: D 'Int'
d = Record { testOne = 3.14159
           , testTwo = 'True'
           , testThree = Product \"test\" \'A\' 123
           }
@

>>> fromJSON (toJSON d) == Right d
> True

Please note that you can derive instances for tuples using the following syntax:

@
-- FromJSON and ToJSON instances for 4-tuples.
$('deriveJSON' 'defaultJsonicOptions' ''(,,,))
@

-}


{-
TODO: Don't constrain phantom type variables.

data Foo a = Foo Int
instance (JSONic a) ⇒ JSONic Foo where ...

The above (JSONic a) constraint is not necessary and perhaps undesirable.
-}


headNArgs :: Type -> (Name, [Type])
headNArgs t = let (a,x) = grabit t in (a, reverse x)
  where grabit (AppT b c) = let (d, e) = grabit b in (d, c : e)
        grabit (ConT p) = (p, [])
        grabit _n = trace ("dont know how to handle " ++ show _n) undefined

-- | Generates a 'JSONic' instance declaration for the given data type.
deriveJSONic :: JsonicOptions -- ^ Encoding options.
             -> Name -- ^ Name of the type for which to generate a 'JSONic' instance declaration.
             -> Q [Dec]
deriveJSONic opts name =
    withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (applyCon ''JSONic typeNames)
                  ( (conT ''JSONic) `appT` instanceType)
                  [ funD 'toJSON
                         [ clause []
                                  (normalB $ consToJSON opts cons)
                                  []
                         ],
                    funD 'fromJSON
                         [ clause []
                                  (normalB $ consFromJSON name opts cons)
                                  []
                         ]
                  ]

      where
        typeNames = map tvbName tvbs
        instanceType = foldl' appT (conT name) $ map varT typeNames


deriveJSONicT :: JsonicOptions -- ^ Encoding options.
              -> Q Type -- ^ Type for which to generate a 'JSONic' instance declaration.
              -> Q [Dec]
deriveJSONicT _opts typ = do
        (tn, _tc) <- fmap headNArgs typ
        _inf <- reify tn
        fmap (:[]) $ instanceD (cxt [])
                  ( (conT ''JSONic) `appT` typ)
                  []
{-
                  [ funD 'toJSON
                         [ clause []
                                  (normalB $ consToJSON opts cons)
                                  []
                         ],
                    funD 'fromJSON
                         [ clause []
                                  (normalB $ consFromJSON name opts cons)
                                  []
                         ]
                  ]
-}

-- | Helper function used by both 'deriveToJSON' and 'mkToJSON'. Generates code
-- to generate the JSON encoding of a number of constructors. All constructors
-- must be from the same type.
consToJSON :: JsonicOptions
           -- ^ Encoding options.
           -> [Con]
           -- ^ Constructors for which to generate JSON generating code.
           -> Q Exp

consToJSON _ [] = error $ "Data.JSONic.TH.consToJSON: "
                          ++ "Not a single constructor given!"

-- A single constructor is directly encoded. The constructor itself may be
-- forgotten.
consToJSON opts [con] = do
    value <- newName "value"
    lam1E (varP value) $ caseE (varE value) [encodeArgs opts False con]

consToJSON opts cons = do
    value <- newName "value"
    lam1E (varP value) $ caseE (varE value) matches
  where
    matches
        | jsonicAllNullaryToStringTag opts && all isNullary cons =
              [ match (conP conName []) (normalB $ conStr opts conName) []
              | con <- cons
              , let conName = getConName con
              ]
        | otherwise = [encodeArgs opts True con | con <- cons]

conStr :: JsonicOptions -> Name -> Q Exp
conStr opts = appE [|JsonString|] . conTxt opts

conTxt :: JsonicOptions -> Name -> Q Exp
conTxt opts = appE [|tpack|] . conStringE opts

conStringE :: JsonicOptions -> Name -> Q Exp
conStringE opts = stringE . jsonicConstructorTagModifier opts . nameBase

-- | If constructor is nullary.
isNullary :: Con -> Bool
isNullary (NormalC _ []) = True
isNullary _ = False

encodeSum :: JsonicOptions -> Bool -> Name -> Q Exp -> Q Exp
encodeSum opts multiCons conName exp2 
    | multiCons =
        case jsonicSumEncoding opts of
          TwoElemArray ->
              [|JsonArray|] `appE` (listE [conStr opts conName, exp2])
          TaggedObject{jsonicTagFieldName=tf, jsonicContentsFieldName=cf} ->
              [|JsonObject . mfl|] `appE` listE
                [ infixApp [|tpack tf|]     [|(.=)|] (conStr opts conName)
                , infixApp [|tpack cf|] [|(.=)|] exp2
                ]
          ObjectWithSingleField ->
              [|JsonObject . mfl|] `appE` listE
                [ infixApp (conTxt opts conName) [|(.=)|] exp2 ]

    | otherwise = exp2

-- | Generates code to generate the JSON encoding of a single constructor.
encodeArgs :: JsonicOptions -> Bool -> Con -> Q Match
-- Nullary constructors. Generates code that explicitly matches against the
-- constructor even though it doesn't contain data. This is useful to prevent
-- type errors.
encodeArgs  opts multiCons (NormalC conName []) =
    match (conP conName [])
          (normalB (encodeSum opts multiCons conName [e|toJSON ([] :: [()])|]))
          []

-- Polyadic constructors with special case for unary constructors.
encodeArgs opts multiCons (NormalC conName ts) = do
    let len = length ts
    args <- mapM newName ["arg" ++ show n | n <- [1..len]]
    js <- case [[|toJSON|] `appE` varE arg | arg <- args] of
            -- Single argument is directly converted.
            [e] -> return e
            es -> do 
                     _ <- do 
                        _p <- mapM runQ es
                        return $ trace (pprint _p) [|()|]
            -- Multiple arguments are converted to a JSON array.
                     return $ [|JsonArray|] `appE` listE es
{-
            es  -> do
              mv <- newName "mv"
              let newMV = bindS (varP mv)
                                ([|VM.unsafeNew|] `appE`
                                  litE (integerL $ fromIntegral len))
                  stmts = [ noBindS $
                              [|VM.unsafeWrite|] `appE`
                                (varE mv) `appE`
                                  litE (integerL ix) `appE`
                                    e
                          | (ix, e) <- zip [(0::Integer)..] es
                          ]
                  ret = noBindS $ [|return|] `appE` varE mv
              return $ [|Array|] `appE`
                         (varE 'V.create `appE`
                           doE (newMV:stmts++[ret]))
-}
    match (conP conName $ map varP args)
          (normalB $ encodeSum opts multiCons conName js)
          []

-- Records.
encodeArgs opts multiCons (RecC conName ts) = do
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip ts [1 :: Integer ..]]
    let exp2 = [|JsonObject . mfl|] `appE` pairs

        pairs | jsonicOmitNothingFields opts = infixApp maybeFields
                                                  [|(++)|]
                                                  restFields
              | otherwise = listE $ map toPair argCons

        argCons = zip args ts

        maybeFields = [|catMaybes|] `appE` listE (map maybeToPair maybes)

        restFields = listE $ map toPair rest

        (maybes, rest) = partition isMaybe argCons

        isMaybe (_, (_, _, AppT (ConT t) _)) = t == ''Maybe
        isMaybe _ = False

        maybeToPair (arg, (field, _, _)) =
            infixApp (infixE (Just $ toFieldName field)
                             [|(.=)|]
                             Nothing)
                     [|(<$>)|]
                     (varE arg)

        toPair (arg, (field, _, _)) =
            infixApp (toFieldName field)
                     [|(.=)|]
                     (varE arg)

        toFieldName field = [|tpack|] `appE` fieldLabelExp opts field

    match (conP conName $ map varP args)
          ( normalB
          $ if multiCons
            then case jsonicSumEncoding opts of
                   TwoElemArray -> [|toJSON|] `appE` tupE [conStr opts conName, exp2]
                   TaggedObject{jsonicTagFieldName=tf} ->
                       [|JsonObject . mfl|] `appE`
                         -- TODO: Maybe throw an error in case
                         -- tagFieldName overwrites a field in pairs.
                         infixApp (infixApp [|tpack tf|]
                                            [|(.=)|]
                                            (conStr opts conName))
                                  [|(:)|]
                                  pairs
                   ObjectWithSingleField ->
                       [|JsonObject . mfl|] `appE` listE
                         [ infixApp (conTxt opts conName) [|(.=)|] exp2 ]
            else exp2
          ) []

-- Infix constructors.
encodeArgs opts multiCons (InfixC _ conName _) = do
    al <- newName "argL"
    ar <- newName "argR"
    match (infixP (varP al) conName (varP ar))
          ( normalB
          $ encodeSum opts multiCons conName
          $ [|toJSON|] `appE` listE [ [|toJSON|] `appE` varE a
                                    | a <- [al,ar]
                                    ]
          )
          []
-- Existentially quantified constructors.
encodeArgs opts multiCons (ForallC _ _ con) = encodeArgs opts multiCons con


--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

-- | Helper function used by both 'deriveFromJSON' and 'mkParseJSON'. Generates
-- code to parse the JSON encoding of a number of constructors. All constructors
-- must be from the same type.
consFromJSON :: Name
             -- ^ Name of the type to which the constructors belong.
             -> JsonicOptions
             -- ^ Encoding options
             -> [Con]
             -- ^ Constructors for which to generate JSON parsing code.
             -> Q Exp

consFromJSON _ _ [] = error $ "Data.JSONic.TH.consFromJSON: "
                              ++ "Not a single constructor given!"

consFromJSON tName opts [con] = do
  value <- newName "value"
  lam1E (varP value) (parseArgs tName opts con (Right value))

consFromJSON tName opts cons = do
  value <- newName "value"
  lam1E (varP value) $ caseE (varE value) $
    if jsonicAllNullaryToStringTag opts && all isNullary cons
    then allNullaryMatches
    else mixedMatches

  where
    allNullaryMatches =
      [ do txt <- newName "txt"
           match (conP 'JsonString [varP txt])
                 (guardedB $
                  [ liftM2 (,) (normalG $
                                  infixApp (varE txt)
                                           [|(==)|]
                                           ([|tpack|] `appE`
                                              conStringE opts conName)
                               )
                               ([|pure|] `appE` conE conName)
                  | con <- cons
                  , let conName = getConName con
                  ]
                  ++
                  [ liftM2 (,)
                      (normalG [|otherwise|])
                      ( [|noMatchFail|]
                        `appE` (litE $ stringL $ show tName)
                        `appE` ([|T.unpack|] `appE` varE txt)
                      )
                  ]
                 )
                 []
      , do other <- newName "other"
           match (varP other)
                 (normalB $ [|noStringFail|]
                    `appE` (litE $ stringL $ show tName)
                    `appE` ([|valueConName|] `appE` varE other)
                 )
                 []
      ]

    mixedMatches =
        case jsonicSumEncoding opts of
          TaggedObject {jsonicTagFieldName=tf, jsonicContentsFieldName=cf} ->
            parseObject $ parseTaggedObject tf cf
          ObjectWithSingleField ->
            parseObject $ parseObjectWithSingleField
          TwoElemArray ->
            [ do arr <- newName "array"
                 match (conP 'JsonArray [varP arr])
                       (guardedB $
                        [ liftM2 (,) (normalG $ infixApp ([|length|] `appE` varE arr)
                                                         [|(==)|]
                                                         (litE $ integerL 2))
                                     (parse2ElemArray arr)
                        , liftM2 (,) (normalG [|otherwise|])
                                     (([|not2ElemArray|]
                                       `appE` (litE $ stringL $ show tName)
                                       `appE` ([|length|] `appE` varE arr)))
                        ]
                       )
                       []
            , do other <- newName "other"
                 match (varP other)
                       ( normalB
                         $ [|noArrayFail|]
                             `appE` (litE $ stringL $ show tName)
                             `appE` ([|valueConName|] `appE` varE other)
                       )
                       []
            ]

    parseObject f =
        [ do obj <- newName "obj"
             match (conP 'JsonObject [varP obj]) (normalB $ f obj) []
        , do other <- newName "other"
             match (varP other)
                   ( normalB
                     $ [|noObjectFail|]
                         `appE` (litE $ stringL $ show tName)
                         `appE` ([|valueConName|] `appE` varE other)
                   )
                   []
        ]

    parseTaggedObject typFieldName valFieldName obj = do
      conKey <- newName "conKey"
      doE [ bindS (varP conKey) 
-- actual should be  case M.lookup key obj of  { Nothing -> pure Nothing; Just v -> parseJSON v } 
-- better known as   fmap parseJSON (M.lookup key obj) 
                  ( [|mlookup|] `appE` ([|tpack|] `appE` stringE typFieldName) `appE` (varE obj) )
          , noBindS $ parseContents conKey (Left (valFieldName, obj)) 'conNotFoundFailTaggedObject
          ]

    parse2ElemArray arr = do
      conKey <- newName "conKey"
      conVal <- newName "conVal"
      let letIx n ix =
              valD (varP n)
                   (normalB ( infixApp (varE arr)
                                       [|(!!)|] 
                                       (litE (integerL ix))))
                   []
      letE [ letIx conKey 0
           , letIx conVal 1
           ]
           (caseE (varE conKey)
                  [ do txt <- newName "txt"
                       match (conP 'JsonString [varP txt])
                             (normalB $ parseContents txt
                                                      (Right conVal)
                                                      'conNotFoundFail2ElemArray
                             )
                             []
                  , do other <- newName "other"
                       match (varP other)
                             ( normalB
                               $ [|firstElemNoStringFail|]
                                     `appE` (litE $ stringL $ show tName)
                                     `appE` ([|valueConName|] `appE` varE other)
                             )
                             []
                  ]
           )

    parseObjectWithSingleField obj = do
      conKey <- newName "conKey"
      conVal <- newName "conVal"
      caseE ([e|M.toList|] `appE` varE obj)
            [ match (listP [tupP [varP conKey, varP conVal]])
                    (normalB $ parseContents conKey (Right conVal) 'conNotFoundFailObjectSingleField)
                    []
            , do other <- newName "other"
                 match (varP other)
                       (normalB $ [|wrongPairCountFail|]
                                  `appE` (litE $ stringL $ show tName)
                                  `appE` ([|show . length|] `appE` varE other)
                       )
                       []
            ]

    parseContents conKey contents errorFun =
        caseE (varE conKey)
              [ match wildP
                      ( guardedB $
                        [ do g <- normalG $ infixApp (varE conKey)
                                                     [|(==)|]
                                                     ([|tpack|] `appE`
                                                        conNameExp opts con)
                             e <- parseArgs tName opts con contents
                             return (g, e)
                        | con <- cons
                        ]
                        ++
                        [ liftM2 (,)
                                 (normalG [e|otherwise|])
                                 ( varE errorFun
                                   `appE` (litE $ stringL $ show tName)
                                   `appE` listE (map ( litE
                                                     . stringL
                                                     . jsonicConstructorTagModifier opts
                                                     . nameBase
                                                     . getConName
                                                     ) cons
                                                )
                                   `appE` ([|T.unpack|] `appE` varE conKey)
                                 )
                        ]
                      )
                      []
              ]

parseNullaryMatches :: Name -> Name -> [Q Match]
parseNullaryMatches tName conName =
    [ do arr <- newName "arr"
         match (conP 'JsonArray [varP arr])
               (guardedB $
                [ liftM2 (,) (normalG $ [|null|] `appE` varE arr)
                             ([|pure|] `appE` conE conName)
                , liftM2 (,) (normalG [|otherwise|])
                             (parseTypeMismatch tName conName
                                (litE $ stringL "an empty JsonArray")
                                (infixApp (litE $ stringL $ "JsonArray of length ")
                                          [|(++)|]
                                          ([|show . length|] `appE` varE arr)
                                )
                             )
                ]
               )
               []
    , matchFailed tName conName "JsonArray"
    ]

parseUnaryMatches :: Name -> [Q Match]
parseUnaryMatches conName =
    [ do arg <- newName "arg"
         match (varP arg)
               ( normalB $ infixApp (conE conName)
                                    [|(<$>)|]
                                    ([|fromJSON|] `appE` varE arg)
               )
               []
    ]

parseRecord :: JsonicOptions -> Name -> Name -> [VarStrictType] -> Name -> ExpQ
parseRecord opts tName conName ts obj =
    foldl' (\a b -> infixApp a [|(<*>)|] b)
           (infixApp (conE conName) [|(<$>)|] x)
           xs
    where
      x:xs = [ [|lookupField|]
               `appE` (litE $ stringL $ show tName)
               `appE` (litE $ stringL $ jsonicConstructorTagModifier opts $ nameBase conName)
               `appE` (varE obj)
               `appE` ( [|tpack|] `appE` fieldLabelExp opts field
                      )
             | (field, _, _) <- ts
             ]

getValField :: Name -> String -> [MatchQ] -> Q Exp
getValField obj valFieldName matches = do
  val <- newName "val"
  doE [ bindS (varP val) $ [|mlookup|] `appE` (varE obj) `appE`
                                    ([|tpack|] `appE`
                                       (litE $ stringL valFieldName))
      , noBindS $ caseE (varE val) matches
      ]

-- | Generates code to parse the JSON encoding of a single constructor.
parseArgs :: Name -- ^ Name of the type to which the constructor belongs.
          -> JsonicOptions -- ^ Encoding options.
          -> Con -- ^ Constructor for which to generate JSON parsing code.
          -> Either (String, Name) Name -- ^ Left (valFieldName, objName) or
                                        --   Right valName
          -> Q Exp
-- Nullary constructors.
parseArgs tName _ (NormalC conName []) (Left (valFieldName, obj)) =
  getValField obj valFieldName $ parseNullaryMatches tName conName
parseArgs tName _ (NormalC conName []) (Right valName) =
  caseE (varE valName) $ parseNullaryMatches tName conName

-- Unary constructors.
parseArgs _ _ (NormalC conName [_]) (Left (valFieldName, obj)) =
  getValField obj valFieldName $ parseUnaryMatches conName
parseArgs _ _ (NormalC conName [_]) (Right valName) =
  caseE (varE valName) $ parseUnaryMatches conName

-- Polyadic constructors.
parseArgs tName _ (NormalC conName ts) (Left (valFieldName, obj)) =
    getValField obj valFieldName $ parseProduct tName conName $ genericLength ts
parseArgs tName _ (NormalC conName ts) (Right valName) =
    caseE (varE valName) $ parseProduct tName conName $ genericLength ts

-- Records.
parseArgs tName opts (RecC conName ts) (Left (_, obj)) =
    parseRecord opts tName conName ts obj
parseArgs tName opts (RecC conName ts) (Right valName) = do
  obj <- newName "recObj"
  caseE (varE valName)
    [ match (conP 'JsonObject [varP obj]) (normalB $ parseRecord opts tName conName ts obj) []
    , matchFailed tName conName "JsonObject"
    ]

-- Infix constructors. Apart from syntax these are the same as
-- polyadic constructors.
parseArgs tName _ (InfixC _ conName _) (Left (valFieldName, obj)) =
    getValField obj valFieldName $ parseProduct tName conName 2
parseArgs tName _ (InfixC _ conName _) (Right valName) =
    caseE (varE valName) $ parseProduct tName conName 2

-- Existentially quantified constructors. We ignore the quantifiers
-- and proceed with the contained constructor.
parseArgs tName opts (ForallC _ _ con) contents =
    parseArgs tName opts con contents

-- | Generates code to parse the JSON encoding of an n-ary
-- constructor.
parseProduct :: Name -- ^ Name of the type to which the constructor belongs.
             -> Name -- ^ 'Con'structor name.
             -> Integer -- ^ 'Con'structor arity.
             -> [Q Match]
parseProduct tName conName numArgs =
    [ do arr <- newName "arr"
         -- List of: "parseJSON (arr `V.unsafeIndex` <IX>)"
         let x:xs = [ [|fromJSON|]
                      `appE`
                      infixApp (varE arr)
                               [|(!!)|]
                               (litE $ integerL ix)
                    | ix <- [0 .. numArgs - 1]
                    ]
         match (conP 'JsonArray [varP arr])
               (normalB $ condE ( infixApp ([|length|] `appE` varE arr)
                                           [|(==)|]
                                           (litE $ integerL numArgs)
                                )
                                ( foldl' (\a b -> infixApp a [|(<*>)|] b)
                                         (infixApp (conE conName) [|(<$>)|] x)
                                         xs
                                )
                                ( parseTypeMismatch tName conName
                                    (litE $ stringL $ "JsonArray of length " ++ show numArgs)
                                    ( infixApp (litE $ stringL $ "JsonArray of length ")
                                               [|(++)|]
                                               ([|show . length|] `appE` varE arr)
                                    )
                                )
               )
               []
    , matchFailed tName conName "JsonArray"
    ]


--------------------------------------------------------------------------------
-- Parsing errors
--------------------------------------------------------------------------------

matchFailed :: Name -> Name -> String -> MatchQ
matchFailed tName conName expected = do
  other <- newName "other"
  match (varP other)
        ( normalB $ parseTypeMismatch tName conName
                      (litE $ stringL expected)
                      ([|valueConName|] `appE` varE other)
        )
        []

parseTypeMismatch :: Name -> Name -> ExpQ -> ExpQ -> ExpQ
parseTypeMismatch tName conName expected actual =
    foldl appE
          [|parseTypeMismatch'|]
          [ litE $ stringL $ nameBase conName
          , litE $ stringL $ show tName
          , expected
          , actual
          ]

{-
newtype Hunh a = Hunh a
instance JSONic a => JSONic (Hunh a)
-}
{-
class JSONic a => LookupField a where
    lookupField :: String -> String -> M.Map Text JSON -> T.Text -> Either String a
-}

lookupField :: JSONic a => String -> String -> Jmap -> Jstr -> Either String a
lookupField tName rec obj key =
        traceShow ("lookupField", obj, key) $ case mlookup key obj of
          Nothing -> unknownFieldFail tName rec (trunpack key)
          Just v -> fromJSON v 

{-
instance JSONic a => LookupField (Hunh a) where
    lookupField tName rec obj key = 
        case mlookup key obj of
          Nothing -> unknownFieldFail tName rec (T.unpack key)
          Just v  -> case fromJSON v of
                       Left er -> Left er
                       Right (Hunh z) -> Right z
-}

{-
instance (JSONic a) => LookupField (Maybe a) where
    lookupField _ _ = M.lookup
-}

unknownFieldFail :: String -> String -> String -> Either String fail
unknownFieldFail tName rec key =
    Left $ printf "When parsing the record %s of type %s the key %s was not present."
                  rec tName key

noArrayFail :: String -> String -> Either String fail
noArrayFail t o = Left $ printf "When parsing %s expected JsonArray but got %s." t o

noObjectFail :: String -> String -> Either String fail
noObjectFail t o = Left $ printf "When parsing %s expected JsonObject but got %s." t o

firstElemNoStringFail :: String -> String -> Either String fail
firstElemNoStringFail t o = Left $ printf "When parsing %s expected an JsonArray of 2 elements where the first element is a String but got %s at the first element." t o

wrongPairCountFail :: String -> String -> Either String fail
wrongPairCountFail t n =
    Left $ printf "When parsing %s expected an JsonObject with a single tag/contents pair but got %s pairs."
                  t n

noStringFail :: String -> String -> Either String fail
noStringFail t o = Left $ printf "When parsing %s expected JsonString but got %s." t o

noMatchFail :: String -> String -> Either String fail
noMatchFail t o =
    Left $ printf "When parsing %s expected a JsonString with the tag of a constructor but got %s." t o

not2ElemArray :: String -> Int -> Either String fail
not2ElemArray t i = Left $ printf "When parsing %s expected an JsonArray of 2 elements but got %i elements" t i

conNotFoundFail2ElemArray :: String -> [String] -> String -> Either String fail
conNotFoundFail2ElemArray t cs o =
    Left $ printf "When parsing %s expected a 2-element JsonArray with a tag and contents element where the tag is one of [%s], but got %s."
                  t (intercalate ", " cs) o

conNotFoundFailObjectSingleField :: String -> [String] -> String -> Either String fail
conNotFoundFailObjectSingleField t cs o =
    Left $ printf "When parsing %s expected an JsonObject with a single tag/contents pair where the tag is one of [%s], but got %s."
                  t (intercalate ", " cs) o

conNotFoundFailTaggedObject :: String -> [String] -> String -> Either String fail
conNotFoundFailTaggedObject t cs o =
    Left $ printf "When parsing %s expected an Object with a tag field where the value is one of [%s], but got %s."
                  t (intercalate ", " cs) o

parseTypeMismatch' :: String -> String -> String -> String -> Either String fail
parseTypeMismatch' tName conName expected actual =
    Left $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                  conName tName expected actual


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Boilerplate for top level splices.
--
-- The given 'Name' must be from a type constructor. Furthermore, the
-- type constructor must be either a data type or a newtype. Any other
-- value will result in an exception.
withType :: Name
         -> ([TyVarBndr] -> [Con] -> Q a)
         -- ^ Function that generates the actual code. Will be applied
         -- to the type variable binders and constructors extracted
         -- from the given 'Name'.
         -> Q a
         -- ^ Resulting value in the 'Q'uasi monad.
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
          DataD    _ _ tvbs cons _ -> f tvbs cons
          NewtypeD _ _ tvbs con  _ -> f tvbs [con]
          other -> error $ "Data.JSONic.TH.withType: Unsupported type: "
                          ++ show other
      _ -> error "Data.JSONic.TH.withType: I need the name of a type."

-- | Extracts the name from a constructor.
getConName :: Con -> Name
getConName (NormalC name _)  = name
getConName (RecC name _)     = name
getConName (InfixC _ name _) = name
getConName (ForallC _ _ con) = getConName con

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name

-- | Makes a string literal expression from a constructor's name.
conNameExp :: JsonicOptions -> Con -> Q Exp
conNameExp opts = litE
                . stringL
                . jsonicConstructorTagModifier opts
                . nameBase
                . getConName

-- | Creates a string literal expression from a record field label.
fieldLabelExp :: JsonicOptions -- ^ Encoding options
              -> Name
              -> Q Exp
fieldLabelExp opts = litE . stringL . jsonicFieldLabelModifier opts . nameBase

-- | The name of the outermost 'JSON' constructor.
valueConName :: JSON -> String
valueConName (JsonObject _) = "JsonObject"
valueConName (JsonArray  _) = "JsonArray"
valueConName (JsonString _) = "JsonString"
valueConName (JsonNumber _) = "JsonNumber"
valueConName (JsonBool   _) = "JsonBool"
valueConName JsonNull       = "JsonNull"

applyCon :: Name -> [Name] -> Q [Pred]
applyCon con typeNames = return (map apply typeNames)
  where apply t =

#if MIN_VERSION_template_haskell(2,10,0)
          AppT (ConT con) (VarT t)
#else
          ClassP con [VarT t]
#endif



--------------------------------------------------------------------------------
-- TH encoding configuration
--------------------------------------------------------------------------------

-- | Options that specify how to encode\/decode your datatype to\/from JSON.
data JsonicOptions = JsonicOptions
    { jsonicFieldLabelModifier :: String -> String
      -- ^ Function applied to field labels.
      -- Handy for removing common record prefixes for example.
    , jsonicConstructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags which could be handy
      -- for lower-casing them for example.
    , jsonicAllNullaryToStringTag :: Bool
      -- ^ If 'True' the constructors of a datatype, with /all/
      -- nullary constructors, will be encoded to just a string with
      -- the constructor tag. If 'False' the encoding will always
      -- follow the `sumEncoding`.
    , jsonicOmitNothingFields :: Bool
      -- ^ If 'True' record fields with a 'Nothing' JSON will be
      -- omitted from the resulting object. If 'False' the resulting
      -- object will include those fields mapping to @null@.
    , jsonicSumEncoding :: JsonicSumEncoding
      -- ^ Specifies how to encode constructors of a sum datatype.
    }

-- | Specifies how to encode constructors of a sum datatype.
data JsonicSumEncoding =
    TaggedObject { jsonicTagFieldName      :: String
                 , jsonicContentsFieldName :: String
                 }
    -- ^ A constructor will be encoded to an object with a field
    -- 'tagFieldName' which specifies the constructor tag (modified by
    -- the 'constructorTagModifier'). If the constructor is a record
    -- the encoded record fields will be unpacked into this object. So
    -- make sure that your record doesn't have a field with the same
    -- label as the 'tagFieldName'. Otherwise the tag gets overwritten
    -- by the encoded JSON of that field! If the constructor is not a
    -- record the encoded constructor contents will be stored under
    -- the 'contentsFieldName' field.
  | ObjectWithSingleField
    -- ^ A constructor will be encoded to an object with a single
    -- field named after the constructor tag (modified by the
    -- 'constructorTagModifier') which maps to the encoded contents of
    -- the constructor.
  | TwoElemArray
    -- ^ A constructor will be encoded to a 2-element array where the
    -- first element is the tag of the constructor (modified by the
    -- 'constructorTagModifier') and the second element the encoded
    -- contents of the constructor.

-- | Default encoding 'Options':
--
-- @
-- 'Options'
-- { 'fieldLabelModifier'      = id
-- , 'constructorTagModifier'  = id
-- , 'allNullaryToStringTag'   = True
-- , 'omitNothingFields'       = False
-- , 'sumEncoding'             = 'defaultTaggedObject'
-- }
-- @
defaultJsonicOptions :: JsonicOptions
defaultJsonicOptions = JsonicOptions
                 { jsonicFieldLabelModifier      = id
                 , jsonicConstructorTagModifier  = id
                 , jsonicAllNullaryToStringTag   = True
                 , jsonicOmitNothingFields       = False
                 , jsonicSumEncoding             = defaultTaggedObject
                 }

-- | Default 'TaggedObject' 'SumEncoding' options:
--
-- @
-- defaultTaggedObject = 'TaggedObject'
--                       { 'tagFieldName'      = \"tag\"
--                       , 'contentsFieldName' = \"contents\"
--                       }
-- @
defaultTaggedObject :: JsonicSumEncoding
defaultTaggedObject = TaggedObject
                      { jsonicTagFieldName      = "tag"
                      , jsonicContentsFieldName = "contents"
                      }

-- | Converts from CamelCase to another lower case, interspersing
--   the character between all capital letters and their previous
--   entries, except those capital letters that appear together,
--   like 'API'.
--
--   For use by Aeson template haskell calls.
--
--   > camelTo '_' 'CamelCaseAPI' == "camel_case_api"
camelTo :: Char -> String -> String
camelTo c = lastWasCap True
  where
    lastWasCap :: Bool    -- ^ Previous was a capital letter
              -> String  -- ^ The remaining string
              -> String
    lastWasCap _    []           = []
    lastWasCap prev (x : xs)     = if isUpper x
                                      then if prev
                                             then toLower x : lastWasCap True xs
                                             else c : toLower x : lastWasCap True xs
                                      else x : lastWasCap False xs


-----------------------------------------------------------------------

