{-# LANGUAGE CPP, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

{-# Language OverloadedStrings, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module TestJson (tests) where

import Distribution.TestSuite (Progress(..), Result(..), Test(..), TestInstance(..), testGroup)

import Preface.R0ml
import Control.Monad (forM, unless)
import Data.Char (toUpper, toLower)
import Test.QuickCheck.Property (Testable)

import Data.Maybe (isJust)
import qualified Test.QuickCheck as Q
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable
import Control.Applicative (empty)

import qualified Data.Map as M
import Data.Data

import Debug.Trace

import Control.Exception

import Data.Function (on)
import Test.QuickCheck (Arbitrary(..), Gen, choose, oneof, elements)
import Data.Time.Clock (DiffTime, UTCTime(..), picosecondsToDiffTime)
import Data.Time (ZonedTime(..), LocalTime(..), TimeZone(..),
                  hoursToTimeZone, Day(..), TimeOfDay(..))

type Assertion = IO ()

data AssertError = AssertError String deriving (Show, Eq)
instance Exception AssertError



assertFailure :: String -> IO Q.Result
assertFailure msg = do
   -- print msg 
   return Q.Failure { numTests=1, numShrinks=0, numShrinkTries=0, numShrinkFinal=0,
                    output=msg } -- (AssertError msg)

assertEqual preface expected actual =
  if actual == expected then return $ Q.Success 1 [] msg else assertFailure msg
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual


roundTripCamel :: String -> IO Q.Result -- Assertion
roundTripCamel name = assertEqual "" name (camelFrom '_' $ camelTo '_' name)
  where
    camelFrom c s = let (p:ps) = split c s
                    in concat $ p : map capitalize ps
    split c s = map L.unpack $ L.split c $ L.pack s
    capitalize t = toUpper (head t) : tail t

encodeDouble :: Double -> Double -> Bool
encodeDouble num denom
    | isInfinite d || isNaN d = (show . toJSON) d == "null"
    | otherwise               = (read . show . toJSON) d == d
  where d =  num / denom 

encodeInteger :: Integer -> Bool
encodeInteger i = show i == (show i)

roundTrip :: (JSONic a, Show a) => (a -> a -> Bool) -> a -> a -> Bool
roundTrip eq _ i = 
    case readJSON (show (toJSON i)) of
      Left m -> trace m False
      Right js -> case fromJSON js of
                     Left m -> trace m False
                     Right obj -> if obj `eq` i then True else traceShow (i, toJSON i) False

roundTripEq :: (Eq a, Show a, JSONic a) => a -> a -> Bool
roundTripEq x y = roundTrip (==) x y

toFromJSON :: (Arbitrary a, Eq a, JSONic a) => a -> Bool
toFromJSON x = case fromJSON (toJSON x) of
                Left _ -> False
                Right x' -> x == x'

--------------------------------------------------------------------------------
-- Value properties
--------------------------------------------------------------------------------

isString :: JSON -> Bool
isString (JsonString _) = True
isString _          = False

is2ElemArray :: JSON -> Bool
is2ElemArray (JsonArray v) = length v == 2 && isString (head v)
is2ElemArray _         = False

isTaggedObjectValue :: JSON -> Bool
-- GOOD ONE!
isTaggedObjectValue obj =  ( "tag" `jsonElemQ` obj) && ( "contents" `jsonElemQ` obj )
-- isTaggedObjectValue = ap ( (&&) . isJust . (.? "tag")) . isJust . (.? "contents") 

isTaggedObject :: JSON -> Bool
isTaggedObject = (jsonElemQ "tag" )

isObjectWithSingleField :: JSON -> Bool
isObjectWithSingleField (JsonObject obj) = length obj == 1
isObjectWithSingleField _            = False

--------------------------------------------------------------------------------
type Clem = Either Integer Double
-- $(deriveJSONicT defaultJsonicOptions [t|Either Integer Double|])

instance JSONic (Either Integer Double) where
  toJSON (Left x) = JsonObject $ mfl [("Left", JsonNumber (II x))]
  toJSON (Right x) = JsonObject $ mfl [("Right", JsonNumber (DD x))]
  fromJSON (JsonObject m) = let a = mlookup "Left" m
                                b = mlookup "Right" m
                             in case a of
                                  Just aa -> case fromJSON aa of
                                                 Left x -> Left x
                                                 Right x -> Right ( Left x )
                                  Nothing -> case b of 
                                               Nothing -> Left "Invalid 'Either'"
                                               Just bb -> case fromJSON bb of
                                                              Left x -> Left x
                                                              Right x -> Right ( Right x)
  fromJSON x = traceShow ("id ", typeOf x, x) (error "clem")

type Flem = Either Integer Integer
-- $(deriveJSONicT defaultJsonicOptions [t| Either Integer Integer |])
instance JSONic (Either Integer Integer) where
  toJSON (Left x) = JsonObject $ mfl [("Left", JsonNumber (II x))]
  toJSON (Right x) = JsonObject $ mfl [("Right", JsonNumber (II x))]
  fromJSON (JsonObject m) = let a = mlookup "Left" m
                                b = mlookup "Right" m
                             in case a of
                                  Just aa -> case fromJSON aa of
                                                     Left x -> Left x
                                                     Right x -> Right (Left x)
                                  Nothing -> case b of 
                                               Nothing -> Left "Invalid 'Either'"
                                               Just bb -> case fromJSON bb of
                                                                 Left x -> Left x
                                                                 Right x -> Right (Right x)
  fromJSON x = traceShow ("ii ",typeOf x, x) (error "err")

ff d = do
  z <- try d
  case z of 
    Left x -> return $ Finished (Fail (show (x :: SomeException ) ))
    Right x -> return $ case x of 
                    Q.Success _ _ _ -> Finished Pass 
                    Q.Failure { output=z } -> Finished (Fail z)

testCase :: String -> (IO Q.Result ) -> Test
testCase n d = Test $ TestInstance (ff d)  n [] [] (\x y -> Left "not implmeneted")


testProperty name = testCase name . Q.quickCheckResult 
data Property = forall a. Testable a => Property a
    deriving Typeable

tests :: IO [Test]
tests = return [
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ],
   testGroup "camelCase" [
      testCase "camelTo aName" $ roundTripCamel "aName"
    , testCase "camelTo another" $ roundTripCamel "another"
    , testCase "camelTo someOtherName" $ roundTripCamel "someOtherName"
    ]
  ,
  testGroup "roundTrip" [
      testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTripEq (1 :: Approx Double)
--    , testProperty "Int" $ roundTripEq (1::Int)
    , testProperty "Integer" $ roundTripEq (1::Integer)
    , testProperty "String" $ roundTripEq (""::String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Foo" $ roundTripEq ( Foo 0 0 ("", "", 0)  M.empty ) -- undefined::Foo)
--    , testProperty "UTCTime" $ roundTripEq (undefined :: UTCTime)
--    , testProperty "ZonedTime" $ roundTripEq (undefined::ZonedTime)
    ]
  ,
  testGroup "toFromJSON" [
      testProperty "Integer" (toFromJSON :: Integer -> Bool)
    , testProperty "Double" (toFromJSON :: Double -> Bool)
    , testProperty "Maybe Integer" (toFromJSON :: Maybe Integer -> Bool)
    , testProperty "Either Integer Double" (toFromJSON :: Clem -> Bool)
    , testProperty "Either Integer Integer" (toFromJSON :: Flem -> Bool)
    ]


{-
  , testGroup "template-haskell" [
      testGroup "Nullary" [
          testProperty "string"                (isString                . thNullaryToJSONString)
        , testProperty "2ElemArray"            (is2ElemArray            . thNullaryToJSON2ElemArray)
        , testProperty "TaggedObject"          (isTaggedObjectValue     . thNullaryToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thNullaryToJSONObjectWithSingleField)

        , testGroup "roundTrip" [
              testProperty "string"                (toParseJSON thNullaryParseJSONString                thNullaryToJSONString)
            , testProperty "2ElemArray"            (toParseJSON thNullaryParseJSON2ElemArray            thNullaryToJSON2ElemArray)
            , testProperty "TaggedObject"          (toParseJSON thNullaryParseJSONTaggedObject          thNullaryToJSONTaggedObject)
            , testProperty "ObjectWithSingleField" (toParseJSON thNullaryParseJSONObjectWithSingleField thNullaryToJSONObjectWithSingleField)
          ]
        ]
    , testGroup "SomeType" [
          testProperty "2ElemArray"            (is2ElemArray            . (thSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
        , testProperty "TaggedObject"          (isTaggedObject          . (thSomeTypeToJSONTaggedObject          :: SomeTypeToJSON))
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . (thSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))

        , testGroup "roundTrip" [
              testProperty "2ElemArray"            (toParseJSON thSomeTypeParseJSON2ElemArray            (thSomeTypeToJSON2ElemArray            :: SomeTypeToJSON))
            , testProperty "TaggedObject"          (toParseJSON thSomeTypeParseJSONTaggedObject          (thSomeTypeToJSONTaggedObject          :: SomeTypeToJSON))
            , testProperty "ObjectWithSingleField" (toParseJSON thSomeTypeParseJSONObjectWithSingleField (thSomeTypeToJSONObjectWithSingleField :: SomeTypeToJSON))
          ]
      ]

    ]
-}
  ]


------------------------------------------------------------------------------
-- Comparison between bytestring and text encoders
------------------------------------------------------------------------------
{-

encoderComparisonTests :: IO Test
encoderComparisonTests = do
    encoderTests <- forM testFiles $ \file0 -> do
        let file = "benchmarks/json-data/" ++ file0
        return $ testCase file $ do
            inp <- L.readFile file
            case eitherDecode inp of
              Left  err -> assertFailure $ "Decoding failure: " ++ err
              Right val -> assertEqual "" (encode val) (encodeViaText val)
    return $ testGroup "Compare bytestring and text encoders" encoderTests
  where
    encodeViaText :: JSON -> L.ByteString
    encodeViaText =
        TLE.encodeUtf8 . TLB.toLazyText . encodeToTextBuilder . toJSON

    testFiles =
      [ "example.json"
      , "integers.json"
      , "jp100.json"
      , "numbers.json"
      , "twitter10.json"
      , "twitter20.json"
      , "geometry.json"
      , "jp10.json"
      , "jp50.json"
      , "twitter1.json"
      , "twitter100.json"
      , "twitter50.json"
      ]

-}
{-
#if defined(DETAILED)
#else
main :: IO ()
main = do
    --    comparisonTest <- encoderComparisonTests
    defaultMain ( {- comparisonTest : -} tests)
#endif
-}

{-
 -
data Coord = Coord { x :: Double, y :: Double }
             deriving (Show)

-- A ToJSON instance allows us to encode a value as JSON.

instance JSONic Coord where
  toJSON (Coord xV yV) = JsonObject $ M.fromList [ "x" .= xV, "y" .= yV ]
  fromJSON v@(JsonObject o) = Right $ Coord (v .: "x")  ( v .: "y")
  fromJSON _          = Left "expecting a Coord, but did not get one"

coordExample :: IO ()
coordExample = do
  let req = either undefined id ( fromJSON (read "{\"x\":3.0,\"y\":-1.0}" :: JSON)) :: Coord
  print req
  let reply = Coord 123.4 20
  print (toJSON reply)



-- We can use Template Haskell (TH) to generate instances of the
-- FromJSON and ToJSON classes automatically.  This is the fastest way
-- to add JSON support for a type.


data Coord2 = Coord2 { x2 :: Double, y2 :: Double } deriving (Show)

-- This splice will derive instances of ToJSON and FromJSON for us.
--
-- The use of "id" below is a placeholder function to transform the
-- names of the type's fields.  We don't want to transform them, so we
-- use the identity function.

$(deriveJSONic defaultJsonicOptions ''Coord2)

coord2Example :: IO ()
coord2Example = do
  let req = either undefined id $ fromJSON (read "{\"x2\":3.0,\"y2\":-1.0}" :: JSON) :: Coord2
  print req
  let reply = Coord2 123.4 20
  print (toJSON reply)
-}

--------------------------------------------------------------------------------
-- Nullary encoders/decoders
--------------------------------------------------------------------------------


class ApproxEq a where
    (=~) :: a -> a -> Bool

newtype Approx a = Approx { fromApprox :: a }
    deriving (Show, Data, Typeable, ApproxEq, Num)

instance (ApproxEq a) => Eq (Approx a) where
    Approx a == Approx b = a =~ b

data Nullary = C1 | C2 | C3 deriving (Eq, Show)

data SomeType a = Nullary
                | Unary Int
                | Product String (Maybe Char) a
                | Record { testOne   :: Double
                         , testTwo   :: Maybe Bool
                         , testThree :: Maybe a
                         } deriving (Eq, Show)

data Foo = Foo {
      fooInt :: Int
    , fooDouble :: Double
    , fooTuple :: (String, Text, Int)
    -- This definition causes an infinite loop in genericTo and genericFrom!
    -- , fooMap :: Map String Foo
    , fooMap :: Map String (Text,Int)
    } deriving (Show, Typeable, Data)

data UFoo = UFoo {
      _UFooInt :: Int
    , uFooInt :: Int
    } deriving (Show, Eq, Data, Typeable)

data OneConstructor = OneConstructor
                      deriving (Show, Eq, Typeable, Data)

data Product2 a b = Product2 a b
                    deriving (Show, Eq, Typeable, Data)

data Product6 a b c d e f = Product6 a b c d e f
                    deriving (Show, Eq, Typeable, Data)

data Sum4 a b c d = Alt1 a | Alt2 b | Alt3 c | Alt4 d
                    deriving (Show, Eq, Typeable, Data)



-- $(deriveJSONic ''Nullary)

{-
thNullaryToJSONString :: Nullary -> JSON
thNullaryToJSONString = $(mkToJSON defaultJsonicOptions ''Nullary)

-- thNullaryParseJSONStringthNullaryParseJSONString :: JSON -> Parser Nullary
-- thNullaryParseJSONString = $(mkParseJSON defaultJsonicOptions ''Nullary)


thNullaryToJSON2ElemArray :: Nullary -> JSON
thNullaryToJSON2ElemArray = $(mkToJSON opts2ElemArray ''Nullary)

-- thNullaryParseJSON2ElemArray :: JSON -> Parser Nullary
-- thNullaryParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''Nullary)


thNullaryToJSONTaggedObject :: Nullary -> JSON
thNullaryToJSONTaggedObject = $(mkToJSON optsTaggedObject ''Nullary)

-- thNullaryParseJSONTaggedObject :: JSON -> Parser Nullary
-- thNullaryParseJSONTaggedObject = $(mkParseJSON optsTaggedObject ''Nullary)


thNullaryToJSONObjectWithSingleField :: Nullary -> JSON
thNullaryToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''Nullary)

-- thNullaryParseJSONObjectWithSingleField :: JSON -> Parser Nullary
-- thNullaryParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''Nullary)


--------------------------------------------------------------------------------
-- SomeType encoders/decoders
--------------------------------------------------------------------------------

type SomeTypeToJSON = SomeType Int -> JSON

thSomeTypeToJSON2ElemArray :: JSONic a => SomeType a -> JSON
thSomeTypeToJSON2ElemArray = $(mkToJSON opts2ElemArray ''SomeType)

thSomeTypeParseJSON2ElemArray :: JSONic a => JSON -> Parser (SomeType a)
thSomeTypeParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''SomeType)


thSomeTypeToJSONTaggedObject :: JSONic a => SomeType a -> JSON
thSomeTypeToJSONTaggedObject = $(mkToJSON optsTaggedObject ''SomeType)


thSomeTypeToJSONObjectWithSingleField :: JSONic a => SomeType a -> JSON
thSomeTypeToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''SomeType)

-- thSomeTypeParseJSONObjectWithSingleField :: FromJSON a => JSON -> Parser (SomeType a)
-- thSomeTypeParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''SomeType)

-}

-- "System" types.

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
    arbitrary = M.fromList <$> arbitrary

instance Arbitrary LocalTime where
    arbitrary = return $ LocalTime (ModifiedJulianDay 1) (TimeOfDay 1 2 3)

instance Arbitrary TimeZone where
    arbitrary = do
      offset <- choose (0,2) :: Gen Int
      return $ hoursToTimeZone offset

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay `liftM` arbitrary

instance Arbitrary DiffTime where
    arbitrary = (picosecondsToDiffTime . (* 1000000000)) <$>
                choose (0, 86400000)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary

instance Arbitrary ZonedTime where
    arbitrary = liftM2 ZonedTime arbitrary arbitrary

deriving instance Eq ZonedTime

-- Compare equality to within a millisecond, allowing for rounding
-- error (ECMA 262 requires milliseconds to rounded to zero, not
-- rounded to nearest).
instance ApproxEq UTCTime where
    a =~ b = ((==) `on` utctDay) a b &&
             (approxEqWith 1 1 `on` ((* 1e3) . utctDayTime)) a b

instance ApproxEq Double where
    (=~) = approxEq

-- Test-related types.

instance Arbitrary Foo where
    arbitrary = liftM4 Foo arbitrary arbitrary arbitrary arbitrary

instance JSONic (String, Text, Int) where
  toJSON (x,y,z) = JsonArray $ [toJSON x, toJSON y, toJSON (fromIntegral z :: Integer) ]
  fromJSON (JsonArray (x:y:z:[] ) ) = Right (either undefined id $ fromJSON x, either undefined id $ fromJSON y, fromIntegral $ ( either undefined id $ fromJSON z :: Integer)  )

instance JSONic (Text, Int) where
  toJSON (y,z) = JsonArray [toJSON y, toJSON (fromIntegral z :: Integer) ]
  fromJSON (JsonArray (y:z:[] ) ) = Right (either undefined id $ fromJSON y, fromIntegral $ ( either undefined id $ fromJSON z :: Integer)  )

instance Eq Foo where
    a == b = fooInt a == fooInt b &&
             fooDouble a `approxEq` fooDouble b &&
             fooTuple a == fooTuple b

mlk x y = case mlookup x y of { Nothing -> undefined {- Left $ "not found: " ++ x -}; Just v -> case fromJSON v of { Left x -> undefined; Right x -> x } }

instance JSONic Foo where
    toJSON Foo{..} = JsonObject $ mfl [ ("fooInt" .= (fromIntegral fooInt :: Integer ) )
                            , ("fooDouble" .= fooDouble)
                            , ("fooTuple" .= fooTuple)
                            , ("fooMap" .= fooMap)
                            ]

    fromJSON (JsonObject v) = Right $ Foo (fromIntegral ((mlk "fooInt" v)::Integer)) (mlk "fooDouble" v) (mlk "fooTuple" v) (mlk "fooMap"  v)
    fromJSON _ = undefined 

instance Arbitrary UFoo where
    arbitrary = UFoo <$> arbitrary <*> arbitrary
        where _ = uFooInt

instance Arbitrary OneConstructor where
    arbitrary = return OneConstructor

instance JSONic OneConstructor

instance (Arbitrary a, Arbitrary b) => Arbitrary (Product2 a b) where
    arbitrary = liftM2 Product2 arbitrary arbitrary

instance (JSONic a, JSONic b) => JSONic (Product2 a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e,
          Arbitrary f) => Arbitrary (Product6 a b c d e f) where
    arbitrary = Product6 <$> arbitrary <*> arbitrary <*> arbitrary <*>
                             arbitrary <*> arbitrary <*> arbitrary

instance (JSONic a, JSONic b, JSONic c, JSONic d, JSONic e,
          JSONic f) => JSONic (Product6 a b c d e f)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Sum4 a b c d) where
    arbitrary = oneof [Alt1 <$> arbitrary, Alt2 <$> arbitrary,
                       Alt3 <$> arbitrary, Alt4 <$> arbitrary]

instance (JSONic a, JSONic b, JSONic c, JSONic d)
    => JSONic (Sum4 a b c d)

instance (Arbitrary a) => Arbitrary (Approx a) where
    arbitrary = Approx <$> arbitrary

instance (JSONic a) => JSONic (Approx a) where
    fromJSON a = Approx <$> fromJSON a
    toJSON = toJSON . fromApprox

instance Arbitrary Nullary where
    arbitrary = elements [C1, C2, C3]

instance Arbitrary a => Arbitrary (SomeType a) where
    arbitrary = oneof [ pure Nullary
                      , Unary   <$> arbitrary
                      , Product <$> arbitrary <*> arbitrary <*> arbitrary
                      , Record  <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

approxEq :: (Fractional a, Ord a) => a -> a -> Bool
approxEq = approxEqWith 1e-15 1e-15

approxEqWith :: (Fractional a, Ord a) => a -> a -> a -> a -> Bool
approxEqWith maxAbsoluteError maxRelativeError a b =
    a == b || d < maxAbsoluteError ||
    d / max (abs b) (abs a) <= maxRelativeError
  where d = abs (a - b)

-- ======================================================================

optsDefault :: JsonicOptions
optsDefault = defaultJsonicOptions
              { jsonicFieldLabelModifier     = map toLower
              , jsonicConstructorTagModifier = map toLower
              }

opts2ElemArray :: JsonicOptions
opts2ElemArray = optsDefault
                 { jsonicAllNullaryToStringTag = False
                 , jsonicSumEncoding     = TwoElemArray
                 }

optsTaggedObject :: JsonicOptions
optsTaggedObject = optsDefault
                   { jsonicAllNullaryToStringTag = False }

optsObjectWithSingleField :: JsonicOptions
optsObjectWithSingleField = optsDefault
                            { jsonicAllNullaryToStringTag = False
                            , jsonicSumEncoding           = ObjectWithSingleField
                            }


clem = assertEqual "json map" (Right $ JsonObject $ [("one", JsonNumber (II 1) ), ("two", JsonNumber (DD 2.1) )]) (readJSON "{\"one\":1,\"two\":2.1}")

