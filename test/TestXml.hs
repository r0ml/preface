{-# LANGUAGE TemplateHaskell #-}

module TestXml (tests) where

import Preface.R0ml

sdbWsdl nam = do
  a <- strReadFile "test/AmazonSimpleDB.wsdl" :: IO String
  return $ case parseXml a of 
    z@(XmlParseError x) -> TestFail (show z)
    _ -> TestPass

data SDBDomainMetadata = SDBDomainMetadata {
  sdbDmdItemCount :: Integer,
  sdbDmdAttributeValueCount :: Integer,
  sdbDmdAttributeNameCount :: Integer,
  sdbDmdTimestamp :: UTCTime} 
  deriving (Show, Eq)
$(deriveXmlic defaultOptions ''SDBDomainMetadata)

data NullaryTest = First | Second | Third deriving (Show, Eq)
$(deriveXmlic defaultOptions ''NullaryTest)

-- data Multix = None | One String | Two Int Double | Three Int Double String deriving (Show, Eq)
-- $(deriveXmlic defaultOptions ''Multix)

-- instance Show Varmint
-- instance Eq Varmint

-- data Varmint = forall a . Num a => Varmint { first :: a, second :: Int } 
-- $(deriveXmlic defaultOptions ''Varmint)


genXml nam = do
  b <- getCurrentTime
  let a = SDBDomainMetadata 5 101 99 b
--  let a = Two
      c = toXML a
      d = fromXML c
  print a
  putStrLn "-----------"
  print c
  putStrLn "-----------"
  print d
  return $ case d of 
    Left x -> TestFail x
    Right x -> if a == x then TestPass else TestFail $ (show (a,c,d)) 
        
tests :: IO [Test]
tests = 
  return [ makeTest "SimpleDB wsdl" sdbWsdl
         , makeTest "to/from XML" genXml         
         ]

