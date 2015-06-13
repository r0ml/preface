
{-# LANGUAGE TemplateHaskell #-}

import Preface.R0ml
-- import Distribution.TestSuite

sdbWsdl :: String -> IO TestResult
sdbWsdl _nam = do
  a <- strReadFile "test/AmazonSimpleDB.wsdl" :: IO String
  return $ case parseXml a of 
    z@(XmlParseError _) -> TestFail (show z)
    _ -> TestPass


data SDBDomainMetadata = SDBDomainMetadata {
  sdbDmdItemCount :: Integer,
  sdbDmdAttributeValueCount :: Integer,
  sdbDmdAttributeNameCount :: Integer,
  sdbDmdTimestamp :: UTCTime} 
  deriving (Show, Eq)
-- $(deriveXmlic defaultOptions ''SDBDomainMetadata)

data NullaryTest = One | Two | Three deriving (Show, Eq)
$(deriveXmlic defaultXmlOptions ''NullaryTest)

genXml :: String -> IO TestResult
genXml _nam = do
  _b <- getCurrentTime
--  let a = SDBDomainMetadata 5 101 99 b
  let a = Two
      c = toXML a
      d = fromXML c
  print a
  putStrLn "-----------"
  print c
  putStrLn "-----------"
  print d
  return $ case d of 
    Left x -> TestFail (show x)
    Right x -> if a == x then TestPass else TestFail $ (show (a,c,d)) 
        
tests :: IO [Test]
tests = return [ makeTest "SimpleDB wsdl" sdbWsdl
         , makeTest "to/from XML" genXml         
         ]

main :: IO ()
main = do
--        _ <- genXml "clem"
--        _ <- sdbWsdl "flem"
        _ <- tests
        return ()

