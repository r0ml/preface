
{-# LANGUAGE TemplateHaskell #-}

import Preface.R0ml
-- import Distribution.TestSuite

makeTest nam dotest = 
  let t = TestInstance { run = do { a <- dotest nam; return (Finished a) }
      , name = nam, tags = [], options = [], setOption = \_  _ -> Right t }
   in Test t

sdbWsdl nam = do
  a <- strReadFile "test/AmazonSimpleDB.wsdl" :: IO String
  return $ case parseXml a of 
    z@(XmlParseError x) -> Fail (show z)
    _ -> Pass


data SDBDomainMetadata = SDBDomainMetadata {
  sdbDmdItemCount :: Integer,
  sdbDmdAttributeValueCount :: Integer,
  sdbDmdAttributeNameCount :: Integer,
  sdbDmdTimestamp :: UTCTime} 
  deriving (Show, Eq)
-- $(deriveXmlic defaultOptions ''SDBDomainMetadata)

data NullaryTest = One | Two | Three deriving (Show, Eq)
$(deriveXmlic defaultOptions ''NullaryTest)

genXml nam = do
  b <- getCurrentTime
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
    Left x -> Fail x
    Right x -> if a == x then Pass else Fail $ (show (a,c,d)) 
        
tests :: IO [Test]
tests = do
  return [ makeTest "SimpleDB wsdl" sdbWsdl
         , makeTest "to/from XML" genXml         
         ]

main = do
        genXml "clem"
        return ()

