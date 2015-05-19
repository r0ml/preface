
module TestXml (tests) where

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

tests :: IO [Test]
tests = do
  return [makeTest "SimpleDB wsdl" sdbWsdl]

