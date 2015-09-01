{-# LANGUAGE QuasiQuotes #-}

module Test (tests) where

import qualified Distribution.TestSuite as T
import qualified Test.HUnit as H
-- import Apache.Enums

tests :: IO [T.Test]
tests = return [T.Test hunit]
   where
      hunit = T.TestInstance { T.run = runUnitTests testCases, T.name = "HUnit test cases", T.tags = [], T.options=[], T.setOption = \_ _ -> Right hunit }
      runUnitTests t = do
        (H.Counts cases tried errors failures) <- H.runTestTT t
        return $ if errors > 0
           then T.Finished $ T.Error "There were errors in the HUnit tests"
           else if failures > 0
              then T.Finished $ T.Fail "There were failures in the HUnit tests"
              else T.Finished T.Pass
    
  

[enum| Clabber Amph 1 Belph 14 Delph 22 Glaph 97|]

testCases = H.TestList [ 
  ("one" H.~: (H.TestCase $ do 
     print (show Delph)
     print  (show  ( fromEnum Delph :: Int))
     print  ( show ( toEnum 14 :: Clabber))
     H.assertEqual "??? str url (acme.com)" 1 1 )
     )]

  
