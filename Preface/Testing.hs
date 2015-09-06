
module Preface.Testing (module X, module Preface.Testing) 
where

import Preface.Imports

import Test.QuickCheck as X ( quickCheck, Arbitrary(..), quickCheckWith, quickCheckResult,
                              quickCheckWithResult )

-- ----------------------------------------
import Distribution.TestSuite as X ( Progress(..), Test, testGroup)
import qualified Distribution.TestSuite as TS

-- data TestInstance  = TS.TestInstance 
--  { testRun :: IO Progress, testName :: String,
--                   testTags :: [String], testOptions :: [OptionDescr],
--                   testSetOptions :: String -> String -> Either String TestInstance }

-- data TestInstance = TestInstance { TS.TestInstance | run -> testRun }

-- | makeTest is used to create a test function for generating test cases
-- the default Cabal functions / datatypes are too awkward for using directly
makeTest :: String -> (String -> IO TestResult) -> Test
makeTest nam dotest = 
  let t = TS.TestInstance { TS.run = do { a <- dotest nam; return (Finished (cvt a)) }
      , TS.name = nam, TS.tags = [], TS.options = []
      , TS.setOption = \_  _ -> Right t }
   in TS.Test t
  where cvt TestPass = TS.Pass
        cvt (TestFail a) = TS.Fail a

data TestResult = TestPass | TestFail String

