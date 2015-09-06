{- | This is r0ml's extended preface for GHC.
 - Do I pick up edits?
-}

module Preface.R0ml (module X, module Preface.R0ml
  -- | A class which implements String functions for various representations (UTF8, ByteString, and String)
  ,Stringy(..)
  -- | A class which implements Char functions for Word8, Char8 and Char
  , Chary(..)
 ) where

{-
import qualified Data.ByteString as B
import qualified Data.Map                as M
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P
import qualified Data.Set                as S
import qualified Data.Text as T (pack, unpack, singleton)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
-}

import Preface.Imports as X
import Preface.Stringy as X hiding()
import Preface.Byter as X hiding ()

import Preface.Binary as X
import Preface.Arrayed as X
import Debug.Trace as X

import Preface.Misc as X
import Preface.Math as X

import Preface.Runner as X

import Preface.Str as X
import Preface.StrUtils as X
import Preface.Symbols as X
import Preface.SecureHash as X

import Preface.Timings as X

import Preface.Xml as X
import Preface.JSONic as X

import Preface.IO as X

import Bindings.Curl as X
import Bindings.Posix as X

import Bindings.Zlib as X

import Preface.Pipes as X

-- import Data.Vector as X ( (!), Vector )
-- import qualified Data.Vector as V

import Preface.SCGI as X
import Preface.WebSocket as X

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

-- | A functional equivalent to if/then/else syntax (CONDitional)
-- The arguments are in the order condition, then, else
cond :: Bool -> a -> a -> a
cond t x y = if t then x else y

-- | A functional equivalent to if/then/else syntax (CONDitional)
-- The arguments are in the order then, else, condition
cond' :: a -> a-> Bool -> a
cond' x y t = if t then x else y

-- | @flip@ takes a function and reverses the first and second arguments
-- Another way of stating that is that moves the second argument into the first position
-- @fflip@ takes the third argument and moves it into the first position
fflip :: (c -> a -> b -> d) -> a -> b -> c -> d
fflip f a b c = f c a b

-- | Apply f to the second element of a tuple
second :: (b->c) -> (a,b) -> (a,c)
second f (a,b) = (a,f b)

prepend :: [a] -> [a] -> [a]
prepend = (++)

instance Show Errno where show (Errno a) = "Errno " ++ show a

