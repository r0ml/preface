{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitParams, BangPatterns, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- module TestAwsSign (tests) where 

import Preface

assertEqualG :: (?glb :: IORef (Int, Int), Eq a, Show a) => String -> a -> a -> IO ()
assertEqualG str a b = do
  let t = a == b
  if t then putStr "passing " else putStr ("failing (\n" ++ show a ++ " /= \n" ++ show b ++ ") ")
  putStrLn str
  modifyIORef ?glb (\(x,y) -> if t then (x+1,y) else (x,y+1) )

{-
assertEqual :: (Eq a) => String -> a -> a -> Test
assertEqual str a b = 
  makeTest "??" $ return (Finished ( if a == b then TestPass else TestFail str))
-}

-- nullt x = makeTest ("test "++show x) (return (Finished (TestFail "no reason")))
 
tests :: IO [Test]
tests = do
  tl <- scavenge_tests -- test_list
  return $ concat $ map (\x -> map x tl) [ mkT1, mkT2, mkT3]
  -- print (show (length z))
  -- let y = [nullt x | x <- [1 .. 549]]
   
  -- return (take 49 (repeat nullt)) -- ( (take 89 z) ++ [nullt, nullt] )

der :: ByteString -> ByteString
der = strReplace "\r\n" "\n"

main :: IO ()
main = do
  t <- newIORef (0,0)
  let ?glb = t

  tl <- scavenge_tests -- test_list

  mapM_ (\x -> do { testTask1 x; testTask2 x; testTask3 x; putStrLn (take 100 (repeat ('-'))) } ) tl
 
  (g,b) <- readIORef ?glb 
  printf "passed %d, failed %d\n" g b
  if b == 0 then exitSuccess else exitFailure


mkT1 nam = 
  makeTest (nam ++ " task 1") doTest
  where doTest x = do
          a <- strReadFile (nam `addExtension` "req") :: IO ByteString
          b <- strReadFile (nam `addExtension` "creq")
          let (r,u,q,h,p,dt) = parseRequest a
              (_,c) = canonicalRequest r u q h p 
          return $ if der b == der c then TestPass else TestFail ("not equal: " ++ asString (der b) ++ "\n" ++ asString (der c))


mkT2 nam = 
  makeTest (nam ++ " task 2") doTest 
  where doTest x = do
          a <- strReadFile (nam `addExtension` "req") :: IO ByteString
          b <- strReadFile (nam `addExtension` "sts")
          let (r,u,q,h,p,dt) = parseRequest a
              (_, c) = signatureString r u q h p dt (fromString "us-east-1") (fromString "host")
          return $ if der b == der c then TestPass else TestFail ("not equal: \n" ++ asString (der b) ++ "\n" ++ asString (der c) )


mkT3 nam = 
  makeTest (nam ++ " task 3" ) doTest
  where doTest x = do
          a <- strReadFile (nam `addExtension` "req") :: IO ByteString
          b <- strReadFile (nam `addExtension` "authz")
          let (r,u,q,h,p,dt) = parseRequest a
              c = awsSignature r u q h p dt aws_test_secret (fromString "us-east-1") (fromString "host") (fromString aws_test_id) :: ByteString
          return $ if b == c then TestPass else TestFail ("not equal: " ++ show b ++ "\n" ++ show c)

--
-- The AWS4 Test Suite
--
testTask1 nam = do
  a <- strReadFile ( nam `addExtension` "req" ) :: IO ByteString
  b <- strReadFile ( nam `addExtension` "creq" )
  let (r,u,q,h,p,dt) = parseRequest a
      (_, c) = canonicalRequest r u q h p 
  assertEqualG (strConcat [nam, " task 1"]) (der b) (der c)

testTask2 nam = do
  a <- strReadFile $ nam `addExtension` "req" :: IO String
  b <- strReadFile $ nam `addExtension` "sts"
  let (r, u, q, h, p, dt) = parseRequest a
      (_,c) = signatureString r u q h p dt (fromString "us-east-1") (fromString "host")
  assertEqualG (strConcat [nam, " task 2"]) (der b) (der c)

testTask3 nam = do
  a <- strReadFile $ nam `addExtension` "req" :: IO ByteString
  b <- strReadFile $ nam `addExtension` "authz"
  let (r, u, q, h, p, dt) = parseRequest a
      c = awsSignature r u q h p dt (asByteString aws_test_secret) (fromString "us-east-1") (fromString "host") (fromString aws_test_id) :: ByteString
  assertEqualG (strConcat [nam, " task 3"]) (der b) (der c)

-- The AWS sample credentials used to calculate the AWS test vectors

parseRequest :: (Stringy a, Show a) => a -> (ByteString,ByteString,ByteString,[(ByteString,ByteString)],ByteString,UTCTime)
parseRequest req = 
  let (gl : hds) = strSplitStr ("\r\n"::ByteString) req
      (rt : url : ht : _) = strSplit (fromChar ' ') gl
      (ur, qs) = strBrk (== (fromChar '?')) url
      qsx = if strNull qs then strEmpty else strTail qs
      (hd1, rpx) = break strNull hds
      rp = intercalate (fromString "\r\n") (if null rpx then [] else tail rpx)
      hd2 = traceShow ("rp",rp) $ map (\x -> let (a,b) = strBrk (== (fromChar ':')) x in (asByteString a, asByteString (strTail b))) hd1
      dt = snd $ fromJust $ find (\(x,y) -> strToLower x == (fromString "date")) hd2
      dtx = fromJust $ parseTimeM True defaultTimeLocale rfc822DateFormat (asString dt) :: UTCTime
   in (asByteString rt, asByteString ur, asByteString qsx, hd2, asByteString rp, dtx)

aws_test_id = "AKIDEXAMPLE"
aws_test_secret = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

test_dir :: FilePath
test_dir = "aws4_testsuite"

scavenge_tests :: IO [String]
scavenge_tests = fmap (map g) ( fmap (filter f)  (getDirectoryContents ( joinPath ["test",test_dir])) )
  where
    f ('.':_) = False
    f fp      = snd (splitExtension fp) == ".req"
    g fp      = joinPath ["test", test_dir, fst ( splitExtension fp)]


