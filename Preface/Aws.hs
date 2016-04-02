{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Preface.AWS where

import Preface.Imports
import Preface.Stringy
import Preface.SecureHash
import Preface.IOQuotes
import Preface.Misc

-- ag :: (Eq a, Show a) => String -> a -> a -> IO ()
-- ag = assertEqualG
-- der :: ByteString -> ByteString
-- der = strReplace "\r\n" "\n"

{- ------------------------------------------------------------------------------------
 - Signing functions
 - ------------------------------------------------------------------------------------ -}
algo :: ByteString
algo = "AWS4-HMAC-SHA256"

{- | Given the bits that go into an AWS request, generate the signed request as a ByteString
 - The arguments are:
 -     1) the request method (e.g. "GET")
 -     2) the request URL (e.g. "/" )
 -     3) the request query string
 -     4) the request headers (not including the headers added by the signature process)
 -     5) the request post ?
 -     6) the Timestamp (UTCTIME)
 -     7) the AWS Secret
 -     8) the AWS region
 -     9) the AWS service
 -     10) the AWS id
-}
awsSignature :: Stringy a => a -> a -> a -> [(a,a)] -> a -> UTCTime -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString
awsSignature rm uri qs hds rp date secret region servic akid =
  let dd = asByteString (formatTime defaultTimeLocale "%Y%m%d" date ) 
      kDate = hmacSha 256 (strConcat ["AWS4", secret]) dd
      kRegion = hmacSha 256 kDate region 
      kService = hmacSha 256 kRegion servic
      dsk = hmacSha 256 kService "aws4_request"
      (ssh, sst) = signatureString rm uri qs hds rp date region servic
      -- shx = intercalate ";" (map (\(x,y) -> asByteString (strMap toLower (asString x))) hds )
   in strConcat[ algo, " Credential=", akid, "/", dd, "/", region,"/",servic,"/aws4_request",", SignedHeaders=",
                 (intercalate ";" ssh), ", Signature=", asByteString $ base16 (hmacSha 256 dsk sst)]
  
signatureString :: Stringy a => a -> a -> a -> [(a,a)] -> a -> UTCTime -> ByteString -> ByteString -> ([ByteString], ByteString)
signatureString rm uri qs hds rp date region servic =
  let (hsx, cr) = canonicalRequest rm uri qs hds rp 
      reqDate = asByteString $ formatTime defaultTimeLocale  "%Y%m%dT%H%M%SZ" date 
     -- credScope = intercalate "/" [strTake 8 reqDate, region, service, "aws4_request"]
      credScope = strConcat [strTake 8 reqDate, "/", region, "/", servic, "/", "aws4_request"] 
      -- hashCanReq = base16 (hmacSha 256 (asByteString secret) (asByteString cr))
      hcr = asByteString $ base16 (sha 256 (asByteString cr))
   in (hsx, asByteString $ intercalate ("\n" :: ByteString) [algo, asByteString reqDate, credScope, asByteString hcr ])

canonicalUrl :: Stringy a => a -> a
canonicalUrl x = let n = strLen x
                     -- z = strPrefixOf "/" x
                     -- y = if z then strDrop 1 x else x
                     d = strSplit (fromChar '/') x
                     n2 = length d
                     j = if n2 >= 2
                         then [True] ++ (replicate (n2 - 2) False) ++ [True]
                         else [True]
                     c = map fst $ filter (\(xx,y) -> y || not ( xx == "." || strNull xx)) (zip d j)
                     b = dd c
                     bb = if length b == 1 then strEmpty : b else b
                     r = intercalate "/" bb
                  in if strLen r == n then r else canonicalUrl r
                 where dd (_x : ".." : xs) = dd xs
                       dd [] = []
                       dd (xx : xs) = xx : dd xs

canonicalQuery :: Stringy a => a -> a
canonicalQuery x = let d = strSplit (fromChar '&') x
                       e = map (second (strDrop 1) . (strBrk (== fromChar '=') ) ) d
                       ee = sort e 
                       ee2 = map (\(xx,y) -> (urlEncode True (asByteString xx), urlEncode True (asByteString y) ) ) ee
                       z = map (\(xx,y) -> strConcat [xx,stringleton (fromChar '='), asByteString y]) ee2
                       h = map stringy z 
                    in intercalate (stringleton (fromChar '&')) h

canonicalRequest :: Stringy a => a -> a -> a -> [(a,a)] -> a -> ([ByteString], ByteString)
canonicalRequest rm urx qs hds rp =
  let reqMeth = rm
      uri = canonicalUrl urx
      nl = "\n" :: ByteString  -- "\r\n"
      srd = nubConcatOn fst snd (sortBy (\x y -> let res = compare (fst x) (fst y) in if res == EQ then compare (snd x) (snd y) else res) $ map (first strToLower) hds)
      srd2 = map (second (intercalate "," . map trim ) ) srd 
      -- shd = sortWith fst srd2
      shd = srd2
      sdx = map fst shd
      hdrs = intercalate nl (map (\(x, y) -> strConcat [ asByteString (strMap toLower (asString x)),":" :: ByteString ,asByteString y]) shd )
      shdrs = intercalate ";" (map (\(x,_y) -> asByteString x) shd )
      hp = asByteString $ base16 (sha 256 (asByteString rp)) 
   in (map asByteString sdx, asByteString (intercalate nl [asByteString reqMeth, asByteString  uri, canonicalQuery $ asByteString qs, asByteString hdrs,"", asByteString shdrs, asByteString hp]))

{-            
canonicalURL :: Stringy a => a -> a
canonicalURL url = let ur4 = if strPrefixOf url (fromString "/./") then strDrop 2 url else url 
                       ur2 = if strPrefixOf ur4 (fromString "/")  then strCons (fromChar '/') $ strDropWhile (== fromChar '/') ur4 else ur4
                       ur3 = if strPrefixOf (strReverse ur2) (fromString "/") then strReverse (strCons (fromChar '/') $ strDropWhile (== fromChar '/') (strReverse ur2)) else ur2
                    in ur3
-}
     
{- I could
- a) assume that I'm in an AWS monad (which would encapsulate the login credential parts, or
- b) store the credentials in an IORef (or State) or
- c) send the queries to a running thread which manages the interaction
- -}

getCred :: IO (String, String, String)
getCred = do
        a <- [opts|$HOME/.aws.hs|] -- "Twisted.Aws.Ec2"
        return $ ( a // "AwsAccessKey", a // "Secret", a // "Region" )
   where (//) a b = maybe "" id (lookup b a)

getAWSKey :: IO (String,String,String)
getAWSKey = do
       a <- fromMaybe "missing" <$> lookupEnv "AWS_ACCESS_KEY_ID"
       b <- fromMaybe "missing" <$> lookupEnv "AWS_SECRET_ACCESS_KEY"
       c <- fromMaybe "missing" <$> lookupEnv "AWS_REGION"
       return (a,b,c)

{- ------------------------------------------------------------------------------------
 - API functions
 - ------------------------------------------------------------------------------------ -}

{-
dynamoDB :: JSONic b => String -> [(String, String)] -> IO b
dynamoDB action args = do
       (idx, key, region) <- getCred
       let region = "us-east-1"
       time <- getCurrentTime
       let endpoint = "dynamodb." ++ region ++ ".amazonaws.com"
           qheader = [("Action", action),
                      ("Version", apiVersion),
                      ("SignatureMethod", "HmacSHA256"),
                      ("Timestamp", awsTimeFormat time),
                      ("AWSAccessKeyId", idx)] 
           signa = "/?" ++ awsSignature endpoint key time qheader
           req = "https://"++endpoint++signa
       let b = awsSignature "POST" "/"
{-
 -     1) the request method (e.g. "GET")
 -     2) the request URL (e.g. "/" )
 -     3) the request query string
 -     4) the request headers (not including the headers added by the signature process)
 -     5) the request post ?
 -     6) the Timestamp (UTCTIME)
 -     7) the AWS Secret
 -     8) the AWS region
 -     9) the AWS service
 -     10) the AWS id
 -}

       r <- withCurlDo (curlPost req [] [toJSON args] )
       return $ toJSON $ asString (respBody r)
-}


-- | The API Version as defined in the API documentation
-- ( http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_ParametersDescription.html )
apiVersion :: String
apiVersion = "2009-04-15"

awsTimeFormat :: UTCTime -> String
awsTimeFormat = formatTime defaultTimeLocale (iso8601DateFormat (Just "%XZ"))

