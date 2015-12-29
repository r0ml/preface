{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Bindings.Curl
       ( CurlResponse(..), HttpAuth(..)
       , withCurlDo
       , curlGet       -- :: URLString -> [CurlOption] -> IO CurlResponse
          -- posting requests.
       -- , curlMultiPost       -- :: URLString -> [CurlOption] -> [HttpPost] -> IO ()
       , module X
       ) where

import Bindings.Curl.CurlX as X
import qualified Data.ByteString as B (take, drop, concat, null, head, tail, empty, init, last, length)
import qualified Data.ByteString.Char8 as B (lines, elemIndex, pack)
import Preface.Imports
import Preface.XImports
import Bindings.Curl.CurlErrors as X (CurlCode(..)) 
import Bindings.Curl.CurlInfo as X
import Bindings.Curl.CurlOptions as X
import Bindings.Curl.CurlSSL as X
import Bindings.Curl.CurlPost as X

type CurlHeader = [(ByteString, ByteString)]

defaultSSLOpts :: [SetCurlOption]
defaultSSLOpts = [ curloptSetSSLVerifyPeer False, curloptSetSSLVerifyHost 0 ]

-- | 'CurlResponse_' is a record type encoding all the information
-- embodied in a response to your Curl request. Currently only used
-- to gather up the results of doing a GET in 'curlGetResponse'.
data CurlResponse = CurlResponse
     { respCurlCode   :: CurlCode
     , respStatus     :: Int
     , respStatusLine :: ByteString
     , respHeaders    :: CurlHeader
     , respBody       :: ByteString
     }
     deriving (Show)

-- | @curlGet url opts curl@ performs a @GET@ and returns the @CurlResponse@ (status, headers, and body)
curlGet :: URLString -> [SetCurlOption] -> Curl -> IO CurlResponse
curlGet url xopts h = do
   -- Note: later options may (and should, probably) override these defaults.
   mapM_ ($ h) (defaultSSLOpts ++ [curloptSetFailOnError False, curloptSetURL url] ++ xopts)


-- Perform the actions already specified on the handle.
-- Collects useful information about the returned message.
-- Note that this function sets the
-- 'CurlWriteFunction' and 'CurlHeaderFunction' options.
-- The returned payload is overloaded over the representation of
-- both headers and body via the 'CurlResponse_' type.
   rr <- newIORef []
   hh <- newIORef []

   _ <- curloptSetWriteFunction (ReadWriteFunction (gatherBytes rr)) h
   _ <- curloptSetHeaderFunction (ReadWriteFunction (gatherBytes hh)) h
   rc <- toCode <$> curlPrim h c_easy_perform
   rspCode <- either (const 999) id <$> curlinfoResponseCode h
   hss <- readIORef hh
   let (st,hs) = finalHeader hss
   bs      <- readIORef rr >>= return . B.concat . reverse
   return CurlResponse
       { respCurlCode   = rc
       , respStatus     = rspCode
       , respStatusLine = st
       , respHeaders    = hs
       , respBody       = bs 
       }
  where
    finalHeader = parseStatusNHeaders . B.concat . reverse 
    parseStatusNHeaders ys = let lns = B.lines ys in
         if null lns then (B.pack "*** Parse Failure ***" , []) else (head lns, map parseHeader (tail lns))
    parseHeader xs =  let -- xs = asString xsi
                          xsr = if B.last xs == (toEnum . fromEnum) '\r' then B.init xs else xs
                          ndx = B.elemIndex ':' xsr
                      in case ndx of
                       Nothing -> (xs, B.empty)
                       Just x -> (B.take (x::Int) xsr, trim (B.drop (x+1) xsr))
                     where trim x = if (not . B.null) x && (isSpace . toEnum . fromEnum . B.head) x
                                    then (trim . B.tail) x
                                    else x
    gatherBytes ref pBuf sz szI _ =
      do let bytes = sz * szI 
         packCStringLen (pBuf,fromIntegral bytes) >>= \x -> modifyIORef ref (x:)
         return bytes

withCurlDo :: ( Curl -> IO a ) -> IO a
withCurlDo f = mkCurl >>= f

