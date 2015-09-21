{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Bindings.Curl.CurlInfo where

import Bindings.Curl.CurlX
import Preface.Imports
import Bindings.Curl.CurlErrors

--------------------------------------------------
-- CurlInfo
--

toCode :: CInt -> CurlCode
toCode = toEnum . fromIntegral

getInfoSList :: Int -> Curl -> IO (Either CurlCode [String])
getInfoSList tg h = do
  a <- getInfoZ (0x400000 + tg) h
  either (return . Left) ( fmap Right . uml ) a
  where uml s = peek s >>= unmarshalList 
        unmarshalList ptr 
          | ptr == nullPtr = return []
          | True = do ps <- peekByteOff ptr 0
                      s  <- if ps == nullPtr then return "" else peekCString ps
                      nx <- peekByteOff ptr (sizeOf nullPtr)
                      ls <- unmarshalList nx
                      return (s:ls)

getInfoZ :: Int -> Curl -> IO (Either CurlCode (Ptr a))
getInfoZ tg h = allocaBytes 16 $ \ps -> do
    rc <- curlPrim h $ \p -> c_curl_easy_getinfo p (fromIntegral tg) (castPtr ps)
    return $ if rc /= 0 then Left ( toCode rc )
             else Right (castPtr ps)

getInfoDouble :: Int -> Curl -> IO (Either CurlCode Double)
getInfoDouble tg h = do
  a <- getInfoZ (0x300000 + tg) h
  either (return . Left) ( fmap Right . peek) a

getInfoLong :: Int -> Curl -> IO (Either CurlCode Int)
getInfoLong tg h = do
  a <- getInfoZ (0x200000 + tg) h 
  either (return . Left) ( fmap Right . peek ) a

getInfoString :: Int -> Curl -> IO (Either CurlCode String)
getInfoString tg h = do
  a <- getInfoZ (0x100000 + tg) h
  either (return . Left) ( fmap Right . spc ) a
  where spc :: Ptr CChar -> IO String
        spc s = if s == nullPtr then return "" else peekCString s

curlinfoEffectiveURL = getInfoString 1
curlinfoResponseCode = getInfoLong 2
curlinfoTotalTime = getInfoDouble 3
curlinfoNameLookupTime = getInfoDouble 4
curlinfoConnectTime = getInfoDouble 5
curlinfoPretransferTime = getInfoDouble 6
curlinfoSizeUpload = getInfoDouble 7
curlinfoSizeDownload = getInfoDouble 8
curlinfoSpeedDownload = getInfoDouble 9
curlinfoSpeedUpload = getInfoDouble 10

curlinfoHeaderSize = getInfoLong 11
curlinfoRequestSize = getInfoLong 12
curlinfoSSLVerifyResult = getInfoLong 13
curlinfoFiletime = getInfoLong 14
curlinfoContentLengthDownload = getInfoDouble 15
curlinfoContentLengthUpload = getInfoDouble 16
curlinfoStartTransferTime = getInfoDouble 17
culrinfoContentType = getInfoString 18
curlinfoRedirectTime = getInfoDouble 19
curlinfoRedirectCount = getInfoLong 20

curlinfoPrivate = getInfoString 21
curlinfoHTTPConnectCode = getInfoLong 22
curlinfoHTTPAuthAvail = getInfoLong 23
curlinfoProxyAuthAvail = getInfoLong 24
curlinfoOSErrno = getInfoLong 25
curlinfoNumConnects = getInfoLong 26
curlinfoSSLEngines = getInfoSList 27
curlinfoCookieList = getInfoSList 28
curlinfoLastSocket = getInfoLong 29
curlinfoFTPEntryPath = getInfoString 30

curlinfoRedirectURL = getInfoString 31
curlinfoPrimaryIP = getInfoString 32
curlinfoAppConnectTime = getInfoDouble 33
curlinfoCertInfo = getInfoSList 34
curlinfoConditionUnmet = getInfoLong 35
curlinfoRTSPSessionID = getInfoString 36
curlinfoRTSPClientCSeq = getInfoLong 37
curlinfoRTSPServerCSeq = getInfoLong 38
curlinfoRTSPCSeqRecv = getInfoLong 39
curlinfoPrimaryPort = getInfoLong 40

curlinfoLocalIP = getInfoString 41
curlinfoLocalPort = getInfoLong 42
curlinfoTLSSession = getInfoSList 43

--------------------------------------------------

