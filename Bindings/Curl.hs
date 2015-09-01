{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Bindings.Curl
       ( CurlResponse(..), HttpAuth(..), CurlOption(..)
       , withCurlDo
       , curlGet       -- :: URLString -> [CurlOption] -> IO CurlResponse
          -- posting requests.
       , curlMultiPost       -- :: URLString -> [CurlOption] -> [HttpPost] -> IO ()
       , curlPost            -- :: URLString -> [String] -> IO ()
       , toMask
       , getInfo
       , CurlInfo(..)
       , CurlCode(..)
       ) where

import Bindings.CurlX as X
import qualified Data.ByteString as B (take, drop, concat, null, head, tail, empty, init, last)
import qualified Data.ByteString.Char8 as B (lines, elemIndex, pack)
import Preface.Imports
import Preface.XImports

type CurlHeader = [(ByteString, ByteString)]

-- getInfo :: CurlInfo -> Curl -> IO CurlInfoValue -- getInfo built by mkEnumCurl
-- need to figure out how to get the type definition of getInfo into mkEnumCurl
mkEnumCurl "CurlInfo" c_curloptDefines

defaultSSLOpts :: [CurlOption]
defaultSSLOpts = [ CurloptSSLVerifyPeer False, CurloptSSLVerifyHost 0 ]

-- | 'CurlResponse_' is a record type encoding all the information
-- embodied in a response to your Curl request. Currently only used
-- to gather up the results of doing a GET in 'curlGetResponse'.
data CurlResponse = CurlResponse
     { respCurlCode   :: CurlCode
     , respStatus     :: Int
     , respStatusLine :: ByteString
     , respHeaders    :: CurlHeader
     , respBody       :: ByteString
--     , respGetInfo    :: Curl -- (Info -> IO InfoValue)
     }
     deriving (Show)


type URLString = String

-- | @curlGet url opts curl@ performs a @GET@ and returns the @CurlResponse@ (status, headers, and body)
curlGet :: URLString -> [CurlOption] -> Curl -> IO CurlResponse
curlGet url xopts h = do
   -- Note: later options may (and should, probably) override these defaults.
  mapM_ (setopt h) (defaultSSLOpts ++ [CurloptFailOnError False, CurloptURL url] ++ xopts)
  perform_with_response h 

-- | Perform the actions already specified on the handle.
-- Collects useful information about the returned message.
-- Note that this function sets the
-- 'CurlWriteFunction' and 'CurlHeaderFunction' options.
-- The returned payload is overloaded over the representation of
-- both headers and body via the 'CurlResponse_' type.
perform_with_response :: Curl -> IO CurlResponse
perform_with_response h = do
   rr <- newIORef []
   hh <- newIORef []

     -- Instead of allocating a separate handler for each
     -- request we could just set this options one and forall
     -- and just clear the IORefs.

   _ <- setopt  h (CurloptWriteFunction (ReadWriteFunction (gatherBytes rr)))
   _ <- setopt  h (CurloptHeaderFunction (ReadWriteFunction (gatherBytes hh)))
   rc <- toCode <$> curlPrim h c_easy_perform
   rspCode <- getResponseCode h
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
    getResponseCode c = do cc <- getInfo CURLINFO_RESPONSE_CODE c
                           case cc of { CurlInfoInt ca -> return (fromIntegral ca); _ -> return 999 }

withCurlDo :: ( Curl -> IO a ) -> IO a
withCurlDo f = mkCurl >>= f

-- | 'curlMultiPost' perform a multi-part POST submission.
curlMultiPost :: URLString -> [CurlOption] -> [HttpPost] -> Curl -> IO ()
curlMultiPost s os ps h =  mapM_ (setopt h) ([CurloptVerbose True, CurloptURL s, CurloptHttpPost ps] ++ os)

-- | 'curlPost' performs. a common POST operation, namely that
-- of submitting a sequence of name=value pairs.
curlPost :: URLString -> [CurlOption] -> [String] -> Curl -> IO CurlCode
curlPost s opts ps h = do
  mapM_ (setopt h) [CurloptVerbose True, CurloptPostFields ps, CurloptCookieJar "cookies", CurloptURL s]
  mapM_ (setopt h) opts
  toCode <$> curlPrim h c_easy_perform

{-   iv <- 
   case iv of
     IString s -> 
       case (reads s) of
         ((v,_):_) -> return v
         _ -> fail ("Curl.getResponseCode: not a valid integer string " ++ s)
     IDouble d -> return (round d)
     ILong x   -> return (fromIntegral x)
     IList{}   -> fail ("Curl.getResponseCode: unexpected response code " ++ show iv)
-}

-- ---------------------------------------------------

toCode :: CInt -> CurlCode
toCode = toEnum . fromIntegral

-- ----------------------------------------------

-- type Header = String

data HttpPost
 = HttpPost {
--     { postName     :: String
--     , contentType  :: Maybe String
--     , content      :: Content
--     , extraHeaders :: [Header]
-- not yet:     , extraEntries :: [HttpPost]
--     , showName     :: Maybe String
     } deriving ( Eq, Show )

data Content
 = ContentFile   FilePath
 | ContentBuffer (Ptr CChar) Int -- byte arrays also?
 | ContentString String
   deriving ( Eq, Show )

{-
multiformString :: String -> String -> HttpPost
multiformString x y = 
  HttpPost { postName      = x
           , content       = ContentString y
           , contentType   = Nothing
           , extraHeaders  = []
           , showName      = Nothing
           } 
-}
-- lower-level marshalling code.
{-
sizeof_httppost :: Int
sizeof_httppost = 12 * sizeOf (nullPtr :: Ptr CChar)

marshallPosts :: [HttpPost] -> IO (Ptr HttpPost)
marshallPosts [] = return nullPtr
marshallPosts ps = do
  ms <- mapM marshallPost ps
  case ms of
    [] -> return nullPtr
    (x:xs) -> do
      linkUp x xs
      return x
 where
  linkUp p [] = pokeByteOff p 0 nullPtr
  linkUp p (x:xs) = do
    pokeByteOff p 0 x
    linkUp x xs
  
marshallPost :: HttpPost -> IO (Ptr HttpPost)
marshallPost p = do
  php <- mallocBytes sizeof_httppost
  pokeByteOff php 0 nullPtr
  newCString (postName p) >>= pokeByteOff php (ptrIndex 1)
  pokeByteOff php (ptrIndex 2) (length (postName p))
  case content p of
    ContentFile f -> do
      newCString f >>= pokeByteOff php (ptrIndex 3)
      pokeByteOff php (ptrIndex 4) (length f)
      pokeByteOff php (ptrIndex 5) nullPtr
      pokeByteOff php (ptrIndex 6) nullPtr
      pokeByteOff php (ptrIndex 10) (0x1 :: Int)
    ContentBuffer ptr len -> do
      pokeByteOff php (ptrIndex 3) nullPtr
      pokeByteOff php (ptrIndex 4) nullPtr
      pokeByteOff php (ptrIndex 5) ptr 
      pokeByteOff php (ptrIndex 6) len
      pokeByteOff php (ptrIndex 10) (0x10 :: Int)
    ContentString s -> do
      newCString s >>= pokeByteOff php (ptrIndex 3)
      pokeByteOff php (ptrIndex 4) (length s)
      pokeByteOff php (ptrIndex 5) nullPtr
      pokeByteOff php (ptrIndex 6) nullPtr
      pokeByteOff php (ptrIndex 10) (0x4 :: Int)
  
  cs1 <- case contentType p of
    Nothing -> return nullPtr
    Just s  -> newCString s
  pokeByteOff php (ptrIndex 7) cs1
  cs2 <- mapM newCString (extraHeaders p)
  ip <- foldM curl_slist_append nullPtr cs2
  pokeByteOff php (ptrIndex 8) ip
  pokeByteOff php (ptrIndex 9) nullPtr
  case showName p of
    Nothing -> pokeByteOff php (ptrIndex 11) nullPtr
    Just s  -> newCString s >>= pokeByteOff php (ptrIndex 11)
  return php
 where
  ptrIndex n = n * sizeOf nullPtr
-}

foreign import ccall "curl_slist_append" curl_slist_append :: Ptr () -> CString -> IO (Ptr ())
foreign import ccall "curl_slist_free_all" curl_slist_free :: Ptr () -> IO ()
-- foreign import ccall "curl_formfree" curl_formfree :: Ptr a -> IO ()
  

-- ---------------------------------------------------------------

data CurlInfoValue = CurlInfoString String | CurlInfoInt Int | CurlInfoDouble Double | CurlInfoList [String]
    | CurlInfoError CurlCode
  deriving (Show)

doGetInfo :: CurlInfoValueType -> Int -> Curl -> IO CurlInfoValue
doGetInfo t tg h =
  allocaBytes 16 $ \ ps -> do
    let fxn = case t of { CURLINFO_LONG -> _CURLINFO_LONG; CURLINFO_STRING -> _CURLINFO_STRING; CURLINFO_DOUBLE -> _CURLINFO_DOUBLE; CURLINFO_SLIST -> _CURLINFO_SLIST } 
    rc <- curlPrim h $ \p -> c_curl_easy_getinfo p (fromIntegral fxn + fromIntegral tg) (castPtr ps)
    if rc /= 0 then (return . CurlInfoError . toCode) rc
    else case t of
      CURLINFO_STRING -> do { s <- peek (castPtr ps); if s == nullPtr then (return . CurlInfoString) "" else peekCString s >>= return . CurlInfoString } 
      CURLINFO_LONG -> peek (castPtr ps) >>= return . CurlInfoInt
      CURLINFO_DOUBLE -> peek (castPtr ps) >>= return . CurlInfoDouble
      CURLINFO_SLIST -> peek (castPtr ps) >>= unmarshallList >>= return . CurlInfoList
 where
   unmarshallList ptr 
     | ptr == nullPtr = return []
     | True = do
         ps <- peekByteOff ptr 0
         s  <- if ps == nullPtr then return "" else peekCString ps
         nx <- peekByteOff ptr (sizeOf nullPtr)
         ls <- unmarshallList nx
         return (s:ls)

---- ----------------------------------------------------------

type LLong = Word64

data CurlOption
 = CurloptFileObj (Ptr ())  -- ^ external pointer to pass to as 'WriteFunction's last argument.
 | CurloptURL URLString    -- ^ the URL to use for next request; can be the full URL or just the authority\/hostname.
 | CurloptPort Word32        -- ^ what port to use.
 | CurloptProxy String      -- ^ name of proxy
 | CurloptUsername String 
 | CurloptPassword String 
 | CurloptRange String      -- ^ byte range to fetch
 | CurloptInFile FilePath   -- ^ external pointer to pass to as 'WriteFunction's last argument.
 | CurloptErrorBuffer (Ptr CChar) -- ^ buffer for curl to deposit error messages (must at least CURL_ERROR_SIZE bytes long). Uses standard error if not specified.
 | CurloptWriteFunction ReadWriteFunction -- ^ callback to handle incoming data.
 | CurloptReadFunction  ReadWriteFunction  -- ^ callback for supplying outgoing\/uploaded data.
 | CurloptTimeout Int{-secs-}        -- ^ number of seconds before timing out curl operation\/request.
 | CurloptInFileSize Int{-bytes-}    -- ^ expected size of uploaded data.
 | CurloptPostFields [String]         -- ^ (Multipart) POST data.
 | CurloptReferer String              -- ^ Set the Referer: header to the given string.
 | CurloptFtpPort String              -- ^ The string to feed to the FTP PORT command.
 | CurloptUserAgent String            -- ^ Set the User-Agent: header to the given string.
 | CurloptLowSpeed Int              -- ^ If the bytes per sec drops below the given value, the operation is aborted.
 | CurloptLowSpeedTime Int           -- ^ Upper bound for request to complete.
 | CurloptResumeFrom Int             -- ^ Byte offset at which the transfer (HTTP or FTP) should start from.
 | CurloptCookie String               -- ^ Set the Cookie: header to the given cookie (name=value pairs, semicolon-separated) string.
 | CurloptHttpHeaders [String]        -- ^ Embellish the outgoing request with the given list of (formatted) header values.
 | CurloptHttpPost  [HttpPost]        -- ^ (Multipart) POST data.
 | CurloptSSLCert FilePath            -- ^ file holding your private SSL certificates (default format is PEM).
 | CurloptSSLPassword String          -- ^ password to the above file.
 | CurloptSSLKeyPassword String       -- ^ an alias for the previous.
 | CurloptCRLF Bool                   -- ^ If true, convert Unix newlines into CRLFs when transferring.
 | CurloptQuote [String]              -- ^ Sequence of FTP commands to execute prior to the main request.
 | CurloptWriteHeader (Ptr ())        -- ^ State \/ pointer argument to pass to WriteFunction callback.
 | CurloptCookieFile FilePath         -- ^ Path to file holding initial cookie data; also enables cookie handling.
 | CurloptSSLVersion Int             -- ^ What protocol to attempt using (0:default;1:TLS;2:SSLv2;3:SSLv3)
 | CurloptTimeCondition TimeCond      -- ^ How to interpret a conditional time value.
 | CurloptTimeValue Int              -- ^ Number of secs since Jan 1, 1970. Interpretation is determined by CurlTimeCondition.
 | CurloptCustomRequest String        -- ^ String holding alternative request command (WebDAV anyone?)
 {- | CurloptStderr String {- XXX: should be FILE* ? -}               -- ^ File object to use for outputting debug info to. -}
 | CurloptPostQuote [String]          -- ^ List of commands to issue to FTP server after the main request.
 | CurloptWriteInfo String            -- ^ Not sure what this one does; something about passing it to the output function.
 | CurloptVerbose Bool                -- ^ Control verbosity
 | CurloptHeader Bool                 -- ^ Display outgoing and incoming headers 
 | CurloptNoProgress Bool             -- ^ Control progress meter
 | CurloptNoBody Bool                 -- ^ Use HEAD instead of GET
 | CurloptFailOnError Bool            -- ^ If status response is >= 300, return an error (and no other output).
 | CurloptUpload Bool                 -- ^ Control the main dataflow, i.e., True to perform uploads.
 | CurloptPost Bool                   -- ^ Issue a POST request.
 | CurloptFtpListOnly Bool            -- ^ Switch NLST for FTP directory listings
 | CurloptFtpAppend Bool              -- ^ Control if FTP uploads append rather than overwrite files 
 | CurloptUseNetRc NetRcOption        -- ^ control how or if a user's.netrc will be consulted for user:password
 | CurloptFollowLocation Bool         -- ^ Handle auto-redirects by chasing down Location: values in responses.
 | CurloptTransferTextASCII Bool      -- ^ Turn on ASCII transfers for FTP transfers; default is binary (i.e. off).
 | CurloptPut Bool                    -- ^ Use PUT to upload data.
 | CurloptProgressFunction ProgressFunction  -- ^ callback for showing progress
 | CurloptProgressData (Ptr ())       -- ^ state argumentto pass to progress callback.
 | CurloptAutoReferer Bool            -- ^ Control if the Referer: field is set upon following Location: redirects
 | CurloptProxyPort Word32              -- ^ (Numeric) proxy port to use.
 | CurloptPostFieldSize Int          -- ^ Size of the POSTed data.
 | CurloptHttpProxyTunnel Bool        -- ^ tunnel all HTTP operations through the proxy.
 | CurloptInterface String            -- ^ Interface name of outgoing network interface ( network interface, IP address, host name.)
 | CurloptKrb4Level String            -- ^ Kerberos security level ("clear", "safe", "confidential", "private" are good values, seemingly.)
 | CurloptSSLVerifyPeer Bool          -- ^ Enable the authentication of peer certificate. Default is True.
 | CurloptCAInfo FilePath             -- ^ If verifying peer's certificate, use certificates in this file to do so.
 | CurloptMaxRedirs Int              -- ^ Maximum number of Location: redirects to chase down before giving up.
 | CurloptFiletime Bool               -- ^ Try to determine the modification date of remote document; can be queried for.
 | CurloptTelnetOptions [String]      -- ^ List of commands to use for initial telnet negotiations.
 | CurloptMaxConnects Int            -- ^ Maximum number of cached active connections.
 | CurloptFreshConnect Bool           -- ^ Force the opening up a new connection rather than try to reuse active connections. Default is not to.
 | CurloptForbidReuse Bool            -- ^ Do not reuse the connection of next transfer when done.
 | CurloptRandomFile FilePath         -- ^ Path to file used to seed (Open)SSL PRNG.
 | CurloptEgdSocket FilePath          -- ^ Path to domain socket of EG Daemon.
 | CurloptConnectTimeout Int         -- ^ max number of seconds to wait for the initial connection to happen.
 | CurloptHeaderFunction ReadWriteFunction -- ^ callback used to handle _incoming_ header data.
 | CurloptHttpGet Bool                -- ^ Revert to a GET for the next request.
 | CurloptSSLVerifyHost Int          -- ^ Perform Common name checking in peer certificate (1=> existence;2=> matches hostname.)
 | CurloptCookieJar FilePath          -- ^ Path to file where additional cookie information will be stored.
 | CurloptSSLCipherList String        -- ^ Colon-separated string list of cipher preferences to use for upcoming connection (e.g., "3DES:+RSA")
 | CurloptHttpVersion HttpVersion     -- ^ What HTTP version to use, should you want to drop back for some reason.
 | CurloptFtpUseEPSV Bool             -- ^ Attempt the use of EPSV before PASV for passive FTP downloads.
 | CurloptSSLCertType String          -- ^ The format of your certificates ("PEM", "DER")
 | CurloptSSLKey FilePath             -- ^ Filename of private key.
 | CurloptSSLKeyType String           -- ^ Format of private key; use "ENG" to load from a crypto engine.
 | CurloptSSLEngine String            -- ^ Name of crypto engine to use.
 | CurloptSSLEngineDefault            -- ^ Make crypto engine the default for crypto operations.
 | CurloptDNSUseGlobalCache Bool      -- ^ Have library uses its MT-unfriendly DNS global cache.
 | CurloptDNSCacheTimeout Int         -- ^ Number of seconds to cache results of DNS lookups in memory.
 | CurloptPreQuote [String]           -- ^ FTP commands to issue after connection and transfer mode has been set.
 | CurloptDebugFunction DebugFunction -- ^ callback to catch and report transfer operations.
 | CurloptDebugData (Ptr ())          -- ^ state argument to pass to debug callback.
 | CurloptCookieSession Bool          -- ^ Signal the start of a cookie session, ignoring previous session cookies.
 | CurloptCAPath FilePath             -- ^ Directory holding CA certificates; used when verifying peer certificate.
 | CurloptBufferSize Int             -- ^ Turn (down, presumably) the buffers the received data is chunked up into (and reported to the WriteFunction.) A hint, library is free to ignore.
 | CurloptNoSignal Bool               -- ^ Turn off use of signals internally.
 | CurloptShare (Ptr ())              -- ^ Share handles are used for sharing data among concurrent Curl objects.
 | CurloptProxyType Int              -- ^ What type of proxy to use.
 | CurloptEncoding String             -- ^ What to report in the Accept-Encoding: header
 | CurloptPrivate (Ptr ())            -- ^ Data associated with a Curl handle.
 | CurloptHttp200Aliases String       -- ^ Alternatives to standard 200 OK response strings; whatever it takes, I suppose.
 | CurloptUnrestrictedAuth Bool       -- ^ Pass on user:pass when following redirects.
 | CurloptFtppUseEPRT Bool            -- ^ For active FTP downloads, try using EPRT command over LPRT.
 | CurloptHttpAuth [HttpAuth]         -- ^ State your authentication preferences.
 | CurloptSSLCtxFunction SSLCtxtFunction -- ^ callback to handle setting up SSL connections; have the power to abort them.
 | CurloptSSLCtxData (Ptr ())         -- ^ state argument to pass into the above callback.
 | CurloptFtpCreateMissingDirs Bool   -- ^ Have remote directories be created if not already there
 | CurloptProxyAuth [HttpAuth]        -- ^ What preferred authentication schemes to use wrt. proxy.
 | CurloptFtpResponseTimeout Int     -- ^ max number of seconds to wait for remote server to ACK commands.
 | CurloptIPResolve Int              -- ^ Whether to resolve wrt IPv4 or IPv6.
 | CurloptMaxFileSize Int            -- ^ Limit the number of bytes you're willing to download.
 | CurloptInFileSizeLarge LLong       -- ^ Wider alternative of option giving upper bound of uploaded content (-1 => unknown.)
 | CurloptResumeFromLarge LLong       -- ^ Wider alternative for specifying initial transfer offset.
 | CurloptMaxFileSizeLarge LLong      -- ^ Wider alternative for specifying max download size.
 | CurloptNetrcFile FilePath          -- ^ Path to user\'s .netrc
 | CurloptFtpSSL Int                  -- ^ Try enabling the use of SSL for FTP control connections and\/or transfers.
 | CurloptPostFieldSizeLarge LLong    -- ^ Size of data to POST; if unspecified (or -1), curl uses strlen().
 | CurloptTCPNoDelay Bool             -- ^ Turn on or off the TCP\/IP NODELAY option.
 | CurloptFtpSSLAuth Int             -- ^ Twiddle if TLS or SSL is used.
 | CurloptIOCTLFunction (Ptr ())      -- ^ somewhat obscure callback for handling read stream resets.
 | CurloptIOCTLData (Ptr ())          -- ^ state argument to the above.
 | CurloptFtpAccount String           -- ^ The string to use when server asks for account info.
 | CurloptCookieList String           -- ^ Cookie string to pass cookie engine; "ALL" scrubs all cookie info; "SESS" scrubs session ones.
 | CurloptIgnoreContentLength Bool    -- ^ If Content-Length: values are troublesome (wrong, perhaps?), use this option to ignore using them as guidance.
 | CurloptFtpSkipPASVIP Bool          -- ^ Ignore IP address in 227 responses.
 | CurloptFtpFileMethod Int          -- ^ How to navigate to a file on the remote server (single, multiple CWDs).
 | CurloptLocalPort Word32              -- ^ What local port to use for established connection.
 | CurloptLocalPortRange Word32         -- ^ Number of attempts at finding local ports (using LocalPort as initial base.)
 | CurloptConnectOnly Bool            -- ^ If enabled, perform all steps up until actual transfer.
     -- next three for completeness.
 | CurloptConvFromNetworkFunction (Ptr ()) -- ^ callback for doing character translations from network format.
 | CurloptConvToNetworkFunction (Ptr ())   -- ^ callback for doing character translations to network format.
 | CurloptConvFromUtf8Function (Ptr ())    -- ^ callback for translating UTF8 into host encoding.
 | CurloptMaxSendSpeedLarge LLong          -- ^ Specifies throttle value for outgoing data.
 | CurloptMaxRecvSpeedLarge LLong          -- ^ Specifies throttle for incoming data.
 | CurloptFtpAlternativeToUser String      -- ^ Alternative (to user:pass) for FTP authentication; weird.
 | CurloptSockOptFunction (Ptr ())         -- ^ callback that's injected between socket creation and connection.
 | CurloptSockOptData (Ptr ())             -- ^ state argument to the above.
 | CurloptSSLSessionIdCache Bool           -- ^ Enable the SSL session id cache; default is on, so use this to disable.
 | CurloptSSHAuthTypes [SSHAuthType]       -- ^ SSH authentication methods to use.
 | CurloptSSHPublicKeyFile FilePath        -- ^ Path to file holding user's SSH public key.
 | CurloptSSHPrivateKeyFile FilePath       -- ^ Path to file holding user's SSH private key.
 | CurloptFtpSSLCCC Bool                   -- ^ Send CCC command after FTP connection has been authenticated.
 | CurloptTimeoutMS Int                   -- ^ Max number of milliseconds that a transfer may take.
 | CurloptConnectTimeoutMS Int            -- ^ Max number of milliseconds that a connection attempt may take to complete.
 | CurloptHttpTransferDecoding Bool        -- ^ Disable transfer decoding; if disabled, curl will turn off chunking.
 | CurloptHttpContentDecoding  Bool        -- ^ Disable content decoding, getting the raw bits.
   -- sync'ed wrt 7.19.2
 | CurloptNewFilePerms Int
 | CurloptNewDirectoryPerms Int
 | CurloptPostRedirect Bool
   -- no support for open socket callbacks/function overrides.
 | CurloptSSHHostPublicKeyMD5 String
 | CurloptCopyPostFields Bool
 | CurloptProxyTransferMode Int
   -- no support for seeking in the input stream.
 | CurloptCRLFile       FilePath
 | CurloptIssuerCert    FilePath
 | CurloptAddressScope  Int
 | CurloptCertInfo      Int
 | CurloptProxyUsername String
 | CurloptProxyPassword String
 deriving (Show)
          
data HttpVersion = HttpVersionNone | HttpVersion10 | HttpVersion11 deriving ( Enum,Show )

data TimeCond = TimeCondNone | TimeCondIfModSince | TimeCondIfUnmodSince | TimeCondLastMode
   deriving ( Enum, Show )
 
data NetRcOption = NetRcIgnored | NetRcOptional | NetRcRequired deriving ( Enum, Show )

data HttpAuth = HttpAuthBasic | HttpAuthDigest | HttpAuthGSSNegotiate | HttpAuthNTLM
   deriving ( Enum, Show )

data SSHAuthType = SSHAuthPublickey | SSHAuthPassword | SSHAuthHost | SSHAuthKeyboard
   deriving ( Enum, Show )

toMask :: Enum a => [a] -> Word32
toMask = foldr (\x y -> y .|.  (shiftL 1 (fromEnum x))) 0

setopt :: Curl -> CurlOption -> IO CurlCode
setopt cc z = (toEnum . fromIntegral) <$> seto z where
  u_ptr :: Int -> Ptr a -> IO CInt
  u_ptr i x = curlPrim cc $ \fh ->  c_curl_easy_setopt_p fh (10000+(fromIntegral i)) (castPtr x)
  u_long :: Integral a => Int -> a -> IO CInt 
  u_long i x = curlPrim cc $ \fh -> c_curl_easy_setopt fh (fromIntegral i) (fromIntegral x)
  u_str i x = withCString x ((u_ptr i) . castPtr) -- does this need to be finalized?
  u_bool i x = u_long i (if x then 1 else 0::Word32)
  u_enum i x = u_long i (fromEnum x)

  u_strs i x = do curl_debug ("ALLOC: " ++ show x)
              -- curl_slist_append will copy its string argument
                  let addOne ip s = withCString s $ curl_slist_append ip 
                  ip <- foldM addOne nullPtr x
                  curl_add_finalizer cc (do curl_debug ("FREE: "++show x) >> curl_slist_free ip)
                  u_ptr i (castPtr ip)
  u_fn i x = u_ptr (10000+i) (castFunPtrToPtr x)
  u_off :: CInt -> Word64 -> IO CInt
  u_off i x = curlPrim cc $ \fh -> c_curl_easy_setopt fh (30000+i) ((toEnum . fromIntegral) x)
    
  seto (CurloptFileObj x) = u_ptr 1 x
  seto (CurloptURL x) = u_str 2 x
  seto (CurloptPort x) = u_long 3 x
  seto (CurloptProxy x) = u_str 4 x
  seto (CurloptUsername x) = u_str 173 x
  seto (CurloptPassword x) = u_str 174 x
  seto (CurloptRange x) = u_str 7 x
  seto (CurloptInFile x) = u_str 9 x
  seto (CurloptErrorBuffer x) = u_ptr 10 x

  seto (CurloptWriteFunction x) = mkRwFn x >>= u_fn 11
  seto (CurloptReadFunction x ) = mkRwFn x >>= u_fn 12
  seto (CurloptTimeout x) =  u_long 13 x
  seto (CurloptInFileSize x) = u_long 14 x
  seto (CurloptPostFields x) = u_str 15 (intercalate "&" x)
  seto (CurloptReferer x) =  u_str 16 x
  seto (CurloptFtpPort x) =  u_str 17 x
  seto (CurloptUserAgent x) = u_str 18 x
  seto (CurloptLowSpeed x) = u_long 19 x
  seto (CurloptLowSpeedTime x) = u_long 20 x
  seto (CurloptResumeFrom x) = u_long 21 x
  seto (CurloptCookie x) = u_str 22 x
  seto (CurloptHttpHeaders x) = u_strs 23 x
  seto (CurloptHttpPost _x) = undefined -- u_posts um (o 24) x
  seto (CurloptSSLCert x) = u_str 25 x
  seto (CurloptSSLPassword x) = u_str 26 x
  seto (CurloptSSLKeyPassword x) = u_str 26 x -- yes, duplicate.
  seto (CurloptCRLF x) = u_bool 27 x
  seto (CurloptQuote x) = u_strs 28 x
  seto (CurloptWriteHeader x) = u_ptr 29 x
  seto (CurloptCookieFile x) = u_str 31 x
  seto (CurloptSSLVersion x) = u_long 32 x
  seto (CurloptTimeCondition x) = u_enum 33 x
  seto (CurloptTimeValue x) = u_long 34 x
  seto (CurloptCustomRequest x) = u_str 36 x

  -- CurlStderr x -> u_string um (o 37) x

  seto (CurloptPostQuote x) = u_strs 39 x
  seto (CurloptWriteInfo x) = u_str 40 x
  seto (CurloptVerbose x) = u_bool 41 x
  seto (CurloptHeader x) = u_bool 42 x
  seto (CurloptNoProgress x) = u_bool 43 x
  seto (CurloptNoBody x) = u_bool 44 x
  seto (CurloptFailOnError x) = u_bool 45 x
  seto (CurloptUpload x) = u_bool 46 x
  seto (CurloptPost x) = u_bool 47 x
  seto (CurloptFtpListOnly x) = u_bool 48 x
  seto (CurloptFtpAppend x) = u_bool 50 x
  seto (CurloptUseNetRc x) = u_enum 51 x
  seto (CurloptFollowLocation x) = u_bool 52 x
  seto (CurloptTransferTextASCII x) = u_bool 53 x
  seto (CurloptPut x) = u_bool 54 x
  seto (CurloptProgressFunction x) = mkProgress x >>= u_fn 56 
  seto (CurloptProgressData x) = u_ptr 57 x
  seto (CurloptAutoReferer x) = u_bool 58 x
  seto (CurloptProxyPort x) = u_long 59 x
  seto (CurloptPostFieldSize x) = u_long 60 x
  seto (CurloptHttpProxyTunnel x) = u_bool 61 x
  seto (CurloptInterface x) = u_str 62 x
  seto (CurloptKrb4Level x) = u_str 63 x
  seto (CurloptSSLVerifyPeer x) = u_bool 64 x
  seto (CurloptCAInfo x) = u_str 65 x
  seto (CurloptMaxRedirs x) = u_long 68 x
  seto (CurloptFiletime x) = u_bool 69 x
  seto (CurloptTelnetOptions x) = u_strs 70 x
  seto (CurloptMaxConnects x) = u_long 71 x

  seto (CurloptFreshConnect x) = u_bool 74 x
  seto (CurloptForbidReuse x) = u_bool 75 x
  seto (CurloptRandomFile x) = u_str 76 x
  seto (CurloptEgdSocket x) = u_str 77 x
  seto (CurloptConnectTimeout x) = u_long 78 x
  seto (CurloptHeaderFunction x ) = mkRwFn x >>= u_fn 79
  seto (CurloptHttpGet x) = u_bool 80 x
  seto (CurloptSSLVerifyHost x ) = u_long 81 x

  seto (CurloptCookieJar x) = u_str 82 x
  seto (CurloptSSLCipherList x) = u_str 83 x -- a string (or a l-list of them)?
  seto (CurloptHttpVersion x) = u_enum 84 x
  seto (CurloptFtpUseEPSV x) = u_bool 85 x
  seto (CurloptSSLCertType x) = u_str 86 x
  seto (CurloptSSLKey x) = u_str 87 x
  seto (CurloptSSLKeyType x) = u_str 88 x
  seto (CurloptSSLEngine x) = u_str 89 x
  seto (CurloptSSLEngineDefault) = u_bool 90 True
  seto (CurloptDNSUseGlobalCache x) = u_bool 91 x
  seto (CurloptDNSCacheTimeout x) = u_long 92 x
  seto (CurloptPreQuote x) = u_strs 93 x
  seto (CurloptDebugFunction x) = mkDebugFun x >>= u_fn 94 
  seto (CurloptDebugData x) = u_ptr 95 x
  seto (CurloptCookieSession x) = u_bool 96 x
  seto (CurloptCAPath x) = u_str 97 x
  seto (CurloptBufferSize x) = u_long 98 x
  seto (CurloptNoSignal x) = u_bool 99 x
  seto (CurloptShare x) = u_ptr 100 x
  seto (CurloptProxyType x) = u_enum 101 x
  seto (CurloptEncoding x) = u_str 102 x
  seto (CurloptPrivate x) = u_ptr 103 x
  seto (CurloptHttp200Aliases x) = u_str 104 x -- correct?
  seto (CurloptUnrestrictedAuth x) = u_bool 105 x
  seto (CurloptFtppUseEPRT x) = u_bool 106 x
  seto (CurloptHttpAuth xs) = u_long 107 (toMask xs)
  seto (CurloptSSLCtxFunction x) = mkSslCtxtFun x >>= u_fn 108
  seto (CurloptSSLCtxData x) = u_ptr 109 x
  seto (CurloptFtpCreateMissingDirs x) = u_bool 110 x
  seto (CurloptProxyAuth x) = u_long 111 (toMask x)
  seto (CurloptFtpResponseTimeout x) = u_long 112 x
  seto (CurloptIPResolve x) = u_long 113 x
  seto (CurloptMaxFileSize x) = u_long 114 x

  seto (CurloptInFileSizeLarge x) = u_off 115 x
  seto (CurloptResumeFromLarge x) = u_off 116 x
  seto (CurloptMaxFileSizeLarge x) = u_off 117 x

  seto (CurloptNetrcFile x) = u_str 118 x
  seto (CurloptFtpSSL x) = u_enum 119 x
  seto (CurloptPostFieldSizeLarge x) = u_off 120 x
  seto (CurloptTCPNoDelay x) = u_bool 121 x
  seto (CurloptFtpSSLAuth x) = u_enum 129 x
  seto (CurloptIOCTLFunction _x) = undefined -- u_fn 130 x
  seto (CurloptIOCTLData x) = u_ptr 131 x
  seto (CurloptFtpAccount x) = u_str 134 x
  seto (CurloptCookieList x) = u_str 135 x
  seto (CurloptIgnoreContentLength x) = u_bool 136 x
  seto (CurloptFtpSkipPASVIP x) = u_bool 137 x
  seto (CurloptFtpFileMethod x) = u_enum 138 x
  seto (CurloptLocalPort x) = u_long 139 x
  seto (CurloptLocalPortRange x) = u_long 140 x
  seto (CurloptConnectOnly x) = u_bool 141 x
  seto (CurloptConvFromNetworkFunction _x) = undefined -- u_fn 142 x
  seto (CurloptConvToNetworkFunction _x) = undefined -- u_fn 143 x
  seto (CurloptConvFromUtf8Function _x) = undefined -- u_fn 144 x
  seto (CurloptMaxSendSpeedLarge x) = u_off 145 x
  seto (CurloptMaxRecvSpeedLarge x) = u_off 146 x
  seto (CurloptFtpAlternativeToUser x) = u_str 147 x

  seto (CurloptSockOptFunction _x) = undefined -- u_fn 148 x

  seto (CurloptSockOptData x) = u_ptr 149 x
  seto (CurloptSSLSessionIdCache x) = u_bool 150 x
  seto (CurloptSSHAuthTypes xs) = u_long 151 (toMask xs)
  seto (CurloptSSHPublicKeyFile x) = u_str 152 x
  seto (CurloptSSHPrivateKeyFile x) = u_str 153 x
  seto (CurloptFtpSSLCCC x) = u_bool 154 x
  seto (CurloptTimeoutMS x) = u_long 155 x
  seto (CurloptConnectTimeoutMS x) = u_long 156 x
  seto (CurloptHttpTransferDecoding x) = u_bool 157 x
  seto (CurloptHttpContentDecoding x) =  u_bool 158 x
  seto (CurloptNewFilePerms x       ) = u_long 159 x
  seto (CurloptNewDirectoryPerms x  ) = u_long 160 x
  seto (CurloptPostRedirect x       ) = u_bool 161 x
  seto (CurloptSSHHostPublicKeyMD5 x) = u_str 162 x
  seto (CurloptCopyPostFields x     ) = u_bool 165 x
  seto (CurloptProxyTransferMode x  ) = u_long 166 x
  seto (CurloptCRLFile x            ) = u_str 169 x
  seto (CurloptIssuerCert x         ) = u_str 170 x
  seto (CurloptAddressScope x       ) = u_long 171 x
  seto (CurloptCertInfo x           ) = u_long 172 x
  seto (CurloptProxyUsername x   ) = u_str 175 x
  seto (CurloptProxyPassword x   ) = u_str 176 x

-- -----------------------------------------------------------------------------
