{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Bindings.Curl.CurlOptions where

import Preface.Imports
import Preface.XImports (intercalate)
import Bindings.Curl.CurlX
import Bindings.Curl.CurlSSL

type SetCurlOption = Curl -> IO CInt 

data HttpVersion = HttpVersionNone | HttpVersion10 | HttpVersion11 deriving ( Enum,Show )

data TimeCond = TimeCondNone | TimeCondIfModSince | TimeCondIfUnmodSince | TimeCondLastMode
   deriving ( Enum, Show )
 
data NetRcOption = NetRcIgnored | NetRcOptional | NetRcRequired deriving ( Enum, Show )

data HttpAuth = HttpAuthBasic | HttpAuthDigest | HttpAuthGSSNegotiate | HttpAuthNTLM
   deriving ( Enum, Show )

data SSHAuthType = SSHAuthPublickey | SSHAuthPassword | SSHAuthHost | SSHAuthKeyboard
   deriving ( Enum, Show )

type URLString = String

toMask :: Enum a => [a] -> Word32
toMask = foldr (\x y -> y .|.  (shiftL 1 (fromEnum x))) 0

-- setopt :: Curl -> CurlOption -> IO CurlCode
-- setopt cc z = (toEnum . fromIntegral) <$> seto z where
u_ptr :: Int -> Ptr a -> Curl -> IO CInt
u_ptr i x cc= curlPrim cc $ \fh ->  c_curl_easy_setopt_p fh (10000+(fromIntegral i)) (castPtr x)

u_long :: Integral a => Int -> a -> Curl -> IO CInt 
u_long i x cc = curlPrim cc $ \fh -> c_curl_easy_setopt fh (fromIntegral i) (fromIntegral x)

u_str :: Int -> String -> Curl -> IO CInt 
u_str i x cc = withCString x (\y -> u_ptr i ( castPtr y) cc) -- does this need to be finalized?

u_bool :: Int  -> Bool -> Curl -> IO CInt
u_bool i x = u_long i (if x then 1 else 0::Word32)

u_enum :: Enum a => Int -> a -> Curl -> IO CInt
u_enum i x = u_long i (fromEnum x)

u_strs :: Int -> [String] -> Curl -> IO CInt
u_strs i x cc = do
              -- curl_slist_append will copy its string argument
                  let addOne ip s = withCString s $ c_curl_slist_append ip 
                  ip <- foldM addOne nullPtr x
                  curl_add_finalizer cc (c_curl_slist_free ip)
                  u_ptr i (castPtr ip) cc

u_fn :: Int -> FunPtr a -> Curl -> IO CInt
u_fn i x = u_ptr (10000+i) (castFunPtrToPtr x)

u_off :: CInt -> Word64 -> Curl -> IO CInt
u_off i x cc = curlPrim cc $ \fh -> c_curl_easy_setopt fh (30000+i) ((toEnum . fromIntegral) x)
   

curloptSetFileObj = u_ptr 1
-- | Set the URL for the request
curloptSetURL = u_str 2 
-- | Set the remote port to connect to
curloptSetPort = u_long 3
curloptSetProxy = u_str 4
curloptSetUsername = u_str 173
curloptSetPassword = u_str 174
curloptSetRange = u_str 7
curloptSetInFile = u_str 9
curloptSetErrorBuffer = u_ptr 10

curloptSetWriteFunction = (. flip (u_fn 11)) . (>>=) . mkRwFn
curloptSetReadFunction = (. flip (u_fn 12)) . (>>=) . mkRwFn
curloptSetTimeout =  u_long 13
curloptSetInFileSize = u_long 14
curloptSetPostFields = u_str 15 . intercalate "&" 

curloptSetReferer = u_str 16
curloptFtpPort = u_str 17
-- | Set the user agent string for the request 
curloptSetUserAgent = u_str 18
curloptSetLowSpeed = u_long 19
curloptSetLowSpeedTime = u_long 20
curloptSetResumeFrom = u_long 21
curloptSetCookie = u_str 22
curloptSetHttpHeaders = u_strs 23
curloptSetHttpPost = const undefined -- u_posts um (o 24)
curloptSetSSLCert = u_str 25
curloptSetSSLPassword = u_str 26
curloptSetSSLKeyPassword = u_str 26 -- yes, duplicate.
curloptSetCRLF = u_bool 27
curloptSetQuote = u_strs 28
curloptSetWriteHeader = u_ptr 29
curloptSetCookieFile = u_str 31
curloptSetSSLVersion = u_long 32
curloptSetTimeCondition :: TimeCond -> Curl -> IO CInt
curloptSetTimeCondition = u_enum 33
curloptSetTimeValue = u_long 34
curloptSetCustomRequest = u_str 36

-- CurlStderr x -> u_string um (o 37) x

curloptSetPostQuote = u_strs 39
curloptSetWriteInfo = u_str 40
curloptSetVerbose = u_bool 41
curloptSetHeader= u_bool 42
curloptSetNoProgress = u_bool 43
curloptSetNoBody = u_bool 44
curloptSetFailOnError = u_bool 45

curloptSetUpload = u_bool 46 
curloptSetPost = u_bool 47 
curloptSetFtpListOnly = u_bool 48 
curloptSetFtpAppend = u_bool 50 
curloptSetUseNetRc :: NetRcOption -> Curl -> IO CInt
curloptSetUseNetRc = u_enum 51 
curloptSetFollowLocation = u_bool 52 
curloptSetTransferTextASCII = u_bool 53 
curloptSetPut = u_bool 54 
curloptSetProgressFunction = (. flip (u_fn 56)) . (>>=) . mkProgress 
curloptSetProgressData = u_ptr 57 
curloptSetAutoReferer = u_bool 58 
curloptSetProxyPort = u_long 59 
curloptSetPostFieldSize = u_long 60 
curloptSetHttpProxyTunnel = u_bool 61 
curloptSetInterface = u_str 62 
curloptSetKrb4Level = u_str 63 
curloptSetSSLVerifyPeer = u_bool 64

curloptSetCAInfo = u_str 65
curloptSetMaxRedirs = u_long 68
curloptSetFiletime = u_bool 69
curloptSetTelnetOptions = u_strs 70
curloptSetMaxConnects = u_long 71

curloptSetFreshConnect = u_bool 74
curloptSetForbidReuse = u_bool 75
curloptSetRandomFile = u_str 76
curloptSetEgdSocket = u_str 77
curloptSetConnectTimeout = u_long 78
curloptSetHeaderFunction = (. flip (u_fn 79)) . (>>=) . mkRwFn 
curloptSetHTTPGet = u_bool 80
curloptSetSSLVerifyHost = u_long 81
curloptSetCookieJar = u_str 82

curloptSetSSLCipherList = u_str 83 -- a string (or a l-list of them)?
curloptSetHttpVersion :: HttpVersion -> Curl -> IO CInt
curloptSetHttpVersion = u_enum 84
curloptSetFtpUseEPSV = u_bool 85
curloptSetSSLCertType = u_str 86
curloptSetSSLKey = u_str 87
curloptSetSSLKeyType = u_str 88
curloptSetSSLEngine = u_str 89
curloptSetSSLEngineDefault = u_bool 90 
curloptSetDNSUseGlobalCache = u_bool 91
curloptSetDNSCacheTimeout = u_long 92
curloptSetPreQuote = u_strs 93
curloptSetDebugFunction = (. flip (u_fn 94)) . (>>=) . mkDebugFun 
curloptSetDebugData = u_ptr 95
curloptSetCookieSession = u_bool 96
curloptSetCAPath = u_str 97
curloptSetBufferSize = u_long 98
curloptSetNoSignal = u_bool 99
curloptSetShare = u_ptr 100
curloptSetProxyType :: Int -> Curl -> IO CInt
curloptSetProxyType = u_enum 101
curloptSetEncoding = u_str 102
curloptSetPrivate = u_ptr 103
curloptSetHttp200Aliases = u_str 104 -- correct?
curloptSetUnrestrictedAuth = u_bool 105
curloptSetFtppUseEPRT = u_bool 106
curloptSetHttpAuth :: [HttpAuth] -> Curl -> IO CInt
curloptSetHttpAuth = u_long 107 . toMask
curloptSetSSLCtxFunction = (. flip (u_fn 108)) . (>>=) . mkSslCtxtFun
curloptSetSSLCtxData = u_ptr 109
curloptSetFtpCreateMissingDirs = u_bool 110
curloptSetProxyAuth :: [HttpAuth] -> Curl -> IO CInt
curloptSetProxyAuth = u_long 111 . toMask
curloptSetFtpResponseTimeout = u_long 112
curloptSetIPResolve = u_long 113
curloptSetMaxFileSize = u_long 114

curloptSetInFileSizeLarge = u_off 115
curloptSetResumeFromLarge = u_off 116
curloptSetMaxFileSizeLarge = u_off 117

curloptSetNetrcFile = u_str 118
curloptSetFtpSSL :: Int -> Curl -> IO CInt
curloptSetFtpSSL = u_enum 119
curloptSetPostFieldSizeLarge = u_off 120
curloptSetTCPNoDelay = u_bool 121
curloptSetFtpSSLAuth :: Int -> Curl -> IO CInt
curloptSetFtpSSLAuth = u_enum 129
curloptSetIOCTLFunction _x = undefined -- u_fn 130 x
curloptSetIOCTLData = u_ptr 131
curloptSetFtpAccount = u_str 134
curloptSetCookieList = u_str 135
curloptSetIgnoreContentLength = u_bool 136
curloptSetFtpSkipPASVIP = u_bool 137
curloptSetFtpFileMethod :: Int -> Curl -> IO CInt
curloptSetFtpFileMethod = u_enum 138
curloptSetLocalPort = u_long 139
curloptSetLocalPortRange = u_long 140
curloptSetConnectOnly = u_bool 141
curloptSetConvFromNetworkFunction _x = undefined -- u_fn 142
curloptSetConvToNetworkFunction _x = undefined -- u_fn 143
curloptSetConvFromUtf8Function _x = undefined -- u_fn 144
curloptSetMaxSendSpeedLarge = u_off 145
curloptSetMaxRecvSpeedLarge = u_off 146
curloptSetFtpAlternativeToUser = u_str 147

curloptSetSockOptFunction _x = undefined -- u_fn 148

curloptSetSockOptData = u_ptr 149
curloptSetSSLSessionIdCache = u_bool 150
curloptSetSSHAuthTypes :: [SSHAuthType] -> Curl -> IO CInt
curloptSetSSHAuthTypes = u_long 151 . toMask
curloptSetSSHPublicKeyFile = u_str 152
curloptSetSSHPrivateKeyFile = u_str 153
curloptSetFtpSSLCCC = u_bool 154
curloptSetTimeoutMS = u_long 155
curloptSetConnectTimeoutMS = u_long 156
curloptSetHttpTransferDecoding = u_bool 157
curloptSetHttpContentDecoding =  u_bool 158
curloptSetNewFilePerms = u_long 159
curloptSetNewDirectoryPerms = u_long 160
curloptSetPostRedirect = u_bool 161
curloptSetSSHHostPublicKeyMD5 = u_str 162
curloptSetCopyPostFields = u_bool 165
curloptSetProxyTransferMode = u_long 166
curloptSetCRLFile = u_str 169
curloptSetIssuerCert = u_str 170
curloptSetAddressScope = u_long 171
curloptSetCertInfo = u_long 172
curloptSetProxyUsername = u_str 175
curloptSetProxyPassword = u_str 176
-- -----------------------------------------------------------------------------
{-
data CurlOption
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
  -}
        
