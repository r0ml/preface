{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Bindings.CurlX ( Curl, curl_debug, mkCurl
                      , curlPrim, c_curl_easy_setopt, c_curl_easy_setopt_p
                      , c_easy_perform, c_curl_easy_getinfo
                      , curl_add_finalizer
                      , _CURLINFO_LONG, _CURLINFO_STRING, _CURLINFO_DOUBLE, _CURLINFO_SLIST
                      , mkSslCtxtFun, mkProgress, mkRwFn, mkDebugFun
                      ,  SSLCtxtFunction, DebugFunction, ProgressFunction, ReadWriteFunction(..)
                      , c_curloptDefines, mkEnumCurl
                      , CurlInfoValueType(..), CurlCode(..)
                      , curl_version_number, curl_version_string
                      )
       where

import Preface.Str (enum, str)
import Foreign.C.Types (CLong(..), CInt(..), CChar)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr_)
import Control.Concurrent (MVar, newMVar, withMVar)
import Foreign.Concurrent (addForeignPtrFinalizer)
import Data.Char (isSpace)
import Language.Haskell.TH

-- | a Curl is the opaque datatype which represents the interface to libcurl
-- As libcurl is not thread-safe, 'Curl' wraps the ForeignPtr to the underlying
-- data structure behind an 'MVar' in an attempt to keep any libcurl operations
-- single threaded.
data Curl = Curl { curl_struct :: MVar (ForeignPtr Curl_struct) }

data Curl_struct
type CurlStruct = Ptr Curl_struct

newtype ReadWriteFunction = ReadWriteFunction (
    Ptr CChar  --  pointer to external buffer holding data
    -> CInt       --  width (in bytes) of each item
    -> CInt       --  number of items
    -> Ptr ()     --  state argument (file pointer etc.)
    -> IO CInt    --  number of bytes written.
    )

instance Show ReadWriteFunction where show _ = "<fun>"

newtype ProgressFunction = ProgressFunction (
  Ptr ()  --  state argument
  -> Double  --  expected download totals
  -> Double  --  download totals so far
  -> Double  --  expected upload totals
  -> Double  --  upload totals so far
  -> IO CInt --  not sure; 0 is a good one.
  )

instance Show ProgressFunction where show _ = "<fun>"
 
newtype DebugFunction = DebugFunction (
  CurlStruct      --  connection handle
  -> DebugInfo  --  type of call
  -> Ptr CChar  --  data buffer
  -> CInt       --  length of buffer
  -> Ptr ()     --  state argument
  -> IO ()      --  always 0
  )

instance Show DebugFunction where show _ = "<fun>"
          
data DebugInfo
 = InfoText
 | InfoHeaderIn
 | InfoHeaderOut
 | InfoDataIn
 | InfoDataOut
 | InfoSslDataIn
 | InfoSslDataOut
   deriving ( Eq, Enum )

type DebugFunctionPrim
  = CurlStruct      --  connection handle
 -> CInt       --  type of call
 -> Ptr CChar  --  data buffer
 -> CInt       --  length of buffer
 -> Ptr ()     --  state argument
 -> IO CInt    --  always 0

type SSLCtxtFunction
  = CurlStruct   --  connection handle
 -> Ptr ()  --  the SSL_CTX handle
 -> Ptr ()  --  state argument
 -> IO CInt

instance Show SSLCtxtFunction where show _ = "<fun>"
                                 
#include <curl/curl.h>

_CURLINFO_LONG :: CInt
_CURLINFO_LONG = (#const CURLINFO_LONG)
_CURLINFO_STRING :: CInt
_CURLINFO_STRING = (#const CURLINFO_STRING)
_CURLINFO_DOUBLE :: CInt
_CURLINFO_DOUBLE = (#const CURLINFO_DOUBLE)
_CURLINFO_SLIST :: CInt
_CURLINFO_SLIST = (#const CURLINFO_SLIST)

curl_version_number :: Int
curl_version_number = #const LIBCURL_VERSION_NUM

curl_version_string :: String
curl_version_string = #const_str LIBCURL_VERSION

foreign import ccall "curl/easy.h curl_easy_getinfo" c_curl_easy_getinfo :: CurlStruct -> CLong -> Ptr () -> IO CInt

-- foreign import ccall "curl/easy.h curl_global_init" c_curl_global_init :: CInt -> IO CInt
-- foreign import ccall "curl/easy.h curl_global_cleanup" c_curl_global_cleanup :: IO ()

foreign import ccall "curl/easy.h curl_easy_init" c_curl_easy_init :: IO CurlStruct
foreign import ccall "curl/easy.h curl_easy_cleanup" c_curl_easy_cleanup :: CurlStruct -> IO ()

foreign import ccall "curl/easy.h curl_easy_perform" c_easy_perform :: CurlStruct -> IO CInt
-- foreign import ccall "curl/easy.h curl_easy_reset" c_curl_easy_reset :: CurlStruct -> IO ()

foreign import ccall "curl_easy_setopt" c_curl_easy_setopt :: CurlStruct -> CInt -> CLong -> IO CInt
foreign import ccall "curl_easy_setopt" c_curl_easy_setopt_p :: CurlStruct -> CInt -> Ptr () -> IO CInt

foreign import ccall "wrapper" mkRwFn :: ReadWriteFunction -> IO (FunPtr ReadWriteFunction)
foreign import ccall "wrapper" mkProgress :: ProgressFunction -> IO (FunPtr ProgressFunction)
foreign import ccall "wrapper" c_mkDebugFun :: DebugFunctionPrim -> IO (FunPtr DebugFunctionPrim)
foreign import ccall "wrapper" mkSslCtxtFun :: SSLCtxtFunction -> IO (FunPtr SSLCtxtFunction)

mkDebugFun :: DebugFunction -> IO (FunPtr DebugFunctionPrim)
mkDebugFun (DebugFunction x) = do
  let wrapFun f _a b c d e = f _a (toEnum (fromIntegral b)) c d e >> return 0
  fp <- c_mkDebugFun (wrapFun x)
  -- somewhere I have to stick in the freeHaskellFunPtr code (foreignptr?)
  return fp

curlPrim :: Curl -> ( CurlStruct -> IO a) -> IO a
curlPrim c f  = withMVar (curl_struct c) $ \h -> withForeignPtr h f 

-- | Allocates a Haskell handle from a C handle.
mkCurl :: IO Curl
mkCurl = do
  curl_debug "ALLOC: CURL"
  h <- c_curl_easy_init
  fh  <- newForeignPtr_ h -- wrap it as a foreignptr ( for finalization)
  v1  <- newMVar fh -- wrap it behind an MVar to prevent multithreaded access
  let fnalizr = curl_debug "FREE: CURL" >> c_curl_easy_cleanup h
  Foreign.Concurrent.addForeignPtrFinalizer fh fnalizr
  return Curl { curl_struct = v1 }

curl_add_finalizer :: Curl -> (IO ()) -> IO ()
curl_add_finalizer h g = withMVar (curl_struct h) $ \fh -> Foreign.Concurrent.addForeignPtrFinalizer fh g

curl_debug :: String -> IO ()
-- curl_debug msg = trace msg $ return ()

-- when not debugging ....
curl_debug _msg = return ()


c_curloptDefines :: String
c_curloptDefines = [str|
  CURLINFO_EFFECTIVE_URL    = CURLINFO_STRING + 1,
  CURLINFO_RESPONSE_CODE    = CURLINFO_LONG   + 2,
  CURLINFO_TOTAL_TIME       = CURLINFO_DOUBLE + 3,
  CURLINFO_NAMELOOKUP_TIME  = CURLINFO_DOUBLE + 4,
  CURLINFO_CONNECT_TIME     = CURLINFO_DOUBLE + 5,
  CURLINFO_PRETRANSFER_TIME = CURLINFO_DOUBLE + 6,
  CURLINFO_SIZE_UPLOAD      = CURLINFO_DOUBLE + 7,
  CURLINFO_SIZE_DOWNLOAD    = CURLINFO_DOUBLE + 8,
  CURLINFO_SPEED_DOWNLOAD   = CURLINFO_DOUBLE + 9,
  CURLINFO_SPEED_UPLOAD     = CURLINFO_DOUBLE + 10,
  CURLINFO_HEADER_SIZE      = CURLINFO_LONG   + 11,
  CURLINFO_REQUEST_SIZE     = CURLINFO_LONG   + 12,
  CURLINFO_SSL_VERIFYRESULT = CURLINFO_LONG   + 13,
  CURLINFO_FILETIME         = CURLINFO_LONG   + 14,
  CURLINFO_CONTENT_LENGTH_DOWNLOAD   = CURLINFO_DOUBLE + 15,
  CURLINFO_CONTENT_LENGTH_UPLOAD     = CURLINFO_DOUBLE + 16,
  CURLINFO_STARTTRANSFER_TIME = CURLINFO_DOUBLE + 17,
  CURLINFO_CONTENT_TYPE     = CURLINFO_STRING + 18,
  CURLINFO_REDIRECT_TIME    = CURLINFO_DOUBLE + 19,
  CURLINFO_REDIRECT_COUNT   = CURLINFO_LONG   + 20,
  CURLINFO_PRIVATE          = CURLINFO_STRING + 21,
  CURLINFO_HTTP_CONNECTCODE = CURLINFO_LONG   + 22,
  CURLINFO_HTTPAUTH_AVAIL   = CURLINFO_LONG   + 23,
  CURLINFO_PROXYAUTH_AVAIL  = CURLINFO_LONG   + 24,
  CURLINFO_OS_ERRNO         = CURLINFO_LONG   + 25,
  CURLINFO_NUM_CONNECTS     = CURLINFO_LONG   + 26,
  CURLINFO_SSL_ENGINES      = CURLINFO_SLIST  + 27,
  CURLINFO_COOKIELIST       = CURLINFO_SLIST  + 28,
  CURLINFO_LASTSOCKET       = CURLINFO_LONG   + 29,
  CURLINFO_FTP_ENTRY_PATH   = CURLINFO_STRING + 30,
  CURLINFO_REDIRECT_URL     = CURLINFO_STRING + 31,
  CURLINFO_PRIMARY_IP       = CURLINFO_STRING + 32,
  CURLINFO_APPCONNECT_TIME  = CURLINFO_DOUBLE + 33,
  CURLINFO_CERTINFO         = CURLINFO_SLIST  + 34,
  CURLINFO_CONDITION_UNMET  = CURLINFO_LONG   + 35,
  CURLINFO_RTSP_SESSION_ID  = CURLINFO_STRING + 36,
  CURLINFO_RTSP_CLIENT_CSEQ = CURLINFO_LONG   + 37,
  CURLINFO_RTSP_SERVER_CSEQ = CURLINFO_LONG   + 38,
  CURLINFO_RTSP_CSEQ_RECV   = CURLINFO_LONG   + 39,
  CURLINFO_PRIMARY_PORT     = CURLINFO_LONG   + 40,
  CURLINFO_LOCAL_IP         = CURLINFO_STRING + 41,
  CURLINFO_LOCAL_PORT       = CURLINFO_LONG   + 42,
  CURLINFO_TLS_SESSION      = CURLINFO_SLIST  + 43,
|]

mkEnumCurl :: String -> String -> Q [Dec]
mkEnumCurl h s = let m = map parseCurlEnum (filter (not . null) (lines s))
                 in genEnumCurl h m
  where
    genEnumCurl :: String -> [(String, String, Int)] -> Q [Dec]
    genEnumCurl name vals = do
      let nam = mkName name
      dd <- dataD (cxt[]) nam [] (dv vals) [''Eq, ''Bounded, ''Show]
      fe <- instanceD (cxt [])
            (appT (conT ''Enum) (conT nam))
            [funD (mkName "fromEnum") $ map genClause vals,
             funD (mkName "toEnum") $ map genClause2 vals]
{-    fe <- [d|instance Enum $(conT nam) where
               toEnum = fromJust . flip lookup (map swap $(vals))
               fromEnum = fromJust . flip lookup $(vals)
          |]-}
      gi <- funD (mkName "getInfo") $ map genClause3 vals
      -- this line generates the type signature for getInfo
      -- mainly this is to avoid warnings when mkEnumCurl is used to define @getInfo@
      tb <- sigD (mkName "getInfo") $ appT (appT arrowT (conT nam)) (appT (appT arrowT (conT (mkName "Curl"))) (appT (conT (mkName "IO")) (conT (mkName "CurlInfoValue"))))
      return [dd, fe, tb, gi]
    dv vals = map (\(n,_,_) -> normalC (mkName n) []) vals
    genClause (k,_,v) = clause [(conP (mkName k) [])] (normalB [|v|]) []
    genClause2 (v,_,k) = clause [(litP (integerL (fromIntegral k)))] (normalB  (conE (mkName v))) []
    genClause3 (k,t,v) = clause [(conP (mkName k) [])] (normalB (appE (appE (varE (mkName "doGetInfo")) (conE (mkName t))) [|v|])) [] 
    trim x = reverse (dropWhile isSpace ( reverse (dropWhile isSpace x)))
    parseCurlEnum ss = let (z,_) = break (==',') ss
                           (x,y) = break (=='=') z
                           (a,b) = break (=='+') (tail y)
                       in (trim x, (trim a), read (trim (tail b)) :: Int)
                                         

data CurlInfoValueType = CURLINFO_STRING | CURLINFO_LONG | CURLINFO_DOUBLE | CURLINFO_SLIST
     deriving (Show, Eq)


-- | libcurl error codes copied from @curl.h@ and quasi-quoted into 'CurlCode'
[enum|CurlCode
  CURLE_OK,
  CURLE_UNSUPPORTED_PROTOCOL,    /* 1 */
  CURLE_FAILED_INIT,             /* 2 */
  CURLE_URL_MALFORMAT,           /* 3 */
  CURLE_NOT_BUILT_IN,            /* 4 - [was obsoleted in August 2007 for
                                    7.17.0, reused in April 2011 for 7.21.5] */
  CURLE_COULDNT_RESOLVE_PROXY,   /* 5 */
  CURLE_COULDNT_RESOLVE_HOST,    /* 6 */
  CURLE_COULDNT_CONNECT,         /* 7 */
  CURLE_FTP_WEIRD_SERVER_REPLY,  /* 8 */
  CURLE_REMOTE_ACCESS_DENIED,    /* 9 a service was denied by the server
                                    due to lack of access - when login fails
                                    this is not returned. */
  CURLE_FTP_ACCEPT_FAILED,       /* 10 - [was obsoleted in April 2006 for
                                    7.15.4, reused in Dec 2011 for 7.24.0]*/
  CURLE_FTP_WEIRD_PASS_REPLY,    /* 11 */
  CURLE_FTP_ACCEPT_TIMEOUT,      /* 12 - timeout occurred accepting server
                                    [was obsoleted in August 2007 for 7.17.0,
                                    reused in Dec 2011 for 7.24.0]*/
  CURLE_FTP_WEIRD_PASV_REPLY,    /* 13 */
  CURLE_FTP_WEIRD_227_FORMAT,    /* 14 */
  CURLE_FTP_CANT_GET_HOST,       /* 15 */
  CURLE_OBSOLETE16,              /* 16 - NOT USED */
  CURLE_FTP_COULDNT_SET_TYPE,    /* 17 */
  CURLE_PARTIAL_FILE,            /* 18 */
  CURLE_FTP_COULDNT_RETR_FILE,   /* 19 */
  CURLE_OBSOLETE20,              /* 20 - NOT USED */
  CURLE_QUOTE_ERROR,             /* 21 - quote command failure */
  CURLE_HTTP_RETURNED_ERROR,     /* 22 */
  CURLE_WRITE_ERROR,             /* 23 */
  CURLE_OBSOLETE24,              /* 24 - NOT USED */
  CURLE_UPLOAD_FAILED,           /* 25 - failed upload "command" */
  CURLE_READ_ERROR,              /* 26 - couldn't open/read from file */
  CURLE_OUT_OF_MEMORY,           /* 27 */
  /* Note: CURLE_OUT_OF_MEMORY may sometimes indicate a conversion error
           instead of a memory allocation error if CURL_DOES_CONVERSIONS
           is defined
  */
  CURLE_OPERATION_TIMEDOUT,      /* 28 - the timeout time was reached */
  CURLE_OBSOLETE29,              /* 29 - NOT USED */
  CURLE_FTP_PORT_FAILED,         /* 30 - FTP PORT operation failed */
  CURLE_FTP_COULDNT_USE_REST,    /* 31 - the REST command failed */
  CURLE_OBSOLETE32,              /* 32 - NOT USED */
  CURLE_RANGE_ERROR,             /* 33 - RANGE "command" didn't work */
  CURLE_HTTP_POST_ERROR,         /* 34 */
  CURLE_SSL_CONNECT_ERROR,       /* 35 - wrong when connecting with SSL */
  CURLE_BAD_DOWNLOAD_RESUME,     /* 36 - couldn't resume download */
  CURLE_FILE_COULDNT_READ_FILE,  /* 37 */
  CURLE_LDAP_CANNOT_BIND,        /* 38 */
  CURLE_LDAP_SEARCH_FAILED,      /* 39 */
  CURLE_OBSOLETE40,              /* 40 - NOT USED */
  CURLE_FUNCTION_NOT_FOUND,      /* 41 */
  CURLE_ABORTED_BY_CALLBACK,     /* 42 */
  CURLE_BAD_FUNCTION_ARGUMENT,   /* 43 */
  CURLE_OBSOLETE44,              /* 44 - NOT USED */
  CURLE_INTERFACE_FAILED,        /* 45 - CURLOPT_INTERFACE failed */
  CURLE_OBSOLETE46,              /* 46 - NOT USED */
  CURLE_TOO_MANY_REDIRECTS ,     /* 47 - catch endless re-direct loops */
  CURLE_UNKNOWN_OPTION,          /* 48 - User specified an unknown option */
  CURLE_TELNET_OPTION_SYNTAX ,   /* 49 - Malformed telnet option */
  CURLE_OBSOLETE50,              /* 50 - NOT USED */
  CURLE_PEER_FAILED_VERIFICATION, /* 51 - peer's certificate or fingerprint
                                     wasn't verified fine */
  CURLE_GOT_NOTHING,             /* 52 - when this is a specific error */
  CURLE_SSL_ENGINE_NOTFOUND,     /* 53 - SSL crypto engine not found */
  CURLE_SSL_ENGINE_SETFAILED,    /* 54 - can not set SSL crypto engine as
                                    default */
  CURLE_SEND_ERROR,              /* 55 - failed sending network data */
  CURLE_RECV_ERROR,              /* 56 - failure in receiving network data */
  CURLE_OBSOLETE57,              /* 57 - NOT IN USE */
  CURLE_SSL_CERTPROBLEM,         /* 58 - problem with the local certificate */
  CURLE_SSL_CIPHER,              /* 59 - couldn't use specified cipher */
  CURLE_SSL_CACERT,              /* 60 - problem with the CA cert (path?) */
  CURLE_BAD_CONTENT_ENCODING,    /* 61 - Unrecognized/bad encoding */
  CURLE_LDAP_INVALID_URL,        /* 62 - Invalid LDAP URL */
  CURLE_FILESIZE_EXCEEDED,       /* 63 - Maximum file size exceeded */
  CURLE_USE_SSL_FAILED,          /* 64 - Requested FTP SSL level failed */
  CURLE_SEND_FAIL_REWIND,        /* 65 - Sending the data requires a rewind
                                    that failed */
  CURLE_SSL_ENGINE_INITFAILED,   /* 66 - failed to initialise ENGINE */
  CURLE_LOGIN_DENIED,            /* 67 - user, password or similar was not
                                    accepted and we failed to login */
  CURLE_TFTP_NOTFOUND,           /* 68 - file not found on server */
  CURLE_TFTP_PERM,               /* 69 - permission problem on server */
  CURLE_REMOTE_DISK_FULL,        /* 70 - out of disk space on server */
  CURLE_TFTP_ILLEGAL,            /* 71 - Illegal TFTP operation */
  CURLE_TFTP_UNKNOWNID,          /* 72 - Unknown transfer ID */
  CURLE_REMOTE_FILE_EXISTS,      /* 73 - File already exists */
  CURLE_TFTP_NOSUCHUSER,         /* 74 - No such user */
  CURLE_CONV_FAILED,             /* 75 - conversion failed */
  CURLE_CONV_REQD,               /* 76 - caller must register conversion
                                    callbacks using curl_easy_setopt options
                                    CURLOPT_CONV_FROM_NETWORK_FUNCTION,
                                    CURLOPT_CONV_TO_NETWORK_FUNCTION, and
                                    CURLOPT_CONV_FROM_UTF8_FUNCTION */
  CURLE_SSL_CACERT_BADFILE,      /* 77 - could not load CACERT file, missing
                                    or wrong format */
  CURLE_REMOTE_FILE_NOT_FOUND,   /* 78 - remote file not found */
  CURLE_SSH,                     /* 79 - error from the SSH layer, somewhat
                                    generic so the error message will be of
                                    interest when this has happened */

  CURLE_SSL_SHUTDOWN_FAILED,     /* 80 - Failed to shut down the SSL
                                    connection */
  CURLE_AGAIN,                   /* 81 - socket is not ready for send/recv,
                                    wait till it's ready and try again (Added
                                    in 7.18.2) */
  CURLE_SSL_CRL_BADFILE,         /* 82 - could not load CRL file, missing or
                                    wrong format (Added in 7.19.0) */
  CURLE_SSL_ISSUER_ERROR,        /* 83 - Issuer check failed.  (Added in
                                    7.19.0) */
  CURLE_FTP_PRET_FAILED,         /* 84 - a PRET command failed */
  CURLE_RTSP_CSEQ_ERROR,         /* 85 - mismatch of RTSP CSeq numbers */
  CURLE_RTSP_SESSION_ERROR,      /* 86 - mismatch of RTSP Session Ids */
  CURLE_FTP_BAD_FILE_LIST,       /* 87 - unable to parse FTP file list */
  CURLE_CHUNK_FAILED,            /* 88 - chunk callback reported error */
  CURLE_NO_CONNECTION_AVAILABLE, /* 89 - No connection available, the
                                    session will be queued */
|]
