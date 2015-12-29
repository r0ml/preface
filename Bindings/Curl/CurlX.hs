{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module Bindings.Curl.CurlX ( 
                        Curl
                      , mkCurl
                      , curlPrim
                      , c_curl_easy_setopt, c_curl_easy_setopt_p
                      , c_curl_slist_append, c_curl_slist_free
                      , c_easy_perform, c_curl_easy_getinfo
                      , curl_add_finalizer
                      , mkProgress
                      , mkRwFn
                      , mkDebugFun
                      , DebugFunction, ProgressFunction
                      , ReadWriteFunction(..)
                      , CurlStruct
                      )
       where

import Preface.Imports

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

foreign import ccall "curl/easy.h curl_easy_getinfo" c_curl_easy_getinfo :: CurlStruct -> CLong -> Ptr () -> IO CInt

foreign import ccall "curl/easy.h curl_easy_init" c_curl_easy_init :: IO CurlStruct

foreign import ccall "curl/easy.h curl_easy_cleanup" c_curl_easy_cleanup :: CurlStruct -> IO ()

foreign import ccall "curl/easy.h curl_easy_perform" c_easy_perform :: CurlStruct -> IO CInt
-- foreign import ccall "curl/easy.h curl_easy_reset" c_curl_easy_reset :: CurlStruct -> IO ()

foreign import ccall "curl_easy_setopt" c_curl_easy_setopt :: CurlStruct -> CInt -> CLong -> IO CInt
foreign import ccall "curl_easy_setopt" c_curl_easy_setopt_p :: CurlStruct -> CInt -> Ptr () -> IO CInt

foreign import ccall "wrapper" mkRwFn :: ReadWriteFunction -> IO (FunPtr ReadWriteFunction)
foreign import ccall "wrapper" mkProgress :: ProgressFunction -> IO (FunPtr ProgressFunction)
foreign import ccall "wrapper" c_mkDebugFun :: DebugFunctionPrim -> IO (FunPtr DebugFunctionPrim)

mkDebugFun :: DebugFunction -> IO (FunPtr DebugFunctionPrim)
mkDebugFun (DebugFunction x) = do
  let wrapFun f _a b c d e = f _a (toEnum (fromIntegral b)) c d e >> return 0
  fp <- c_mkDebugFun (wrapFun x)
  -- somewhere I have to stick in the freeHaskellFunPtr code (foreignptr?)
  return fp

curlPrim :: Curl -> ( CurlStruct -> IO a) -> IO a
curlPrim c f  = withMVar (curl_struct c) $ \h -> do
   a <- withForeignPtr h f 
   return a

-- | Allocates a Haskell handle from a C handle.
mkCurl :: IO Curl
mkCurl = do
  h <- c_curl_easy_init
  fh  <- newForeignPtr_ h -- wrap it as a foreignptr ( for finalization)
  v1  <- newMVar fh -- wrap it behind an MVar to prevent multithreaded access
  let fnalizr = c_curl_easy_cleanup h
  addForeignPtrFinalizer fh fnalizr
  return Curl { curl_struct = v1 }

curl_add_finalizer :: Curl -> (IO ()) -> IO ()
curl_add_finalizer h g = withMVar (curl_struct h) $ \fh -> addForeignPtrFinalizer fh g

data CurlInfoValueType = CURLINFO_STRING | CURLINFO_LONG | CURLINFO_DOUBLE | CURLINFO_SLIST
     deriving (Show, Eq)


foreign import ccall "curl_slist_append" c_curl_slist_append :: Ptr () -> CString -> IO (Ptr ())
foreign import ccall "curl_slist_free_all" c_curl_slist_free :: Ptr () -> IO ()
-- foreign import ccall "curl_formfree" curl_formfree :: Ptr a -> IO ()
  
