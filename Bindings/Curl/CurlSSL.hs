{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bindings.Curl.CurlSSL where

import Preface.Imports
import Bindings.Curl.CurlX

type SSLCtxtFunction
  = CurlStruct   --  connection handle
 -> Ptr ()  --  the SSL_CTX handle
 -> Ptr ()  --  state argument
 -> IO CInt

instance Show SSLCtxtFunction where show _ = "<fun>"
                                 
foreign import ccall "wrapper" mkSslCtxtFun :: SSLCtxtFunction -> IO (FunPtr SSLCtxtFunction)

