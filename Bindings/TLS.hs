{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module Bindings.TLS where

import Preface.Imports

ssl_load_error_strings :: IO ()
ssl_load_error_strings = c_ssl_load_error_strings

ssl_library_init :: IO ()
ssl_library_init = c_ssl_library_init

sslv23_client_method :: IO SSL_METHOD
sslv23_client_method = c_sslv23_client_method

ssl_ctx_new :: SSL_METHOD -> IO SSL_CTX
ssl_ctx_new = c_ssl_ctx_new

-- bio_new_socket x y = c_bio_new_socket (fromIntegral x) (fromIntegral y)

ssl_new :: SSL_CTX -> IO SSL
ssl_new = c_ssl_new

-- ssl_set_bio = c_ssl_set_bio

ssl_set_fd :: SSL -> Fd -> IO CInt 
ssl_set_fd x (Fd y) = c_ssl_set_fd x y

ssl_set_sock :: SSL -> Socket -> IO CInt
ssl_set_sock x y = c_ssl_set_fd x (fdSocket y)

ssl_connect :: SSL -> IO Int
ssl_connect = fmap fromEnum . c_ssl_connect 

ssl_get_current_cipher :: SSL -> IO CIPHER 
ssl_get_current_cipher = c_ssl_get_current_cipher

ssl_err_get_error :: IO CLong
ssl_err_get_error = c_err_get_error

ssl_cipher_get_name :: CIPHER -> IO String
ssl_cipher_get_name x = peekCString =<< c_ssl_cipher_get_name x

ssl_err_error_string :: CLong -> IO String
ssl_err_error_string n = peekCString =<< c_err_error_string n nullPtr

ssl_errs :: IO [String]
ssl_errs = do 
   e <- ssl_err_get_error
   if e == 0 then return [] else do
      t <- ssl_err_error_string e
      r <- ssl_errs
      return (t : r)

ssl_get_error :: SSL -> CInt -> IO CInt
ssl_get_error = c_ssl_get_error

ssl_get_peer_certificate = c_ssl_get_peer_certificate

x509_get_subject_name = c_x509_get_subject_name
x509_get_issuer_name = c_x509_get_issuer_name

x509_name_oneline x = peekCString =<< c_x509_name_oneline x nullPtr 0

ssl_write x b = do
  r <- unsafeUseAsCStringLen b (\(p,q) -> c_ssl_write x p (fromIntegral q))
  return r

ssl_get_rfd :: SSL -> IO Fd
ssl_get_rfd = fmap Fd . c_ssl_get_rfd

ssl_get_wfd :: SSL -> IO Fd
ssl_get_wfd = fmap Fd . c_ssl_get_wfd

ssl_read :: SSL -> IO (Either CInt ByteString)
ssl_read x = do
  let bs = 4096
  fbuff <- mallocForeignPtrBytes bs
  a <- withForeignPtr fbuff $ \p -> c_ssl_read x (castPtr p) (fromIntegral bs)
  if a > 0 then do
     return $ Right (fromForeignPtr fbuff 0 (fromEnum a))
  else do
     return $ Left a

{- 
# define SSL_ERROR_NONE                  0
# define SSL_ERROR_SSL                   1
# define SSL_ERROR_WANT_READ             2
# define SSL_ERROR_WANT_WRITE            3
# define SSL_ERROR_WANT_X509_LOOKUP      4
# define SSL_ERROR_SYSCALL               5/* look at error stack/return
                                           * value/errno */
# define SSL_ERROR_ZERO_RETURN           6
# define SSL_ERROR_WANT_CONNECT          7
# define SSL_ERROR_WANT_ACCEPT           8
-}

foreign import ccall "SSL_load_error_strings" c_ssl_load_error_strings :: IO ()
foreign import ccall "SSL_library_init" c_ssl_library_init :: IO ()

data SSL_CTX_
type SSL_CTX = Ptr SSL_CTX_

data SSL_METHOD_
type SSL_METHOD = Ptr SSL_METHOD_

data SSL_
type SSL = Ptr SSL_

foreign import ccall "SSLv23_client_method" c_sslv23_client_method :: IO SSL_METHOD
foreign import ccall "SSL_CTX_new" c_ssl_ctx_new :: SSL_METHOD -> IO SSL_CTX
foreign import ccall "SSL_new" c_ssl_new :: SSL_CTX -> IO SSL

data BIO_
type BIO = Ptr BIO_

foreign import ccall "BIO_new_socket" c_bio_new_socket :: CInt -> CInt -> IO BIO

foreign import ccall "SSL_set_bio" c_ssl_set_bio :: SSL -> BIO -> BIO -> IO ()

foreign import ccall "SSL_connect" c_ssl_connect :: SSL -> IO CInt

foreign import ccall "SSL_set_fd" c_ssl_set_fd :: SSL -> CInt -> IO CInt

data CIPHER_
type CIPHER = Ptr CIPHER_

foreign import ccall "SSL_get_current_cipher" c_ssl_get_current_cipher :: SSL -> IO CIPHER

foreign import ccall "SSL_CIPHER_get_name" c_ssl_cipher_get_name :: CIPHER -> IO CString

foreign import ccall "ERR_get_error" c_err_get_error :: IO CLong
foreign import ccall "ERR_error_string" c_err_error_string :: CLong -> Ptr CChar -> IO CString

foreign import ccall "SSL_get_error" c_ssl_get_error :: SSL -> CInt -> IO CInt

data X509_
type X509 = Ptr X509_

foreign import ccall "SSL_get_peer_certificate" c_ssl_get_peer_certificate :: SSL -> IO X509

data X509_NAME_
type X509_NAME = Ptr X509_NAME_

foreign import ccall "X509_get_subject_name" c_x509_get_subject_name :: X509 -> IO X509_NAME
foreign import ccall "X509_get_issuer_name" c_x509_get_issuer_name :: X509 -> IO X509_NAME

foreign import ccall "X509_NAME_oneline" c_x509_name_oneline :: X509_NAME -> CString -> CInt -> IO CString

foreign import ccall "SSL_read" c_ssl_read :: SSL -> Ptr CChar -> CInt -> IO CInt
foreign import ccall "SSL_write" c_ssl_write :: SSL -> Ptr CChar -> CInt -> IO CInt

foreign import ccall "SSL_get_rfd" c_ssl_get_rfd :: SSL -> IO CInt
foreign import ccall "SSL_get_wfd" c_ssl_get_wfd :: SSL -> IO CInt


