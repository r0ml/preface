
import Preface.R0ml

main = do
   ssl_load_error_strings
   ssl_library_init
   ctx <- ssl_ctx_new =<< sslv23_client_method

   ssl <- ssl_new ctx

   mapM_ strPutLn =<< ssl_errs

   let host="localhost"
       port = 443
   let hints = defaultHints {addrFamily = AF_INET, addrSocketType = Stream}
   addrInfos <- getAddrInfo (Just hints) (Just host) (Just $ show port)
   sock      <- socket AF_INET Stream defaultProtocol

    -- Connect WebSocket and run client
   -- finally (
   sktConnect sock (addrAddress $ head addrInfos)
   -- >>
   --          doClient sock (host++":"++show port) path app)
   --        (sClose sock)




   strPutLn =<< fmap show (getSocketName sock)
   strPutLn =<< fmap show (getPeerName sock)

   -- sbio <- bio_new_socket (fdSocket sock) 0
   r <- ssl_set_sock ssl sock
   strPutLn (show r)

   do_connect ssl 
 
   mapM_ strPutLn =<< ssl_errs

   cp <- ssl_get_current_cipher ssl

   mapM_ strPutLn =<< ssl_errs

   cn <- ssl_cipher_get_name cp


   mapM_ strPutLn =<< ssl_errs
   strPutLn cn

   strPutLn =<< showCert ssl


   e <- ssl_write ssl (asByteString "GET / HTTP/1.0\r\n\r\n")
   strPutLn (show e)

   z <- do_read ssl
   strPutLn (show z)

   z2 <- do_read ssl
   strPutLn (show z2)

   strPutLn (show sock)
   strPutLn (show ctx) 
   strPutLn "done"

{-
   if ((ctx = SSL_CTX_new(SSLv23_client_method())) == NULL)
        fatalx("ctx");
   if (!SSL_CTX_load_verify_locations(ctx, SSL_CA_CRT, NULL))
        fatalx("verify");
   if (!SSL_CTX_use_certificate_file(ctx, SSL_CLIENT_CRT, SSL_FILETYPE_PEM))
        fatalx("cert");
   if (!SSL_CTX_use_PrivateKey_file(ctx, SSL_CLIENT_KEY, SSL_FILETYPE_PEM))
        fatalx("key");
   if (!SSL_CTX_check_private_key(ctx))
        fatalx("cert/key");
   SSL_CTX_set_mode(ctx, SSL_MODE_AUTO_RETRY);
   SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
   SSL_CTX_set_verify_depth(ctx, 1);
   /* setup connection */
   if ((hp = gethostbyname("localhost")) == NULL)
        err(EX_OSERR, "gethostbyname");
   /* init socket – socket()/connect() */
   /* go do ssl magic */
   ssl = SSL_new(ctx);
   sbio = BIO_new_socket(sock, BIO_NOCLOSE);
   SSL_set_bio(ssl, sbio, sbio);
   if (SSL_connect(ssl) <= 0)
        fatalx("SSL_connect");
   if (SSL_get_verify_result(ssl) != X509_V_OK)
        fatalx("cert");
   printf("connected to server!\n");
   SSL_free(ssl);
   BIO_free_all(sbio);
   SSL_CTX_free(ctx);
-}

showCert :: SSL -> IO String
showCert ssl = do
    cert <- ssl_get_peer_certificate ssl -- get the server's certificate
    if cert == nullPtr then return "No certificates"
    else do
        subj <- x509_name_oneline =<< x509_get_subject_name cert
        -- free the result of x509_get_subject_name ?
        iss <- x509_name_oneline =<< x509_get_issuer_name cert
        -- x509_free cert
        return (subj ++ " <--> " ++ iss)

do_connect ssl = do
   e <- ssl_connect ssl
   if e < 0 then do
      a <- ssl_get_error ssl (fromIntegral e)
      if a == 2 then do
         f <- ssl_get_rfd ssl
         threadWaitRead f
         do_connect ssl
         strPutLn "connected?"
      else do
         strPutLn $ "e<0: "++(show a)
   else do
      strPutLn $ "e>=0: "++(show e)
  
do_read ssl = do
  e <- ssl_read ssl
  case e of 
     Right b -> return b
     Left b -> if b < 0 then do
         a <- ssl_get_error ssl (fromIntegral b)
         if a == 2 then do
             f <- ssl_get_rfd ssl
             threadWaitRead f
             do_read ssl
         else do
             strPutLn $ "a<0: "++(show a)
             return strEmpty
     else do
        strPutLn $ "a>=0: "++(show b) 
        return strEmpty
