{-# LANGUAGE OverloadedStrings, StandaloneDeriving, DeriveDataTypeable #-}

module Github where

import Preface

-- | user/password for HTTP basic access authentication
data GithubAuth = GithubBasicAuth ByteString ByteString | GithubOAuth ByteString deriving (Show, Eq, Ord)

buildUrl :: [String] -> String
buildUrl paths = "https://api.github.com/" ++ intercalate "/" paths

-- ["users", username, "gists"]
githubPublic :: [String] -> IO (Either String JSON) 
githubPublic action = do
       -- (idx, key, region) <- getCred
       -- time <- getCurrentTime
       -- let endpoint = "sdb" ++ region ++ ".amazonaws.com"
           -- qheader = [("Action", action),
           --            ("Version", apiVersion),
           --            ("SignatureMethod", "HmacSHA256"),
           --            ("Timestamp", awsTimeFormat time),
           --            ("AWSAccessKeyId", idx)]
           -- signa = "/?" ++ signatureV2 endpoint key qheader
           -- req = "https://"++endpoint++signa
       r <- withCurlDo $ \crl -> do
              -- curloptSetUserAgent "r0hkit" crl
              curlGet (buildUrl action) [curloptSetUserAgent "r0hkit"] crl
            
       return (readJSON (asString (respBody r))) 

{-
githubGet :: String -> Maybe GithubAuth -> Maybe ByteString -> IO (Either Error ByteString)
githubGet auth body = do
  result <- doHttps apimethod url auth (encodeBody body)
  case result of
      Left e     -> return (Left (HTTPConnectionError e))
      Right resp -> either Left (\x -> jsonResultToE (LBS.pack (show x))
                                                   (fromJSON x))
                          <$> handleBody resp

  where
    encodeBody = Just . RequestBodyLBS . encode . toJSON

    handleBody resp = either (return . Left) (handleJson resp)
                             (parseJsonRaw (responseBody resp))

    -- This is an "escaping" version of "for", which returns (Right esc) if
    -- the value 'v' is Nothing; otherwise, it extracts the value from the
    -- Maybe, applies f, and return an IO (Either Error b).
    forE :: b -> Maybe a -> (a -> IO (Either Error b))
         -> IO (Either Error b)
    forE = flip . maybe . return . Right

    handleJson resp gotjson@(Array ary) =
        -- Determine whether the output was paginated, and if so, we must
        -- recurse to obtain the subsequent pages, and append those result
        -- bodies to the current one.  The aggregate will then be parsed.
        forE gotjson (lookup "Link" (responseHeaders resp)) $ \l ->
            forE gotjson (getNextUrl (BS.unpack l)) $ \nu ->
                either (return . Left . HTTPConnectionError)
                       (\nextResp -> do
                             nextJson <- handleBody nextResp
                             return $ (\(Array x) -> Array (ary <> x))
                                          <$> nextJson)
                       =<< doHttps apimethod nu auth Nothing
    handleJson _ gotjson = return (Right gotjson)

    getNextUrl l =
        if "rel=\"next\"" `isInfixOf` l
        then let s  = l
                 s' = Data.List.tail $ Data.List.dropWhile (/= '<') s
             in Just (Data.List.takeWhile (/= '>') s')
        else Nothing

doHttps :: ByteString
           -> [Char]
           -> Maybe GithubAuth
           -> Maybe RequestBody
           -> IO (Either E.SomeException (Response LBS.ByteString))    
doHttps reqMethod url auth body = do
  let reqBody = fromMaybe (RequestBodyBS $ pack "") body
      reqHeaders = maybe [] getOAuth auth
      Just uri = parseUrl url
      request = uri { method = reqMethod
                    , secure = True
                    , port = 443
                    , requestBody = reqBody
                    , responseTimeout = Just 20000000
                    , requestHeaders = reqHeaders <>
                                       [("User-Agent", "github.hs/0.7.4")]
                                       <> [("Accept", "application/vnd.github.preview")]
                    , checkStatus = successOrMissing
                    }
      authRequest = getAuthRequest auth request

  (getResponse authRequest >>= return . Right) `E.catches` [
      -- Re-throw AsyncException, otherwise execution will not terminate on
      -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
      -- UserInterrupt) because all of them indicate severe conditions and
      -- should not occur during normal operation.
      E.Handler (\e -> E.throw (e :: E.AsyncException)),
      E.Handler (\e -> (return . Left) (e :: E.SomeException))
      ]
  where
    getAuthRequest (Just (GithubBasicAuth user pass)) = applyBasicAuth user pass
    getAuthRequest _ = id
    getOAuth (GithubOAuth token) = [("Authorization", "token " ++ token)]
    getOAuth _ = []
    getResponse request = withManager $ \manager -> httpLbs request manager
    successOrMissing s@(Status sci _) hs cookiejar
      | (200 <= sci && sci < 300) || sci == 404 = Nothing
      | otherwise = Just $ E.toException $ StatusCodeException s hs cookiejar

-}

