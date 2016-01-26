{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- The 'Network.HTTP' module provides a simple interface for sending and
-- receiving content over HTTP in Haskell. Here's how to fetch a document from
-- a URL and return it as a String:
--
-- >
-- >    simpleHTTP (getRequest "http://www.haskell.org/") >>= fmap (take 100) . getResponseBody
-- >        -- fetch document and return it (as a 'String'.)
--
-- Other functions let you control the submission and transfer of HTTP
-- 'Request's and 'Response's more carefully, letting you integrate the use
-- of 'Network.HTTP' functionality into your application.
--
-- The module also exports the main types of the package, 'Request' and 'Response',
-- along with 'Header' and functions for working with these.
--
-- The actual functionality is implemented by modules in the @Network.HTTP.*@
-- namespace, letting you either use the default implementation here
-- by importing @Network.HTTP@ or, for more specific uses, selectively
-- import the modules in @Network.HTTP.*@. To wit, more than one kind of
-- representation of the bulk data that flows across a HTTP connection is 
-- supported. (see "Network.HTTP.HandleStream".)
-- 
-- /NOTE:/ The 'Request' send actions will normalize the @Request@ prior to transmission.
-- Normalization such as having the request path be in the expected form and, possibly,
-- introduce a default @Host:@ header if one isn't already present.
-- Normalization also takes the @"user:pass\@"@ portion out of the the URI,
-- if it was supplied, and converts it into @Authorization: Basic$ header.
-- If you do not 
-- want the requests tampered with, but sent as-is, please import and use the
-- the "Network.HTTP.HandleStream" or "Network.HTTP.Stream" modules instead. They
-- export the same functions, but leaves construction and any normalization of 
-- @Request@s to the user.
--
-----------------------------------------------------------------------------
module Preface.HTTP 
       ( 

         {- the functionality that the implementation modules, 
    Network.HTTP.HandleStream and Network.HTTP.Stream,
    exposes:
 -}
         simpleHTTP      -- :: Request -> IO (SResult Response)
       , simpleHTTP_     -- :: Stream s => s -> Request -> IO (SResult Response)
       , sendHTTP        -- :: Stream s => s -> Request -> IO (SResult Response)
       , sendHTTP_notify -- :: Stream s => s -> Request -> IO () -> IO (SResult Response)
       , receiveHTTP     -- :: Stream s => s -> IO (SResult Request)
       , respondHTTP     -- :: Stream s => s -> Response -> IO ()

       , getRequest      -- :: String -> Request_String
       , headRequest     -- :: String -> Request_String
       , postRequest     -- :: String -> Request_String
       , postRequestWithBody -- :: String -> String -> String -> Request_String
       
       , getResponseBody -- :: SResult (Request ty) -> IO ty
       , getResponseCode -- :: SResult (Request ty) -> IO ResponseCode

{- | Session-level interactions over HTTP.
   Additional features supported are:
   * HTTP Authentication handling
   * Transparent handling of redirects
   * Cookie stores + transmission.
   * Transaction logging

Example use:
>    do
>      (_, rsp)
>         <- Network.Browser.browse $ do
>               request $ getRequest "http://www.haskell.org/"
>      return (take 100 (rspBody rsp))
-}
       , BrowserState
       
       , request            -- :: Request -> BrowserAction Response
    
       , Authority(..)
       , getAuthorities
       , setAuthorities
       , addAuthority
       , Challenge(..)
       , Qop(..)
       , Algorithm(..)
       
       , getAuthorityGen
       , setAuthorityGen
       
       , HttpCookie(..)
       , setCookies        -- :: [HttpCookie] -> BrowserAction t ()
       , addCookie         -- :: HttpCookie   -> BrowserAction t ()
       
       , defaultGETRequest
       , defaultGETRequest_
       
       , uriDefaultTo
      
       -- Cookie 
       , cookieMatch          -- :: (String,String) -> Cookie -> Bool

          -- functions for translating cookies and headers.
       , cookiesToHeader      -- :: [Cookie] -> HttpHeader
       , processCookieHeaders -- :: String -> [HttpHeader] -> ([String], [Cookie])
      
       -- was Auth stuff 
       {-
       , Authority(..)
       , Algorithm(..)
       , Challenge(..)
       , Qop(..)

       , headerToChallenge -- :: URI -> HttpHeader -> Maybe Challenge
       , withAuthority     -- :: Authority -> Request ty -> String
       -}

  , unEscapeString

  ,  ConnError(..), failWith, fmapE, SResult, httpOpenStream
  , httpClose, httpCloseOnEnd, httpWriteBlock
  , httpReadBlock, httpReadLine, URI(..), URIAuth(..), parseURIReference
  , sp, readsOne, encode64, crlf, readsOne 
-- maybe not?
   , relativeTo, parseURI
   , HandleStream(..)
   , EndPoint(..)
   , isTCPConnectedTo
   , satisfy, oneOf, sequenceOf, lastOf, catManyOf
   , encode64, sepBy
       ) where


{-
 -
 - 
module Network.HTTPHandleStream 
       ( s_simpleHTTP      -- :: Request -> IO (SResult Response)
       , s_simpleHTTP_     -- :: HandleStream -> Request -> IO (SResult Response)
       , s_sendHTTP        -- :: HandleStream -> Request -> IO (SResult Response)
       , s_sendHTTP_notify -- :: HandleStream -> Request -> IO () -> IO (SResult Response)
       , s_receiveHTTP     -- :: HandleStream -> IO (SResult Request))
       , s_respondHTTP     -- :: HandleStream -> Response -> IO ()
       
       -- , simpleHTTP_debug -- :: FilePath -> Request DebugString -> IO (Response DebugString)

          -- ** Constants
       -- , httpVersion                 -- :: String

          -- ** HTTP
       , HTTPRequest(..)
       , HTTPResponse(..)
       , RequestMethod(..)
       
          -- ** URL Encoding
       , httpUrlEncode
       , urlDecode
       , urlEncodeVars

          -- ** URI authority parsing
       , URIAuthority(..)
       , parseURIAuthority
       
          -- internal
       , uriToAuthorityString   -- :: URI     -> String
       , uriAuthToString        -- :: URIAuth -> String
       , uriAuthPort            -- :: Maybe URI -> URIAuth -> Int
       , reqURIAuth             -- :: Request ty -> URIAuth

       , parseResponseHead      -- :: [String] -> SResult ResponseData
       , parseRequestHead       -- :: [String] -> SResult RequestData

       , ResponseNextStep(..)
       , matchResponse
       , ResponseData
       , ResponseCode
       , RequestData
       
       , splitRequestURI

       , getAuth
       , findConnClose

         -- internal export (for the use by Network.HTTP.{Stream,ByteStream} )
       , linearTransfer
       , hopefulTransfer
       , chunkedTransfer
       , uglyDeathTransfer
       , readTillEmpty1
       , readTillEmpty2
       
       , defaultGETRequest
       , defaultGETRequest_
       , mkRequest
       , setRequestBody

       , defaultUserAgent
       
       , catchIO
       , responseParseError
       
       , failHTTPS
       
       , HttpHeader(..)

       , HeaderName(..)

       , insertHeaderIfMissing -- :: HasHeaders a => HeaderName -> String -> a -> a
       , retrieveHeaders       -- :: HasHeaders a => HeaderName -> a -> [HttpHeader]
       , replaceHeader         -- :: HasHeaders a => HeaderName -> String -> a -> a
       , findHeader            -- :: HeaderName -> [HttpHeader] -> Maybe String

       -- , parseHeader           -- :: parseHeader :: String -> SResult HttpHeader
       , parseHeaders          -- :: [String] -> Result [HttpHeader]
   
       ) where

 -}

-- import Paths_HTTP as Self (version)

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------


{-
import Network.HTTPHandleStream ( s_sendHTTP_notify, uriAuthToString, uriAuthPort
   , HttpHeader(..), HTTPRequest(..), HTTPResponse(..), HeaderName(..)
   , retrieveHeaders, replaceHeader, reqURIAuth, uriToAuthorityString
   , failHTTPS, RequestMethod(..), defaultGETRequest, defaultGETRequest_
   , getAuth, host, port
   , s_sendHTTP, s_receiveHTTP, s_respondHTTP
   , mkRequest, setRequestBody, ResponseCode
   )
-}

-- import Control.Monad.State (StateT (..), MonadIO (..), modify, gets, withStateT, evalStateT, MonadState (..))

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Preface.Imports
import Preface.Stringy
import Preface.Misc
import Preface.SecureHash (md5, stringDigest)
import Preface.ParseUtils

import qualified Data.Array.IArray as A ((!))

-- | @simpleHTTP req@ transmits the 'Request' @req@ by opening a /direct/, non-persistent
-- connection to the HTTP server that @req@ is destined for, followed by transmitting
-- it and gathering up the response as a 'SResult'. Prior to sending the request,
-- it is normalized (via 'normalizeRequest'). If you have to mediate the request
-- via an HTTP proxy, you will have to normalize the request yourself. Or switch to
-- using 'Network.Browser' instead.
--
-- Examples:
--
-- > simpleHTTP (getRequest "http://hackage.haskell.org/")
-- > simpleHTTP (getRequest "http://hackage.haskell.org:8012/")

simpleHTTP :: HTTPRequest -> IO (SResult (HTTPResponse))
simpleHTTP r = do
  let (Just auth) = getAuth r
  failHTTPS (rqURI r)
  c <- httpOpenStream (uriHost auth) (fromMaybe 80 (uriPortNum auth))
  simpleHTTP_ c r
   
-- | Identical to 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: HandleStream -> HTTPRequest -> IO (SResult (HTTPResponse))
simpleHTTP_ s r = do 
  s_sendHTTP s r

-- | @sendHTTP hStream httpRequest@ transmits @httpRequest@ (after normalization) over
-- @hStream@, but does not alter the status of the connection, nor request it to be
-- closed upon receiving the response.
sendHTTP :: HandleStream -> HTTPRequest -> IO (SResult (HTTPResponse))
sendHTTP conn rq = do
  s_sendHTTP conn rq

-- | @sendHTTP_notify hStream httpRequest action@ behaves like 'sendHTTP', but
-- lets you supply an IO @action@ to execute once the request has been successfully
-- transmitted over the connection. Useful when you want to set up tracing of
-- request transmission and its performance.
sendHTTP_notify :: HandleStream -> HTTPRequest -> IO () -> IO (SResult (HTTPResponse))
sendHTTP_notify conn rq onSendComplete = do
  s_sendHTTP_notify conn rq onSendComplete

-- | @receiveHTTP hStream@ reads a 'Request' from the 'HandleStream' @hStream@
receiveHTTP :: HandleStream -> IO (SResult (HTTPRequest))
receiveHTTP conn = s_receiveHTTP conn

-- | @respondHTTP hStream httpResponse@ transmits an HTTP 'Response' over
-- the 'HandleStream' @hStream@. It could be used to implement simple web
-- server interactions, performing the dual role to 'sendHTTP'.
respondHTTP :: HandleStream -> HTTPResponse -> IO ()
respondHTTP conn rsp = s_respondHTTP conn rsp


-- | A convenience constructor for a GET 'Request'.
--
-- If the URL isn\'t syntactically valid, the function raises an error.
getRequest
    :: String             -- ^URL to fetch
    -> HTTPRequest     -- ^The constructed request
getRequest urlString = 
  case parseURI urlString of
    Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest GET u

-- | A convenience constructor for a HEAD 'Request'.
--
-- If the URL isn\'t syntactically valid, the function raises an error.
headRequest
    :: String             -- ^URL to fetch
    -> HTTPRequest     -- ^The constructed request
headRequest urlString = 
  case parseURI urlString of
    Nothing -> error ("headRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest HEAD u

-- | A convenience constructor for a POST 'Request'.
--
-- If the URL isn\'t syntactically valid, the function raises an error.
postRequest
    :: String                   -- ^URL to POST to
    -> HTTPRequest           -- ^The constructed request
postRequest urlString = 
  case parseURI urlString of
    Nothing -> error ("postRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest POST u

-- | A convenience constructor for a POST 'Request'.
--
-- It constructs a request and sets the body as well as
-- the Content-Type and Content-Length headers. The contents of the body
-- are forced to calculate the value for the Content-Length header.
--
-- If the URL isn\'t syntactically valid, the function raises an error.
postRequestWithBody
    :: String                      -- ^URL to POST to
    -> String                      -- ^Content-Type of body
    -> String                      -- ^The body of the request
    -> HTTPRequest              -- ^The constructed request
postRequestWithBody urlString typ body = 
  case parseURI urlString of
    Nothing -> error ("postRequestWithBody: Not a valid URL - " ++ urlString)
    Just u  -> setRequestBody (mkRequest POST u ) (typ, body)

-- | @getResponseBody response@ takes the response of a HTTP requesting action and
-- tries to extricate the body of the 'Response' @response@. If the request action
-- returned an error, an IO exception is raised.
getResponseBody :: SResult (HTTPResponse) -> IO ByteString
getResponseBody (Left err) = fail (show err)
getResponseBody (Right r)  = return (rspBody r)

-- | @getResponseBody response@ takes the response of a HTTP requesting action and
-- tries to extricate the status code of the 'Response' @response@. If the request action
-- returned an error, an IO exception is raised.
getResponseCode :: SResult (HTTPResponse) -> IO ResponseCode
getResponseCode (Left err) = fail (show err)
getResponseCode (Right r)  = return (rspCode r)


--
-- * TODO
--     - request pipelining
--     - https upgrade (includes full TLS, i.e. SSL, implementation)
--         - use of Stream classes will pay off
--         - consider C implementation of encryption\/decryption
--     - comm timeouts
--     - MIME & entity stuff (happening in separate module)
--     - support \"*\" uri-request-string for OPTIONS request method
-- 
-- 
-- * Header notes:
--
--     [@Host@]
--                  Required by HTTP\/1.1, if not supplied as part
--                  of a request a default Host value is extracted
--                  from the request-uri.
-- 
--     [@Connection@] 
--                  If this header is present in any request or
--                  response, and it's value is "close", then
--                  the current request\/response is the last 
--                  to be allowed on that connection.
-- 
--     [@Expect@]
--                  Should a request contain a body, an Expect
--                  header will be added to the request.  The added
--                  header has the value \"100-continue\".  After
--                  a 417 \"Expectation Failed\" response the request
--                  is attempted again without this added Expect
--                  header.
--                  
--     [@TransferEncoding,ContentLength,...@]
--                  if request is inconsistent with any of these
--                  header values then you may not receive any response
--                  or will generate an error response (probably 4xx).
--
--
-- * Response code notes
-- Some response codes induce special behaviour:
--
--   [@1xx@]   \"100 Continue\" will cause any unsent request body to be sent.
--             \"101 Upgrade\" will be returned.
--             Other 1xx responses are ignored.
-- 
--   [@417@]   The reason for this code is \"Expectation failed\", indicating
--             that the server did not like the Expect \"100-continue\" header
--             added to a request.  Receipt of 417 will induce another
--             request attempt (without Expect header), unless no Expect header
--             had been added (in which case 417 response is returned).
--
------------------------------------------------------------------
----------------------- Cookie Stuff -----------------------------
------------------------------------------------------------------

-- | @addCookie c@ adds a cookie to the browser state, removing duplicates.
addCookie :: HttpCookie -> BrowserState -> BrowserState
addCookie c b = b {bsCookies = c : filter (/=c) (bsCookies b) }

-- | @setCookies cookies@ replaces the set of cookies known to
-- the browser to @cookies@. Useful when wanting to restore cookies
-- used across 'browse' invocations.
setCookies :: [HttpCookie] -> BrowserState -> BrowserState
setCookies cs b = b { bsCookies=cs }

-- ...get domain specific cookies...
-- ... this needs changing for consistency with rfc2109...
-- ... currently too broad.
getCookiesFor :: String -> String -> BrowserState -> [HttpCookie]
getCookiesFor dom path b = filter (cookieMatch (dom,path)) (bsCookies b)

------------------------------------------------------------------
----------------------- Authorisation Stuff ----------------------
------------------------------------------------------------------

{-

The browser handles 401 responses in the following manner:
  1) extract all WWW-Authenticate headers from a 401 response
  2) rewrite each as a Challenge object, using "headerToChallenge"
  3) pick a challenge to respond to, usually the strongest
     challenge understood by the client, using "pickChallenge"
  4) generate a username/password combination using the browsers
     "bsAuthorityGen" function (the default behaviour is to ask
     the user)
  5) build an Authority object based upon the challenge and user
     data, store this new Authority in the browser state
  6) convert the Authority to a request header and add this
     to a request using "withAuthority"
  7) send the amended request

Note that by default requests are annotated with authority headers
before the first sending, based upon previously generated Authority
objects (which contain domain information).  Once a specific authority
is added to a rejected request this predictive annotation is suppressed.

407 responses are handled in a similar manner, except
   a) Authorities are not collected, only a single proxy authority
      is kept by the browser
   b) If the proxy used by the browser (type Proxy) is NoProxy, then
      a 407 response will generate output on the "err" stream and
      the response will be returned.


Notes:
 - digest authentication so far ignores qop, so fails to authenticate 
   properly with qop=auth-int challenges
 - calculates a1 more than necessary
 - doesn't reverse authenticate
 - doesn't properly receive AuthenticationInfo headers, so fails
   to use next-nonce etc

-}

-- | Return authorities for a given domain and path.
-- Assumes "dom" is lower case
getAuthFor :: String -> String -> BrowserState -> [Authority]
getAuthFor dom pth b = filter match ( getAuthorities b)
   where
    match :: Authority -> Bool
    match au@AuthBasic{}  = matchURI (auSite au)
    match au@AuthDigest{} = or (map matchURI (auDomain au))

    matchURI :: URI -> Bool
    matchURI s = (uriToAuthorityString s == dom) && (uriPath s `isPrefixOf` pth)
    

-- | @getAuthorities@ return the current set of @Authority@s known
-- to the browser.
getAuthorities :: BrowserState -> [Authority]
getAuthorities = bsAuthorities

-- @setAuthorities as@ replaces the Browser's known set
-- of 'Authority's to @as@.
setAuthorities :: [Authority] -> BrowserState -> BrowserState
setAuthorities as b = b { bsAuthorities=as }

-- @addAuthority a@ adds 'Authority' @a@ to the Browser's
-- set of known authorities.
addAuthority :: Authority -> BrowserState -> BrowserState
addAuthority a b = b { bsAuthorities=a:bsAuthorities b }

-- | @getAuthorityGen@ returns the current authority generator
getAuthorityGen :: BrowserState -> (URI -> String -> IO (Maybe (String,String)))
getAuthorityGen = bsAuthorityGen

-- | @setAuthorityGen genAct@ sets the auth generator to @genAct@.
setAuthorityGen :: (URI -> String -> IO (Maybe (String,String))) -> BrowserState -> BrowserState
setAuthorityGen f b = b { bsAuthorityGen=f }

-- TO BE CHANGED!!!
pickChallenge :: [Challenge] -> Maybe Challenge
pickChallenge ls = if null ls then Just (ChalBasic "/") else listToMaybe ls

-- | Retrieve a likely looking authority for a Request.
anticipateChallenge :: HTTPRequest -> BrowserState -> Maybe Authority
anticipateChallenge rq b =
    let uri = rqURI rq in
       listToMaybe (getAuthFor (uriAuthToString2 $ reqURIAuth rq) (uriPath uri) b)

-- | Asking the user to respond to a challenge
challengeToAuthority :: URI -> Challenge -> BrowserState -> IO (Maybe Authority)
challengeToAuthority uri ch b
 | not (answerable ch) = return Nothing
 | otherwise = do
      -- prompt user for authority
     let prompt = getAuthorityGen b
     userdetails <- prompt uri (chRealm ch)
     case userdetails of
          Nothing    -> return Nothing
          Just (u,p) -> return (Just $ buildAuth ch u p)
 where
  answerable :: Challenge -> Bool
  answerable ChalBasic{} = True
  answerable chall       = (chAlgorithm chall) == Just AlgMD5

  buildAuth :: Challenge -> String -> String -> Authority
  buildAuth (ChalBasic r) u p = 
       AuthBasic { auSite=uri
                 , auRealm=r
                 , auUsername=u
                 , auPassword=p
                 }

    -- note to self: this is a pretty stupid operation
    -- to perform isn't it? ChalX and AuthX are so very
    -- similar.
  buildAuth (ChalDigest r d n o _stale a q) u p =
            AuthDigest { auRealm=r
                       , auUsername=u
                       , auPassword=p
                       , auDomain=d
                       , auNonce=n
                       , auOpaque=o
                       , auAlgorithm=a
                       , auQop=q
                       }


------------------------------------------------------------------
------------------ Browser State Actions -------------------------
------------------------------------------------------------------

-- | @BrowserState@ is the (large) record type tracking the current
-- settings of the browser.
data BrowserState 
 = BS { bsCookies         :: [HttpCookie]
      , bsAuthorityGen    :: URI -> String -> IO (Maybe (String,String))
      , bsAuthorities     :: [Authority]
      , bsMaxRedirects    :: Int
      , bsMaxErrorRetries :: Int
      , bsMaxPoolSize     :: Int
      , bsConnectionPool  :: [HandleStream]
      , bsUserAgent       :: String
      }

instance Show BrowserState where
    show bs =  "BrowserState { " 
            ++ shows (bsCookies bs) ("\n"
           {- ++ show (bsAuthorities bs) ++ "\n"-}
            ++ "MaxRedirects: " ++ shows (bsMaxRedirects bs) "} ")

-- | The default browser state has the settings 
defaultBrowserState :: BrowserState 
defaultBrowserState = res
 where
   res = BS
     { bsCookies          = []
     , bsAuthorityGen     = \ _uri _realm -> do
          traceIO "No action for prompting/generating user+password credentials provided (use: setAuthorityGen); returning Nothing"
          return Nothing
     , bsAuthorities      = []
     , bsMaxRedirects     = 4
     , bsMaxErrorRetries  = 4
     , bsMaxPoolSize      = 5
     , bsConnectionPool   = []
     , bsUserAgent        = "Haskell HTTP User Agent 1.0"
     }

-- | @RequestState@ is an internal tallying type keeping track of various 
-- per-connection counters, like the number of authorization attempts and 
-- forwards we've gone through.
data RequestState 
  = RequestState
      { reqDenies     :: Int   -- ^ number of 401 responses so far
      , reqRedirects  :: Int   -- ^ number of redirects so far
      , reqRetries    :: Int   -- ^ number of retries so far
      , reqStopOnDeny :: Bool  -- ^ whether to pre-empt 401 response
      }


nullRequestState :: RequestState
nullRequestState = RequestState
      { reqDenies     = 0
      , reqRedirects  = 0
      , reqRetries    = 0
      , reqStopOnDeny = True
      }

-- | @request httpRequest@ tries to submit the 'Request' @httpRequest@
-- to some HTTP server (possibly going via a /proxy/, see 'setProxy'.)
-- Upon successful delivery, the URL where the response was fetched from
-- is returned along with the 'Response' itself.
request :: HTTPRequest -> BrowserState -> IO (URI,HTTPResponse)
request req b = do 
  res <- request' nullVal initialState req b
  case res of
    Right r -> return r
    Left e  -> do
     let errStr = ("Network.Browser.request: Error raised " ++ show e)
     traceIO errStr
     fail errStr
 where
  initialState = nullRequestState
  nullVal      = strEmpty

-- | Internal helper function, explicitly carrying along per-request 
-- counts.
request' :: ByteString -> RequestState -> HTTPRequest -> BrowserState -> IO (SResult (URI,HTTPResponse))
request' nullVal rqState rq b = do
   let uri = rqURI rq
   failHTTPS uri
   let uria = reqURIAuth rq 
     -- add cookies to request
       cookies = getCookiesFor (uriAuthToString2 uria) (uriPath uri) b
{- Not for now:
   (case uriUserInfo uria of
     "" -> id
     xs ->
       case chopAtDelim ':' xs of
         (_,[])    -> id
 (usr,pwd) -> withAuth
                  AuthBasic{ auUserName = usr
                                   , auPassword = pwd
           , auRealm    = "/"
           , auSite     = uri
           }) $ do
-}
   when (not $ null cookies) 
        (putStrLn $ "Adding cookies to request.  Cookie names: "  ++ unwords (map ckName cookies))
    -- add credentials to request
   rq' <- 
    if not (reqStopOnDeny rqState) 
     then return rq 
     else do 
       case anticipateChallenge rq b of
         Nothing -> return rq
         Just x  -> return (rq { rqHeaders = (HttpHeader (HeaderName "Authorization") (withAuthority x rq)) : rqHeaders rq } )
   let final_req = if not $ null cookies then rq' { rqHeaders = [cookiesToHeader cookies] ++ rqHeaders rq' } else rq'
   let def_ua = bsUserAgent b
   putStrLn ("Sending:\n" ++ show final_req)
   e_rsp <- dorequest (reqURIAuth final_req) final_req b
   let mbMx = bsMaxErrorRetries b
   case e_rsp of
    Left v 
     | (reqRetries rqState < mbMx) && 
       (v == ErrorReset || v == ErrorClosed) -> do
       --empty connnection pool in case connection has become invalid
       return $  b { bsConnectionPool=[] }
       request' nullVal rqState{reqRetries=succ (reqRetries rqState)} rq b
     | otherwise -> 
       return (Left v)
    Right rsp -> do 
     putStrLn $ ("Received:\n" ++ show rsp)
      -- add new cookies to browser state
     let b0 = handleCookies uri (uriAuthToString2 $ reqURIAuth rq) 
                       (retrieveHeaders (HeaderName "Set-Cookie") (rspHeaders rsp) ) b
     -- Deal with "Connection: close" in response.
     handleConnectionClose (reqURIAuth rq) (retrieveHeaders (HeaderName "Connection") (rspHeaders rsp)) b0
     case rspCode rsp of
      (4,0,1) -> -- Credentials not sent or refused.
         do
            putStrLn "401 - credentials not supplied or refused; retrying.."
            let hdrs = retrieveHeaders (HeaderName "WWW-Authenticate") (rspHeaders rsp)
            case pickChallenge (catMaybes $ map (headerToChallenge uri) hdrs) of
              Nothing -> do
                   putStrLn "no challenge"
                   return (Right (uri,rsp))   {- do nothing -}
              Just x  -> do
                ca <- challengeToAuthority uri x b
                case ca of
                  Nothing  -> do
                        putStrLn "no auth"
                        return (Right (uri,rsp)) {- do nothing -}
                  Just au' -> do
                    putStrLn "Retrying request with new credentials"
                    request' nullVal
                        rqState{ reqDenies     = succ(reqDenies rqState) , reqStopOnDeny = False }
                        (rq { rqHeaders = (HttpHeader (HeaderName "Authorization") (withAuthority au' rq) ) : rqHeaders rq} ) b

      (3,0,x) | x `elem` [2,3,1,7]  ->  do
        putStrLn ("30" ++ show x ++  " - redirect")
        case allowRedirect rqState b of 
           False -> return (Right (uri,rsp))
           _ -> do
              case retrieveHeaders (HeaderName "Location") (rspHeaders rsp) of
                 [] -> do 
                          traceIO "No Location: header in redirect response"
                          return (Right (uri,rsp))
                 (HttpHeader _ u:_) -> 
                     case parseURIReference u of
                       Nothing -> do
                          traceIO ("Parse of Location: header in a redirect response failed: " ++ u)
                          return (Right (uri,rsp))
                       Just newURI
                         | {-uriScheme newURI_abs /= uriScheme uri && -}(not (supportedScheme newURI_abs)) -> do
                           traceIO ("Unable to handle redirect, unsupported scheme: " ++ show newURI_abs)
                           return (Right (uri, rsp))
                         | otherwise -> do     
                           putStrLn ("Redirecting to " ++ show newURI_abs ++ " ...") 
                     
                    -- Redirect using GET request method, depending on
                    -- response code.
                           let toGet = x `elem` [2,3]
                               method = if toGet then GET else rqMethod rq
                               rq1 = rq { rqMethod=method, rqURI=newURI_abs }
                               rq2 = if toGet then (replaceHeader (HeaderName "Content-Length") "0") (rq1 {rqBody = nullVal}) else rq1
                    
                           request' nullVal
                             rqState{ reqDenies     = 0 , reqRedirects  = succ(reqRedirects rqState) , reqStopOnDeny = True }
                             rq2 b
                         where
                           newURI_abs = uriDefaultTo newURI uri

      (3,0,5) ->
        case retrieveHeaders (HeaderName "Location") (rspHeaders rsp) of
         [] -> do 
           traceIO "No Location header in proxy redirect response."
           return (Right (uri,rsp))
         (HttpHeader _ u:_) -> 
           case parseURIReference u of
            Nothing -> do
             traceIO ("Parse of Location header in a proxy redirect response failed: " ++ u)
             return (Right (uri,rsp))
            _       -> return (Right (uri,rsp))

-- | The internal request handling state machine.
dorequest :: URIAuth -> HTTPRequest -> BrowserState -> IO (SResult (HTTPResponse))
dorequest hst rqst b = do
  let pool = bsConnectionPool b
      uPort = uriAuthPort Nothing{-ToDo: feed in complete URL-} hst
  conn <- filterM (\c -> c `isTCPConnectedTo` EndPoint (uriRegName hst) uPort) pool
  rsp <- 
    case conn of
      [] -> do 
        putStrLn ("Creating new connection to " ++ uriAuthToString2 hst)
        c <- httpOpenStream (uriRegName hst) uPort
        updateConnectionPool c b
        dorequest2 c rqst
      (c:_) -> do
        putStrLn ("Recovering connection to " ++ uriAuthToString2 hst)
        dorequest2 c rqst
  return rsp
 where
  dorequest2 c r = s_sendHTTP_notify c r (return ())

updateConnectionPool :: HandleStream -> BrowserState -> IO BrowserState
updateConnectionPool c b = do
   let pool = bsConnectionPool b
       len_pool = length pool
       maxPoolSize = bsMaxPoolSize b
   when (len_pool > maxPoolSize) (httpClose (last pool))
   let pool' 
        | len_pool > maxPoolSize = init pool
        | otherwise              = pool
   return $ if maxPoolSize > 0 then b { bsConnectionPool=c:pool' } else b
                             
cleanConnectionPool :: URIAuth -> BrowserState -> IO BrowserState
cleanConnectionPool uri b = do
  let ep = EndPoint (uriRegName uri) (uriAuthPort Nothing uri)
      pool = bsConnectionPool b
  bad <- mapM (\c -> c `isTCPConnectedTo` ep) pool
  let tmp = zip bad pool
      newpool = map snd $ filter (not . fst) tmp
      toclose = map snd $ filter fst tmp
  forM_ toclose httpClose
  return $ b { bsConnectionPool = newpool }

handleCookies :: URI -> String -> [HttpHeader] -> BrowserState -> BrowserState
handleCookies _   _              [] b = b -- cut short the silliness.
handleCookies uri dom cookieHeaders b = foldr (\x y -> addCookie x y) b newCookies
 where
  (errs, newCookies) = processCookieHeaders dom cookieHeaders

handleConnectionClose :: URIAuth -> [HttpHeader]
                      -> BrowserState -> IO BrowserState 
handleConnectionClose _ [] b = return b
handleConnectionClose uri headers b = do
  let doClose = any (== "close") $ map headerToConnType headers
  if doClose then cleanConnectionPool uri b else return b
  where headerToConnType (HttpHeader _ t) = map toLower t

------------------------------------------------------------------
----------------------- Miscellaneous ----------------------------
------------------------------------------------------------------

allowRedirect :: RequestState -> BrowserState -> Bool
allowRedirect rqState b = let mbMxRetries = bsMaxRedirects b in reqRedirects rqState <= mbMxRetries

-- | Return @True@ iff the package is able to handle requests and responses
-- over it.
supportedScheme :: URI -> Bool
supportedScheme u = uriScheme u == "http:"

-- | @uriDefaultTo a b@ returns a URI that is consistent with the first
-- argument URI @a@ when read in the context of the second URI @b@.
-- If the second argument is not sufficient context for determining
-- a full URI then anarchy reins.
uriDefaultTo :: URI -> URI -> URI
uriDefaultTo a b = a `relativeTo` b
-- uriDefaultTo a b = maybe a id (a `relativeTo` b)

------------------------------------------------------------------
----------------------- Cookie Stuff -----------------------------
------------------------------------------------------------------

-- | @Cookie@ is the Haskell representation of HTTP cookie values.
-- See its relevant specs for authoritative details.
data HttpCookie = HttpCookie 
    { ckDomain  :: String
    , ckName    :: String
    , ckValue   :: String
    , ckPath    :: Maybe String
    , ckComment :: Maybe String
    , ckVersion :: Maybe String
    }
    deriving(Show,Read)

instance Eq HttpCookie where
    a == b  =  ckDomain a == ckDomain b && ckName a == ckName b && ckPath a == ckPath b

-- | @cookieToHeaders ck@ serialises @Cookie@s to an HTTP request header.
cookiesToHeader :: [HttpCookie] -> HttpHeader
cookiesToHeader cs = HttpHeader (HeaderName "Cookie") ((intercalate "; " . map f) cs)
  where f c = ckName c ++ "=" ++ ckValue c

-- | @cookieMatch (domain,path) ck@ performs the standard cookie
-- match wrt the given domain and path. 
cookieMatch :: (String, String) -> HttpCookie -> Bool
cookieMatch (dom,path) ck =
 ckDomain ck `isSuffixOf` dom &&
 case ckPath ck of
   Nothing -> True
   Just p  -> p `isPrefixOf` path

-- | @processCookieHeaders dom hdrs@ 
processCookieHeaders :: String -> [HttpHeader] -> ([String], [HttpCookie])
processCookieHeaders dom hdrs = foldr (headerToCookies dom) ([],[]) hdrs

-- | @headerToCookies dom hdr acc@ 
headerToCookies :: String -> HttpHeader -> ([String], [HttpCookie]) -> ([String], [HttpCookie])
headerToCookies dom (HttpHeader (HeaderName "Set-Cookie") val) (accErr, accCookie) = 
   let (a,b) = cookies val 
    in if not (null b) then (["couldnt parse entire string"], maybe [] id a) 
       else ([], maybe [] id a)
  where
   cookies :: String -> (Maybe [HttpCookie], String)
   cookies s = let (a,b) = cookie s
                in case a of
                       Nothing -> (Nothing, s)
                       Just aa -> if null b || head b == ',' 
                                  then let bb = if null b then b else tail b
                                           (c,d) = cookies bb in
                                                    case c of 
                                                          Nothing -> (Just [aa], b)
                                                          Just cc -> (Just (aa : fromJust c), d)
                                  else (Nothing, s)

   cookie :: String -> (Maybe HttpCookie, String)
   cookie s = let (c, s2) = sequenceOf [word, spc, enString . satisfy (=='='), spc, cvalue] s
                  (d, s3) = cdetail s2
               in case c of 
                    Nothing -> (Nothing,s3)
                    Just [w,_,_,_,v] -> (Just $ mkCookie w v d, s3)
{- (name, s1) = word s
                  (i1, s2) = spaces_l s1
                  (e1:s3) = s2
                  (i2, s4) = spaces_l s3
                  (val1, s5) = cvalue s4
                  (args, s6) = cdetail s5
               in if isNothing name || null s2 || e1 /= '=' 
                     || isNothing args
                  then (Nothing, s)
                  else (Just $ mkCookie (fromJust name) val1 (fromJust args), s6)
-}   
   spc :: String -> (Maybe String, String)
   spc = manyOf (satisfy isSpace)

   cvalue :: String -> (Maybe String, String)
   cvalue s = let (a,b) = quotedstring s
                  (c,d) = break (==';') s
               in if isJust a then (a,b) else if not (null c) then (Just c,d) else (Just "", s)
   
   -- all keys in the result list MUST be in lower case
   cdetail :: String -> ([(String,String)], String)
   cdetail s = let (a,b) = cdx s in
                  case a of 
                    Nothing -> ([], b)
                    Just aa -> let (c,d) = cdetail b in ((aa:c), d)
       where cdx s = let (j,s2) = sequenceOf [spc, enString . satisfy (==';'), spc, word, spc, 
                                       oneOf [ lastOf . sequenceOf [enString . satisfy (=='='), spc, cvalue ]
                                             , \x -> (Just "",x) ]] s
                      in case j of 
                           Nothing -> (Nothing, s2)
                           Just [_,_,_,w,_,v] -> (Just (map toLower w, v), s2)

   mkCookie :: String -> String -> [(String,String)] -> HttpCookie
   mkCookie nm cval more = 
      HttpCookie { ckName    = nm
                   , ckValue   = cval
                   , ckDomain  = map toLower (fromMaybe dom (lookup "domain" more))
                   , ckPath    = lookup "path" more
                   , ckVersion = lookup "version" more
                   , ckComment = lookup "comment" more
                   }
headerToCookies _ _ acc = acc

      


quotedstring :: String -> (Maybe String, String)
quotedstring ('"':s) = let (a,b) = break (=='"') s in if not (null b) && head b == '"'
                         then (Just (concat [['"'],a,['"']]), tail b)
                         else (Nothing, '"':s)
quotedstring s = (Nothing, s)

word :: String -> (Maybe String, String)
word (c:s) = if isAlphaNum c || c=='_' || c=='.' || c=='-' || c==':'
             then let (a,b) = word s in (Just (maybe [c] (c:) a), b)
             else (Nothing, s)
word s = (Nothing, s)



-- import Text.ParserCombinators.Parsec
--    ( Parser, char, many, many1, satisfy, parse, spaces, sepBy1 )

-- | @Authority@ specifies the HTTP Authentication method to use for
-- a given domain/realm; @Basic@ or @Digest@.
data Authority 
 = AuthBasic { auRealm    :: String
             , auUsername :: String
             , auPassword :: String
             , auSite     :: URI
             }
 | AuthDigest{ auRealm     :: String
             , auUsername  :: String
             , auPassword  :: String
             , auNonce     :: String
             , auAlgorithm :: Maybe Algorithm
             , auDomain    :: [URI]
             , auOpaque    :: Maybe String
             , auQop       :: [Qop]
             }


data Challenge 
 = ChalBasic  { chRealm   :: String }
 | ChalDigest { chRealm   :: String
              , chDomain  :: [URI]
              , chNonce   :: String
              , chOpaque  :: Maybe String
              , chStale   :: Bool
              , chAlgorithm ::Maybe Algorithm
              , chQop     :: [Qop]
              }

-- | @Algorithm@ controls the digest algorithm to, @MD5@ or @MD5Session@.
data Algorithm = AlgMD5 | AlgMD5sess
    deriving(Eq)

instance Show Algorithm where
    show AlgMD5 = "md5"
    show AlgMD5sess = "md5-sess"

-- | 
data Qop = QopAuth | QopAuthInt
    deriving(Eq,Show)

-- | @withAuthority auth req@ generates a credentials value from the @auth@ 'Authority',
-- in the context of the given request.
-- 
-- If a client nonce was to be used then this function might need to be of type ... -> BrowserAction String
withAuthority :: Authority -> HTTPRequest -> String
withAuthority a rq = case a of
        AuthBasic{}  -> "Basic " ++ base64encode (auUsername a ++ ':' : auPassword a)
        AuthDigest{} ->
            "Digest " ++
          concat [ "username="  ++ quo (auUsername a)
               , ",realm="    ++ quo (auRealm a)
               , ",nonce="    ++ quo (auNonce a)
               , ",uri="      ++ quo digesturi
               , ",response=" ++ quo rspdigest
                       -- plus optional stuff:
               , fromMaybe "" (fmap (\ alg -> ",algorithm=" ++ quo (show alg)) (auAlgorithm a))
               , fromMaybe "" (fmap (\ o   -> ",opaque=" ++ quo o) (auOpaque a))
               , if null (auQop a) then "" else ",qop=auth"
               ]
    where
        quo s = '"':s ++ "\""

        rspdigest = map toLower (kd (md5x a1) (noncevalue ++ ":" ++ md5x a2))

        a1, a2 :: String
        a1 = auUsername a ++ ":" ++ auRealm a ++ ":" ++ auPassword a
        
        {-
        If the "qop" directive's value is "auth" or is unspecified, then A2
        is:
           A2  = Method ":" digest-uri-value
        If the "qop" value is "auth-int", then A2 is:
           A2  = Method ":" digest-uri-value ":" H(entity-body)
        -}
        a2 = show (rqMethod rq) ++ ":" ++ digesturi

        digesturi = show (rqURI rq)
        noncevalue = auNonce a

type Octet = Word8

-- FIXME: these probably only work right for latin-1 strings
stringToOctets :: String -> [Octet]
stringToOctets = map (fromIntegral . fromEnum)

base64encode :: String -> String
base64encode = encode64 . stringToOctets

md5x :: String -> String
md5x = stringDigest . md5 . asByteString 

kd :: String -> String -> String
kd a b = md5x (a ++ ":" ++ b)




-- | @headerToChallenge base www_auth@ tries to convert the @WWW-Authenticate@ header 
-- @www_auth@  into a 'Challenge' value.
headerToChallenge :: URI -> HttpHeader -> Maybe Challenge
headerToChallenge baseURI (HttpHeader _ str) =
    case challenge str of
        (Nothing, _) -> Nothing
        (Just (name,props), []) -> case name of
            "basic"  -> mkBasic props
            "digest" -> mkDigest props
            _        -> Nothing
        (_,_) -> Nothing
    where
        challenge :: String -> (Maybe (String,[(String,String)]), String)
        challenge s = let (a,b) = sequenceOf [wordAuth, manyOf (satisfy isSpace)] s
                          (j,k) = cprops b
                       in case a of 
                             Nothing -> (Nothing, s)
                             Just [d,e] -> case j of
                                              Nothing -> (Nothing, s)
                                              Just jj -> (Just (map toLower d, jj), k)

        cprops = sepBy cprop comma

        comma s = let (a,b) = sequenceOf [manyOf (satisfy isSpace), enString . satisfy (==','), manyOf (satisfy isSpace)] s
                   in (maybe Nothing (const (Just ",")) a, b)

        cprop s = let (a,b) = sequenceOf [wordAuth, enString . satisfy (=='='), quotedstringAuth] s
                   in case a of
                        Nothing -> (Nothing, s)
                        Just [d,e,f] -> (Just (map toLower d, f), b)

        mkBasic, mkDigest :: [(String,String)] -> Maybe Challenge

        mkBasic params = fmap ChalBasic (lookup "realm" params)

        mkDigest params =
            -- with Maybe monad
            do { r <- lookup "realm" params
               ; n <- lookup "nonce" params
               ; return $ 
                    ChalDigest { chRealm  = r
                               , chDomain = (annotateURIs 
                                            $ map parseURI
                                            $ words 
                                            $ fromMaybe [] 
                                            $ lookup "domain" params)
                               , chNonce  = n
                               , chOpaque = lookup "opaque" params
                               , chStale  = "true" == (map toLower
                                           $ fromMaybe "" (lookup "stale" params))
                               , chAlgorithm= readAlgorithm (fromMaybe "MD5" $ lookup "algorithm" params)
                               , chQop    = readQop (fromMaybe "" $ lookup "qop" params)
                               }
               }

        annotateURIs :: [Maybe URI] -> [URI]
        annotateURIs = map (`relativeTo` baseURI) . catMaybes
        -- annotateURIs = (map (\u -> fromMaybe u (u `relativeTo` baseURI))) . catMaybes

        -- Change These:
        readQop :: String -> [Qop]
        readQop = catMaybes . (map strToQop) . (strSplit ',')

        strToQop qs = case map toLower (trim qs) of
            "auth"     -> Just QopAuth
            "auth-int" -> Just QopAuthInt
            _          -> Nothing

        readAlgorithm astr = case map toLower (trim astr) of
            "md5"      -> Just AlgMD5
            "md5-sess" -> Just AlgMD5sess
            _          -> Nothing

wordAuth, quotedstringAuth :: String -> (Maybe String, String)
quotedstringAuth s = let (a,b) = sequenceOf [enString . satisfy (=='"'), manyOf (satisfy ('"' /=)), enString . satisfy (=='"')] s
                in case a of 
                     Nothing -> (Nothing, s)
                     Just [d,e,f] -> (Just e, b)

wordAuth s = let (a,b) = manyOf (satisfy (\x -> isAlphaNum x || x=='_' || x=='.' || x=='-' || x==':')) s
          in case a of
               Nothing -> (Nothing, s)
               Just aa -> if length aa > 0 then (Just aa, b) else (Nothing, s)




-----------------------------------------------------------------
------------------ Misc -----------------------------------------
-----------------------------------------------------------------

-- | @simpleHTTP@ transmits a resource across a non-persistent connection.
s_simpleHTTP :: HTTPRequest -> IO (SResult HTTPResponse)
s_simpleHTTP r = do 
  let (Just auth) = getAuth r
  failHTTPS (rqURI r)
  c <- httpOpenStream (uriHost auth) (fromMaybe 80 (uriPortNum auth))
  s_simpleHTTP_ c r

{-
-- | @simpleHTTP_debug debugFile req@ behaves like 'simpleHTTP', but logs
-- the HTTP operation via the debug file @debugFile@.
simpleHTTP_debug :: FilePath -> HTTPRequest -> IO (SResult HTTPResponse)
simpleHTTP_debug httpLogFile r = do 
  let (Just auth) = getAuth r
  failHTTPS (rqURI r)
  c   <- httpOpenStream (uriHost auth) (fromMaybe 80 (uriPortNum auth))
  simpleHTTP_ c r
-}

-- | Like 'simpleHTTP', but acting on an already opened stream.
s_simpleHTTP_ :: HandleStream -> HTTPRequest -> IO (SResult HTTPResponse)
s_simpleHTTP_ s r = s_sendHTTP s r

-- | @sendHTTP hStream httpRequest@ transmits @httpRequest@ over
-- @hStream@, but does not alter the status of the connection, nor request it to be
-- closed upon receiving the response.
s_sendHTTP :: HandleStream -> HTTPRequest -> IO (SResult HTTPResponse)
s_sendHTTP conn rq = s_sendHTTP_notify conn rq (return ())

-- | @sendHTTP_notify hStream httpRequest action@ behaves like 'sendHTTP', but
-- lets you supply an IO @action@ to execute once the request has been successfully
-- transmitted over the connection. Useful when you want to set up tracing of
-- request transmission and its performance.
s_sendHTTP_notify :: HandleStream -> HTTPRequest -> IO () -> IO (SResult HTTPResponse)
s_sendHTTP_notify conn rq onSendComplete = do
  when providedClose $ (httpCloseOnEnd conn True)
  onException (sendMain conn rq onSendComplete)
              (httpClose conn)
 where
  providedClose = findConnClose (rqHeaders rq)

-- From RFC 2616, section 8.2.3:
-- 'Because of the presence of older implementations, the protocol allows
-- ambiguous situations in which a client may send "Expect: 100-
-- continue" without receiving either a 417 (Expectation Failed) status
-- or a 100 (Continue) status. Therefore, when a client sends this
-- header field to an origin server (possibly via a proxy) from which it
-- has never seen a 100 (Continue) status, the client SHOULD NOT wait
-- for an indefinite period before sending the request body.'
--
-- Since we would wait forever, I have disabled use of 100-continue for now.
sendMain :: HandleStream -> HTTPRequest -> (IO ()) -> IO (SResult HTTPResponse)
sendMain conn rqst onSendComplete = do
      --let str = if null (rqBody rqst)
      --              then show rqst
      --              else show (insertHeader HdrExpect "100-continue" rqst)
  -- TODO review throwing away of result
  _ <- httpWriteBlock conn (asByteString $ show rqst)
    -- write body immediately, don't wait for 100 CONTINUE
  -- TODO review throwing away of result
  _ <- httpWriteBlock conn (rqBody rqst)
  onSendComplete
  rsp <- getResponseHead conn
  switchResponse conn True False rsp rqst

   -- Hmmm, this could go bad if we keep getting "100 Continue"
   -- responses...  Except this should never happen according
   -- to the RFC.

switchResponse :: HandleStream -> Bool {- allow retry? -} -> Bool {- is body sent? -} -> SResult ResponseData -> HTTPRequest -> IO (SResult HTTPResponse)
switchResponse _ _ _ (Left e) _ = return (Left e)
                -- retry on connreset?
                -- if we attempt to use the same socket then there is an excellent
                -- chance that the socket is not in a completely closed state.

switchResponse conn allow_retry bdy_sent (Right (cd,rn,hdrs)) rqst = 
   case matchResponse (rqMethod rqst) cd of
     Continue
      | not bdy_sent -> do {- Time to send the body -}
        httpWriteBlock conn (rqBody rqst) >>= either (return . Left)
             (\ _ -> do
              rsp <- getResponseHead conn
              switchResponse conn allow_retry True rsp rqst)
      | otherwise    -> do {- keep waiting -}
        rsp <- getResponseHead conn
        switchResponse conn allow_retry bdy_sent rsp rqst

     Retry -> do {- Request with "Expect" header failed.
                    Trouble is the request contains Expects
                    other than "100-Continue" -}
        -- TODO review throwing away of result
        _ <- httpWriteBlock conn (strAppend (asByteString (show rqst)) (rqBody rqst))
        rsp <- getResponseHead conn
        switchResponse conn False bdy_sent rsp rqst
                     
     HttpDone -> do
       when (findConnClose hdrs) (httpCloseOnEnd conn True)
       return (Right $ HTTPResponse cd rn hdrs strEmpty)

     DieHorribly str -> do
       httpClose conn
       return (responseParseError "Invalid response:" str)
     ExpectEntity -> do
       r <- fmapE (\ (ftrs,bdy) -> Right (HTTPResponse cd rn (hdrs++ftrs) bdy)) $
             maybe (maybe (hopefulTransfer (httpReadLine conn) [])
               (\ x -> readsOne (linearTransfer (httpReadBlock conn))
                   (return $ responseParseError "unrecognized content-length value" x) x) cl)
           (ifChunked (chunkedTransfer (httpReadLine conn) (httpReadBlock conn))
                      (uglyDeathTransfer "sendHTTP"))
                   tc
       case r of
         Left{} -> do
                    httpClose conn
                    return r
         Right (HTTPResponse _ _ hs _) -> do
                      when (findConnClose hs) (httpCloseOnEnd conn True)
                      return r

      where
       tc = findHeader (HeaderName "Transfer-Encoding") hdrs
       cl = findHeader (HeaderName "Content-Length") hdrs
                    
-- reads and parses headers
getResponseHead :: HandleStream -> IO (SResult ResponseData)
getResponseHead conn = 
   fmapE (\es -> parseResponseHead (map asString es))
         (readTillEmpty1 (httpReadLine conn))

-- | @receiveHTTP hStream@ reads a 'Request' from the 'HandleStream' @hStream@
s_receiveHTTP :: HandleStream -> IO (SResult (HTTPRequest))
s_receiveHTTP conn = getRequestHead >>= either (return . Left) processRequest
  where
    -- reads and parses headers
   getRequestHead :: IO (SResult RequestData)
   getRequestHead = do
      fmapE (\es -> parseRequestHead (map asString es))
            (readTillEmpty1 (httpReadLine conn))

   processRequest (rm,uri,hdrs) =
      fmapE (\ (ftrs,bdy) -> Right (HTTPRequest uri rm (hdrs++ftrs) bdy)) $
        maybe 
          (maybe (return (Right ([], strEmpty))) -- hopefulTransfer ""
             (\ x -> readsOne (linearTransfer (httpReadBlock conn))
              (return $ responseParseError "unrecognized Content-Length value" x) x) cl)
          (ifChunked (chunkedTransfer (httpReadLine conn) (httpReadBlock conn))
                (uglyDeathTransfer "receiveHTTP"))
              tc
    where
     -- FIXME : Also handle 100-continue.
     tc = findHeader (HeaderName "Transfer-Encoding") hdrs
     cl = findHeader (HeaderName "Content-Length") hdrs

-- | @respondHTTP hStream httpResponse@ transmits an HTTP 'Response' over
-- the 'HandleStream' @hStream@. It could be used to implement simple web
-- server interactions, performing the dual role to 'sendHTTP'.
s_respondHTTP :: HandleStream -> HTTPResponse -> IO ()
s_respondHTTP conn rsp = do 
  -- TODO: review throwing away of result
  _ <- httpWriteBlock conn (asByteString $ show rsp)
   -- write body immediately, don't wait for 100 CONTINUE
  -- TODO: review throwing away of result
  _ <- httpWriteBlock conn (rspBody rsp)
  return ()

------------------------------------------------------------------------------

ifChunked :: a -> a -> String -> a
ifChunked a b s = 
  case map toLower (trim s) of
    "chunked" -> a
    _ -> b



----------------------------------------------------------------------------
--

-----------------------------------------------------------------
------------------ URI Authority parsing ------------------------
-----------------------------------------------------------------

hdrHost :: HeaderName 
hdrHost = HeaderName "Host"

data URIAuthority = URIAuthority { user :: Maybe String, password :: Maybe String, uriHost :: String, uriPortNum :: Maybe Int } deriving (Eq,Show)

-- | Parse the authority part of a URL.
--
-- > RFC 1732, section 3.1:
-- >
-- >       //<user>:<password>@<host>:<port>/<url-path>
-- >  Some or all of the parts "<user>:<password>@", ":<password>",
-- >  ":<port>", and "/<url-path>" may be excluded.
parseURIAuthority :: String -> Maybe URIAuthority
parseURIAuthority s = let a = strTakeWhile (/= '/') s
                          b = strUntil (=='@') a
                          (u, pw, z) = case b of { Nothing -> (Nothing, Nothing, a);
                                          Just (c,d) -> let (e,f) = pUserInfo c in (Just e, f, d) }
                          (h, p) = pHostPort z
                       in Just $ URIAuthority { user=u, password=pw, uriHost=h, uriPortNum=p }


pHostPort :: String -> (String, Maybe Int)
pHostPort s =
  -- h <- rfc2732host <++ munch (/=':')
  case strUntil (==':') s of
      Nothing -> (s, Nothing)
      Just (c,d) -> case str2decimal d of { Left x -> (c, Nothing); Right (e,f) -> (c,Just e) } 

{-
-- RFC2732 adds support for '[literal-ipv6-address]' in the host part of a URL
rfc2732host :: ReadP String
rfc2732host = do
    _ <- char '['
    res <- munch1 (/=']')
    _ <- char ']'
    return res
-}

pUserInfo :: String -> (String, Maybe String)
pUserInfo s = case strUntil (==':') s of { Nothing -> (s, Nothing); Just (u, p) -> (u, Just p) }

-- This function duplicates old Network.URI.authority behaviour.
uriToAuthorityString :: URI -> String
uriToAuthorityString u = maybe "" uriAuthToString2 (uriAuthority u)

uriAuthPort :: Maybe URI -> URIAuth -> Int
uriAuthPort mbURI u = 
  case uriPort u of
    (':':s) -> readsOne id (default_port mbURI) s
    _       -> default_port mbURI
 where
  default_port Nothing = default_http
  default_port (Just url) = 
    case map toLower $ uriScheme url of
      "http:" -> default_http
      "https:" -> default_https
        -- todo: refine
      _ -> default_http

  default_http  = 80
  default_https = 443

failHTTPS :: Monad m => URI -> m ()
failHTTPS uri
  | map toLower (uriScheme uri) == "https:" = fail "https not supported"
  | otherwise = return ()

-- Fish out the authority from a possibly normalized Request, i.e.,
-- the information may either be in the request's URI or inside
-- the Host: header.
reqURIAuth :: HTTPRequest -> URIAuth
reqURIAuth req = 
  case uriAuthority (rqURI req) of
    Just ua -> ua
    _ -> case findHeader hdrHost (rqHeaders req) of
           Nothing -> error ("reqURIAuth: no URI authority for: " ++ show req)
           Just h  -> 
             case toHostPort h of
                (ht,p) -> URIAuth { uriUserInfo = "" , uriRegName  = ht , uriPort     = p }
  where
    -- Note: just in case you're wondering..the convention is to include the ':'
    -- in the port part..
   toHostPort h = break (==':') h

-----------------------------------------------------------------
------------------ HTTP Messages --------------------------------
-----------------------------------------------------------------


-- Protocol version
httpVersion :: String
httpVersion = "HTTP/1.1"


-- | The HTTP request method, to be used in the 'Request' object.
-- We are missing a few of the stranger methods, but these are
-- not really necessary until we add full TLS.
data RequestMethod = HEAD | PUT | GET | POST | DELETE | OPTIONS | TRACE | CONNECT | Custom String
    deriving(Eq)

instance Show RequestMethod where
  show x = 
    case x of
      HEAD     -> "HEAD"
      PUT      -> "PUT"
      GET      -> "GET"
      POST     -> "POST"
      DELETE   -> "DELETE"
      OPTIONS  -> "OPTIONS"
      TRACE    -> "TRACE"
      CONNECT  -> "CONNECT"
      Custom c -> c

rqMethodMap :: [(String, RequestMethod)]
rqMethodMap = [("HEAD",    HEAD), ("PUT",     PUT), ("GET",     GET), ("POST",    POST),
               ("DELETE",  DELETE), ("OPTIONS", OPTIONS), ("TRACE",   TRACE), ("CONNECT", CONNECT)]


-- | An HTTP Request.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output.
data HTTPRequest =
     HTTPRequest { rqURI       :: URI  
             , rqMethod    :: RequestMethod
             , rqHeaders   :: [HttpHeader]
             , rqBody      :: ByteString
             }

-- Notice that request body is not included,
-- this show function is used to serialise
-- a request for the transport link, we send
-- the body separately where possible.
instance Show HTTPRequest where
    show (HTTPRequest u m h _) =
        show m ++ sp ++ alt_uri ++ sp ++ httpVersion ++ crlf
        ++ foldr (++) [] (map show h) ++ crlf
        where
            alt_uri = show $ if null (uriPath u) || head (uriPath u) /= '/' 
                        then u { uriPath = '/' : uriPath u } 
                        else u
instance HasHeaders HTTPRequest where
    getHeaders = rqHeaders
    setHeaders rq hdrs = rq { rqHeaders=hdrs }

-- | For easy pattern matching, HTTP response codes @xyz@ are
-- represented as @(x,y,z)@.
type ResponseCode  = (Int,Int,Int)

-- | @ResponseData@ contains the head of a response payload;
-- HTTP response code, accompanying text description + header
-- fields.
type ResponseData  = (ResponseCode,String,[HttpHeader])

-- | @RequestData@ contains the head of a HTTP request; method,
-- its URL along with the auxillary/supporting header data.
type RequestData   = (RequestMethod,URI,[HttpHeader])

-- | An HTTP Response.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output, additionally the output will
-- show an HTTP version of 1.1 instead of the actual version returned
-- by a server.
data HTTPResponse =
    HTTPResponse { rspCode     :: ResponseCode
             , rspReason   :: String
             , rspHeaders  :: [HttpHeader]
             , rspBody     :: ByteString
             }
                   
-- This is an invalid representation of a received response, 
-- since we have made the assumption that all responses are HTTP/1.1
instance Show HTTPResponse where
    show (HTTPResponse (a,b,c) reason headers _) =
        httpVersion ++ ' ' : map intToDigit [a,b,c] ++ ' ' : reason ++ crlf
        ++ foldr (++) [] (map show headers) ++ crlf

instance HasHeaders HTTPResponse where
    getHeaders = rspHeaders
    setHeaders rsp hdrs = rsp { rspHeaders=hdrs }

------------------------------------------------------------------
------------------ Request Building ------------------------------
------------------------------------------------------------------

-- | A default user agent string. The string is @\"haskell-HTTP/$version\"@
-- where @$version@ is the version of this HTTP package.
--
defaultUserAgent :: String
defaultUserAgent = "haskell-HTTP/0.3"

defaultGETRequest :: URI -> HTTPRequest 
defaultGETRequest uri = defaultGETRequest_ uri

defaultGETRequest_ :: URI -> HTTPRequest
defaultGETRequest_ uri = mkRequest GET uri 

-- | 'mkRequest method uri' constructs a well formed
-- request for the given HTTP method and URI. It does not
-- normalize the URI for the request _nor_ add the required 
-- Host: header. That is done either explicitly by the user
-- or when requests are normalized prior to transmission.
mkRequest :: RequestMethod -> URI -> HTTPRequest
mkRequest meth uri = req
 where
  req = 
    HTTPRequest { rqURI      = uri
            , rqBody     = empty
            , rqHeaders  = [ HttpHeader (HeaderName "Content-Length") "0"
                           , HttpHeader (HeaderName "User-Agent") defaultUserAgent
                           ]
            , rqMethod   = meth
            }

  empty = strEmpty 

-- set rqBody, Content-Type and Content-Length headers.
setRequestBody :: HTTPRequest -> (String, String) -> HTTPRequest
setRequestBody req (typ, body) = req' { rqBody= asByteString body }
  where
    req' = replaceHeader (HeaderName "Content-Type") typ .
           replaceHeader (HeaderName "Content-Length") (show $ strLen body) $
           req

{-
    -- stub out the user info.
  updAuth = fmap (\ x -> x{uriUserInfo=""}) (uriAuthority uri)

  withHost = 
    case uriToAuthorityString uri{uriAuthority=updAuth} of
      "" -> id
      h  -> ((Header HdrHost h):)

  uri_req 
   | forProxy  = uri
   | otherwise = snd (splitRequestURI uri)
-}


-----------------------------------------------------------------
------------------ Parsing --------------------------------------
-----------------------------------------------------------------

-- Parsing a request
parseRequestHead :: [String] -> SResult RequestData
parseRequestHead         [] = Left ErrorClosed
parseRequestHead (com:hdrs) = do
  (_version,rqm,uri) <- requestCommand com (words com)
  let hdrs' = parseHeaders hdrs
  return (rqm,uri, hdrs')
 where

  requestCommand l _yes@(rqm:uri:version) =
    case (parseURIReference uri, lookup rqm rqMethodMap) of
     (Just u, Just r) -> return (version,r,u)
     (Just u, Nothing) -> return (version,Custom rqm,u)
     _                -> parse_err l
  requestCommand l _
   | null l    = failWith ErrorClosed
   | otherwise = parse_err l

  parse_err l = responseParseError "parseRequestHead"
                   ("Request command line parse failure: " ++ l)

-- Parsing a response
parseResponseHead :: [String] -> SResult ResponseData
parseResponseHead []         = failWith ErrorClosed
parseResponseHead (sts:hdrs) = do
  (_version,code,reason)  <- responseStatus sts (words sts)
  let hdrs' = parseHeaders hdrs
  return (code,reason, hdrs')
 where
  responseStatus _l _yes@(version:code:reason) =
    return (version,matchx code,concatMap (++" ") reason)
  responseStatus l _no 
    | null l    = failWith ErrorClosed  -- an assumption
    | otherwise = parse_err l

  parse_err l = 
    responseParseError 
        "parseResponseHead"
        ("Response status line parse failure: " ++ l)

  matchx [a,b,c] = (digitToInt a,
                   digitToInt b,
                   digitToInt c)
  matchx _ = (-1,-1,-1)  -- will create appropriate behaviour

-----------------------------------------------------------------
------------------ HTTP Send / Recv ----------------------------------
-----------------------------------------------------------------

data ResponseNextStep
 = Continue
 | Retry
 | HttpDone
 | ExpectEntity
 | DieHorribly String

matchResponse :: RequestMethod -> ResponseCode -> ResponseNextStep
matchResponse rqst rsp =
    case rsp of
        (1,0,0) -> Continue
        (1,0,1) -> HttpDone        -- upgrade to TLS
        (1,_,_) -> Continue    -- default
        (2,0,4) -> HttpDone
        (2,0,5) -> HttpDone
        (2,_,_) -> ans
        (3,0,4) -> HttpDone
        (3,0,5) -> HttpDone
        (3,_,_) -> ans
        (4,1,7) -> Retry       -- Expectation failed
        (4,_,_) -> ans
        (5,_,_) -> ans
        (a,b,c) -> DieHorribly ("Response code " ++ map intToDigit [a,b,c] ++ " not recognised")
    where
        ans | rqst == HEAD = HttpDone
            | otherwise    = ExpectEntity
        

        
-----------------------------------------------------------------
------------------ A little friendly funtionality ---------------
-----------------------------------------------------------------


{-
    I had a quick look around but couldn't find any RFC about
    the encoding of data on the query string.  I did find an
    IETF memo, however, so this is how I justify the urlEncode
    and urlDecode methods.

    Doc name: draft-tiwari-appl-wxxx-forms-01.txt  (look on www.ietf.org)

    Reserved chars:  ";", "/", "?", ":", "@", "&", "=", "+", ",", and "$" are reserved.
    Unwise: "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
    URI delims: "<" | ">" | "#" | "%" | <">
    Unallowed ASCII: <US-ASCII coded characters 00-1F and 7F hexadecimal>
                     <US-ASCII coded character 20 hexadecimal>
    Also unallowed:  any non-us-ascii character

    Escape method: char -> '%' a b  where a, b :: Hex digits
-}

replacement_character :: Char
replacement_character = '\xfffd'

-- | Encode a single Haskell Char to a list of Word8 values, in UTF8 format.
--
-- Shamelessly stolen from utf-8string-0.3.7
encodeChar :: Char -> [Word8]
encodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

-- | Decode a UTF8 string packed into a list of Word8 values, directly to String
--
-- Shamelessly stolen from utf-8string-0.3.7
decodeUtf8 :: [Word8] -> String
decodeUtf8 [    ] = ""
decodeUtf8 (c:cs)
  | c < 0x80  = chr (fromEnum c) : decodeUtf8 cs
  | c < 0xc0  = replacement_character : decodeUtf8 cs
  | c < 0xe0  = multi1
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = replacement_character : decodeUtf8 cs
  where
    multi1 = case cs of
      c1 : ds | c1 .&. 0xc0 == 0x80 ->
        let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
        in if d >= 0x000080 then toEnum d : decodeUtf8 ds
                            else replacement_character : decodeUtf8 ds
      _ -> replacement_character : decodeUtf8 cs

    multi_byte :: Int -> Word8 -> Int -> [Char]
    multi_byte i maskx overlong = aux i cs (fromEnum (c .&. maskx))
      where
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : decodeUtf8 rs
          | otherwise = replacement_character : decodeUtf8 rs

        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        aux _ rs     _ = replacement_character : decodeUtf8 rs


-- This function is a bit funny because potentially the input String could contain some actual Unicode
-- characters (though this shouldn't happen for most use cases), so we have to preserve those characters
-- while simultaneously decoding any UTF-8 data
urlDecode :: String -> String
urlDecode = go []
  where
    go bs ('%':a:b:rest)           = go (fromIntegral (16 * digitToInt a + digitToInt b) : bs) rest
    go bs (h:t) | fromEnum h < 256 = go (fromIntegral (fromEnum h) : bs) t -- Treat ASCII as just another byte of UTF-8
    go [] []                       = []
    go [] (h:t)                    = h : go [] t -- h >= 256, so can't be part of any UTF-8 byte sequence
    go bs rest                     = decodeUtf8 (reverse bs) ++ go [] rest


httpUrlEncode :: String -> String
httpUrlEncode     [] = []
httpUrlEncode (ch:t) 
  | (isAscii ch && isAlphaNum ch) || ch `elem` "-_.~" = ch : httpUrlEncode t
  | not (isAscii ch) = foldr escape (httpUrlEncode t) (encodeChar ch)
  | otherwise = escape (fromIntegral (fromEnum ch)) (httpUrlEncode t)
    where
     escape b rs = '%':showH (b `div` 16) (showH (b `mod` 16) rs)

     showH :: Word8 -> String -> String
     showH x xs
       | x <= 9    = tox (o_0 + x) : xs
       | otherwise = tox (o_A + (x-10)) : xs
      where
       tox  = toEnum  .  fromIntegral
       fro = fromIntegral . fromEnum

       o_0 = fro '0'
       o_A = fro 'A'

-- Encode form variables, useable in either the
-- query part of a URI, or the body of a POST request.
-- I have no source for this information except experience,
-- this sort of encoding worked fine in CGI programming.
urlEncodeVars :: [(String,String)] -> String
urlEncodeVars ((n,v):t) =
    let (same, diffx) = partition ((==n) . fst) t
    in httpUrlEncode n ++ '=' : foldl (\x y -> x ++ ',' : httpUrlEncode y) (httpUrlEncode $ v) (map snd same)
       ++ urlEncodeRest diffx
       where urlEncodeRest [] = []
             urlEncodeRest diffx = '&' : urlEncodeVars diffx
urlEncodeVars [] = []

-- | @getAuth req@ fishes out the authority portion of the URL in a request's @Host@
-- header.
getAuth :: HTTPRequest -> Maybe URIAuthority
getAuth r = 
   -- ToDo: verify that Network.URI functionality doesn't take care of this (now.)
  case parseURIAuthority auth of
    Just x -> return x 
    Nothing -> fail $ "Network.HTTP.Base.getAuth: Error parsing URI authority '" ++ auth ++ "'"
 where 
  auth = maybe (uriToAuthorityString uri) id (findHeader hdrHost (getHeaders r) )
  uri  = rqURI r

-- | @normalizeBasicAuth opts req@ sets the header @Authorization: Basic...@
-- if the "user:pass@" part is present in the "http://user:pass@host/path"
-- of the URI. If Authorization header was present already it is not replaced.
normalizeBasicAuth :: HTTPRequest -> HTTPRequest
normalizeBasicAuth req =
  case getAuth req of
    Just uriauth ->
      case (user uriauth, password uriauth) of
        (Just u, Just p) ->
          insertHeaderIfMissing (HttpHeader (HeaderName "Authorization") astr) req
            where
              astr = "Basic " ++ base64encode (u ++ ":" ++ p)
              base64encode = encode64 . stringToOctets :: String -> String
              stringToOctets = map (fromIntegral . fromEnum) :: String -> [Word8]
        (_, _) -> req
    Nothing ->req

{-
-- | @normalizeHostURI forProxy req@ rewrites your request to have it
-- follow the expected formats by the receiving party (proxy or server.)
normalizeHostURI :: RequestNormalizer ty
normalizeHostURI optsx req = 
  case splitRequestURI uri of
    ("",_uri_abs)
      | forProxy -> 
         case findHeader hdrHost req of
           Nothing -> req -- no host/authority in sight..not much we can do.
           Just h  -> req{rqURI=uri{ uriAuthority=Just URIAuth{uriUserInfo="", uriRegName=hst, uriPort=pNum}
                           , uriScheme=if (null (uriScheme uri)) then "http" else uriScheme uri
                }}
            where 
              hst = case span (/='@') user_hst of
                (as,'@':bs) -> case span (/=':') as of
                        (_,_:_) -> bs
                        _ -> user_hst
                _ -> user_hst

              (user_hst, pNum) = 
                 case span isDigit (reverse h) of
                    (ds,':':bs) -> (reverse bs, ':':reverse ds)
                    _ -> (h,"")
      | otherwise -> 
         case findHeader hdrHost req of
           Nothing -> req -- no host/authority in sight..not much we can do...complain?
           Just{}  -> req
    (h,uri_abs) 
      | forProxy  -> insertHeaderIfMissing (HttpHeader hdrHost h) req 
      | otherwise -> replaceHeader hdrHost h req{rqURI=uri_abs} -- Note: _not_ stubbing out user:pass
 where
   uri0     = rqURI req 
     -- stub out the user:pass 
   uri      = uri0{uriAuthority=fmap (\ x -> x{uriUserInfo=""}) (uriAuthority uri0)}

   forProxy = normForProxy optsx
-}

{- Comments re: above rewriting:
    RFC 2616, section 5.1.2:
     "The most common form of Request-URI is that used to identify a
      resource on an origin server or gateway. In this case the absolute
      path of the URI MUST be transmitted (see section 3.2.1, abs_path) as
      the Request-URI, and the network location of the URI (authority) MUST
      be transmitted in a Host header field." 
   We assume that this is the case, so we take the host name from
   the Host header if there is one, otherwise from the request-URI.
   Then we make the request-URI an abs_path and make sure that there
   is a Host header.
-}

splitRequestURI :: URI -> ({-authority-}String, URI)
splitRequestURI uri = (uriToAuthorityString uri, uri{uriScheme="", uriAuthority=Nothing})

-- Looks for a "Connection" header with the value "close".
-- Returns True when this is found.
findConnClose :: [HttpHeader] -> Bool
findConnClose hdrs =
  maybe False
        (\ x -> map toLower (trim x) == "close") (findHeader (HeaderName "Connection") hdrs)

-- | Used when we know exactly how many bytes to expect.
linearTransfer :: (Int -> IO (SResult a)) -> Int -> IO (SResult ([HttpHeader],a))
linearTransfer readBlk n = fmapE (\str -> Right ([],str)) (readBlk n)

-- | Used when nothing about data is known,
--   Unfortunately waiting for a socket closure
--   causes bad behaviour.  Here we just
--   take data once and give up the rest.
hopefulTransfer :: IO (SResult ByteString) -> [ByteString] -> IO (SResult ([HttpHeader],ByteString))
hopefulTransfer readL strs 
    = readL >>= 
      either (\v -> return $ Left v)
             (\more -> if (strNull more)
                         then return (Right ([], strConcat $ reverse strs))
                         else hopefulTransfer readL (more:strs))

-- | A necessary feature of HTTP\/1.1
--   Also the only transfer variety likely to
--   return any footers.
chunkedTransfer :: IO (SResult ByteString)
                -> (Int -> IO (SResult ByteString))
                -> IO (SResult ([HttpHeader], ByteString))
chunkedTransfer readL readBlk = chunkedTransferC readL readBlk [] 0

chunkedTransferC :: IO (SResult ByteString)
                 -> (Int -> IO (SResult ByteString)) -> [ByteString] -> Int -> IO (SResult ([HttpHeader], ByteString))
chunkedTransferC readL readBlk acc n = do
  v <- readL
  case v of
    Left e -> return (Left e)
    Right line 
     | size == 0 -> 
         -- last chunk read; look for trailing headers..
        fmapE (\ strs -> do
             let ftrs = parseHeaders (map asString strs)
   -- insert (computed) Content-Length header.
             let ftrs' = HttpHeader (HeaderName "Content-Length") (show n) : ftrs
             return (ftrs', strConcat (reverse acc)))

        (readTillEmpty2 readL [])

     | otherwise -> do
         some <- readBlk size
         case some of
           Left e -> return (Left e)
           Right cdata -> do
              _ <- readL -- CRLF is mandated after the chunk block; ToDo: check that the line is empty.?
              chunkedTransferC readL readBlk (cdata:acc) (n+size)
     where
      size 
       | strNull line = 0
       | otherwise = case readHex (asString line) of
          (hx,_):_ -> hx
          _        -> 0

-- | Maybe in the future we will have a sensible thing
--   to do here, at that time we might want to change
--   the name.
uglyDeathTransfer :: String -> IO (SResult ([HttpHeader],a))
uglyDeathTransfer loc = return (responseParseError loc "Unknown Transfer-Encoding")

isLineTerm a = a == asByteString "\r\n" || a == asByteString "\n"

-- | Remove leading crlfs then call readTillEmpty2 (not required by RFC)
readTillEmpty1 :: IO (SResult ByteString) -> IO (SResult [ByteString])
readTillEmpty1 readL =
  readL >>=
    either (return . Left)
           (\ s -> if isLineTerm s
                then readTillEmpty1 readL
                else readTillEmpty2 readL [s])

-- | Read lines until an empty line (CRLF),
--   also accepts a connection close as end of
--   input, which is not an HTTP\/1.1 compliant
--   thing to do - so probably indicates an
--   error condition.
readTillEmpty2 :: IO (SResult ByteString) -> [ByteString] -> IO (SResult [ByteString])
readTillEmpty2 readL list =
    readL >>=
      either (return . Left)
             (\ s ->
              if isLineTerm s || strNull s
                 then return (Right $ reverse (s:list))
                 else readTillEmpty2 readL (s:list))

--
-- Misc
--

-- | @catchIO a h@ handles IO action exceptions throughout codebase; version-specific
-- tweaks better go here.
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO a h = catch a h

responseParseError :: String -> String -> SResult a
responseParseError loc v = failWith (ErrorParse (loc ++ ' ':v))

-- | The @Header@ data type pairs header names & values.
data HttpHeader = HttpHeader HeaderName String

instance Show HttpHeader where show (HttpHeader key value) = shows key (':':' ':value ++ crlf)

-- Encoding HTTP header names differently, as Strings perhaps, is an
-- equally fine choice..no decidedly clear winner, but let's stick
-- with data constructors here.
newtype HeaderName = HeaderName String
instance Eq HeaderName where
  (HeaderName a) == (HeaderName b) = map toLower a == map toLower b

instance Show HeaderName where
    show (HeaderName a) = a

-- | @HasHeaders@ is a type class for types containing HTTP headers, allowing
-- you to write overloaded header manipulation functions
-- for both 'Request' and 'Response' data types, for instance.
class HasHeaders x where
    getHeaders :: x -> [HttpHeader]
    setHeaders :: x -> [HttpHeader] -> x

-- Header manipulation functions

-- | @insertHeaderIfMissing hdr val x@ adds the new header only if no previous
-- header with name @hdr@ exists in @x@.
insertHeaderIfMissing :: HasHeaders a => HttpHeader -> a -> a 
insertHeaderIfMissing hdr@(HttpHeader name _) x = setHeaders x (newHeaders $ getHeaders x)
    where
        newHeaders list@(h@(HttpHeader n _): rest)
            | n == name  = list
            | otherwise  = h : newHeaders rest
        newHeaders [] = [hdr]

-- | @replaceHeader hdr val o@ replaces the header @hdr@ with the
-- value @val@, dropping any existing 
replaceHeader :: HasHeaders a => HeaderName -> String -> a -> a
replaceHeader name v h = setHeaders h ( HttpHeader name v : [ x | x@(HttpHeader n _) <- getHeaders h, name /= n ])
          
retrieveHeaders :: HeaderName -> [HttpHeader] -> [HttpHeader]
retrieveHeaders name x = filter (\(HttpHeader n _) -> n == name) x

-- | @findHeader hdrNm x@ looks up @hdrNm@ in @x@, returning the first
-- header that matches, if any.
findHeader :: HeaderName -> [HttpHeader] -> Maybe String
findHeader k x = maybe Nothing (\(HttpHeader _ s) -> Just s) $ find (\(HttpHeader n s) -> n == k) x

-- | @parseHeaders hdrs@ takes a sequence of strings holding header
-- information and parses them into a set of headers (preserving their
-- order in the input argument.) Handles header values split up over
-- multiple lines.
parseHeaders :: [String] -> [HttpHeader]
parseHeaders = catMaybes . map parseHeader . joinExtended 
   where
     -- Joins consecutive lines where the second line begins with ' ' or '\t'.
     joinExtended lst = if null lst then []
        else let (h : t) = lst
          in if null t then [h]
                       else if isLineExtension (head t) 
                            then joinExtended ((h ++ ' ' : tail (head t)) : tail t)
                            else h : joinExtended t
     isLineExtension n = let x = strHead n in (not (strNull n)) && (x == ' ' || x == '\t')

     parseHeader :: String -> Maybe HttpHeader
     parseHeader str =
        case strUntil (==':') (trimL str) of
            Nothing -> Nothing
            Just (k,v) -> Just $ HttpHeader (HeaderName k) (trimL v)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | @crlf@ is our beloved two-char line terminator.
crlf :: String
crlf = "\r\n"

-- | @lf@ is a tolerated line terminator, per RFC 2616 section 19.3.
lf :: String
lf = "\n"

-- | @sp@ lets you save typing one character.
sp :: String
sp   = " "

{-
-- | @split delim ls@ splits a list into two parts, the @delim@ occurring
-- at the head of the second list.  If @delim@ isn't in @ls@, @Nothing@ is
-- returned.
split :: Eq a => a -> [a] -> Maybe ([a],[a])
split delim list = case delim `elemIndex` list of
    Nothing -> Nothing
    Just x  -> Just $ splitAt x list
-}

{-
-- | @splitMany delim ls@ removes the delimiter @delim@ from @ls@.
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c xs = 
    case break (==c) xs of
      (_,[]) -> [xs]
      (as,_:bs) -> as : splitBy c bs
-}

-- | @readsOne f def str@ tries to 'read' @str@, taking
-- the first result and passing it to @f@. If the 'read'
-- doesn't succeed, return @def@.
readsOne :: Read a => (a -> b) -> b -> String -> b
readsOne f n str = 
 case reads str of
   ((v,_):_) -> f v
   _ -> n

-- | @dropWhileTail p ls@ chops off trailing elements from @ls@
-- until @p@ returns @False@.
dropWhileTail :: (a -> Bool) -> [a] -> [a]
dropWhileTail f ls =
 case foldr chop Nothing ls of { Just xs -> xs; Nothing -> [] }
  where
    chop x (Just xs) = Just (x:xs)
    chop x _
     | f x       = Nothing
     | otherwise = Just [x]

-- | @chopAtDelim elt ls@ breaks up @ls@ into two at first occurrence
-- of @elt@; @elt@ is elided too. If @elt@ does not occur, the second
-- list is empty and the first is equal to @ls@.
chopAtDelim :: Eq a => a -> [a] -> ([a],[a])
chopAtDelim elt xs =
  case break (==elt) xs of
    (_,[])    -> (xs,[])
    (as,_:bs) -> (as,bs)

encodeArray :: Array Int Char
encodeArray = array (0,64) (zip [0..63] "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

-- Convert between 4 base64 (6bits ea) integers and 1 ordinary integer (32 bits)
-- clearly the upmost/leftmost 8 bits of the answer are 0.
-- Hack Alert: In the last entry of the answer, the upper 8 bits encode 
-- the integer number of 6bit groups encoded in that integer, ie 1, 2, 3.
-- 0 represents a 4 :(
int4_char3 :: [Int] -> [Char]
int4_char3 (a:b:c:d:t) = 
    let n = (a `shiftL` 18 .|. b `shiftL` 12 .|. c `shiftL` 6 .|. d)
    in (chr (n `shiftR` 16 .&. 0xff))
     : (chr (n `shiftR` 8 .&. 0xff))
     : (chr (n .&. 0xff)) : int4_char3 t

int4_char3 [a,b,c] =
    let n = (a `shiftL` 18 .|. b `shiftL` 12 .|. c `shiftL` 6)
    in [ (chr (n `shiftR` 16 .&. 0xff))
       , (chr (n `shiftR` 8 .&. 0xff)) ]

int4_char3 [a,b] = 
    let n = (a `shiftL` 18 .|. b `shiftL` 12)
    in [ (chr (n `shiftR` 16 .&. 0xff)) ]

int4_char3 [_] = error "Network.HTTP.Base64.int4_char3: impossible number of Ints."
int4_char3 [] = []

-- Convert triplets of characters to
-- 4 base64 integers.  The last entries
-- in the list may not produce 4 integers,
-- a trailing 2 character group gives 3 integers,
-- while a trailing single character gives 2 integers.
char3_int4 :: [Char] -> [Int]
char3_int4 (a:b:c:t) 
    = let n = (ord a `shiftL` 16 .|. ord b `shiftL` 8 .|. ord c)
      in (n `shiftR` 18 .&. 0x3f) : (n `shiftR` 12 .&. 0x3f) : (n `shiftR` 6  .&. 0x3f) : (n .&. 0x3f) : char3_int4 t

char3_int4 [a,b]
    = let n = (ord a `shiftL` 16 .|. ord b `shiftL` 8)
      in [ (n `shiftR` 18 .&. 0x3f)
         , (n `shiftR` 12 .&. 0x3f)
         , (n `shiftR` 6  .&. 0x3f) ]
    
char3_int4 [a]
    = let n = (ord a `shiftL` 16)
      in [(n `shiftR` 18 .&. 0x3f),(n `shiftR` 12 .&. 0x3f)]

char3_int4 [] = []

-- Retrieve base64 char, given an array index integer in the range [0..63]
enc1 :: Int -> Char
enc1 ch = encodeArray A.! ch

-- | Cut up a string into 72 char lines, each line terminated by CRLF.
chop72 :: String -> String
chop72 str =  let (bgn,end) = splitAt 70 str
              in if null end then bgn else "\r\n" ++ chop72 end

-- Pads a base64 code to a multiple of 4 characters, using the special
-- '=' character.
quadruplets :: [Char] -> [Char]
quadruplets (a:b:c:d:t) = a:b:c:d:quadruplets t
quadruplets [a,b,c]     = [a,b,c,'=']      -- 16bit tail unit
quadruplets [a,b]       = [a,b,'=','=']    -- 8bit tail unit
quadruplets [_]         = error "Network.HTTP.Base64.quadruplets: impossible number of characters."
quadruplets []          = []               -- 24bit tail unit

enc :: [Int] -> [Char]
enc = quadruplets . map enc1

dcd :: String -> [Int]
dcd [] = []
dcd (h:t)
    | h <= 'Z' && h >= 'A'  =  ord h - ord 'A'      : dcd t
    | h >= '0' && h <= '9'  =  ord h - ord '0' + 52 : dcd t
    | h >= 'a' && h <= 'z'  =  ord h - ord 'a' + 26 : dcd t
    | h == '+'  = 62 : dcd t
    | h == '/'  = 63 : dcd t
    | h == '='  = []  -- terminate data stream
    | otherwise = dcd t

-- Principal encoding and decoding functions.
encode64 :: [Octet] -> String
encode64 = enc . char3_int4 . (map (chr .fromIntegral))

decode64 :: String -> [Octet]
decode64 = (map (fromIntegral . ord)) . int4_char3 . dcd

-----------------------------------------------------------------
------------------ TCP Connections ------------------------------
-----------------------------------------------------------------

newtype HandleStream = HandleStream {getRef :: MVar (Conn ByteString)}

data EndPoint = EndPoint { epHost :: String, epPort :: Int }

instance Eq EndPoint where
   EndPoint host1 port1 == EndPoint host2 port2 =
     map toLower host1 == map toLower host2 && port1 == port2

data Conn a 
 = MkConn { connSock      :: ! Socket , connHandle    :: Handle , connInput     :: Maybe a , connEndPoint  :: EndPoint , connCloseEOF  :: Bool -- True => close socket upon reaching end-of-stream.
          }
 | ConnClosed
   deriving(Eq)

httpOpenStream       = openTCPConnection
httpReadBlock c n    = readBlockBS c n
httpReadLine c       = readLineBS c
httpWriteBlock c str = writeBlockBS c str
httpClose c          = closeIt c strNull True
closeQuick c     = closeIt c strNull False
httpCloseOnEnd c f   = closeEOF c f

{-
-- | @openTCPPort uri port@  establishes a connection to a remote
-- host, using 'getHostByName' which possibly queries the DNS system, hence 
-- may trigger a network connection.
openTCPPort :: String -> Int -> IO Connection
openTCPPort uri port = openTCPConnection uri port >>= return.Connection
-}

-- Add a "persistent" option?  Current persistent is default.
-- Use "Result" type for synchronous exception reporting?
openTCPConnection :: String -> Int -> IO HandleStream 
openTCPConnection uri port = openTCPConnection_ uri port False

openTCPConnection_ :: String -> Int -> Bool -> IO HandleStream
openTCPConnection_ uri port stashInput = do
    -- HACK: uri is sometimes obtained by calling Network.URI.uriRegName, and this includes
    -- the surrounding square brackets for an RFC 2732 host like [::1]. It's not clear whether
    -- it should, or whether all call sites should be using something different instead, but
    -- the simplest short-term fix is to strip any surrounding square brackets here.
    -- It shouldn't affect any as this is the only situation they can occur - see RFC 3986.
    let fixedUri =
         case uri of
            '[':(rest@(c:_)) | last rest == ']'
              -> if c == 'v' || c == 'V'
                     then error $ "Unsupported post-IPv6 address " ++ uri
                     else init rest
            _ -> uri


    -- use withSocketsDo here in case the caller hasn't used it, which would make getAddrInfo fail on Windows
    -- although withSocketsDo is supposed to wrap the entire program, in practice it is safe to use it locally
    -- like this as it just does a once-only installation of a shutdown handler to run at program exit,
    -- rather than actually shutting down after the action
    addrinfos <- withSocketsDo $ getAddrInfo (Just $ defaultHints { addrFamily = AF_UNSPEC, addrSocketType = Stream }) (Just fixedUri) (Just . show $ port)
    case addrinfos of
        [] -> fail "openTCPConnection: getAddrInfo returned no address information"
        (a:_) -> do
                s <- socket (addrFamily a) Stream defaultProtocol
                onException (do
                            setSocketOption s KeepAlive 1
                            sktConnect s (addrAddress a)
                            socketConnection_ fixedUri port s stashInput
                            ) (sClose s)
{-
-- | @socketConnection@, like @openConnection@ but using a pre-existing 'Socket'.
socketConnection :: String -> Int -> Socket -> IO HandleStream
socketConnection hst port sock = socketConnection_ hst port sock False
-}

-- Internal function used to control the on-demand streaming of input
-- for /lazy/ streams.
socketConnection_ :: String -> Int -> Socket -> Bool -> IO HandleStream
socketConnection_ hst port sock stashInput = do
    h <- socketToHandle sock ReadWriteMode
    mb <- case stashInput of { True -> liftM Just $ strHGetContents h; _ -> return Nothing }
    let conn = MkConn 
         { connSock     = sock , connHandle   = h , connInput    = mb , connEndPoint = EndPoint hst port , connCloseEOF = False }
    v <- newMVar conn
    return (HandleStream v)

closeConnection :: HandleStream -> IO Bool -> IO ()
closeConnection ref readL = do
    -- won't hold onto the lock for the duration
    -- we are draining it...ToDo: have Connection
    -- into a shutting-down state so that other
    -- threads will simply back off if/when attempting
    -- to also close it.
  c <- readMVar (getRef ref)
  closeConn c `catchIO` (\_ -> return ())
  modifyMVar_ (getRef ref) (\ _ -> return ConnClosed)
 where
   -- Be kind to peer & close gracefully.
  closeConn ConnClosed = return ()
  closeConn conn = do
    let sk = connSock conn
    hFlush (connHandle conn)
    shutdown sk ShutdownSend
    suck readL
    hClose (connHandle conn)
    shutdown sk ShutdownReceive
    sClose sk

  suck :: IO Bool -> IO ()
  suck rd = do
    f <- rd
    if f then return () else suck rd
{-
-- | Checks both that the underlying Socket is connected
-- and that the connection peer matches the given
-- host name (which is recorded locally).
isConnectedTo :: Connection -> EndPoint -> IO Bool
isConnectedTo (Connection conn) endPoint = do
   v <- readMVar (getRef conn)
   case v of
     ConnClosed -> print "aa" >> return False
     _ 
      | connEndPoint v == endPoint ->
          catchIO (getPeerName (connSock v) >> return True) (const $ return False)
      | otherwise -> return False
-}

isTCPConnectedTo :: HandleStream -> EndPoint -> IO Bool
isTCPConnectedTo conn endPoint = do
   v <- readMVar (getRef conn)
   case v of
     ConnClosed -> return False
     _ 
      | connEndPoint v == endPoint ->
          catchIO (getPeerName (connSock v) >> return True) (const $ return False)
      | otherwise -> return False

readBlockBS :: HandleStream -> Int -> IO (SResult ByteString)
readBlockBS ref n = onNonClosedDo ref $ \ conn -> do
   x <- bufferGetBlock ref n
   return x

-- This function uses a buffer, at this time the buffer is just 1000 characters.
-- (however many bytes this is is left for the user to decipher)
readLineBS :: HandleStream -> IO (SResult ByteString)
readLineBS ref = onNonClosedDo ref $ \ conn -> do
   x <- bufferReadLine ref
   return x

-- The 'Connection' object allows no outward buffering, 
-- since in general messages are serialised in their entirety.
writeBlockBS :: HandleStream -> ByteString -> IO (SResult ())
writeBlockBS ref b = (onNonClosedDo ref $ \conn -> do
  x    <- bufferPutBlock (connHandle conn) b
  return (Right strEmpty)) >> return (Right ())

closeIt :: HandleStream -> (ByteString -> Bool) -> Bool -> IO ()
closeIt c p b = do
   closeConnection c (if b
                      then readLineBS c >>= \ x -> case x of { Right xs -> return (p xs); _ -> return True}
                      else return True)
   conn <- readMVar (getRef c)
   return ()

closeEOF :: HandleStream -> Bool -> IO ()
closeEOF c flg = modifyMVar_ (getRef c) (\ co -> return co{connCloseEOF=flg})

bufferGetBlock :: HandleStream -> Int -> IO (SResult ByteString)
bufferGetBlock ref n = onNonClosedDo ref $ \ conn -> do
   case connInput conn of
    Just c -> do
      let (a,b) = strSplitAt n c
      modifyMVar_ (getRef ref) (\ co -> return co{connInput=Just b})
      return (return a)
    _ -> do
      catchIO (strHGet (connHandle conn) n >>= return.return)
              (\e ->
                if isEOFError e 
                then do
                  when (connCloseEOF conn) $ catchIO (closeQuick ref) (\ _ -> return ())
                  return (return strEmpty)
                else return (failMisc (show e)))

bufferPutBlock :: Handle -> ByteString -> IO (SResult ())
bufferPutBlock h b = 
  catchIO (strHPut h b >> hFlush h >> return (return ()))
          (\ e -> return (failMisc (show e)))

bufferReadLine :: HandleStream -> IO (SResult ByteString)
bufferReadLine ref = onNonClosedDo ref $ \ conn -> do
  case connInput conn of
   Just c -> do
    let (a,b1)  = case strUntil (== asByte '\n') c of { Nothing -> (c, strEmpty); Just (d,e) -> (d,e) }
    modifyMVar_ (getRef ref) (\co -> return co{connInput=Just b1})
    return (return (strAppend a (asByteString "\n")))
   _ -> catchIO
              (strHGetLine (connHandle conn) >>= 
                 return . return . appendNL )
              (\ e ->
                 if isEOFError e
                  then do
                      when (connCloseEOF conn) $ catchIO (closeQuick ref) (\ _ -> return ())
                      return (return   (strEmpty ))
                  else return (failMisc (show e)))
 where
   -- yes, this s**ks.. _may_ have to be addressed if perf
   -- suggests worthiness.
  appendNL :: ByteString -> ByteString
  appendNL b = strSnoc b nl
  
  nl :: Word8
  nl = fromIntegral (fromEnum '\n')

onNonClosedDo :: HandleStream -> (Conn ByteString -> IO (SResult ByteString)) -> IO (SResult ByteString)
onNonClosedDo h act = do
  x <- readMVar (getRef h)
  case x of
    ConnClosed{} -> return (failWith ErrorClosed)
    _ -> act x
 
-----------------------------------------------------------------------------
-- Module      :  Network.Stream

data ConnError 
 = ErrorReset 
 | ErrorClosed
 | ErrorParse String
 | ErrorMisc String
   deriving(Show,Eq)

instance Exception ConnError where
--   noMsg = strMsg "unknown error"
--   strMsg x = ErrorMisc x

-- in GHC 7.0 the Monad instance for Error no longer
-- uses fail x = Left (strMsg x). failMisc is therefore
-- used instead.
failMisc :: String -> SResult a
failMisc x = failWith (ErrorMisc x)

failWith :: ConnError -> SResult a
failWith x = Left x

bindE :: SResult a -> (a -> SResult b) -> SResult b
bindE (Left e)  _ = Left e
bindE (Right v) f = f v

fmapE :: (a -> SResult b) -> IO (SResult a) -> IO (SResult b)
fmapE f a = do
 x <- a
 case x of
   Left  e -> return (Left e)
   Right r -> return (f r) 
  
-- | This is the type returned by many exported network functions.
type SResult a = Either ConnError   {- error  -}
                       a           {- result -}

-- | Streams should make layering of TLS protocol easier in future,
-- they allow reading/writing to files etc for debugging,
-- they allow use of protocols other than TCP/IP
-- and they allow customisation.
--
-- Instances of this class should not trim
-- the input in any way, e.g. leave LF on line
-- endings etc. Unless that is exactly the behaviour
-- you want from your twisted instances ;)
class SStream x where 
    sreadLine   :: x -> IO (SResult String)
    sreadBlock  :: x -> Int -> IO (SResult String)
    swriteBlock :: x -> String -> IO (SResult ())
    sclose      :: x -> IO ()
    scloseOnEnd :: x -> Bool -> IO ()
      -- ^ True => shutdown the connection when response has been read / end-of-stream
      --           has been reached.
-----------------------------------------------------------------------------
-- Module      :  Network.StreamDebugger

-- | Allows stream logging.  Refer to 'debugStream' below.
data StreamDebugger x
   = Dbg Handle x

instance (SStream x) => SStream (StreamDebugger x) where
    sreadBlock (Dbg h x) n =
        do val <- sreadBlock x n
           hPutStrLn h ("--readBlock " ++ show n)
           hPutStrLn h (show val)
           return val
    sreadLine (Dbg h x) =
        do val <- sreadLine x
           hPutStrLn h ("--readLine")
           hPutStrLn h (show val)
           return val
    swriteBlock (Dbg h x) str =
        do val <- swriteBlock x str
           hPutStrLn h ("--writeBlock" ++ show str)
           hPutStrLn h (show val)
           return val
    sclose (Dbg h x) =
        do hPutStrLn h "--closing..."
           hFlush h
           sclose x
           hPutStrLn h "--closed."
           hClose h
    scloseOnEnd (Dbg h x) f =
        do hPutStrLn h ("--close-on-end.." ++ show f)
           hFlush h 
           scloseOnEnd x f
{-
-- | Wraps a stream with logging I\/O.
--   The first argument is a filename which is opened in @AppendMode@.
debugStream :: (SStream a) => FilePath -> a -> IO (StreamDebugger a)
debugStream file stream = 
    do h <- openFile file AppendMode
       hPutStrLn h ("File \"" ++ file ++ "\" opened for appending.")
       return (Dbg h stream)
-}

-----------------------------------------------------------------------------
-- Module      :  Network.StreamSocket
-----------------------------------------------------------------------------

-- | Exception handler for socket operations.
handleSocketError :: Socket -> IOException -> IO (SResult a)
handleSocketError sk e =
    do se <- getSocketOption sk SoError
       case se of
          0     -> ioError e
          10054 -> return $ Left ErrorReset  -- reset
          _     -> return $ Left $ ErrorMisc $ show se

myrecv :: Socket -> Int -> IO String
myrecv sock len =
    (let handler e = if isEOFError e then return strEmpty else ioError e
         in catchIO (sktRecv sock len) handler) >>= return . asString

instance SStream Socket where
    sreadBlock sk n    = readBlockSocket sk n
    sreadLine sk       = readLineSocket sk
    swriteBlock sk str = writeBlockSocket sk str
    sclose sk          = do
        -- This slams closed the connection (which is considered rude for TCP\/IP)
         shutdown sk ShutdownBoth
         sClose sk
    scloseOnEnd _sk _  = return () -- can't really deal with this, so do run the risk of leaking sockets here.

readBlockSocket :: Socket -> Int -> IO (SResult String)
readBlockSocket sk n = (liftM Right $ fn n) `catchIO` (handleSocketError sk)
  where
   fn x = do { str <- myrecv sk x
             ; let len = length str
             ; if len < x
                then ( fn (x-len) >>= \more -> return (str++more) )
                else return str
             }

-- Use of the following function is discouraged.
-- The function reads in one character at a time, 
-- which causes many calls to the kernel recv()
-- hence causes many context switches.
readLineSocket :: Socket -> IO (SResult String)
readLineSocket sk = (liftM Right $ fn "") `catchIO` (handleSocketError sk)
  where
   fn str = do
     c <- myrecv sk 1 -- like eating through a straw.
     if null c || c == "\n"
      then return (reverse str++c)
      else fn (head c:str)
    
writeBlockSocket :: Socket -> String -> IO (SResult ())
writeBlockSocket sk str = (liftM Right $ fn str) `catchIO` (handleSocketError sk)
  where
   fn :: String -> IO ()
   fn [] = return ()
   fn x  = sktSend sk (asByteString x) >>= \i -> fn (drop i x)

--------------------------------------------------------------------------
--
------------------------------------------------------------
--  The URI datatype
------------------------------------------------------------

-- |Represents a general universal resource identifier using
--  its component parts.
data URI = URI
    { uriScheme     :: String
    , uriAuthority  :: Maybe URIAuth
    , uriPath       :: String
    , uriQuery      :: String
    , uriFragment   :: String
    } deriving (Eq, Ord)

-- |Type for authority value within a URI
data URIAuth = URIAuth { uriUserInfo :: String , uriRegName :: String , uriPort :: String }
     deriving (Eq, Ord, Show)

-- |Blank URI
nullURI :: URI
nullURI = URI { uriScheme="", uriAuthority=Nothing, uriPath="", uriQuery="", uriFragment="" }

instance Show URI where
    showsPrec _ = uriToString 
------------------------------------------------------------
--  Parse a URI
------------------------------------------------------------

-- |Turn a string containing a URI into a 'URI'.
--  Returns 'Nothing' if the string is not a valid URI;
--  (an absolute URI with optional fragment identifier).
--
--  NOTE: this is different from the previous network.URI,
--  whose @parseURI@ function works like 'parseURIReference'
--  in this module.
--

parseURI :: String -> Maybe URI
parseURI = parseURIAny uri

-- |Parse a URI reference to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid URI reference.
--  (an absolute or relative URI with optional fragment identifier).
--
parseURIReference :: String -> Maybe URI
parseURIReference = parseURIAny uriReference

parseURIAny :: (String -> (Maybe URI, String)) -> String -> Maybe URI
parseURIAny f uristr = let (a,b) = f uristr in if null b then a else Nothing

-- |Parse a relative URI to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid relative URI.
--  (a relative URI with optional fragment identifier).
--
parseRelativeReference :: String -> Maybe URI
parseRelativeReference = parseURIAny relativeRef

-- |Parse an absolute URI to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid absolute URI.
--  (an absolute URI without a fragment identifier).
--
parseAbsoluteURI :: String -> Maybe URI
parseAbsoluteURI = parseURIAny absoluteURI

{- r0ml
-- |Test if string contains a valid URI
--  (an absolute URI with optional fragment identifier).
--
isURI :: String -> Bool
isURI = isValidParse uri

-- |Test if string contains a valid URI reference
--  (an absolute or relative URI with optional fragment identifier).
--
isURIReference :: String -> Bool
isURIReference = isValidParse uriReference

-- |Test if string contains a valid relative URI
--  (a relative URI with optional fragment identifier).
--
isRelativeReference :: String -> Bool
isRelativeReference = isValidParse relativeRef

-- |Test if string contains a valid absolute URI
--  (an absolute URI without a fragment identifier).
--
isAbsoluteURI :: String -> Bool
isAbsoluteURI = isValidParse absoluteURI

-- |Test if string contains a valid IPv6 address
--
isIPv6address :: String -> Bool
isIPv6address = isValidParse ipv6address

-- |Test if string contains a valid IPv4 address
--
isIPv4address :: String -> Bool
isIPv4address = isValidParse ipv4address

--  Helper function for turning a string into a URI
--
--  Helper function to test a string match to a parser
--
isValidParse :: URIParser a -> String -> Bool
isValidParse parser uristr = case parseAll parser "" uristr of
        -- Left  e -> error (show e)
        Left  _ -> False
        Right _ -> True

parseAll :: URIParser a -> String -> String -> Either ParseError a
parseAll parser filename uristr = parse newparser filename uristr
    where
        newparser =
            do  { res <- parser
                ; eof
                ; return res
                }
-}
------------------------------------------------------------
--  Predicates
------------------------------------------------------------

uriIsAbsolute :: URI -> Bool
uriIsAbsolute (URI {uriScheme = scheme'}) = scheme' /= ""

uriIsRelative :: URI -> Bool
uriIsRelative = not . uriIsAbsolute

------------------------------------------------------------
--  URI parser body based on Parsec elements and combinators
------------------------------------------------------------

--  RFC3986, section 2.1
--
--  Parse and return a 'pct-encoded' sequence
--
escaped :: String -> (Maybe String, String)
escaped z@('%':h1:h2:r) = if isHexDigit h1 && isHexDigit h2 then (Just ['%',h1,h2], r)
                      else (Nothing, z)
escaped z = (Nothing, z)

--  RFC3986, section 2.2
--
-- |Returns 'True' if the character is a \"reserved\" character in a
--  URI.  To include a literal instance of one of these characters in a
--  component of a URI, it must be escaped.
--
{-
isReserved :: Char -> Bool
isReserved c = isGenDelims c || isSubDelims c
-}

{-
isGenDelims :: Char -> Bool
isGenDelims c = c `elem` ":/?#[]@"
-}

isSubDelims :: Char -> Bool
isSubDelims c = c `elem` "!$&'()*+,;="

--  RFC3986, section 2.3
--
-- |Returns 'True' if the character is an \"unreserved\" character in
--  a URI.  These characters do not need to be escaped in a URI.  The
--  only characters allowed in a URI are either \"reserved\",
--  \"unreserved\", or an escape sequence (@%@ followed by two hex digits).
--
isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNumChar c || (c `elem` "-_.~")

--  RFC3986, section 3
--
--   URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
--
--   hier-part   = "//" authority path-abempty
--               / path-abs
--               / path-rootless
--               / path-empty
uri :: String -> (Maybe URI, String)
uri s = let (us, s2) = uscheme s
            ((ua, up), s3) = hierPart s2
            (uq, s4) = uquery s3
            (uf, s5) = ufragment s4 
         in if isNothing us then (Nothing, s)
            else (Just $ URI { uriScheme = fromJust us , uriAuthority = ua , uriPath = up
                            , uriQuery = uq , uriFragment = uf }, s5)

--  RFC3986, section 3.1

uscheme :: String -> (Maybe String, String)
uscheme s = if not (null s) && isAlphaChar (head s)
            then let p = takeWhile isSchemeChar (tail s)
                     j = drop (1+length p) s
                  in if not (null j) && head j == ':'
                     then (Just (head s : p), tail j) else (Nothing, s)
            else (Nothing, s)

--  RFC3986, section 3.2
uauthority :: String -> (Maybe URIAuth, String)
uauthority s = let (uu,s2) = userinfo s
                   (uh,s3) = host s2
                   (up,s4) = port s3
                in case uh of
                      Nothing -> (Nothing, s)
                      Just hh -> (Just $ URIAuth { uriUserInfo = maybe "" id uu
                                   , uriRegName = hh
                                   , uriPort = maybe "" id up }, s4)

--  RFC3986, section 3.2.1
userinfo :: String -> (Maybe String, String)
userinfo = catOf [ catManyOf (uchar ";:&=+$,"), enString . satisfy (=='@') ]

--  RFC3986, section 3.2.2
host :: String -> (Maybe String, String)
host = oneOf [ipLiteral, ipv4address, regName]

ipLiteral :: String -> (Maybe String, String)
ipLiteral = catOf [enString . satisfy (=='['), oneOf [ipv6address, ipvFuture], enString . satisfy (==']') ]

ipvFuture :: String -> (Maybe String, String)
ipvFuture = catOf [ enString . satisfy (=='v'), enString . satisfy isHexDigit, enString . satisfy (=='.'), jfy . popWhile isIpvFutureChar ]

isIpvFutureChar :: Char -> Bool
isIpvFutureChar c = isUnreserved c || isSubDelims c || (c==';')

ipv6address :: String -> (Maybe String, String)
ipv6address = oneOf [ catOf [h4c, h4c, h4c, h4c, h4c, h4c, h4c, ls32 ]
  , catOf [ stringOf "::", h4c, h4c, h4c, h4c, h4c, ls32 ]
  , catOf [ opt_n_h4c_h4 0, stringOf "::", h4c, h4c, h4c, h4c, ls32]
  , catOf [ opt_n_h4c_h4 1, stringOf "::", h4c, h4c, h4c, ls32]
  , catOf [ opt_n_h4c_h4 2, stringOf "::", h4c, h4c, ls32]
  , catOf [ opt_n_h4c_h4 3, stringOf "::", h4c, ls32]
  , catOf [ opt_n_h4c_h4 4, stringOf "::", ls32 ]
  , catOf [ opt_n_h4c_h4 5, stringOf "::", h4 ]
  , catOf [ opt_n_h4c_h4 6, stringOf "::" ]
  ]

opt_n_h4c_h4 :: Int -> String -> (Maybe String, String) 
opt_n_h4c_h4 n s = catOf [ concOf . countMinMax 0 n h4c, h4] s
                  
                       
ls32 :: String -> (Maybe String, String)
ls32 =  oneOf [lss, ipv4address]
  where lss = catOf [h4c, h4]

h4c :: String -> (Maybe String, String)
h4c s = let (a1, s1) = catOf [h4, enString . satisfy (==':')] s
            (d, s3) = satisfy (==':') s1
         in case a1 of
               Nothing -> (Nothing, s)
               Just a11 -> case d of 
                              Nothing -> (a1, s1)
                              Just _ -> (Nothing, s)

h4 :: String -> (Maybe String, String)
h4 = concOf . countMinMax 1 4 (enString . satisfy isHexDigit)

ipv4address :: String -> (Maybe String, String)
ipv4address s = let (a,b) = catOf [decOctet, dot, decOctet, dot, decOctet, dot, decOctet] s
                    (c,d) = nameChar b
                 in case c of 
                      Nothing -> (a, b)
                      Just cc -> (Nothing, s)
  where dot = enString . satisfy (=='.')

decOctet :: String -> (Maybe String, String)
decOctet s = let (a1, s1) = concOf (countMinMax 1 3 (enString . satisfy isDigit) s)
              in case a1 of 
                   Nothing -> (Nothing, s)
                   Just a11 -> if (read a11 :: Integer) > 255 then (Nothing, s) else (a1, s1)

regName :: String -> (Maybe String, String)
regName = concOf . countMinMax 0 255 nameChar

nameChar :: String -> (Maybe String, String)
nameChar s = let (a,b) = escaped s
              in if a == Nothing 
                 then if null b then (Nothing, b)
                      else let c = head b
                            in if isUnreserved c || isSubDelims c 
                               then (Just [c], tail b)
                               else (Nothing, b)
                 else (a,b)

--  RFC3986, section 3.2.3
port :: String -> (Maybe String, String)
port = catOf [ enString . satisfy (== ':'), manyOf (satisfy isDigit) ]

--
--  RFC3986, section 3.3
--
--   path          = path-abempty    ; begins with "/" or is empty
--                 / path-abs        ; begins with "/" but not "//"
--                 / path-noscheme   ; begins with a non-colon segment
--                 / path-rootless   ; begins with a segment
--                 / path-empty      ; zero characters
--
--   path-abempty  = *( "/" segment )
--   path-abs      = "/" [ segment-nz *( "/" segment ) ]
--   path-noscheme = segment-nzc *( "/" segment )
--   path-rootless = segment-nz *( "/" segment )
--   path-empty    = 0<pchar>
--
--   segment       = *pchar
--   segment-nz    = 1*pchar
--   segment-nzc   = 1*( unreserved / pct-encoded / sub-delims / "@" )
--
--   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

pathAbEmpty :: String -> (Maybe String, String)
pathAbEmpty s = let ss = slashSegments s in concOf (jfy ss) 

pathAbs :: String -> (Maybe String, String)
pathAbs ('/':s) = let (a,b) = pathRootLess s
                   in (Just (maybe "/" ('/':) a), b)

pathNoScheme :: String -> PopString
pathNoScheme s = let (s1, s3) = segmentNzc s
                     (ss, s4) = slashSegments s3
                  in case s1 of 
                       Nothing -> (Nothing, s)
                       Just s11 -> (Just (concat (s11:ss)), s4)

pathRootLess :: String -> PopString
pathRootLess s = let (s1, s2) = segmentNz s
                  in case s1 of 
                        Nothing -> (Nothing, s)
                        Just s3 -> let (s4, s5) = slashSegments s2
                                    in (Just (concat (s3:s4)), s5)

slashSegments s5 = let (a,b) = slashSegment s5
                    in case a of
                          Nothing -> ([], b)
                          Just aa -> let (d,e) = slashSegments b in (aa:d, e)

slashSegment :: String -> PopString
slashSegment ('/':s) = let (Just a,b) = segment s in (Just ('/':a), b)
slashSegment s = (Nothing, s)

segment :: String -> PopString
segment s = let (a,b) = segmentNz s in (Just (maybe "" id a) ,b)

type PopString = (Maybe String, String)

segmentNz :: String -> PopString 
segmentNz = segmentNzx ":@"

segmentNzc :: String -> PopString
segmentNzc = segmentNzx "@"

segmentNzx :: String -> String -> PopString
segmentNzx x s = let (r, s2) = uchar x s
                 in case r of
                      Nothing -> (Nothing, s2)
                      Just r1 -> let (r2, s3) = segmentNzx x s2
                                  in (Just (maybe r1 (head r1:) r2), s3)

pchar :: String -> (Maybe String, String)
pchar = uchar ":@"

-- helper function for pchar and friends
uchar :: String -> String -> (Maybe String, String)
uchar extras s = let (r,s2) = nameChar s
                  in if r == Nothing 
                     then if not (null s) && head s `elem` extras then (Just [head s], tail s)
                                                                  else (Nothing, s)
                     else (r, s2)

--  RFC3986, section 3.4
uquery :: String -> (String, String)
uquery s = let (a,s1) = concOf (manyOf (uchar ":@/?") s) in (maybe "?" ('?':) a, s1)

--  RFC3986, section 3.5
ufragment :: String -> (String, String)
ufragment s = let (a,s1) = concOf (manyOf (uchar ":@/?") s) in (maybe "#" ('#':) a, s1)

--  Reference, Relative and Absolute URI forms
--
--  RFC3986, section 4.1
uriReference :: String -> (Maybe URI, String)
uriReference s = let (a,s1) = uri s 
                     (b,s2) = relativeRef s
                  in if isJust a then (a, s1) else (b, s2)

--  RFC3986, section 4.2
--
--   relative-URI  = relative-part [ "?" query ] [ "#" fragment ]
--
--   relative-part = "//" authority path-abempty
--                 / path-abs
--                 / path-noscheme
--                 / path-empty
relativeRef :: String -> (Maybe URI, String)
relativeRef s = let (us, s2) = uscheme s
                    ((ua, up), s3) = relativePart s2
                    (uq, s4) = uquery s3
                    (uf, s5) = ufragment s4
                 in (Just $ URI { uriScheme = ""
                               , uriAuthority = ua
                               , uriPath      = up
                               , uriQuery     = uq
                               , uriFragment  = uf
                               }, s5)

relativePart :: String -> (((Maybe URIAuth),String), String)
relativePart s =
    let (uph, s1) = pathUAb s
        (upa, s2) = pathAbs s
        (ups, s3) = pathNoScheme s
     in if isJust uph then (fromJust uph, s1)
        else if isJust upa then ((Nothing, fromJust upa), s2)
        else if isJust ups then ((Nothing, fromJust ups), s3)
        else ((Nothing, ""), s)

hierPart :: String -> (((Maybe URIAuth),String), String)
hierPart s =  
    let (uph, s1) = pathUAb s
        (upa, s2) = pathAbs s
        (upl, s3) = pathRootLess s
     in if isJust uph then (fromJust uph, s1)
        else if isJust upa then ((Nothing, fromJust upa), s2)
        else if isJust upl then ((Nothing, fromJust upl), s3)
        else ((Nothing, ""), s)

pathUAb ('/':'/':s) = let (ua, s1) = uauthority s
                          (up, s2) = pathAbEmpty s1
                       in if isNothing ua then (Nothing, s)
                          else if isNothing up then (Nothing, s)
                          else (Just (ua, fromJust up), s2)
pathUAb s = (Nothing, s)



--  RFC3986, section 4.3

absoluteURI :: String -> (Maybe URI, String)
absoluteURI s = let (us, s2) = uscheme s
                    ((ua,up), s3) = hierPart s2
                    (uq, s4) = uquery s3
                 in if isNothing us then (Nothing, s)
                    else (Just $ URI {
                            uriScheme = fromJust us, uriAuthority = ua,
                            uriPath = up, uriQuery = uq, uriFragment = "" }, s4)

--  Imports from RFC 2234
isAlphaChar :: Char -> Bool
isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isDigitChar :: Char -> Bool
isDigitChar c    = (c >= '0' && c <= '9')

isAlphaNumChar :: Char -> Bool
isAlphaNumChar c = isAlphaChar c || isDigitChar c

isSchemeChar :: Char -> Bool
isSchemeChar c   = (isAlphaNumChar c) || (c `elem` "+-.")

------------------------------------------------------------
--  Reconstruct a URI string
------------------------------------------------------------
--
-- |Turn a 'URI' into a string.
--
--  Uses a supplied function to map the userinfo part of the URI.
--
--  The Show instance for URI uses a mapping that hides any password
--  that may be present in the URI.  Use this function with argument @id@
--  to preserve the password in the formatted output.
--
uriToString :: URI -> ShowS
uriToString URI { uriScheme=myscheme
                            , uriAuthority=myauthority
                            , uriPath=mypath
                            , uriQuery=myquery
                            , uriFragment=myfragment
                            } =
    (myscheme++) . (uriAuthToString myauthority)
               . (mypath++) . (myquery++) . (myfragment++)

uriAuthToString2 :: URIAuth -> String
uriAuthToString2 ua = concat [ uriUserInfo ua , uriRegName ua , uriPort ua ]

uriAuthToString :: (Maybe URIAuth) -> ShowS
uriAuthToString Nothing = id          -- shows ""
uriAuthToString
        (Just URIAuth { uriUserInfo = myuinfo
                      , uriRegName  = myregname
                      , uriPort     = myport
                      } ) =
    ("//"++) . (myuinfo++) . (myregname++) . (myport++) 

------------------------------------------------------------
--  Character classes
------------------------------------------------------------

{-
-- | Returns 'True' if the character is allowed in a URI.
--
isAllowedInURI :: Char -> Bool
isAllowedInURI c = isReserved c || isUnreserved c || c == '%' -- escape char
-}

{-
-- | Returns 'True' if the character is allowed unescaped in a URI.
--
isUnescapedInURI :: Char -> Bool
isUnescapedInURI c = isReserved c || isUnreserved c
-}

{-
-- | Returns 'True' if the character is allowed unescaped in a URI component.
--
isUnescapedInURIComponent :: Char -> Bool
isUnescapedInURIComponent c = not (isReserved c || not (isUnescapedInURI c))
-}
------------------------------------------------------------
--  Escape sequence handling
------------------------------------------------------------

-- |Escape character if supplied predicate is not satisfied,
--  otherwise return character as singleton string.
--
{-
escapeURIChar :: (Char->Bool) -> Char -> String
escapeURIChar p c
    | p c       = [c]
    | otherwise = concatMap (\i -> '%' : myShowHex i "") (utf8EncodeChar c)
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 (toChrHex) n r of
            []  -> "00"
            [x] -> ['0',x]
            cs  -> cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))
-}


-- From http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
-- Returns [Int] for use with showIntAtBase
utf8EncodeChar :: Char -> [Int]
utf8EncodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

{-
-- |Can be used to make a string valid for use in a URI.
--
escapeURIString
    :: (Char->Bool)     -- ^ a predicate which returns 'False'
                        --   if the character should be escaped
    -> String           -- ^ the string to process
    -> String           -- ^ the resulting URI string
escapeURIString p s = concatMap (escapeURIChar p) s
-}

-- |Turns all instances of escaped characters in the string back
--  into literal characters.
--
unEscapeString :: String -> String
unEscapeString [] = ""
unEscapeString s@(c:cs) = case unEscapeByte s of
    Just (byte, rest) -> unEscapeUtf8 byte rest
    Nothing -> c : unEscapeString cs

unEscapeByte :: String -> Maybe (Int, String)
unEscapeByte ('%':x1:x2:s) | isHexDigit x1 && isHexDigit x2 =
    Just (digitToInt x1 * 16 + digitToInt x2, s)
unEscapeByte _ = Nothing

-- Adapted from http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
unEscapeUtf8 :: Int -> String -> String
unEscapeUtf8 c rest
    | c < 0x80 = chr c : unEscapeString rest
    | c < 0xc0 = replacement_character : unEscapeString rest
    | c < 0xe0 = multi1
    | c < 0xf0 = multi_byte 2 0xf 0x800
    | c < 0xf8 = multi_byte 3 0x7 0x10000
    | c < 0xfc = multi_byte 4 0x3 0x200000
    | c < 0xfe = multi_byte 5 0x1 0x4000000
    | otherwise    = replacement_character : unEscapeString rest
    where
    replacement_character = '\xfffd'
    multi1 = case unEscapeByte rest of
      Just (c1, ds) | c1 .&. 0xc0 == 0x80 ->
        let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
        in if d >= 0x000080 then toEnum d : unEscapeString ds
                            else replacement_character : unEscapeString ds
      _ -> replacement_character : unEscapeString rest

    multi_byte :: Int -> Int -> Int -> String
    multi_byte i mask overlong =
      aux i rest (unEscapeByte rest) (c .&. mask)
      where
        aux 0 rs _ acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : unEscapeString rs
          | otherwise = replacement_character : unEscapeString rs

        aux n _ (Just (r, rs)) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs (unEscapeByte rs)
                               $! shiftL acc 6 .|. (r .&. 0x3f)

        aux _ rs _ _ = replacement_character : unEscapeString rs

------------------------------------------------------------
-- Resolving a relative URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the value of the
--  first 'URI' interpreted as relative to the second 'URI'.
--  For example:
--
--  > "foo" `relativeTo` "http://bar.org/" = "http://bar.org/foo"
--  > "http:foo" `nonStrictRelativeTo` "http://bar.org/" = "http://bar.org/foo"
--
--  Algorithm from RFC3986 [3], section 5.2.2
--

nonStrictRelativeTo :: URI -> URI -> URI
nonStrictRelativeTo ref base = relativeTo ref' base
    where
        ref' = if uriScheme ref == uriScheme base
               then ref { uriScheme="" }
               else ref

-- | Returns a new 'URI' which represents the value of the first 'URI'
-- interpreted as relative to the second 'URI'.
--
-- Algorithm from RFC3986 [3], section 5.2
relativeTo :: URI -> URI -> URI
relativeTo ref base
    | isDefined ( uriScheme ref ) =
        just_segments ref
    | isDefined ( uriAuthority ref ) =
        just_segments ref { uriScheme = uriScheme base }
    | isDefined ( uriPath ref ) =
        if (head (uriPath ref) == '/') then
            just_segments ref
                { uriScheme    = uriScheme base
                , uriAuthority = uriAuthority base
                }
        else
            just_segments ref
                { uriScheme    = uriScheme base
                , uriAuthority = uriAuthority base
                , uriPath      = mergePaths base ref
                }
    | isDefined ( uriQuery ref ) =
        just_segments ref
            { uriScheme    = uriScheme base
            , uriAuthority = uriAuthority base
            , uriPath      = uriPath base
            }
    | otherwise =
        just_segments ref
            { uriScheme    = uriScheme base
            , uriAuthority = uriAuthority base
            , uriPath      = uriPath base
            , uriQuery     = uriQuery base
            }
    where
        isDefined a = (not (null a))

        just_segments u =
            u { uriPath = removeDotSegments (uriPath u) }
        mergePaths b r
            | isDefined (uriAuthority b) && null pb = '/':pr
            | otherwise                             = dropLast pb ++ pr
            where
                pb = uriPath b
                pr = uriPath r
        dropLast = fst . splitLast -- reverse . dropWhile (/='/') . reverse

--  Remove dot segments, but protect leading '/' character
removeDotSegments :: String -> String
removeDotSegments ('/':ps) = '/':elimDots ps []
removeDotSegments ps       = elimDots ps []

--  Second arg accumulates segments processed so far in reverse order
elimDots :: String -> [String] -> String
-- elimDots ps rs | traceVal "\nps " ps $ traceVal "rs " rs $ False = error ""
elimDots [] [] = ""
elimDots [] rs = concat (reverse rs)
elimDots (    '.':'/':ps)     rs = elimDots ps rs
elimDots (    '.':[]    )     rs = elimDots [] rs
elimDots (    '.':'.':'/':ps) rs = elimDots ps (drop 1 rs)
elimDots (    '.':'.':[]    ) rs = elimDots [] (drop 1 rs)
elimDots ps rs = elimDots ps1 (r:rs)
    where
        (r,ps1) = nextSegment ps

--  Returns the next segment and the rest of the path from a path string.
--  Each segment ends with the next '/' or the end of string.
--
nextSegment :: String -> (String,String)
nextSegment ps =
    case break (=='/') ps of
        (r,'/':ps1) -> (r++"/",ps1)
        (r,_)       -> (r,[])

--  Split last (name) segment from path, returning (path,name)
splitLast :: String -> (String,String)
splitLast p = (reverse revpath,reverse revname)
    where
        (revname,revpath) = break (=='/') $ reverse p

------------------------------------------------------------
-- Finding a URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the relative location of
--  the first 'URI' with respect to the second 'URI'.  Thus, the
--  values supplied are expected to be absolute URIs, and the result
--  returned may be a relative URI.
--
--  Example:
--
--  > "http://example.com/Root/sub1/name2#frag"
--  >   `relativeFrom` "http://example.com/Root/sub2/name2#frag"
--  >   == "../sub1/name2#frag"
--
--  There is no single correct implementation of this function,
--  but any acceptable implementation must satisfy the following:
--
--  > (uabs `relativeFrom` ubase) `relativeTo` ubase == uabs
--
--  For any valid absolute URI.
--  (cf. <http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html>
--       <http://lists.w3.org/Archives/Public/uri/2003Jan/0005.html>)
--
relativeFrom :: URI -> URI -> URI
relativeFrom uabs base
    | diff uriScheme    uabs base = uabs
    | diff uriAuthority uabs base = uabs { uriScheme = "" }
    | diff uriPath      uabs base = uabs
        { uriScheme    = ""
        , uriAuthority = Nothing
        , uriPath      = relPathFrom (removeBodyDotSegments $ uriPath uabs)
                                     (removeBodyDotSegments $ uriPath base)
        }
    | diff uriQuery     uabs base = uabs
        { uriScheme    = ""
        , uriAuthority = Nothing
        , uriPath      = ""
        }
    | otherwise = uabs          -- Always carry fragment from uabs
        { uriScheme    = ""
        , uriAuthority = Nothing
        , uriPath      = ""
        , uriQuery     = ""
        }
    where
        diff :: Eq b => (a -> b) -> a -> a -> Bool
        diff sel u1 u2 = sel u1 /= sel u2
        -- Remove dot segments except the final segment
        removeBodyDotSegments p = removeDotSegments p1 ++ p2
            where
                (p1,p2) = splitLast p

relPathFrom :: String -> String -> String
relPathFrom []   _    = "/"
relPathFrom pabs []   = pabs
relPathFrom pabs base =                 -- Construct a relative path segments
    if sa1 == sb1                       -- if the paths share a leading segment
        then if (sa1 == "/")            -- other than a leading '/'
            then if (sa2 == sb2)
                then relPathFrom1 ra2 rb2
                else pabs
            else relPathFrom1 ra1 rb1
        else pabs
    where
        (sa1,ra1) = nextSegment pabs
        (sb1,rb1) = nextSegment base
        (sa2,ra2) = nextSegment ra1
        (sb2,rb2) = nextSegment rb1

--  relPathFrom1 strips off trailing names from the supplied paths,
--  and calls difPathFrom to find the relative path from base to
--  target
relPathFrom1 :: String -> String -> String
relPathFrom1 pabs base = relName
    where
        (sa,na) = splitLast pabs
        (sb,nb) = splitLast base
        rp      = relSegsFrom sa sb
        relName = if null rp then
                      if (na == nb) then ""
                      else if protect na then "./"++na
                      else na
                  else
                      rp++na
        -- Precede name with some path if it is null or contains a ':'
        protect s = null s || ':' `elem` s

--  relSegsFrom discards any common leading segments from both paths,
--  then invokes difSegsFrom to calculate a relative path from the end
--  of the base path to the end of the target path.
--  The final name is handled separately, so this deals only with
--  "directory" segtments.
--
relSegsFrom :: String -> String -> String
{-
relSegsFrom sabs base
    | traceVal "\nrelSegsFrom\nsabs " sabs $ traceVal "base " base $
      False = error ""
-}
relSegsFrom []   []   = ""      -- paths are identical
relSegsFrom sabs base =
    if sa1 == sb1
        then relSegsFrom ra1 rb1
        else difSegsFrom sabs base
    where
        (sa1,ra1) = nextSegment sabs
        (sb1,rb1) = nextSegment base

--  difSegsFrom calculates a path difference from base to target,
--  not including the final name at the end of the path
--  (i.e. results always ends with '/')
--
--  This function operates under the invariant that the supplied
--  value of sabs is the desired path relative to the beginning of
--  base.  Thus, when base is empty, the desired path has been found.
--
difSegsFrom :: String -> String -> String
{-
difSegsFrom sabs base
    | traceVal "\ndifSegsFrom\nsabs " sabs $ traceVal "base " base $
      False = error ""
-}
difSegsFrom sabs ""   = sabs
difSegsFrom sabs base = difSegsFrom ("../"++sabs) (snd $ nextSegment base)

------------------------------------------------------------
--  Other normalization functions
------------------------------------------------------------

-- |Case normalization; cf. RFC3986 section 6.2.2.1
--  NOTE:  authority case normalization is not performed
--
normalizeCase :: String -> String
normalizeCase uristr = ncScheme uristr
    where
        ncScheme (':':cs)                = ':':ncEscape cs
        ncScheme (c:cs) | isSchemeChar c = toLower c:ncScheme cs
        ncScheme _                       = ncEscape uristr -- no scheme present
        ncEscape ('%':h1:h2:cs) = '%':toUpper h1:toUpper h2:ncEscape cs
        ncEscape (c:cs)         = c:ncEscape cs
        ncEscape []             = []

-- |Encoding normalization; cf. RFC3986 section 6.2.2.2
--
normalizeEscape :: String -> String
normalizeEscape ('%':h1:h2:cs)
    | isHexDigit h1 && isHexDigit h2 && isUnreserved escval =
        escval:normalizeEscape cs
    where
        escval = chr (digitToInt h1*16+digitToInt h2)
normalizeEscape (c:cs)         = c:normalizeEscape cs
normalizeEscape []             = []

-- |Path segment normalization; cf. RFC3986 section 6.2.2.3
--
{- r0ml
normalizePathSegments :: String -> String
normalizePathSegments uristr = normstr juri
    where
        juri = parseURI uristr
        normstr Nothing  = uristr
        normstr (Just u) = show (normuri u)
        normuri u = u { uriPath = removeDotSegments (uriPath u) }
-}
