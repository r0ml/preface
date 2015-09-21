module Bindings.Curl.CurlPost where

import Preface.Imports
import Bindings.Curl.CurlX
import Bindings.Curl.CurlOptions
import Bindings.Curl.CurlErrors
import Bindings.Curl.CurlInfo

-- | 'curlMultiPost' perform a multi-part POST submission.
-- curlMultiPost :: URLString -> [CurlOption] -> [HttpPost] -> Curl -> IO ()
-- curlMultiPost s os ps h =  mapM_ (setopt h) ([CurloptVerbose True, CurloptURL s, CurloptHttpPost ps] ++ os)

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
  ip <- foldM c_curl_slist_append nullPtr cs2
  pokeByteOff php (ptrIndex 8) ip
  pokeByteOff php (ptrIndex 9) nullPtr
  case showName p of
    Nothing -> pokeByteOff php (ptrIndex 11) nullPtr
    Just s  -> newCString s >>= pokeByteOff php (ptrIndex 11)
  return php
 where
  ptrIndex n = n * sizeOf nullPtr
-}

{-
data HttpPost
 = HttpPost {
--     { postName     :: String
--     , contentType  :: Maybe String
--     , content      :: Content
--     , extraHeaders :: [Header]
-- not yet:     , extraEntries :: [HttpPost]
--     , showName     :: Maybe String
     } deriving ( Eq, Show )
-}

-- | 'curlPost' performs. a common POST operation, namely that
-- of submitting a sequence of name=value pairs.
curlPost :: URLString -> [SetCurlOption] -> [String] -> Curl -> IO CurlCode
curlPost s opts ps h = do
  mapM_ ($ h) [curloptSetVerbose True, curloptSetPostFields ps, curloptSetCookieJar "cookies", curloptSetURL s]
  mapM_ ($ h) opts
  toCode <$> curlPrim h c_easy_perform

-- ----------------------------------------------
data Content
 = ContentFile   FilePath
 | ContentBuffer (Ptr CChar) Int -- byte arrays also?
 | ContentString String
   deriving ( Eq, Show )
