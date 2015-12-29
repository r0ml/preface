{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module Preface.SCGI ( runSCGI, CGI(..), cgiGetHeaders, cgiGetBody
    , writeResponse, sendResponse, sendRedirect, sendLocalRedirect, doCGI
    , dumpCGI, isCGI, CGIFunction
    
  ) where

import Preface.Imports

import Network.Socket (accept)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket.ByteString as B
import Preface.Stringy

-- | A @CGIFunction@ will be called by either the CGI or SCGI runtime to execute
-- the CGI call.  The function will take a list of key/value pairs representing the 
-- environment variables set by the CGI server, and a CGI instance which provides access
-- to retrieving the request parameters and response stream.
type CGIFunction = (CGIVars -> CGI -> IO HTTPHeaders)

{- this would be a handler function which limits the number of threads -}
{-
handlern :: CGIFunction -> QSem -> Socket -> IO ()
handlern f qsem sockt = do
  waitQSem qsem
  (sock, _) <- accept sockt
  _ <- forkIO $ do
      catch (doSCGI f sock) (\e -> hPutStrLn stderr $ "scgi: "++show (e::SomeException))
      signalQSem qsem
  handlern f qsem sockt
-}

handler :: CGIFunction -> Socket -> IO ()
handler f sockt = do
  (sock, _) <- accept sockt
  _ <- forkIO $ catch (doSCGI f sock) (\e -> hPutStrLn stderr $ "scgi: "++show (e::SomeException))
  return ()

-- | runSCGIn threads port cgi-function (in the IO monad)
-- runSCGIn :: Int -> Int -> (CGI -> IO () ) -> IO ()
-- runSCGIn maxThreads port f = join (doListen port <$> handlern f <$> newQSem maxThreads )
--
-- | runSCGI port cgi-function (in the IO monad)
runSCGI :: Int -> CGIFunction -> IO ()
-- runSCGI port f = doListen port ( \x -> forever ( handler f x) )
runSCGI = (. ((forever .) . handler)) . doListen

doSCGI :: CGIFunction -> Socket -> IO ()
doSCGI f sock = do
  a <- getSCGI sock
  b <- cgiGetHeaders a
  c <- f b a
  sendResponse a c
  writeResponse a B.empty

-- | WARNING: this ignores non-digits
toInt :: String -> Int
toInt z = foldl (\x y -> 10*x+y) 0 (map digitToInt (filter isDigit z))

netstring :: NetData -> IO (ByteString, NetData)
netstring nd =
  let (lenx, rest) = case strUntil (== asByte ':') (ndBuf nd) of { Nothing -> (asByteString (show (ndBuf nd)), (ndBuf nd)); Just (a,b) -> (a,b) }
      lens = asString lenx
      len = toInt lens
   in do
        (res, ndxx) <- ndGet len nd { ndBuf = B.drop 1 rest }
        (_,ndxxx) <- ndGet (1 :: Int) ndxx
        return (res,ndxxx)

ndGet :: Integral b => b -> NetData -> IO (ByteString, NetData)
ndGet x nd@(NetData sok buf) =
  let nn = B.length buf 
      xx = fromIntegral x
  in if xx <= nn then let (bh, bt) = strSplitAt xx buf in return (bh, nd { ndBuf = bt } )
     else do
       let y = xx - nn
       more <- B.recv sok (fromIntegral (max y 4096 ) )
       if not (B.null more) then do
         (res, ndx) <-  ndGet y nd { ndBuf = more }
         return ( B.concat [buf, res], ndx )
       else return (ndBuf nd, nd { ndBuf = B.empty})

ndNew :: Socket -> IO NetData
ndNew sock = NetData sock <$> B.recv sock 4096

-- | Given a socket, sparks a thread to read it, and returns an MVar which can retrieve the
-- CGI variables, and a channel which can retrieve the post body in 4k chunks
getSCGI :: Socket -> IO CGI
getSCGI sock = do
    eenv <- newEmptyMVar
    chan <- newChan
    _ <- forkIO $ do
        (input, ndx) <- netstring =<< ndNew sock
        let vars = headersx ( asString input)
            len = case lookup "CONTENT_LENGTH" vars of { Nothing -> 0; Just x -> toInt x }
        putMVar eenv vars
        recurse len ndx chan
    hdrs <- newEmptyMVar
    wchan <- newChan
    wv <- newEmptyMVar
    _ <- forkIO $ do
        hd <- takeMVar hdrs
        -- I could send the headers immediately and then wait for the body, OR insert into the output stream
        -- sendFully sock hdx
        writeBody wchan sock (genhdrs hd)
    return $ CGI eenv chan hdrs wchan wv
  where blksiz = 4096
        writeBody x k buf = do
              buf2 <- sendBlocks k buf (8192 :: Int)
              bb <- readChan x
              if B.null bb then sendBlocks k buf2 (-1 :: Integer) >> sClose k
              else writeBody x k (B.concat [buf2, bb])
        recurse n nnd ch =
              if n <= 0 then writeChan ch B.empty
              else do (b,nnd')<-ndGet (min blksiz n) nnd
                      writeChan ch b
                      recurse (n- blksiz) nnd' ch
        headersx = pairs . splitx
        pairs (x:y:xys) = (x, y) : pairs xys
        pairs _ = []
        splitx str = case strUntil (== '\NUL') str of
                        Nothing -> [ str ]
                        Just (token, rest) -> token : splitx (tail rest)

genhdrs :: HTTPHeaders -> ByteString 
genhdrs hd =  BC.pack $ intercalate "\r\n" $ [ n++": "++v | (n,v) <- hd] ++ ["",""]

sendRedirect :: CGI -> String -> IO ()
sendRedirect rsp loc = putMVar (cgiRspHeaders rsp) [("Status","302 Found"),("Location",loc)]

sendLocalRedirect :: CGI -> String -> IO ()
sendLocalRedirect rsp loc = putMVar (cgiRspHeaders rsp) [("Status","200 Local Redirect"),("Location",loc)]

sendResponse :: CGI -> HTTPHeaders -> IO ()
sendResponse rsp = putMVar (cgiRspHeaders rsp)

writeResponse :: Stringy a => CGI -> a -> IO ()
writeResponse rsp = writeChan (cgiRspBody rsp) . asByteString 

sendBlocks :: Integral b => Socket -> ByteString -> b -> IO ByteString
sendBlocks s bs n = if B.length bs < fromIntegral n then return bs else do
  sent <- B.send s bs
  let res = B.drop sent bs
  if B.null res then return res else sendBlocks s res n

doListen :: Int -> (Socket -> IO ()) -> IO ()
doListen port loop = withSocketsDo $ bracket (listenOn (PortNumber (fromIntegral port))) sClose loop

-- defaultContentType :: String
-- defaultContentType = "text/html; charset=ISO-8859-1"

data NetData = NetData {
  _ndSocket :: Socket,
  ndBuf :: ByteString
} deriving (Show)

type CGIVars = [(String,String)]
type HTTPHeaders = [(String,String)]
type PostBody = ByteString

data CGI = CGI { cgiRqHeaders :: MVar CGIVars,  cgiRqBody :: Chan PostBody
                , cgiRspHeaders :: MVar HTTPHeaders, cgiRspBody :: Chan PostBody
                , cgiWriterVar :: MVar Bool
               }

cgiGetHeaders :: CGI -> IO CGIVars
cgiGetHeaders = takeMVar . cgiRqHeaders

cgiGetBody :: CGI -> IO ByteString
cgiGetBody cgir = cgiGetBody' cgir ""
  where cgiGetBody' cgx sf = do
          x <- readChan (cgiRqBody cgx)
          if B.null x then return sf else cgiGetBody' cgx (B.concat [sf, x])

dumpCGI :: CGIFunction
dumpCGI h a = do
  writeResponse a (show h)
  return [("Status","200 OK"), ("Content-Type","text/plain")]

-- | check to see if I'm being called as a CGI (else run as main)
isCGI :: IO Bool
isCGI = do
  v <- lookupEnv "GATEWAY_INTERFACE"
  return $ maybe False (=="CGI/1.1") v

-- | run a @CGIFunction@.  This is typically used by defining a @main@ which is:
-- 
-- > main :: IO ()
-- > main = doCGI cgif
--
doCGI :: CGIFunction -> IO ()
doCGI f = do
  a <- getCGI
  b <- cgiGetHeaders a 
  c <- f b a
  sendResponse a c
  writeResponse a B.empty
  _ <- takeMVar (cgiWriterVar a)
  return ()

writeBlocks :: ByteString -> IO ()
writeBlocks = B.putStr

getCGI :: IO CGI
getCGI = do
    eenv <- newEmptyMVar
    chan <- newChan
    vars <- getEnvironment
    putMVar eenv vars
    wv <- newEmptyMVar

    hdrs <- newEmptyMVar
    wchan <- newChan
    _ <- forkIO $ do
        hd <- takeMVar hdrs
        -- I could send the headers immediately and then wait for the body, OR insert into the output stream
        -- sendFully sock hdx
        writeBody wchan (genhdrs hd)
        putMVar wv True
    return $ CGI eenv chan hdrs wchan wv
  where writeBody x buf = do
              writeBlocks buf
              -- bb <- readChan x
              -- unless ( B.null bb ) (writeBody x bb)
              readChan x >>= liftM2 unless B.null (writeBody x)

