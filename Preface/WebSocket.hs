{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}

module Preface.WebSocket where

import Preface.Imports
import Preface.Stringy
import Preface.Binary
import Preface.Misc
import Preface.SecureHash
import Bindings.Zlib -- (ZData, feed, flush, initInflate)
import Preface.Str

encodeFrame :: ByteString -> FrameType -> ByteString -> ByteString
encodeFrame fmask ft f = strCat [pack [byte0, byte1], len , fmask, 
                                 if strNull fmask then f else maskPayload fmask f]
  where
    byte0  = fromIntegral $ fin .|. rsv1 .|. rsv2 .|. rsv3 .|. opcode
    fin    = 0x80 -- if frameFin f  then 0x80 else 0x00
    rsv1   = 0x00 -- if frameRsv1 f then 0x40 else 0x00
    rsv2   = 0x00 -- if frameRsv2 f then 0x20 else 0x00
    rsv3   = 0x00 -- if frameRsv3 f then 0x10 else 0x00
    opcode = fromEnum ft 
    maskflag = if strNull fmask then 0x00 else 0x80 :: Int
    len'  = strLen f 
    (lenflag, len)
        | len' < 126     = ( len', zilde)
        | len' < 0x10000 = (126, putWord16be $ fromIntegral len')
        | otherwise      = (127, putWord64be $ fromIntegral len')
    byte1 = fromIntegral (maskflag .|. lenflag)

[enumIr|FrameType
  ContinuationFrame 0 TextFrame 1 BinaryFrame 2
  CloseFrame 8 PingFrame 9 PongFrame 10
  |]

type Mask = ByteString

data WSFrame = Frame Bool (Bool,Bool,Bool) FrameType ByteString deriving (Show)

maskPayload :: ByteString -> ByteString -> ByteString
maskPayload fmask = snd . (strMapAccumL f 0)
  where f i c = ( (i+1) `mod` strLen fmask,  c `xor` nth fmask i)


--------------------------------------------------------------------------------
-- | Parse a frame from the input.  The result is the parsed frame and the 
-- remainder of the input -- which is the content (or beginning) of the next frame.
-- The frame is a Maybe, because it is possible that the input does not contain a
-- complete frame, in which case the result is Nothing and the original input.
decodeFrame :: ByteString -> (Maybe WSFrame, ByteString)
decodeFrame z =
    let bl = strLen z
        byte0 = nth z 0 
        byte1 = nth z 1
        fin    = byte0 .&. 0x80 == 0x80
        rsv1   = byte0 .&. 0x40 == 0x40
        rsv2   = byte0 .&. 0x20 == 0x20
        rsv3   = byte0 .&. 0x10 == 0x10
        opcode = byte0 .&. 0x0f
        ft = (toEnum . fromIntegral) opcode :: FrameType
        fmask = byte1 .&. 0x80 == 0x80
        lenflag = fromIntegral (byte1 .&. 0x7f) :: Int
        bofx = traceShow ( ("fin", fin, "rsv1", rsv1, "rsv2", rsv2, "rsv3", rsv3, "opcode", opcode, "ft", ft) , "mask", fmask,  "lenflag", lenflag,  "z", z) $ strDrop 2 z
        clem = strConcat [bofx, pack [0,0,0xff,0xff]]
        bof = clem
        (mdl, len) = case lenflag of
          126 -> if bl < 4 then (2,-1) else (2, fromIntegral $ getWord16be 0 bof)
          127 -> if bl < 10 then (8, -1) else (8, fromIntegral $ getWord64be 0 bof)
          _   -> (0, lenflag)
        mdlm = if fmask then mdl+4 else mdl
        masker = if fmask then maskPayload (strTake 4 (strDrop mdl bof)) else id
        mskr = masker chunk
        pl = strDrop mdlm bof
        plx = strDrop len pl
        chunk = strTake len pl -- in theory this could be a chunk greater than 2^31 in length
        (bleh, leftover) = if rsv1
                              then (strConcat [mskr, strTake 4 plx], strDrop 4 plx)
                              else (mskr, plx)
    in  if bl < 2 || len < 0 || bl < mdlm + fromIntegral len then (Nothing, z)
        else (Just $ Frame fin (rsv1, rsv2, rsv3) ft bleh, strDrop 4 leftover)

wsConnectTo :: String -> Int -> String -> (WebSocket -> IO ()) -> IO () -- host port path headers 
wsConnectTo host port path app = do
    -- Create and connect socket
    let hints = defaultHints {addrFamily = AF_INET, addrSocketType = Stream}
    addrInfos <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    sock      <- socket AF_INET Stream defaultProtocol

    -- Connect WebSocket and run client
    finally (sktConnect sock (addrAddress $ head addrInfos) >>
             doClient sock (host++":"++show port) path app)
            (sClose sock)


getEntropy :: Int -> IO ByteString
getEntropy n = do
  a <- newStdGen
  let b = take n (randomRs (0,255) a)
  return (pack b)

doClient :: Socket -> String -> String -> (WebSocket -> IO ()) -> IO ()
doClient skt host path app = do
    -- Create the request and send it
    key <- base64 `liftM` getEntropy 16
    let headers = [("Host", host)
         , ("Connection", "Upgrade")
         , ("Upgrade", "websocket")
         , ("Sec-WebSocket-Key", asString key)
         , ("Sec-WebSocket-Version", "13")
         ]
        request = concat $ concat [
                    ["GET ", path , " HTTP/1.1\r\n"],
                    concatMap (\(k,v) -> [k,": ",v,"\r\n"] ) headers,
                    ["\r\n"]]
    ws <- getWS skt (asByteString request)
    app ws
              
type HTTPHeaders = [(String,String)]
type PostBody = ByteString

data Message = Close | Text String | Binary ByteString deriving (Show)

-- ---------------------------
-- Copied from SCGI (could be collapsed)
data WebSocket = WebSocket { underlyingSocket :: Socket,
                             wsSend :: Chan Message
                           , wsRecv :: Chan WSFrame
             --              , wsState :: IORef (Maybe a)

                           }

instance Eq (WebSocket) where
  (==) a b = fdSocket (underlyingSocket a) == fdSocket (underlyingSocket b)

instance Ord (WebSocket) where
  (<=) a b = fdSocket (underlyingSocket a) <= fdSocket (underlyingSocket b)

data WebSocketMessage = WebSocketTextMessage String | WebSocketBinaryMessage ByteString

instance Show WebSocketMessage where
  show (WebSocketTextMessage a) = a
  show (WebSocketBinaryMessage a) = show a

readHttpResponse :: Socket -> ByteString -> IO (HTTPHeaders, ByteString)
readHttpResponse sock lft = do
  more <- sRecv sock
  traceIO ("readHttpResponse: " ++ show more)
  if strNull more then return ([], lft) else do
    let utn = strCat [lft, more]
        (prev,post) = strBreakSubstring (asByteString "\r\n\r\n") utn
    if strNull post then readHttpResponse sock utn
    else do let lns = breakSubstrings (asByteString "\r\n") prev
                hds = ("Status",asString (head lns) ) : map split (tail lns)
            return (hds, strDrop 4 post)
  where breakSubstrings b s = let (f,l) = strBreakSubstring b s
                               in if strNull l then [f] else f : breakSubstrings b (strDrop (strLen b) l)
        split :: ByteString -> (String, String)
        split strx = let (key, val) = strBrk (== ':') (asString strx)
                         (_, valx) = strBrk (/= ' ') (strDrop (1::Int) val)
                      in ( key, valx)

        sRecv sockx = catch (sktRecv sockx 4096 :: IO ByteString) 
                            (\e -> putStrLn ("readHttpResponse ==> " ++ show (e :: SomeException)) >> sClose sockx >> putStrLn "closed sock" >> return zilde)


getWS :: Socket -> ByteString -> IO (WebSocket)
getWS sock hdx = do
    let envx = []
    chan <- newChan 
    inf <- inflater
    _ <- forkIO $ do
        h <- readHttpResponse sock zilde
        traceIO (show h)
        readMsgs sock chan zilde inf -- readHdrs False sock envx chan zilde
    wchan <- newChan :: IO (Chan Message)
    sendAll sock hdx
    _ <- forkIO $ forever $ do
          wrd <-fromIntegral <$> fst <$> next <$> newStdGen 
          writeMsg (putWord32be wrd) wchan sock
    return $ WebSocket sock wchan chan -- envx

mkCloseFrame :: WSFrame
mkCloseFrame = Frame False (False, False, False) CloseFrame zilde

writeMsg :: ByteString -> Chan Message -> Socket -> IO ()
writeMsg msk x k = do
  buf <- readChan x
  case buf of
    Close -> sClose k
    Binary bs -> let ef = encodeFrame msk BinaryFrame bs in sendAll k ef
    Text strx -> let ef = encodeFrame msk TextFrame (asByteString strx)
                  in traceShow ("sendAll", ef) sendAll k ef

readMsgs :: Socket -> Chan WSFrame -> ByteString -> Zlib -> IO ()
readMsgs sock chx lftx inf = do
            let (frm, lfto) = decodeFrame lftx
            traceIO ("readMsgs: " ++ (show frm))
            case frm of
              Nothing -> return ()
              Just ff@(Frame fin (rsv1, rsv2, rsv3) ft bleh) -> do
                ffrrmm <- if rsv1 then do
                  zPut inf bleh
                  -- assuming that popper is Nothing here
                  -- but that is not a good assumption
                  res <- zGet inf
                  case res of 
                     Left e -> return (Frame fin (rsv1, rsv2, rsv3) ft (asByteString ("error "++show e) ) )
                     Right rs -> return (Frame fin (rsv1, rsv2, rsv3) ft rs )
                else 
                  return ff
                writeChan chx ffrrmm
            more <- sRecv sock
            traceIO ("readMsgs "++show more)
            if strNull more then writeChan chx mkCloseFrame >> return () else readMsgs sock chx (strCat [lfto, more]) inf
  where
    sRecv sockx = catch (sktRecv sockx 4096 :: IO ByteString) 
                        (\e -> putStrLn ("readMsgs ==> " ++ show (e :: SomeException)) >> sClose sockx >> putStrLn "closed sock" >> return zilde)

doHandshake :: Socket -> HTTPHeaders -> IO ()
doHandshake k hds = do
  traceIO "handshake"
  let wsk = case lookup "Sec-WebSocket-Key" hds of {Nothing -> zilde; Just b -> asByteString b}
      hash = hashKey wsk
      encoded = base64 hash
  let rsp = "HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n" ::String
      rsp2 = "Sec-WebSocket-Accept: "++ encoded ++"\r\n"
      -- pcol = "postgresql"
      -- rsp9 = "Sec-WebSocket-Protocol: "++ pcol++"\r\n"::String
      rsp9 = ""
      rsp4 = "Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits=15\r\n"::String
      rsp3 = "Server: hs-www/WSServer\r\n\r\n"::String
  -- _ <- sendAll k (asByteString ("HTTP/1.1 400 Bad Request\r\n\r\n"::String))
  -- sClose k
  _ <- sendAll k (asByteString (rsp ++ rsp2 ++ rsp4 ++ rsp3 ++ rsp9))
  return ()
  where hashKey = sha 1 . (\x -> strCat [x,asByteString "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"])
    
readHdrs :: Bool -> Socket -> HTTPHeaders -> Chan WSFrame -> ByteString -> IO (HTTPHeaders, ByteString)
readHdrs dohandshake sock envx ch lft = do
  more <- sRecv sock
  traceIO ("readHdrs: " ++ show more)
  if strNull more then writeChan ch mkCloseFrame >> return ([], zilde) else do 
    let utn = strCat [lft, more]
        (prev,post) = strBreakSubstring (asByteString "\r\n\r\n") utn
    if strNull post then readHdrs dohandshake sock envx ch utn
    else do let lns = breakSubstrings (asByteString "\r\n") prev
                hds = map split (tail lns)
            return (hds, strDrop 4 post)
  where breakSubstrings b s = let (f,l) = strBreakSubstring b s
                               in if strNull l then [f] else f : breakSubstrings b (strDrop (strLen b) l)
        split :: ByteString -> (String, String)
        split strx = let (key, val) = strBrk (== ':') (asString strx)
                         (_, valx) = strBrk (/= ' ') (strDrop (1::Int) val)
                      in ( key, valx)

        sRecv sockx = catch (sktRecv sockx 4096 :: IO ByteString) 
                            (\e -> putStrLn ("readHdrs ==> " ++ show (e :: SomeException)) >> sClose sockx >> putStrLn "closed sock" >> return zilde)

sendAll :: Socket -> ByteString -> IO ()
sendAll s bs = do 
  sent <- sktSend s bs
  let res = strDrop sent bs in if strNull res then return () else sendAll s res 

sendMessage :: WebSocket -> Message -> IO ()
sendMessage rsp = writeChan (wsSend rsp)

-- | runServer creates a thread which listens on a port.
--  That thread will start a new thread for every connection.
--  Each connection will run an initialization function, a "processing" function, and a teardown function on the socket
--    (so it has the same arguments as a bracket).
runServer :: (Integral b) => b -> (HTTPHeaders -> WebSocket -> IO ()) -> (WebSocket -> IO() ) -> (WebSocketMessage -> WebSocket -> IO () ) -> IO (IORef [WebSocket])
runServer port g h runRecv = do
  y <- newIORef [] -- keep track of all websockets
  _ <- forkIO $ withSocketsDo $ bracket acquire release (forever . process y )
  return y
  where 
    acquire = listenOn (PortNumber (fromIntegral port))
    release = sClose
    process y skt = do
      (sock, _) <- sktAccept skt 
      _ <- forkIO $ bracket (sinit y sock) (srel y) (forever . srun)
      return ()
    sinit y sock = do
      chan <- newChan
      wchan <- newChan :: IO (Chan Message)
      nio <- newIORef Nothing
      let a = WebSocket sock wchan chan -- nio
      atomicModifyIORef' y (\x -> (a : x, ())) -- keep track of the websockets
      _ <- forkIO $ do
        (hdrs, bs) <- readHdrs True sock [] chan zilde
        doHandshake sock hdrs
        g hdrs a
        inf <- inflater
        readMsgs sock chan bs inf
      _ <- forkIO $ forever $ writeMsg zilde wchan sock
      return a
    srun chanWs = do
      fr <- readChan (wsRecv chanWs)
      let Frame _ _ bt bs = fr 
       in case bt of 
          CloseFrame -> sendMessage chanWs Close
          TextFrame -> runRecv (WebSocketTextMessage (asString bs)) chanWs
          BinaryFrame -> runRecv (WebSocketBinaryMessage bs) chanWs
          _ -> error "other kind of frame"
    srel y a = do
      atomicModifyIORef' y (\x -> (filter (/= a) x, () ) )
      h a
