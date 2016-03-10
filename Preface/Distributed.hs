{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module Preface.Distributed 

where

import Preface.Imports 
import qualified Data.Binary as B
import Preface.Stringy
import Preface.Misc
import Preface.Binary
import qualified Data.ByteString.Lazy as BL
import GHC.Conc

class (Binary a, Typeable a) => Serializable a
instance (Binary a, Typeable a) => Serializable a

encode :: Serializable a => a -> ByteString
encode = BL.toStrict . B.encode 

decode :: Serializable a => ByteString -> Either String a
decode s = let j = (B.decodeOrFail . BL.fromStrict) s
            in case j of
                  Left (_a,_b,c) -> Left c
                  Right (a,_b,c) -> if 0 == BL.length a then Right c else Left "extra bytes available"

sendTask :: Serializable a => Chan a -> Socket -> IO ()
sendTask c s = do
    q <- readChan c
    let b = encode q
        n = strLen b
        nx = putWord32be (fromIntegral n)
     in sendData s (strConcat [nx, b])
  where sendData j x = do
             n <- sktSend j x
             if n <= 0 then error "failed to send"
             else let rm = strDrop n x in unless (strNull rm) (sendData j rm)

recvTask :: Serializable a => Chan a -> Socket -> IO ()
recvTask c s = do
     nl <- recvData s 4
     let n = fromIntegral (getWord32be 0 nl)
     m <- recvData s n
     let dm = decode m
     case dm of 
       Left d -> do { traceIO d >> error d }
       Right d -> writeChan c d
  where recvData j n = do
          msg <- sktRecv s n
          let lm = strLen msg
          if lm == n then return msg
          else if lm == 0 then error "recvData read nothing"
          else do 
             m2 <- recvData j (n - lm)
             return $ strCat [msg, m2]

data RpcConnection a b = RpcConnection (ThreadId, Chan a) (ThreadId, Chan b)

rpcTo :: (Serializable a, Serializable b) => String -> Int -> IO (RpcConnection a b)
rpcTo host port = do
   qChan <- newChan
   rChan <- newChan 

   sock <- socket AF_INET Stream defaultProtocol
   let hints = defaultHints {addrFamily = AF_INET, addrSocketType = Stream}
   addrInfos <- getAddrInfo (Just hints) (Just (asString host)) (Just $ show port)
   _connq <- catch (sktConnect sock (addrAddress $ head addrInfos) >> return True )
                   (\j -> traceIO (show (j::SomeException)) >> return False)

   p1 <- forkFinally (forever $ sendTask qChan sock) (logerr "rpcTo send" sock)
   p2 <- forkFinally (forever $ recvTask rChan sock) (\x -> logerr "rpcTo recv" sock x >> killThread p1) -- when the reader dies, kill the sender
   return $ RpcConnection (p1, qChan) (p2, rChan)

rpcOn :: (Serializable a, Serializable b) => Int -> (RpcConnection a b -> IO ()) -> IO ()
rpcOn port f = do
   qChan <- newChan
   rChan <- newChan

   sock <- socket AF_INET Stream defaultProtocol
   setSocketOption sock ReuseAddr 1
   bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
   sktListen sock 5
   _ <- forkIO $ forever $ do
      (conn, _addr) <- sktAccept sock
      p2 <- forkFinally (forever $ sendTask qChan conn) (logerr "rpcOn send" conn)
      p1 <- forkFinally (forever $ recvTask rChan conn) (\x -> logerr "rpcOn recv" conn x >> killThread p2) -- when the reader dies, kill the sender
      let rc = RpcConnection (p2, qChan) (p1, rChan) 
      _ <- forkFinally (f rc) (\_x -> do
         case _x of 
            Left e -> traceIO ("rpcOn listening: "++show e)
            Right _e -> error "distributed: what now ? "
         killThread p1 >> killThread p2 >> sClose conn )
      return ()
   return ()

logerr :: String -> Socket -> (Either SomeException a) -> IO ()
logerr s c x = do
  case x of
      Left e -> traceIO (s ++ ": " ++ show e)
      Right _e -> error "logerr right?"
  sClose c

alive :: ThreadId -> IO Bool
alive t = do
  ts <- threadStatus t
  case ts of
    ThreadRunning -> return True
    ThreadFinished -> return False
    ThreadDied -> return False
    ThreadBlocked _ -> return True

rpcSend :: (Serializable a, Serializable b) => RpcConnection a b -> a -> IO ()
rpcSend (RpcConnection (t1,a) (_,_)) x = do
  f1 <- alive t1
  if f1 then writeChan a x else error "sending thread dead"

rpcRecv :: (Serializable a, Serializable b) => RpcConnection a b -> IO b
rpcRecv (RpcConnection _ (t2,a)) = do
  f2 <- alive t2
  if f2 then readChan a else error "reading thread dead"

