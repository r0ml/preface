{-# LANGUAGE ForeignFunctionInterface, QuasiQuotes #-}

module Bindings.Msg
where

import Preface.Imports
import Preface.FFITemplates (enumInt)
import Preface.FFITemplates2 (storable)

foreign import ccall unsafe "sys/msg.h msgget" c_msgget :: CUInt -> CInt -> IO CInt
foreign import ccall unsafe "sys/msg.h msgsnd" c_msgsnd :: CUInt -> Ptr CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "sys/msg.h msgrcv" c_msgrcv :: CUInt -> Ptr CInt -> CInt -> CInt -> CUInt -> IO CInt
foreign import ccall unsafe "sys/msg.h msgctl" c_msgctl :: CUInt -> CUInt -> Ptr MsqidDs -> IO CInt
foreign import ccall unsafe "sys/ipc.h ftok" c_ftok :: CString -> CInt -> IO CInt

data Msgq = Msgq Int deriving (Eq, Show)

[enumInt|IPC_Control
  IPC_RMID 0
  IPC_SET  1
  IPC_STAT 2
|]

[enumInt|IPC_Flags
  IPC_R      0x0100
  IPC_W      0x0080
  IPC_M      0x1000
  IPC_CREAT  0x0200
  IPC_EXCL   0x0400
  IPC_NOWAIT 0x0800
|]

msgget :: String -> IO (Either Errno Msgq)
msgget ks = do
  k <- withCString ks $ \sn -> do c_ftok sn (toEnum 33)
  if k < 0 then Left <$> getErrno 
           else do kr <- fromIntegral <$> c_msgget (fromIntegral k) (fromIntegral (fromEnum IPC_CREAT .|. 0x1B0))
                   if kr < 0 then Left <$> getErrno
                             else return . Right . Msgq $ kr

msgsnd :: Msgq -> Int -> ByteString -> IO (Either Errno ())
msgsnd (Msgq q) typ bs = do
  let (fp, off, len) = toForeignPtr bs
      zxl = 8
  fpx <- mallocForeignPtrBytes (zxl+len)
  k <- withForeignPtr fpx $ \sn -> withForeignPtr fp $ \sn2 -> do 
      copyBytes (plusPtr sn zxl) (plusPtr sn2 off) len
      poke sn (toEnum typ :: CInt)
      c_msgsnd (toEnum q) sn (toEnum (zxl+len)) (ff IPC_NOWAIT)
  if k < 0 then Left <$> getErrno else return (Right ())
  
msgrcv :: Msgq -> Int -> IO (Either Errno ByteString)
msgrcv (Msgq q) typ = do
  let siz = 800
  fpx <- mallocForeignPtrBytes siz -- this is the max size
  k <- withForeignPtr fpx $ \sn -> c_msgrcv (toEnum q) sn (toEnum siz) (toEnum typ) (ff IPC_NOWAIT)
  if k < 0 then Left <$> getErrno else return $ Right (fromForeignPtr (castForeignPtr fpx) 0 (fromIntegral k))

[storable|IpcPerm
  uid CUInt
  gid CUInt
  cuid CUInt
  cgid CUInt
  mode CUInt
|]

[storable|MsqidDs
  perm IpcPerm
  stime CTime
  rtime CTime
  ctime CTime
  cbytes CLong
  qnum CLong
  qbytes CLong
  lspid CUInt
  lrpid CUInt
|]
  

{-  
data MsqidDs = MsqidDs {
  msqidDs_perm :: IpcPerm,
  msqidDs_stime :: UTCTime,
  msqidDs_rtime :: UTCTime,
  msqidDs_ctime :: UTCTime,
  msqidDs_cbytes :: Int,
  msqidDs_qnum :: Int,
  msqidDs_qbytes :: Int,
  msqidDs_lspid :: Int,
  msqidDs_lrpid :: Int
  } deriving Show
-}

ct2t :: CTime -> UTCTime
ct2t = posixSecondsToUTCTime . realToFrac

ff :: (Enum a, Integral b) => a -> b
ff = fromIntegral . fromEnum

{-
instance Storable MsqidDs where
  sizeOf _ = (#size struct msqid_ds) 
  alignment _ = 4
  peek e = MsqidDs <$> (peek ((#ptr struct msqid_ds, msg_perm) e) :: IO IpcPerm)
                   <*> fmap ct2t ((#peek struct msqid_ds, msg_stime) e :: IO CTime)
                   <*> fmap ct2t ((#peek struct msqid_ds, msg_rtime) e :: IO CTime)
                   <*> fmap ct2t ((#peek struct msqid_ds, msg_ctime) e :: IO CTime)
                   <*> fmap fromIntegral ((#peek struct msqid_ds, msg_cbytes) e :: IO CLong)
                   <*> fmap fromIntegral ((#peek struct msqid_ds, msg_qnum) e :: IO CLong)
                   <*> fmap fromIntegral ((#peek struct msqid_ds, msg_qbytes) e :: IO CLong)
                   <*> fmap fromIntegral ((#peek struct msqid_ds, msg_lspid) e :: IO CUInt)
                   <*> fmap fromIntegral ((#peek struct msqid_ds, msg_lrpid) e :: IO CUInt)
                  
  poke _e _ev = undefined
-}

ipcStat :: Msgq -> IO (Either Errno MsqidDs)
ipcStat (Msgq q) = alloca $ \p -> do 
    e <- c_msgctl (toEnum q) (ff IPC_STAT) p
    if e < 0 then Left <$> getErrno else Right <$> peek p

ipcRm :: Msgq -> IO (Either Errno MsqidDs)
ipcRm (Msgq q) = alloca $ \p -> do 
    e <- c_msgctl (toEnum q) (ff IPC_RMID) p
    if e < 0 then Left <$> getErrno else Right <$> peek p


{- this is linux only
data IpcInfo = IpcInfo {
  ipcInfo_msgpool :: Int,
  ipcInfo_msgmap :: Int,
  ipcInfo_msgmax :: Int,
  ipcInfo_msgmnb :: Int,
  ipcInfo_msgmni :: Int,
  ipcInfo_msgssz :: Int,
  ipcInfo_msgtql :: Int,
  ipcInfo_msgseg :: Int
} deriving Show
instance Storable IpcInfo where
  sizeOf _ = (#size struct msginfo)
  alignment _ = 4
  poke e ev = undefined
  peek e = IpcInfo <$> fmap fromIntegral ((#peek struct msginfo, msgpool) e :: IO CInt)
                   <*> fmap fromIntegral ((#peek struct msginfo, msgmap) e :: IO CInt)
                   <*> fmap fromIntegral ((#peek struct msginfo, msgmax) e :: IO CInt)
                   <*> fmap fromIntegral ((#peek struct msginfo, msgmnb) e :: IO CInt)
                   <*> fmap fromIntegral ((#peek struct msginfo, msgmni) e :: IO CInt)
                   <*> fmap fromIntegral ((#peek struct msginfo, msgssz) e :: IO CInt)
                   <*> fmap fromIntegral ((#peek struct msginfo, msgtql) e :: IO CInt)
                   <*> fmap fromIntegral ((#peek struct msginfo, msgseg) e :: IO CShort)
-}

