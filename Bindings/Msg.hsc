{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Msg
where

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CTime, CLong, CUInt(..), CInt(..))
import Foreign.C.String (CString, withCString)
import Foreign.C.Error (throwErrnoIf)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.ForeignPtr (castForeignPtr, withForeignPtr, mallocForeignPtrBytes)
import Foreign.Marshal.Alloc (alloca)
import Data.Time (UTCTime(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (fromForeignPtr, toForeignPtr)
import Data.Bits ( (.|.) )
import Foreign.Marshal.Utils (copyBytes)

#include <sys/msg.h>
#include <sys/ipc.h>

foreign import ccall unsafe "sys/msg.h msgget" c_msgget :: CUInt -> CInt -> IO CInt
foreign import ccall unsafe "sys/msg.h msgsnd" c_msgsnd :: CUInt -> Ptr CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "sys/msg.h msgrcv" c_msgrcv :: CUInt -> Ptr CInt -> CInt -> CInt -> CUInt -> IO CInt
foreign import ccall unsafe "sys/msg.h msgctl" c_msgctl :: CUInt -> CUInt -> Ptr MsqidDs -> IO CInt
foreign import ccall unsafe "sys/ipc.h ftok" c_ftok :: CString -> CInt -> IO CInt

data Msgq = Msgq Int deriving (Eq, Show)

msgget :: String -> IO Msgq
msgget ks = do
  k <- throwErrnoIf (== -1) "ftok" $ withCString ks $ \sn -> do c_ftok sn (toEnum 33)
  kr <- throwErrnoIf (== -1) "msgget" $ fmap fromIntegral (c_msgget (fromIntegral k) ((#const IPC_CREAT) .|. 0x1B0))
  return (Msgq kr)

msgsnd :: Msgq -> Int -> ByteString -> IO ()
msgsnd (Msgq q) typ bs = do
  let (fp, off, len) = toForeignPtr bs
      zxl = 8
  fpx <- mallocForeignPtrBytes (zxl+len)
  _k <- throwErrnoIf (== -1) "msgsnd" $ withForeignPtr fpx $ \sn -> withForeignPtr fp $ \sn2 -> do 
      copyBytes (plusPtr sn zxl) (plusPtr sn2 off) len
      poke sn (toEnum typ :: CInt)
      c_msgsnd (toEnum q) sn (toEnum (zxl+len)) (#const IPC_NOWAIT)
  return ()
  
msgrcv :: Msgq -> Int -> IO ByteString
msgrcv (Msgq q) typ = do
  let siz = 800
  fpx <- mallocForeignPtrBytes siz -- this is the max size
  k <- throwErrnoIf (== -1) "msgrcv" $ withForeignPtr fpx $ \sn -> c_msgrcv (toEnum q) sn (toEnum siz) (toEnum typ) ((#const IPC_NOWAIT))
  return (fromForeignPtr (castForeignPtr fpx) 0 (fromEnum k))

data IpcPerm = IpcPerm {
  ipcPerm_uid :: Int,
  ipcPerm_gid :: Int,
  ipcPerm_cuid :: Int,
  ipcPerm_cgid :: Int,
  ipcPerm_mode :: Int
} deriving Show
instance Storable IpcPerm where
  sizeOf _ = (#size struct ipc_perm)
  alignment _ = 4
  poke _e _ev = undefined
  peek e = IpcPerm <$> fmap fromIntegral ((#peek struct ipc_perm, uid) e :: IO CUInt)
                   <*> fmap fromIntegral ((#peek struct ipc_perm, gid) e :: IO CUInt)
                   <*> fmap fromIntegral ((#peek struct ipc_perm, cuid) e :: IO CUInt)
                   <*> fmap fromIntegral ((#peek struct ipc_perm, cgid) e :: IO CUInt)
                   <*> fmap fromIntegral ((#peek struct ipc_perm, mode) e :: IO CUInt)
                   
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

ct2t :: CTime -> UTCTime
ct2t = posixSecondsToUTCTime . realToFrac

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




  
ipcStat :: Msgq -> IO MsqidDs
ipcStat (Msgq q) = alloca $ \p -> do 
    _ <- throwErrnoIf (== -1) "msgctl" $ c_msgctl (toEnum q) (#const IPC_STAT) p
    peek p

ipcRm :: Msgq -> IO MsqidDs
ipcRm (Msgq q) = alloca $ \p -> do 
    _ <- throwErrnoIf (== -1) "msgctl" $ c_msgctl (toEnum q) (#const IPC_RMID) p
    peek p


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

