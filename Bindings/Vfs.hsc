{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

#include "sys/statvfs.h"
#include "sys/mount.h"

module Bindings.Vfs (
{- | Returns the result of a call to statvfs as described in "sys/statvfs.h".
     This result is a data structure consisting of integers representing:
          filesystem block size,
          filesystem fragment size,
          number of fragments in the file system,
          number of free fragments,
          number of free fragments for unprivileged users,
          number of inodes,
          number of free inodes,
          number of free inodes for unprivileged users,
          the filesystem ID,
          mount flags (rdonly and nosuid),
          maximum filename length
  -}
  statVFS
  , getStatFS
  , StatVFS(..)
  , StatFS(..)
) where

import Foreign.Storable (Storable, peek, pokeByteOff, peekByteOff, sizeOf, alignment, poke)
import Foreign.C.Types (CULong, CUInt, CInt(..), CLong, CShort)
import Foreign.C.Error (throwErrnoIfMinus1_, throwErrnoIfMinus1)
import Foreign.C.String (CString, withCString, peekCString)
import Data.Bits (testBit, finiteBitSize, bit)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Marshal (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Data.Maybe (mapMaybe)
import Preface.Str (storable)

type HTYPE_FSBLKCNT_T = CUInt
type HTYPE_FSFILCNT_T = CUInt

foreign import ccall unsafe "sys/statvfs.h statvfs" c_statvfs :: CString -> Ptr StatVFS -> IO CInt


[storable|StatVFS
  bsize ulong
  frsize ulong
  blocks uint
  bfree uint
  bavail uint
  files uint
  ffree uint
  favail uint
  fsid ulong
  flag ulong
  namemax ulong
  |]

{-
data StatVFS = StatVFS { 
  statVFS_bsize :: Int,
  statVFS_frsize :: Int,
  statVFS_blocks :: Int,
  statVFS_bfree :: Int,
  statVFS_bavail :: Int,
  statVFS_files :: Int, 
  statVFS_ffree :: Int, 
  statVFS_favail :: Int,
  statVFS_fsid :: Int, 
  statVFS_flag :: Int, 
  statVFS_namemax :: Int
  } deriving Show


instance Storable StatVFS where
  sizeOf _ = (#const sizeof(struct statvfs)) 
  alignment _ = 4
  peek e = StatVFS <$> fmap fromIntegral ((#peek struct statvfs,f_bsize) e :: IO CULong)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_frsize) e :: IO CULong)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_blocks) e :: IO HTYPE_FSBLKCNT_T)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_bfree) e :: IO HTYPE_FSBLKCNT_T)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_bavail) e :: IO HTYPE_FSBLKCNT_T)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_files) e :: IO HTYPE_FSFILCNT_T)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_ffree) e :: IO HTYPE_FSFILCNT_T)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_favail) e :: IO HTYPE_FSFILCNT_T)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_fsid) e :: IO CULong)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_flag) e :: IO CULong)
                   <*> fmap fromIntegral ((#peek struct statvfs,f_namemax) e :: IO CULong)
  poke e ev =
    do (#poke struct statvfs,f_bsize) e (fromIntegral (statVFS_bsize ev) :: CULong)
       (#poke struct statvfs,f_frsize) e (fromIntegral (statVFS_frsize ev) :: CULong)
       (#poke struct statvfs,f_blocks) e (fromIntegral (statVFS_blocks ev) :: HTYPE_FSBLKCNT_T)
       (#poke struct statvfs,f_bfree) e (fromIntegral (statVFS_bfree ev) :: HTYPE_FSBLKCNT_T)
       (#poke struct statvfs,f_bavail) e (fromIntegral (statVFS_bavail ev) :: HTYPE_FSBLKCNT_T)
       (#poke struct statvfs,f_files) e (fromIntegral (statVFS_files ev) :: HTYPE_FSFILCNT_T)
       (#poke struct statvfs,f_ffree) e (fromIntegral (statVFS_ffree ev) :: HTYPE_FSFILCNT_T)
       (#poke struct statvfs,f_favail) e (fromIntegral (statVFS_favail ev) :: HTYPE_FSFILCNT_T)
       (#poke struct statvfs,f_fsid) e (fromIntegral (statVFS_fsid ev) :: CULong)
       (#poke struct statvfs,f_flag) e (fromIntegral (statVFS_flag ev) :: CULong)
       (#poke struct statvfs,f_namemax) e (fromIntegral (statVFS_namemax ev) :: CULong)
-}

statVFS :: FilePath -> IO StatVFS
statVFS path = 
  withCString path $ \c_path ->
  alloca $ \p -> (throwErrnoIfMinus1_ "statVFS" $ c_statvfs c_path p) >> peek p
 
-- --------------------------------------------------------------------------------------------------
     
foreign import ccall unsafe "sys/mount.h getfsstat" c_getfsstat :: Ptr StatFS -> CInt -> CInt -> IO CInt

data StatFS = StatFS {
  statFS_bsize :: Int,
  statFS_iosize :: Int,
  statFS_blocks :: Int,
  statFS_bfree :: Int,
  statFS_bavail :: Int,
  statFS_files :: Int,
  statFS_ffree :: Int,
  statFS_fsid :: [Int], -- two element int32 struct
  statFS_owner :: Int,
  statFS_type :: Int,
  statFS_flags :: [MountFlag],
  statFS_fstypename :: String,
  statFS_mntonname :: String,
  statFS_mntfromname :: String
  } deriving Show

instance Storable StatFS where
  sizeOf _ = (#size struct statfs) 
  alignment _ = 4
  peek e = StatFS <$> fmap fromIntegral ((#peek struct statfs,f_bsize) e :: IO CInt)
                  <*> fmap fromIntegral ((#peek struct statfs,f_iosize) e :: IO CUInt)
                  <*> fmap fromIntegral ((#peek struct statfs,f_blocks) e :: IO CLong)
                  <*> fmap fromIntegral ((#peek struct statfs,f_bfree) e :: IO CLong)
                  <*> fmap fromIntegral ((#peek struct statfs,f_bavail) e :: IO CLong)
                  <*> fmap fromIntegral ((#peek struct statfs,f_files) e :: IO CLong)
                  <*> fmap fromIntegral ((#peek struct statfs,f_ffree) e :: IO CLong)
                  <*> mapM id [ fmap fromIntegral ((#peek struct statfs,f_fsid) e :: IO CUInt), fmap fromIntegral (peekByteOff e (4+(#offset struct statfs,f_fsid)) :: IO CUInt)]
                  <*> fmap fromIntegral ((#peek struct statfs,f_owner) e :: IO CUInt)
                  <*> fmap fromIntegral ((#peek struct statfs,f_type) e :: IO CShort)
                  <*> fmap bitmaskToEnum ((#peek struct statfs,f_flags) e :: IO CUInt)
                  <*> (peekCString ((#ptr struct statfs,f_fstypename) e :: CString))
                  <*> (peekCString ((#ptr struct statfs,f_mntonname) e :: CString))
                  <*> (peekCString ((#ptr struct statfs,f_mntfromname) e :: CString))
                  
  poke _e _ev = undefined

data MountFlag = MountRdOnly | MountSynchronous | MountNoExec | MountNoSuid | MountNoDev
  | MountUnion | MountAsync | MountCProtect | MountExported | MountQuarantine | MountLocal | MountQuota
  | MountRootFS | MountDoVolFS | MountDontBrowse | MountIgnoreOwnership
  | MountAutomounted | MountJournaled | MountNoUserXAttr | MountDefWrite | MountMultiLabel 
  | MountNoATime | MountUpdate | MountNoBlock | MountReload | MountForce 
  | MountUnknownFlag
  deriving (Eq, Show)
  
instance Enum MountFlag where
  fromEnum x = case x of 
    MountRdOnly -> (#const MNT_RDONLY)
    MountSynchronous -> (#const MNT_SYNCHRONOUS)
    MountNoExec -> (#const MNT_NOEXEC)
    MountNoSuid -> (#const MNT_NOSUID)
    MountNoDev -> (#const MNT_NODEV)
    MountUnion -> (#const MNT_UNION)
    MountAsync -> (#const MNT_ASYNC)
    MountCProtect -> (#const MNT_CPROTECT)
    MountExported -> (#const MNT_EXPORTED)
    MountQuarantine -> (#const MNT_QUARANTINE)
    MountLocal -> (#const MNT_LOCAL)
    MountQuota -> (#const MNT_QUOTA)
    MountRootFS -> (#const MNT_ROOTFS)
    MountDoVolFS -> (#const MNT_DOVOLFS)
    MountDontBrowse -> (#const MNT_DONTBROWSE)
    MountIgnoreOwnership -> (#const MNT_IGNORE_OWNERSHIP)
    MountAutomounted -> (#const MNT_AUTOMOUNTED)
    MountJournaled -> (#const MNT_JOURNALED)
    MountNoUserXAttr -> (#const MNT_NOUSERXATTR)
    MountDefWrite -> (#const MNT_DEFWRITE)
    MountMultiLabel -> (#const MNT_MULTILABEL)
    MountNoATime -> (#const MNT_NOATIME)
    MountUpdate -> (#const MNT_UPDATE)
    MountNoBlock -> (#const MNT_NOBLOCK)
    MountReload -> (#const MNT_RELOAD)
    MountForce -> (#const MNT_FORCE)
    MountUnknownFlag -> undefined
    
  toEnum x = case x of 
    (#const MNT_RDONLY) -> MountRdOnly
    (#const MNT_SYNCHRONOUS) -> MountSynchronous
    (#const MNT_NOEXEC) -> MountNoExec
    (#const MNT_NOSUID) -> MountNoSuid
    (#const MNT_NODEV) -> MountNoDev
    (#const MNT_UNION) -> MountUnion
    (#const MNT_ASYNC) -> MountAsync
    (#const MNT_CPROTECT) -> MountCProtect
    (#const MNT_EXPORTED) -> MountExported
    (#const MNT_QUARANTINE) -> MountQuarantine
    (#const MNT_LOCAL) -> MountLocal
    (#const MNT_QUOTA) -> MountQuota
    (#const MNT_ROOTFS) -> MountRootFS
    (#const MNT_DOVOLFS) -> MountDoVolFS
    (#const MNT_DONTBROWSE) -> MountDontBrowse
    (#const MNT_IGNORE_OWNERSHIP) -> MountIgnoreOwnership
    (#const MNT_AUTOMOUNTED) -> MountAutomounted
    (#const MNT_JOURNALED) -> MountJournaled
    (#const MNT_NOUSERXATTR) -> MountNoUserXAttr
    (#const MNT_DEFWRITE) -> MountDefWrite
    (#const MNT_MULTILABEL) -> MountMultiLabel
    (#const MNT_NOATIME) -> MountNoATime
    (#const MNT_UPDATE) -> MountUpdate
    (#const MNT_NOBLOCK) -> MountNoBlock
    (#const MNT_RELOAD) -> MountReload
    (#const MNT_FORCE) -> MountForce
    _ -> MountUnknownFlag

-- | Convert a list of enumeration values to an integer by combining
-- them with bitwise 'or'.
-- enumToBitmask :: Enum a => [a] -> Int
-- enumToBitmask = foldl' (.|.) 0 . map fromEnum

-- | Convert an integer to a list of enumeration values by testing
-- each bit, and if set, convert it to an enumeration member.
bitmaskToEnum :: (Enum a, Integral b) => b -> [a]
bitmaskToEnum bmx = mapMaybe maybeBit [0 .. finiteBitSize bm - 1]
  where
    bm = fromIntegral bmx :: Int
    maybeBit b | testBit bm b = Just . toEnum . bit $ b
               | otherwise    = Nothing

getStatFS :: IO [StatFS]
getStatFS = do
  a <- fmap fromIntegral (c_getfsstat nullPtr 0 0)  :: IO Int
  allocaArray (a+1) $ \p -> do
    let bufsiz = (a+1) * (#size struct statfs)
    b <- fmap fromIntegral (throwErrnoIfMinus1 "getfsstat" $ c_getfsstat p (fromIntegral bufsiz :: CInt) (#const MNT_NOWAIT)) :: IO Int
    (peekArray b p :: IO [StatFS])
  
