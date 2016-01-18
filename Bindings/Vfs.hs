{-# LANGUAGE ForeignFunctionInterface, QuasiQuotes #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

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
  , MountFlag(..)
) where

import Preface.FFITemplates (enum, storable)
import Preface.Imports

foreign import ccall unsafe "sys/statvfs.h statvfs" c_statvfs :: CString -> Ptr StatVFS -> IO CInt
foreign import ccall unsafe "sys/mount.h getfsstat" c_getfsstat :: Ptr StatFS -> CInt -> CInt -> IO CInt

-- --------------------------------------------------------------------------------------------------

[storable|StatVFS
  bsize CULong
  frsize CULong
  blocks CUInt
  bfree CUInt
  bavail CUInt
  files CUInt
  ffree CUInt
  favail CUInt
  fsid CULong
  flag CULong
  namemax CULong
  |]

statVFS :: FilePath -> IO (Either Errno StatVFS)
statVFS path = 
  withCString path $ \c_path ->
  alloca $ \p -> do
      res <- c_statvfs c_path p
      if res < 0 then Left <$> getErrno else Right <$> peek p
 
-- --------------------------------------------------------------------------------------------------

[storable|StatFS
  f_otype CUShort
  f_oflags CUShort
  f_pad CUInt
  f_bsize CULong
  f_iosize CULong
  f_blocks CULong
  f_bfree CULong
  f_bavail CULong
  f_files CULong
  f_ffree CULong
  f_fsid CUInt/2
  f_owner CUInt
  f_reserved1 CUShort
  f_type CUShort
  f_flags CULong
  f_reserved2 CULong/2
  f_fstypename CChar/15
  f_mntonname CChar/90
  f_mntfromname CChar/90
  f_reserved3 CChar
  f_reserved4 CULong/4
  f_reserved5 CUInt
|]

{-
                  <*> mapM id [ fmap fromIntegral ((#peek struct statfs,f_fsid) e :: IO CUInt), fmap fromIntegral (peekByteOff e (4+(#offset struct statfs,f_fsid)) :: IO CUInt)]
                  <*> fmap bitmaskToEnum ((#peek struct statfs,f_flags) e :: IO CUInt)
                  <*> (peekCString ((#ptr struct statfs,f_fstypename) e :: CString))
                  <*> (peekCString ((#ptr struct statfs,f_mntonname) e :: CString))
                  <*> (peekCString ((#ptr struct statfs,f_mntfromname) e :: CString))
 -}                 

[enum|MountFlag
MNT_RDONLY      0x00000001      /* read only filesystem */
MNT_SYNCHRONOUS 0x00000002      /* file system written synchronously */
MNT_NOEXEC      0x00000004      /* can't exec from filesystem */
MNT_NOSUID      0x00000008      /* don't honor setuid bits on fs */
MNT_NODEV       0x00000010      /* don't interpret special files */
MNT_UNION       0x00000020      /* union with underlying filesystem */
MNT_ASYNC       0x00000040      /* file system written asynchronously */
MNT_CPROTECT    0x00000080      /* file system supports content protection */
MNT_EXPORTED    0x00000100      /* file system is exported */
MNT_QUARANTINE  0x00000400      /* file system is quarantined */
MNT_LOCAL       0x00001000      /* filesystem is stored locally */
MNT_QUOTA       0x00002000      /* quotas are enabled on filesystem */
MNT_ROOTFS      0x00004000      /* identifies the root filesystem */
MNT_DOVOLFS     0x00008000      /* FS supports volfs (deprecated flag in Mac OS X 10.5) */
MNT_DONTBROWSE  0x00100000      /* file system is not appropriate path to user data */
MNT_IGNORE_OWNERSHIP 0x00200000 /* VFS will ignore ownership information on filesystem objects */
MNT_AUTOMOUNTED 0x00400000      /* filesystem was mounted by automounter */
MNT_JOURNALED   0x00800000      /* filesystem is journaled */
MNT_NOUSERXATTR 0x01000000      /* Don't allow user extended attributes */
MNT_DEFWRITE    0x02000000      /* filesystem should defer writes */
MNT_MULTILABEL  0x04000000      /* MAC support for individual labels */
MNT_NOATIME     0x10000000      /* disable update of file access time */
|]

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
               
getStatFS :: IO (Either Errno [StatFS])
getStatFS = do
  a <- fmap fromIntegral (c_getfsstat nullPtr 0 0)  :: IO Int
  allocaArray (a+1) $ \p -> do
    let bufsiz = (a+1) * sizeOf (undefined :: StatFS)
    res <- c_getfsstat p (fromIntegral bufsiz :: CInt) 2 -- (fromEnum MNT_NOWAIT)
    if res < 0 then Left <$> getErrno else do
         z <- (peekArray a p :: IO [StatFS])
         return (Right z)


{-
 - Right a <- getStatFS
 -
 -}
