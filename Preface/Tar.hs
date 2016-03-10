{-# LANGUAGE FlexibleContexts #-}

module Preface.Tar (

{- | Tar archive files are used to store a collection of other files in a
   single file. They consists of a sequence of entries. Each entry describes
   a file or directory (or some other special kind of file). The entry stores
   a little bit of meta-data, in particular the file or directory name.
  
   Unlike some other archive formats, a tar file contains no index. The
   information about each entry is stored next to the entry. Because of this,
   tar files are almost always processed linearly rather than in a
   random-access fashion.
  
   The functions in this package are designed for working on tar files
   linearly and lazily. This makes it possible to do many operations in
   constant space rather than having to load the entire archive into memory.
  
   It can read and write standard POSIX tar files and also the GNU and old
   Unix V7 tar formats. The convenience functions are designed for 
   creating standard portable archives. If you need to
   construct GNU format archives or exactly preserve file ownership and
   permissions then you will need to write some extra helper functions.
-}
  tarCreate,
  tarExtract,
  tarAppend,

  -- Creating a compressed \"@.tar.gz@\" file is just a minor variation on the
  -- 'create' function, but where throw compression into the pipeline:
  --
  -- > BS.writeFile tar . GZip.compress . Tar.write =<< Tar.pack base dir
  --
  -- Similarly, extracting a compressed \"@.tar.gz@\" is just a minor variation
  -- on the 'extract' function where we use decompression in the pipeline:
  --
  -- > Tar.unpack dir . Tar.read . GZip.decompress =<< BS.readFile tar
  --

  -- ** Security
  -- | This is pretty important. A maliciously constructed tar archives could
  -- contain entries that specify bad file names. It could specify absolute
  -- file names like \"@\/etc\/passwd@\" or relative files outside of the
  -- archive like \"..\/..\/..\/something\". This security problem is commonly
  -- called a \"directory traversal vulnerability\". Historically, such
  -- vulnerabilities have been common in packages handling tar archives.
  --
  -- The 'extract' and 'unpack' functions check for bad file names. See the
  -- 'checkSecurity' function for more details. If you need to do any custom
  -- unpacking then you should use this.

  -- ** Tarbombs
  -- | A \"tarbomb\" is a @.tar@ file where not all entries are in a
  -- subdirectory but instead files extract into the top level directory. The
  -- 'extract' function does not check for these however if you want to do
  -- that you can use the 'checkTarbomb' function like so:
  --
  -- > Tar.unpack dir . Tar.checkTarbomb expectedDir
  -- >                . Tar.read =<< BS.readFile tar
  --
  -- In this case extraction will fail if any file is outside of @expectedDir@.

  tarRead,
  tarList,
  tarWrite,

  tarPack,
  tarUnpack,

  -- * Types
  -- ** Tar entry type
  -- | This module provides only very simple and limited read-only access to
  -- the 'Entry' type. If you need access to the details or if you need to
  -- construct your own entries then also import "Codec.Archive.Tar.Entry".
  TarEntry,
  tarEntryPath,
  tarEntryContent,
  TarEntryContent(..),

  -- ** Sequences of tar entries
  -- TarEntries(..),
  -- tarMapEntries,
  -- tarMapEntriesNoFail,
  -- tarFoldEntries,
  -- tarUnfoldEntries,

  -- * Error handling
  -- | Reading tar files can fail if the data does not match the tar file
  -- format correctly.
  --
  -- The style of error handling by returning structured errors. The pure
  -- functions in the library do not throw exceptions, they return the errors
  -- as data. The IO actions in the library can throw exceptions, in particular
  -- the 'unpack' action does this. All the error types used are an instance of
  -- the standard 'Exception' class so it is possible to 'throw' and 'catch'
  -- them.

  -- ** Errors from reading tar files
  TarFormatError(..),
  ) where

import Preface.Imports
import Preface.Stringy

tarBlkSize :: Int
tarBlkSize = 512

-- | Create a new @\".tar\"@ file from a directory of files.
--
-- It is equivalent to calling the standard @tar@ program like so:
--
-- @$ tar -f tarball.tar -C base -c dir@
--
-- This assumes a directory @.\/base\/dir@ with files inside, eg
-- @.\/base\/dir\/foo.txt@. The file names inside the resulting tar file will be
-- relative to @dir@, eg @dir\/foo.txt@.
--
-- This is a high level \"all in one\" operation. Since you may need variations
-- on this function it is instructive to see how it is written. It is just:
--
-- > BS.writeFile tar . Tar.write =<< Tar.pack base paths
--
-- Notes:
--
-- The files and directories must not change during this operation or the
-- result is not well defined.
--
-- The intention of this function is to create tarballs that are portable
-- between systems. It is /not/ suitable for doing file system backups because
-- file ownership and permissions are not fully preserved. File ownership is
-- not preserved at all. File permissions are set to simple portable values:
--
-- * @rw-r--r--@ for normal files
--
-- * @rwxr-xr-x@ for executable files
--
-- * @rwxr-xr-x@ for directories
--
tarCreate :: FilePath   -- ^ Path of the \".tar\" file to write.
       -> FilePath   -- ^ Base directory
       -> [FilePath] -- ^ Files and directories to archive, relative to base dir
       -> IO ()
tarCreate tar base paths = strWriteFile tar . tarWrite =<< tarPack base paths

-- | Extract all the files contained in a @\".tar\"@ file.
--
-- It is equivalent to calling the standard @tar@ program like so:
--
-- @$ tar -x -f tarball.tar -C dir@
--
-- So for example if the @tarball.tar@ file contains @foo\/bar.txt@ then this
-- will extract it to @dir\/foo\/bar.txt@.
--
-- This is a high level \"all in one\" operation. Since you may need variations
-- on this function it is instructive to see how it is written. It is just:
--
-- > Tar.unpack dir . Tar.read =<< BS.readFile tar
--
-- Notes:
--
-- Extracting can fail for a number of reasons. The tarball may be incorrectly
-- formatted. There may be IO or permission errors. In such cases an exception
-- will be thrown and extraction will not continue.
--
-- Since the extraction may fail part way through it is not atomic. For this
-- reason you may want to extract into an empty directory and, if the
-- extraction fails, recursively delete the directory.
--
-- Security: only files inside the target directory will be written. Tarballs
-- containing entries that point outside of the tarball (either absolute paths
-- or relative paths) will be caught and an exception will be thrown.
--
tarExtract :: FilePath -- ^ Destination directory
        -> FilePath -- ^ Tarball
        -> IO ()
tarExtract dir tar = tarUnpack dir . tarRead =<< strReadFile tar

tarList :: FilePath -> IO [FilePath]
tarList tar = do
  a <- strReadFile tar
  let b = tarRead a
  return (foldr (\entry rest -> tarEntryPath entry : rest) []  b)

-- | Append new entries to a @\".tar\"@ file from a directory of files.
--
-- This is much like 'create', except that all the entries are added to the
-- end of an existing tar file. Or if the file does not already exists then
-- it behaves the same as 'create'.
--
tarAppend :: FilePath   -- ^ Path of the \".tar\" file to write.
       -> FilePath   -- ^ Base directory
       -> [FilePath] -- ^ Files and directories to archive, relative to base dir
       -> IO ()
tarAppend tar base paths =
    withFile tar ReadWriteMode $ \hnd -> do
      _ <- hSeekEndEntryOffset hnd Nothing
      strHPut hnd . tarWrite =<< tarPack base paths


-- | This function checks a sequence of tar entries for file name security
-- problems. It checks that:
--
-- * file paths are not absolute
--
-- * file paths do not contain any path components that are \"@..@\"
--
-- * file names are valid
--
-- These checks are from the perspective of the current OS. That means we check
-- for \"@C:\blah@\" files on Windows and \"\/blah\" files on Unix. For archive
-- entry types 'HardLink' and 'SymbolicLink' the same checks are done for the
-- link target. A failure in any entry terminates the sequence of entries with
-- an error.
--
{-
checkSecurity :: TarEntries e -> TarEntries (Either e FileNameError)
checkSecurity = checkEntries checkEntrySecurity
-}

checkEntrySecurity :: TarEntry -> Maybe FileNameError
checkEntrySecurity entry = case tarEntryContent entry of
    HardLink     xlink -> check (tarEntryPath entry)
                 `mplus` check (fromLinkTarget xlink)
    SymbolicLink xlink -> check (tarEntryPath entry)
                 `mplus` check (fromLinkTarget xlink)
    _                 -> check (tarEntryPath entry)

  where
    check name
      | isAbsolute name = Just $ AbsoluteFileName name
      | not (isValid name) = Just $ InvalidFileName name
      | any (=="..") (splitDirectories name) = Just $ InvalidFileName name
      | otherwise = Nothing

-- | Errors arising from tar file names being in some way invalid or dangerous
data FileNameError = InvalidFileName FilePath | AbsoluteFileName FilePath

instance Show FileNameError where show = showFileNameError

instance Exception FileNameError

showFileNameError :: FileNameError -> String
showFileNameError err = case err of
    InvalidFileName  path -> "Invalid file name in tar archive: " ++ show path
    AbsoluteFileName path -> "Absolute file name in tar archive: " ++ show path

--------------------------
-- Utils
--

{-
checkEntries :: (TarEntry -> Maybe e') -> [TarEntry] -> [Maybe Either e e')
checkEntries f = map f 
  tarMapEntries (\entry -> maybe (Right entry) Left (checkEntry entry))
-}

-- | An offset within a tar file. Use 'hReadEntry', 'hReadEntryHeader' or
-- 'hSeekEntryOffset'.  This is a tar \"record\" number, not a byte offset.
type TarEntryOffset = FileOffset


-------------------------
-- I/O operations
--

-- | Set the 'Handle' position to the position corresponding to the given
-- 'TarEntryOffset'.
--
-- This position is where the entry metadata can be read. If you already know
-- the entry has a body (and perhaps know it's length), you may wish to seek to
-- the body content directly using 'hSeekEntryContentOffset'.
--
hSeekEntryOffset :: Handle -> TarEntryOffset -> IO ()
hSeekEntryOffset hnd blockOff =
    hSeek hnd AbsoluteSeek ( fromIntegral blockOff * fromIntegral tarBlkSize)

-- | Seek to the end of a tar file, to the position where new entries can
-- be appended, and return that 'TarEntryOffset'.
--
-- If you have a valid 'TarIndex' for this tar file then you should supply it
-- because it allows seeking directly to the correct location.
--
-- If you do not have an index, then this becomes an expensive linear
-- operation because we have to read each tar entry header from the beginning
-- to find the location immediately after the last entry (this is because tar
-- files have a variable length trailer and we cannot reliably find that by
-- starting at the end). In this mode, it will fail with an exception if the
-- file is not in fact in the tar format.
--
hSeekEndEntryOffset :: Handle -> Maybe TarEntryOffset -> IO TarEntryOffset
hSeekEndEntryOffset hnd (Just ndex) = do
    let offset = ndex
    hSeekEntryOffset hnd offset
    return offset
hSeekEndEntryOffset _hnd Nothing = error "not yet implemented"


-- | Creates a tar archive from a list of directory or files. Any directories
-- specified will have their contents included recursively. Paths in the
-- archive will be relative to the given base directory.
--
-- This is a portable implementation of packing suitable for portable archives.
-- In particular it only constructs 'NormalFile' and 'Directory' entries. Hard
-- links and symbolic links are treated like ordinary files. It cannot be used
-- to pack directories containing recursive symbolic links. Special files like
-- FIFOs (named pipes), sockets or device files will also cause problems.
--
-- An exception will be thrown for any file names that are too long to
-- represent as a 'TarPath'.
--
-- * This function returns results lazily. Subdirectories are scanned
-- and files are read one by one as the list of entries is consumed.
--
tarPack :: FilePath   -- ^ Base directory
     -> [FilePath] -- ^ Files and directories to pack, relative to the base dir
     -> IO [TarEntry]
tarPack baseDir paths0 = preparePaths baseDir paths0 >>= packPaths baseDir

preparePaths :: FilePath -> [FilePath] -> IO [FilePath]
preparePaths baseDir paths =
  fmap concat $ interleave
    [ do isDir  <- doesDirectoryExist (baseDir </> path)
         if isDir
           then do entries <- getDirectoryContentsRecursive (baseDir </> path)
                   let entries' = map (path </>) entries
                       dir = addTrailingPathSeparator path
                   if null path then return entries'
                                else return (dir : entries')
           else return [path]
    | path <- paths ]

packPaths :: FilePath -> [FilePath] -> IO [TarEntry]
packPaths baseDir paths =
  interleave
    [ do tarpath <- maybe (fail "invalid filename") (\(x,y) -> return $ x ++ y ) (toTarPath isDir relpath)
         if isDir then packDirectoryEntry filepath tarpath
                  else packFileEntry      filepath tarpath
    | relpath <- paths
    , let isDir    = hasTrailingPathSeparator filepath
          filepath = baseDir </> relpath ]

interleave :: [IO a] -> IO [a]
interleave = unsafeInterleaveIO . go
  where
    go []     = return []
    go (x:xs) = do
      x'  <- x
      xs' <- interleave xs
      return (x':xs')

-- | Construct a tar 'Entry' based on a local file.
--
-- This sets the entry size, the data contained in the file and the file's
-- modification time. If the file is executable then that information is also
-- preserved. File ownership and detailed permissions are not preserved.
--
-- * The file contents is read lazily.
--
packFileEntry :: FilePath -- ^ Full path to find the file on the local disk
              -> FilePath  -- ^ Path to use for the tar Entry in the archive
              -> IO TarEntry
packFileEntry filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  file    <- openBinaryFile filepath ReadMode
  size    <- hFileSize file
  content <- strHGetContents file
  return (simpleEntry tarpath (NormalFile content (fromIntegral size))) {
    tarEntryPermissions = if executable perms then 0o0755 else 0o0644,
    tarEntryTime = mtime
  }

-- | Construct a tar 'Entry' based on a local directory (but not its contents).
--
-- The only attribute of the directory that is used is its modification time.
-- Directory ownership and detailed permissions are not preserved.
--
packDirectoryEntry :: FilePath -- ^ Full path to find the file on the local disk
                   -> FilePath  -- ^ Path to use for the tar Entry in the archive
                   -> IO TarEntry
packDirectoryEntry filepath tarpath = do
  mtime   <- getModTime filepath
  return (directoryEntry tarpath) {
    tarEntryTime = mtime
  }

-- | This is a utility function, much like 'getDirectoryContents'. The
-- difference is that it includes the contents of subdirectories.
--
-- The paths returned are all relative to the top directory. Directory paths
-- are distinguishable by having a trailing path separator
-- (see 'FilePath.Native.hasTrailingPathSeparator').
--
-- All directories are listed before the files that they contain. Amongst the
-- contents of a directory, subdirectories are listed after normal files. The
-- overall result is that files within a directory will be together in a single
-- contiguous group. This tends to improve file layout and IO performance when
-- creating or extracting tar archives.
--
-- * This function returns results lazily. Subdirectories are not scanned
-- until the files entries in the parent directory have been consumed.
--
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir0 =
  fmap tail (recurseDirectories dir0 [""])

recurseDirectories :: FilePath -> [FilePath] -> IO [FilePath]
recurseDirectories _    []         = return []
recurseDirectories base (dir:dirs) = unsafeInterleaveIO $ do
  (files, dirs') <- collect [] [] =<< getDirectoryContents (base </> dir)

  files' <- recurseDirectories base (dirs' ++ dirs)
  return (dir : files ++ files')

  where
    collect files dirs' []              = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) | ignore entry
                                        = collect files dirs' entries
    collect files dirs' (entry:entries) = do
      let dirEntry  = dir </> entry
          dirEntry' = addTrailingPathSeparator dirEntry
      isDirectory <- doesDirectoryExist (base </> dirEntry)
      if isDirectory
        then collect files (dirEntry':dirs') entries
        else collect (dirEntry:files) dirs' entries

    ignore ['.']      = True
    ignore ['.', '.'] = True
    ignore _          = False

getModTime :: FilePath -> IO EpochTime
getModTime path = floor . utcTimeToPOSIXSeconds <$> getModificationTime path

-- | Errors that can be encountered when parsing a Tar archive.
data TarFormatError
  = TruncatedArchive
  | ShortTrailer
  | BadTrailer
  | TrailingJunk
  | ChecksumIncorrect
  | NotTarFormat
  | UnrecognisedTarFormat
  | HeaderBadNumericEncoding
  deriving (Show)

instance Exception TarFormatError

-- | Parse a tar format byte stream in into a data structure. 
tarRead :: ByteString -> [TarEntry]
tarRead = unfoldr getEntry
  where 
    getEntry :: ByteString -> Maybe (TarEntry, ByteString)
    getEntry bs
      | strLen header < tarBlkSize = error (show TruncatedArchive)

  -- Tar files end with at least two blocks of all '0'. Checking this serves
  -- two purposes. It checks the format but also forces the tail of the data
  -- which is necessary to close the file if it came from a lazily read file.
      | strHead bs == 0 = case strSplitAt (2*tarBlkSize)  bs of
          (end, trailing)
            | strLen end /= (2*tarBlkSize)        -> error (show ShortTrailer)
            | Nothing /= strUntil (/= 0) end      -> error (show BadTrailer)
            | Nothing /= strUntil (/= 0) trailing -> error (show TrailingJunk)
            | otherwise                    -> Nothing

      | otherwise  = 
          let name       = getString   0 100 header
              mode       = getOct    100   8 header
              uid        = getOct    108   8 header
              gid        = getOct    116   8 header
              size_      = getOct    124  12 header
              mtime      = getOct    136  12 header
              chksum     = getOct    148   8 header
              typecode   = nth header 156 :: Word8
              linkname   = getString 157 100 header
              magic      = getChars  257   8 header
              uname      = getString 265  32 header
              gname      = getString 297  32 header
              devmajor   = getOct    329   8 header
              devminor   = getOct    337   8 header
              prefix     = getString 345 155 header
-- trailing   = getBytes  500  12 header

              format = case magic of
                          "\0\0\0\0\0\0\0\0" -> V7Format
                          "ustar\NUL00"      -> UstarFormat
                          "ustar  \NUL"      -> GnuFormat
                          _                  -> UnrecognisedFormat


              cksum = case (chksum, format) of
                (Right chks, _   ) | correctChecksum header chks -> Right ()
                (Right _,    _) -> Left ChecksumIncorrect
                _             -> Left NotTarFormat

  -- These fields are partial, have to check them
--   format   <- format_;   mode     <- mode_;
--   uid      <- uid_;      gid      <- gid_;
--   size     <- size_;     mtime    <- mtime_;
--   devmajor <- devmajor_; devminor <- devminor_;
              size = either (const 0 ) id size_ :: FileOffset
              content = genericStrTake size (strDrop tarBlkSize bs)
              padding = (tarBlkSize - fromIntegral size) `mod` tarBlkSize
              bs'     = strDrop (fromIntegral size + (tarBlkSize + padding) ) bs

              haserr = isLeft mode || isLeft uid || isLeft gid || isLeft size_
                  || isLeft mtime || isLeft chksum || isLeft devminor
                  || isLeft devmajor 

              entry = TarEntry {
                 tarEntryPath     = prefix </> name,
                 tarEntryContent     = case chr (fromEnum typecode) of
                   '\0' -> NormalFile      content size
                   '0'  -> NormalFile      content size
                   '1'  -> HardLink        linkname
                   '2'  -> SymbolicLink    linkname
                   '3'  -> CharacterDevice (either (const 0) id devmajor) (either (const 0) id devminor)
                   '4'  -> BlockDevice     (either (const 0) id devmajor) (either (const 0) id devminor)
                   '5'  -> Directory
                   '6'  -> NamedPipe
                   '7'  -> NormalFile      content size
                   _    -> OtherEntryType  (chr (fromEnum typecode)) content size,
                 tarEntryPermissions = either (const 0) id mode,
                 tarEntryOwnership   = Ownership uname gname (either (const 0) id uid)  (either (const 0) id gid),
                 tarEntryTime        = (either (const 0) id mtime),
                 tarEntryFormat      = format
              }
           in if haserr then error (show HeaderBadNumericEncoding)
              else if isLeft cksum then error (show ChecksumIncorrect)
              else Just (entry, bs')

      where header = strTake tarBlkSize bs

correctChecksum :: ByteString -> Int -> Bool
correctChecksum header checksum = checksum == checksum'
  where
    -- sum of all 512 bytes in the header block,
    -- treating each byte as an 8-bit unsigned value
    checksum' = foldl' (\x y -> x + ord y) 0 header'
    -- treating the 8 bytes of chksum as blank characters.
    header'   = asString $ strConcat [strTake 148 header,
                           strReplicate 8 (fromIntegral (fromEnum ' ')),
                           strDrop 156 header]

-- * TAR format primitive input

getOct :: Integral a => Int -> Int -> ByteString -> Either TarFormatError a
getOct off len = parseOct
               . asString
               . strTakeWhile (\c -> c /= 0  && c /= (fromIntegral (fromEnum ' ') ) )
               . strDropWhile (== (fromIntegral (fromEnum ' ')))
               . getBytes off len
  where
    parseOct "" = Right 0
    -- As a star extension, octal fields can hold a base-256 value if the high
    -- bit of the initial character is set. The initial character can be:
    --   0x80 ==> trailing characters hold a positive base-256 value
    --   0xFF ==> trailing characters hold a negative base-256 value
    --
    -- In both cases, there won't be a trailing NUL/space.
    --
    -- GNU tar seems to contain a half-implementation of code that deals with
    -- extra bits in the first character, but I don't think it works and the
    -- docs I can find on star seem to suggest that these will always be 0,
    -- which is what I will assume.
    parseOct ('\128':xs) = Right (readBytes xs)
    parseOct ('\255':xs) = Right (negate (readBytes xs))
    parseOct s  = case readOct s of
      [(x,[])] -> Right x
      _        -> Left HeaderBadNumericEncoding

    readBytes = go 0
      where go acc []     = acc
            go acc (x:xs) = go (acc * 256 + fromIntegral (ord x)) xs

getBytes :: Int -> Int -> ByteString -> ByteString
getBytes off len = strTake len . strDrop off

-- getByte :: Int -> ByteString -> Word8
-- getByte off bs = nth bs off

getChars :: Int -> Int -> ByteString -> String
getChars off len = asString . getBytes off len

getString :: Int -> Int -> ByteString -> String
getString off len = asString . strTakeWhile (/= 0) . getBytes off len

-- type FileSize  = Int64
-- | The number of seconds since the UNIX epoch
type EpochTime = Int64
type DevMajor  = Int
type DevMinor  = Int
type TypeCode  = Char
type TarPermissions = FileMode

-- | Tar archive entry.
data TarEntry = TarEntry {

    -- | The path of the file or directory within the archive. This is in a
    -- tar-specific form. Use 'tarEntryPath' to get a native 'FilePath'.
    tarEntryPath :: FilePath,

    -- | The real content of the entry. For 'NormalFile' this includes the
    -- file data. An entry usually contains a 'NormalFile' or a 'Directory'.
    tarEntryContent :: TarEntryContent,

    -- | File permissions (Unix style file mode).
    tarEntryPermissions :: TarPermissions,

    -- | The user and group to which this file belongs.
    tarEntryOwnership :: Ownership,

    -- | The time the file was last modified.
    tarEntryTime :: EpochTime,

    -- | The tar format the archive is using.
    tarEntryFormat :: Format
  } deriving Show

-- | The content of a tar archive entry, which depends on the type of entry.
-- Portable archives should contain only 'NormalFile' and 'Directory'.
data TarEntryContent = NormalFile ByteString FileOffset
                  | Directory
                  | SymbolicLink FilePath
                  | HardLink FilePath
                  | CharacterDevice DevMajor DevMinor
                  | BlockDevice DevMajor DevMinor
                  | NamedPipe
                  | OtherEntryType TypeCode ByteString FileOffset
  deriving Show

data Ownership = Ownership {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | Numeric owner user id. Should be set to @0@ if unknown.
    ownerId :: Int,

    -- | Numeric owner group id. Should be set to @0@ if unknown.
    groupId :: Int
  } deriving Show

-- | There have been a number of extensions to the tar file format over the
-- years. They all share the basic entry fields and put more meta-data in
-- different extended headers.
data Format =

     -- | This is the classic Unix V7 tar format. It does not support owner and
     -- group names, just numeric Ids. It also does not support device numbers.
     V7Format

     -- | The \"USTAR\" format is an extension of the classic V7 format. It was
     -- later standardised by POSIX. It has some restrictions but is the most
     -- portable format.
   | UstarFormat

     -- | The GNU tar implementation also extends the classic V7 format, though
     -- in a slightly different way from the USTAR format. In general for new
     -- archives the standard USTAR/POSIX should be used.
   | GnuFormat
   | UnrecognisedFormat
  deriving (Eq, Show)

-- | An 'Entry' with all default values except for the file name and type. It
-- uses the portable USTAR/POSIX format (see 'UstarHeader').
--
-- You can use this as a basis and override specific fields, eg:
--
-- > (emptyEntry name HardLink) { linkTarget = target }
--
simpleEntry :: FilePath -> TarEntryContent -> TarEntry
simpleEntry tarpath content = TarEntry {
    tarEntryPath     = tarpath,
    tarEntryContent     = content,
    tarEntryPermissions = case content of
                         Directory -> 0o0755
                         _         -> 0o0644,
    tarEntryOwnership   = Ownership "" "" 0 0,
    tarEntryTime        = 0,
    tarEntryFormat      = UstarFormat
  }

-- | A tar 'Entry' for a file.
--
-- Entry  fields such as file permissions and ownership have default values.
--
-- You can use this as a basis and override specific fields. For example if you
-- need an executable file you could use:
--
-- > (fileEntry name content) { fileMode = executableFileMode }
--
{-
fileEntry :: TarPath -> ByteString -> TarEntry
fileEntry name fileContent =
  simpleEntry name (NormalFile fileContent (fromIntegral (strLen fileContent)))
-}

-- | A tar 'Entry' for a directory.
--
-- Entry fields such as file permissions and ownership have default values.
--
directoryEntry :: FilePath -> TarEntry
directoryEntry name = simpleEntry name Directory

-- | Convert a 'TarPath' to a native 'FilePath'.
--
-- The native 'FilePath' will use the native directory separator but it is not
-- otherwise checked for validity or sanity. In particular:
--
-- * The tar path may be invalid as a native path, eg the file name @\"nul\"@
--   is not valid on Windows.
--
-- * The tar path may be an absolute path or may contain @\"..\"@ components.
--   For security reasons this should not usually be allowed, but it is your
--   responsibility to check for these conditions (eg using 'checkSecurity').
--
{-
fromTarPath :: FilePath -> FilePath
fromTarPath (TarPath name prefix) = adjustDirectory $
  joinPath $ splitDirectories prefix
                          ++ splitDirectories name
  where
    adjustDirectory | hasTrailingPathSeparator name
                    = addTrailingPathSeparator
                    | otherwise = id
-}


-- | The classic tar format allowed just 100 characters for the file name. The
-- USTAR format extended this with an extra 155 characters, however it uses a
-- complex method of splitting the name between the two sections.
--
-- Instead of just putting any overflow into the extended area, it uses the
-- extended area as a prefix. The aggravating insane bit however is that the
-- prefix (if any) must only contain a directory prefix. That is the split
-- between the two areas must be on a directory separator boundary. So there is
-- no simple calculation to work out if a file name is too long. Instead we
-- have to try to find a valid split that makes the name fit in the two areas.
--
-- * Tar paths use Posix format (ie @\'/\'@ directory separators), irrespective
--   of the local path conventions.
--
-- * The directory separator between the prefix and name is /not/ stored.
--

-- | Convert a native 'FilePath' to a 'TarPath'.
--
-- The conversion may fail if the 'FilePath' is too long. See 'TarPath' for a
-- description of the problem with splitting long 'FilePath's.
--
toTarPath :: Bool -- ^ Is the path for a directory? This is needed because for
                  -- directories a 'TarPath' must always use a trailing @\/@.
          -> FilePath -> Maybe (FilePath, FilePath)
toTarPath isDir = splitLongPath
                . addTrailingSep
                . joinPath
                . splitDirectories
  where
    addTrailingSep | isDir     = addTrailingPathSeparator
                   | otherwise = id

-- | Take a sanitised path, split on directory separators and try to pack it
-- into the 155 + 100 tar file name format.
--
-- The strategy is this: take the name-directory components in reverse order
-- and try to fit as many components into the 100 long name area as possible.
-- If all the remaining components fit in the 155 name area then we win.
--
splitLongPath :: FilePath -> Maybe (FilePath, FilePath)
splitLongPath fn = let a = splitPath fn
                       b = scanr (+) 0 $ map length a
                       (c,_d) = span (>100) b
                       e = length c
                       f = concat (drop e a)
                       g = concat (take e a)
                       h = if null g then g else init g
                    in if length h > 155 then Nothing else Just (h, f) 

-- | The tar format allows just 100 ASCII characters for the 'SymbolicLink' and
-- 'HardLink' entry types.
--
-- | Convert a tar 'LinkTarget' to a native 'FilePath'.
--
fromLinkTarget :: FilePath -> FilePath
fromLinkTarget path = adjustDirectory $ joinPath $ splitDirectories path
  where
    adjustDirectory | hasTrailingPathSeparator path = addTrailingPathSeparator
                    | otherwise = id

-- | Create local files and directories based on the entries of a tar archive.
--
-- This is a portable implementation of unpacking suitable for portable
-- archives. It handles 'NormalFile' and 'Directory' entries and has simulated
-- support for 'SymbolicLink' and 'HardLink' entries. Links are implemented by
-- copying the target file. This therefore works on Windows as well as Unix.
-- All other entry types are ignored, that is they are not unpacked and no
-- exception is raised.
--
-- If the 'Entries' ends in an error then it is raised an an exception. Any
-- files or directories that have been unpacked before the error was
-- encountered will not be deleted. For this reason you may want to unpack
-- into an empty directory so that you can easily clean up if unpacking fails
-- part-way.
--
-- On its own, this function only checks for security (using 'checkSecurity').
-- You can do other checks by applying checking functions to the 'Entries' that
-- you pass to this function. For example:
--
-- > unpack dir (checkTarbomb expectedDir entries)
--
-- If you care about the priority of the reported errors then you may want to
-- use 'checkSecurity' before 'checkTarbomb' or other checks.
--
tarUnpack :: FilePath -> [TarEntry] -> IO ()
tarUnpack baseDir entries = do
  let _a = map checkEntrySecurity entries
  mapM_ unpackEntry entries

  where
    -- We're relying here on 'checkSecurity' to make sure we're not scribbling
    -- files all over the place.

    unpackEntry entry = case tarEntryContent entry of
      NormalFile file _ -> extractFile path file
      Directory         -> extractDir path
      HardLink     xlink -> createLink xlink path
      SymbolicLink xlink -> createSymbolicLink xlink path
      _                 -> traceIO ("ignored: "++path)
      where
        path = tarEntryPath entry

    extractFile path content = do
      -- Note that tar archives do not make sure each directory is created
      -- before files they contain, indeed we may have to create several
      -- levels of directory.
      createDirectoryIfMissing True absDir
      strWriteFile absPath content
      where
        absDir  = baseDir </> takeDirectory path
        absPath = baseDir </> path

    extractDir path = createDirectoryIfMissing True (baseDir </> path)


-- | Create a tar archive by serialising a list of tar entries.
tarWrite :: [TarEntry] -> ByteString
tarWrite es = strConcat $ map putEntry es ++ [strReplicate (tarBlkSize*2) 0]

putEntry :: TarEntry -> ByteString
putEntry entry = case tarEntryContent entry of
  NormalFile       content size -> strConcat [ header, content, padding size ]
  OtherEntryType _ content size -> strConcat [ header, content, padding size ]
  _                             -> header
  where
    header       = putHeader entry
    padding size = strReplicate (fromIntegral (-size) `mod` tarBlkSize) 0 

    putHeader :: TarEntry -> ByteString
    putHeader ntry =
      let block = putHeaderNoChkSum ntry
          checksum = foldl' (\x y -> x + ord y) 0 (asString block)
       in strConcat [ strTake 148 block, putOct 7 checksum
                    , asByteString " ", strDrop 156 block]
    putString :: Int -> String -> ByteString
    putString n s = strConcat [strTake n (asByteString s)
                              , strReplicate (n - length s) 0]

    putOct :: (Integral a, Show a) => Int -> a -> ByteString
    putOct n x =
      let octStr = asByteString (take (n-1) $ showOct x "")
       in strConcat [strReplicate (n - strLen octStr - 1) (asByte '0')
                    , octStr, stringleton 0]

    putGnuDev w content n = case content of
       CharacterDevice _ _ -> putOct w n
       BlockDevice     _ _ -> putOct w n
       _                   -> strReplicate w 0

    putHeaderNoChkSum :: TarEntry -> ByteString
    putHeaderNoChkSum TarEntry {
       tarEntryPath     = tarPath,
       tarEntryContent     = content,
       tarEntryPermissions = permissions,
       tarEntryOwnership   = ownership,
       tarEntryTime        = modTime,
       tarEntryFormat      = format
     } = let Just (prefix, name) = splitLongPath tarPath
             (typeCode, contentSize, linkTarget,
              deviceMajor, deviceMinor) = case content of
                 NormalFile _ size     -> ('0' , size, [], 0,  0)
                 Directory             -> ('5' , 0,    [], 0,  0)
                 SymbolicLink xl       -> ('2' , 0,    xl, 0,  0)
                 HardLink     xl       -> ('1' , 0,    xl, 0,  0)
                 CharacterDevice ma mi -> ('3' , 0,    [], ma, mi)
                 BlockDevice     ma mi -> ('4' , 0,    [], ma, mi)
                 NamedPipe             -> ('6' , 0,    [], 0,  0)
                 OtherEntryType code _ size -> (code, size, [], 0,  0)

          in strConcat
       [ putString 100 name, putOct 8 permissions
       , putOct 8 (ownerId ownership), putOct 8 (groupId ownership)
       , putOct 12 contentSize, putOct 12 modTime, strReplicate 8 (asByte ' ') 
       , stringleton (asByte typeCode), putString 100 linkTarget
       , case format of
            V7Format -> strReplicate 255 0
            UstarFormat -> strConcat [ putString 8 "ustar\NUL00"
               , putString 32 (ownerName ownership)
               , putString 32 (groupName ownership)
               , putOct 8 deviceMajor, putOct 8 deviceMinor
               , putString 155 prefix, strReplicate 12 0 ]
            GnuFormat -> strConcat [ putString 8 "ustar  \NUL"
               , putString 32 (ownerName ownership)
               , putString 32 (groupName ownership)
               , putGnuDev 8 content deviceMajor, putGnuDev 8 content deviceMinor
               , putString 155 prefix, strReplicate 12 0]
            UnrecognisedFormat -> error "Unrecognised Tar format" ]

