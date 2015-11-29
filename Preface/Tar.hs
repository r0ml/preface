{-# LANGUAGE FlexibleContexts #-}

-- Reading, writing and manipulating \"@.tar@\" archive files.
--
module Preface.Tar (

  -- | Tar archive files are used to store a collection of other files in a
  -- single file. They consists of a sequence of entries. Each entry describes
  -- a file or directory (or some other special kind of file). The entry stores
  -- a little bit of meta-data, in particular the file or directory name.
  --
  -- Unlike some other archive formats, a tar file contains no index. The
  -- information about each entry is stored next to the entry. Because of this,
  -- tar files are almost always processed linearly rather than in a
  -- random-access fashion.
  --
  -- The functions in this package are designed for working on tar files
  -- linearly and lazily. This makes it possible to do many operations in
  -- constant space rather than having to load the entire archive into memory.
  --
  -- It can read and write standard POSIX tar files and also the GNU and old
  -- Unix V7 tar formats. The convenience functions that are provided in the
  -- "Codec.Archive.Tar.Entry" module for creating archive entries are
  -- primarily designed for standard portable archives. If you need to
  -- construct GNU format archives or exactly preserve file ownership and
  -- permissions then you will need to write some extra helper functions.
  --
  -- This module contains just the simple high level operations without
  -- exposing the all the details of tar files. If you need to inspect tar
  -- entries in more detail or construct them directly then you also need
  -- the module "Codec.Archive.Tar.Entry".

  -- * High level \"all in one\" operations
  tarCreate,
  tarExtract,
  tarAppend,

  -- * Notes
  -- ** Compressed tar archives
  -- | Tar files are commonly used in conjunction with gzip compression, as in
  -- \"@.tar.gz@\" or \"@.tar.bz2@\" files. This module does not directly
  -- handle compressed tar files however they can be handled easily by
  -- composing functions from this module and the modules
  -- @Codec.Compression.GZip@ or @Codec.Compression.BZip@
  -- (see @zlib@ or @bzlib@ packages).
  --
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

  -- * Converting between internal and external representation
  -- | Note, you cannot expect @write . read@ to give exactly the same output
  -- as input. You can expect the information to be preserved exactly however.
  -- This is because 'read' accepts common format variations while 'write'
  -- produces the standard format.
  tarRead,
  tarWrite,

  -- * Packing and unpacking files to\/from internal representation
  -- | These functions are for packing and unpacking portable archives. They
  -- are not suitable in cases where it is important to preserve file ownership
  -- and permissions or to archive special files like named pipes and Unix
  -- device files.
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
  TarEntries(..),
  tarMapEntries,
  tarMapEntriesNoFail,
  tarFoldEntries,
  tarUnfoldEntries,

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
import Preface.Binary

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
checkSecurity :: TarEntries e -> TarEntries (Either e FileNameError)
checkSecurity = checkEntries checkEntrySecurity

checkEntrySecurity :: TarEntry -> Maybe FileNameError
checkEntrySecurity entry = case tarEntryContent entry of
    HardLink     link -> check (tarEntryPath entry)
                 `mplus` check (fromLinkTarget link)
    SymbolicLink link -> check (tarEntryPath entry)
                 `mplus` check (fromLinkTarget link)
    _                 -> check (tarEntryPath entry)

  where
    check name
      | isAbsolute name
      = Just $ AbsoluteFileName name

      | not (isValid name)
      = Just $ InvalidFileName name

      | any (=="..") (splitDirectories name)
      = Just $ InvalidFileName name

      | otherwise = Nothing

-- | Errors arising from tar file names being in some way invalid or dangerous
data FileNameError
  = InvalidFileName FilePath
  | AbsoluteFileName FilePath
  deriving (Typeable)

instance Show FileNameError where
  show = showFileNameError Nothing

instance Exception FileNameError

showFileNameError :: Maybe PortabilityPlatform -> FileNameError -> String
showFileNameError mb_plat err = case err of
    InvalidFileName  path -> "Invalid"  ++ plat ++ " file name in tar archive: " ++ show path
    AbsoluteFileName path -> "Absolute" ++ plat ++ " file name in tar archive: " ++ show path
  where plat = maybe "" (' ':) mb_plat


--------------------------
-- Tarbombs
--

-- | This function checks a sequence of tar entries for being a \"tar bomb\".
-- This means that the tar file does not follow the standard convention that
-- all entries are within a single subdirectory, e.g. a file \"foo.tar\" would
-- usually have all entries within the \"foo/\" subdirectory.
--
-- Given the expected subdirectory, this function checks all entries are within
-- that subdirectroy.
--
-- Note: This check must be used in conjunction with 'checkSecurity'
-- (or 'checkPortability').
--
checkTarbomb :: FilePath -> TarEntries e -> TarEntries (Either e TarBombError)
checkTarbomb expectedTopDir = checkEntries (checkEntryTarbomb expectedTopDir)

checkEntryTarbomb :: FilePath -> TarEntry -> Maybe TarBombError
checkEntryTarbomb expectedTopDir entry =
  case splitDirectories (tarEntryPath entry) of
    (topDir:_) | topDir == expectedTopDir -> Nothing
    _ -> Just $ TarBombError expectedTopDir

-- | An error that occurs if a tar file is a \"tar bomb\" that would extract
-- files outside of the intended directory.
data TarBombError = TarBombError FilePath
                  deriving (Typeable)

instance Exception TarBombError

instance Show TarBombError where
  show (TarBombError expectedTopDir)
    = "File in tar archive is not in the expected directory " ++ show expectedTopDir


--------------------------
-- Portability
--

-- | This function checks a sequence of tar entries for a number of portability
-- issues. It will complain if:
--
-- * The old \"Unix V7\" or \"gnu\" formats are used. For maximum portability
--   only the POSIX standard \"ustar\" format should be used.
--
-- * A non-portable entry type is used. Only ordinary files, hard links,
--   symlinks and directories are portable. Device files, pipes and others are
--   not portable between all common operating systems.
--
-- * Non-ASCII characters are used in file names. There is no agreed portable
--   convention for Unicode or other extended character sets in file names in
--   tar archives.
--
-- * File names that would not be portable to both Unix and Windows. This check
--   includes characters that are valid in both systems and the \'/\' vs \'\\\'
--   directory separator conventions.
--
checkPortability :: TarEntries e -> TarEntries (Either e PortabilityError)
checkPortability = checkEntries checkEntryPortability

checkEntryPortability :: TarEntry -> Maybe PortabilityError
checkEntryPortability entry
  | entryFormat entry `elem` [V7Format, GnuFormat]
  = Just $ NonPortableFormat (entryFormat entry)

  | not (portableFileType (tarEntryContent entry))
  = Just NonPortableFileType

  | not (all portableChar posixPath)
  = Just $ NonPortableEntryNameChar posixPath

  | not (isValid posixPath)
  = Just $ NonPortableFileName "unix"    (InvalidFileName posixPath)
  | not (isValid windowsPath)
  = Just $ NonPortableFileName "windows" (InvalidFileName windowsPath)

  | isAbsolute posixPath
  = Just $ NonPortableFileName "unix"    (AbsoluteFileName posixPath)
  | isAbsolute windowsPath
  = Just $ NonPortableFileName "windows" (AbsoluteFileName windowsPath)

  | any (=="..") (splitDirectories posixPath)
  = Just $ NonPortableFileName "unix"    (InvalidFileName posixPath)
  | any (=="..") (splitDirectories windowsPath)
  = Just $ NonPortableFileName "windows" (InvalidFileName windowsPath)

  | otherwise = Nothing

  where
    tarPath     = entryTarPath entry
    posixPath   = fromTarPathToPosixPath   tarPath
    windowsPath = fromTarPathToWindowsPath tarPath

    portableFileType ftype = case ftype of
      NormalFile   {} -> True
      HardLink     {} -> True
      SymbolicLink {} -> True
      Directory       -> True
      _               -> False

    portableChar c = c <= '\127'

-- | Portability problems in a tar archive
data PortabilityError
  = NonPortableFormat Format
  | NonPortableFileType
  | NonPortableEntryNameChar FilePath
  | NonPortableFileName PortabilityPlatform FileNameError
  deriving (Typeable)

-- | The name of a platform that portability issues arise from
type PortabilityPlatform = String

instance Exception PortabilityError

instance Show PortabilityError where
  show (NonPortableFormat format) = "Archive is in the " ++ fmt ++ " format"
    where fmt = case format of V7Format    -> "old Unix V7 tar"
                               UstarFormat -> "ustar" -- I never generate this but a user might
                               GnuFormat   -> "GNU tar"
  show NonPortableFileType        = "Non-portable file type in archive"
  show (NonPortableEntryNameChar posixPath)
    = "Non-portable character in archive entry name: " ++ show posixPath
  show (NonPortableFileName platform err)
    = showFileNameError (Just platform) err


--------------------------
-- Utils
--

checkEntries :: (TarEntry -> Maybe e') -> TarEntries e -> TarEntries (Either e e')
checkEntries checkEntry =
  tarMapEntries (\entry -> maybe (Right entry) Left (checkEntry entry))

-- | An index of the entries in a tar file.
--
-- This index type is designed to be quite compact and suitable to store either
-- on disk or in memory.
--
data TarIndex = TarIndex

  -- As an example of how the mapping works, consider these example files:
  --   "foo/bar.hs" at offset 0
  --   "foo/baz.hs" at offset 1024
  --
  -- We split the paths into components and enumerate them.
  --   { "foo" -> TokenId 0, "bar.hs" -> TokenId 1,  "baz.hs" -> TokenId 2 }
  --
  -- We convert paths into sequences of 'TokenId's, i.e.
  --   "foo/bar.hs" becomes [PathComponentId 0, PathComponentId 1]
  --   "foo/baz.hs" becomes [PathComponentId 0, PathComponentId 2]
  --
  -- We use a trie mapping sequences of 'PathComponentId's to the entry offset:
  --  { [PathComponentId 0, PathComponentId 1] -> offset 0
  --  , [PathComponentId 0, PathComponentId 2] -> offset 1024 }

  -- The mapping of filepath components as strings to ids.
  (StringTable Int)

  -- Mapping of sequences of filepath component ids to tar entry offsets.
  (IntTrie Int TarEntryOffset)

  -- The offset immediatly after the last entry, where we would append any
  -- additional entries.
  TarEntryOffset

  deriving (Eq, Show, Typeable)

-- | The result of 'lookup' in a 'TarIndex'. It can either be a file directly,
-- or a directory entry containing further entries (and all subdirectories
-- recursively). Note that the subtrees are constructed lazily, so it's
-- cheaper if you don't look at them.
--
data TarIndexEntry = TarFileEntry TarEntryOffset
                   | TarDir [(FilePath, TarIndexEntry)]
  deriving (Show, Typeable)


-- | An offset within a tar file. Use 'hReadEntry', 'hReadEntryHeader' or
-- 'hSeekEntryOffset'.
--
-- This is actually a tar \"record\" number, not a byte offset.
--
type TarEntryOffset = Word32


-- | Look up a given filepath in the 'TarIndex'. It may return a 'TarFileEntry'
-- containing the 'TarEntryOffset' of the file within the tar file, or if
-- the filepath identifies a directory then it returns a 'TarDir' containing
-- the list of files within that directory.
--
-- Given the 'TarEntryOffset' you can then use one of the I\/O operations:
-- 
-- * 'hReadEntry' to read the whole entry;
--
-- * 'hReadEntryHeader' to read just the file metadata (e.g. its length);
--
lookup :: TarIndex -> FilePath -> Maybe TarIndexEntry
lookup (TarIndex pathTable pathTrie _) path = do
    fpath  <- toComponentIds pathTable path
    tentry <- trieLookup pathTrie fpath
    return (mkIndexEntry tentry)
  where
    mkIndexEntry (TrieEntry offset)        = TarFileEntry offset
    mkIndexEntry (Completions entries) =
      TarDir [ (fromComponentId pathTable key, mkIndexEntry entry)
             | (key, entry) <- entries ]


toComponentIds :: StringTable Int -> FilePath -> Maybe [Int]
toComponentIds table =
    lookupComponents []
  . filter (/= ".")
  . splitDirectories
  where
    lookupComponents cs' []     = Just (reverse cs')
    lookupComponents cs' (c:cs) = case tarLookup table c of
      Nothing  -> Nothing
      Just cid -> lookupComponents (cid:cs') cs

fromComponentId :: StringTable Int -> Int -> FilePath
fromComponentId table = tarIndex table


-- | Build a 'TarIndex' from a sequence of tar 'Entries'. The 'Entries' are
-- assumed to start at offset @0@ within a file.
--
build :: TarEntries e -> Either e TarIndex
build = go emptyIndex
  where
    go builder (TarNext e es) = go (addNextEntry e builder) es
    go builder  TarDone       = Right $! finaliseIndex builder
    go _       (TarFail err)  = Left err


-- $incremental-construction
-- If you need more control than 'build' then you can construct the index
-- in an acumulator style using the 'IndexBuilder' and operations.
--
-- Start with the 'emptyIndex' and use 'addNextEntry' (or 'skipNextEntry') for
-- each 'Entry' in the tar file in order. Every entry must added or skipped in
-- order, otherwise the resulting 'TarIndex' will report the wrong
-- 'TarEntryOffset's. At the end use 'finaliseIndex' to get the 'TarIndex'.
--
-- For example, 'build' is simply:
--
-- > build = go emptyIndex
-- >   where
-- >     go !builder (Next e es) = go (addNextEntry e builder) es
-- >     go !builder  Done       = Right $! finaliseIndex builder
-- >     go !_       (Fail err)  = Left err


-- | The intermediate type used for incremental construction of a 'TarIndex'.
--
data IndexBuilder = IndexBuilder [(FilePath, TarEntryOffset)]
                                 TarEntryOffset

-- | The initial empty 'IndexBuilder'.
--
emptyIndex :: IndexBuilder
emptyIndex = IndexBuilder [] 0

-- | Add the next 'Entry' into the 'IndexBuilder'.
--
addNextEntry :: TarEntry -> IndexBuilder -> IndexBuilder
addNextEntry entry (IndexBuilder acc nextOffset) =
    IndexBuilder ((entrypath, nextOffset):acc)
                 (nextEntryOffset entry nextOffset)
  where
    entrypath  = tarEntryPath entry

-- | Use this function if you want to skip some entries and not add them to the
-- final 'TarIndex'.
--
skipNextEntry :: TarEntry -> IndexBuilder -> IndexBuilder
skipNextEntry entry (IndexBuilder acc nextOffset) =
    IndexBuilder acc (nextEntryOffset entry nextOffset)

-- | Finish accumulating 'Entry' information and build the compact 'TarIndex'
-- lookup structure.
--
finaliseIndex :: IndexBuilder -> TarIndex
finaliseIndex (IndexBuilder pathsOffsets finalOffset) =
    TarIndex pathTable pathTrie finalOffset
  where
    pathComponents = concatMap (splitDirectories . fst) pathsOffsets
    pathTable = construct pathComponents
    pathTrie  = trieConstruct
                  [ (cids, offset)
                  | (path, offset) <- pathsOffsets
                  , let Just cids = toComponentIds pathTable path ]

-- | This is the offset immediately following the entry most recently added
-- to the 'IndexBuilder'. You might use this if you need to know the offsets
-- but don't want to use the 'TarIndex' lookup structure.
-- Use with 'hSeekEntryOffset'. See also 'nextEntryOffset'.
--
indexNextEntryOffset :: IndexBuilder -> TarEntryOffset
indexNextEntryOffset (IndexBuilder _ off) = off

-- | This is the offset immediately following the last entry in the tar file.
-- This can be useful to append further entries into the tar file.
-- Use with 'hSeekEntryOffset', or just use 'hSeekEndEntryOffset' directly.
--
indexEndEntryOffset :: TarIndex -> TarEntryOffset
indexEndEntryOffset (TarIndex _ _ off) = off

-- | Calculate the 'TarEntryOffset' of the next entry, given the size and
-- offset of the current entry.
--
-- This is much like using 'skipNextEntry' and 'indexNextEntryOffset', but without
-- using an 'IndexBuilder'.
--
nextEntryOffset :: TarEntry -> TarEntryOffset -> TarEntryOffset
nextEntryOffset entry offset =
    offset
  + 1
  + case tarEntryContent entry of
      NormalFile     _   size -> blocks size
      OtherEntryType _ _ size -> blocks size
      _                       -> 0
  where
    blocks size = 1 + ((fromIntegral size - 1) `div` 512)


-------------------------
-- I/O operations
--

-- | Reads an entire 'Entry' at the given 'TarEntryOffset' in the tar file.
-- The 'Handle' must be open for reading and be seekable.
--
-- This reads the whole entry into memory strictly, not incrementally. For more
-- control, use 'hReadEntryHeader' and then read the entry content manually.
--
hReadEntry :: Handle -> TarEntryOffset -> IO TarEntry
hReadEntry hnd off = do
    entry <- hReadEntryHeader hnd off
    case tarEntryContent entry of
      NormalFile       _ size -> do body <- strHGet hnd (fromIntegral size)
                                    return entry {
                                      tarEntryContent = NormalFile body size
                                    }
      OtherEntryType c _ size -> do body <- strHGet hnd (fromIntegral size)
                                    return entry {
                                      tarEntryContent = OtherEntryType c body size
                                    }
      _                       -> return entry

-- | Read the header for a 'Entry' at the given 'TarEntryOffset' in the tar
-- file. The 'entryContent' will contain the correct metadata but an empty file
-- content. The 'Handle' must be open for reading and be seekable.
--
-- The 'Handle' position is advanced to the beginning of the entry content (if
-- any). You must check the 'entryContent' to see if the entry is of type
-- 'NormalFile'. If it is, the 'NormalFile' gives the content length and you
-- are free to read this much data from the 'Handle'.
--
-- > entry <- Tar.hReadEntryHeader hnd
-- > case Tar.entryContent entry of
-- >   Tar.NormalFile _ size -> do content <- BS.hGet hnd size
-- >                               ...
--
-- Of course you don't have to read it all in one go (as 'hReadEntry' does),
-- you can use any appropriate method to read it incrementally.
--
-- In addition to I\/O errors, this can throw a 'FormatError' if the offset is
-- wrong, or if the file is not valid tar format.
--
-- There is also the lower level operation 'hSeekEntryOffset'.
--
hReadEntryHeader :: Handle -> TarEntryOffset -> IO TarEntry
hReadEntryHeader hnd blockOff = do
    hSeekEntryOffset hnd blockOff
    header <- strHGet hnd 512
    case tarRead header of
      TarNext entry _ -> return entry
      TarFail e       -> throwIO e
      TarDone         -> fail "hReadEntryHeader: impossible"

-- | Set the 'Handle' position to the position corresponding to the given
-- 'TarEntryOffset'.
--
-- This position is where the entry metadata can be read. If you already know
-- the entry has a body (and perhaps know it's length), you may wish to seek to
-- the body content directly using 'hSeekEntryContentOffset'.
--
hSeekEntryOffset :: Handle -> TarEntryOffset -> IO ()
hSeekEntryOffset hnd blockOff =
    hSeek hnd AbsoluteSeek (fromIntegral blockOff * 512)

-- | Set the 'Handle' position to the entry content position corresponding to
-- the given 'TarEntryOffset'.
--
-- This position is where the entry content can be read using ordinary I\/O
-- operations (though you have to know in advance how big the entry content
-- is). This is /only valid/ if you /already know/ the entry has a body (i.e.
-- is a normal file).
--
hSeekEntryContentOffset :: Handle -> TarEntryOffset -> IO ()
hSeekEntryContentOffset hnd blockOff =
    hSeekEntryOffset hnd (blockOff + 1)

-- | This is a low level variant on 'hReadEntryHeader', that can be used to
-- iterate through a tar file, entry by entry.
--
-- It has a few differences compared to 'hReadEntryHeader':
--
-- * It returns an indication when the end of the tar file is reached.
--
-- * It /does not/ move the 'Handle' position to the beginning of the entry
--   content.
--
-- * It returns the 'TarEntryOffset' of the next entry.
--
-- After this action, the 'Handle' position is not in any useful place. If
-- you want to skip to the next entry, take the 'TarEntryOffset' returned and
-- use 'hReadEntryHeaderOrEof' again. Or if having inspected the 'Entry'
-- header you want to read the entry content (if it has one) then use
-- 'hSeekEntryContentOffset' on the original input 'TarEntryOffset'.
--
hReadEntryHeaderOrEof :: Handle -> TarEntryOffset
                      -> IO (Maybe (TarEntry, TarEntryOffset))
hReadEntryHeaderOrEof hnd blockOff = do
    hSeekEntryOffset hnd blockOff
    header <- strHGet hnd 1024
    case tarRead header of
      TarNext entry _ -> let blockOff' = nextEntryOffset entry blockOff
                           in return (Just (entry, blockOff'))
      TarDone         -> return Nothing
      TarFail e       -> throwIO e

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
hSeekEndEntryOffset :: Handle -> Maybe TarIndex -> IO TarEntryOffset
hSeekEndEntryOffset hnd (Just index) = do
    let offset = indexEndEntryOffset index
    hSeekEntryOffset hnd offset
    return offset

hSeekEndEntryOffset hnd Nothing = do
    size <- hFileSize hnd
    if size == 0
      then return 0
      else seekToEnd 0
  where
    seekToEnd offset = do
      mbe <- hReadEntryHeaderOrEof hnd offset
      case mbe of
        Nothing -> do hSeekEntryOffset hnd offset
                      return offset
        Just (_, offset') -> seekToEnd offset'

-------------------------
-- (de)serialisation
--

-- | The 'TarIndex' is compact in memory, and it has a similarly compact
-- external representation.
--
serialise :: TarIndex -> ByteString
serialise (TarIndex stringTable intTrie finalOffset) =
     strConcat [ putWord32be 1 -- format version
       , putWord32be (fromIntegral finalOffset)
       , serialiseStringTable stringTable
       , serialiseIntTrie intTrie ]

-- | Read the external representation back into a 'TarIndex'.
--
deserialise :: ByteString -> Maybe (TarIndex, ByteString)
deserialise bs
  | strLen bs >= 8
  , let ver = readWord32BE bs 0
  , ver == 1
  = do let finalOffset = readWord32BE bs 4
       (stringTable, bs')  <- deserialiseStringTable (strDrop 8 bs)
       (intTrie,     bs'') <- deserialiseIntTrie bs'
       return (TarIndex stringTable intTrie finalOffset, bs'')

  | otherwise = Nothing

serialiseIntTrie :: IntTrie k v -> ByteString
serialiseIntTrie (IntTrie arr) =
    let (_, ixEnd) = bounds arr in
      strConcat [putWord32be (fromIntegral (ixEnd+1))
        , foldr (\n r -> strConcat [putWord32be (fromIntegral n), r]) strEmpty (elems arr)]

deserialiseIntTrie :: ByteString -> Maybe (IntTrie k v, ByteString)
deserialiseIntTrie bs
  | strLen bs >= 4
  , let lenArr   = readWord32BE bs 0
        lenTotal = 4 + 4 * fromIntegral lenArr
  , strLen bs >= 4 + 4 * fromIntegral lenArr
  , let arr = array (0, lenArr-1)
                      [ (i, readWord32BE bs off)
                      | (i, off) <- zip [0..lenArr-1] [4,8 .. lenTotal - 4] ]
        bs' = strDrop lenTotal bs
  = Just (IntTrie arr, bs')

  | otherwise
  = Nothing

serialiseStringTable :: StringTable id -> ByteString
serialiseStringTable (StringTable strs arr) =
      let (_, ixEnd) = bounds arr in
     
      strConcat [  
        putWord32be (fromIntegral (strLen strs))
      , putWord32be (fromIntegral ixEnd + 1)
      , asByteString strs
      , foldr (\n r -> strConcat [ putWord32be (fromIntegral n), r]) strEmpty (elems arr)]

deserialiseStringTable :: ByteString -> Maybe (StringTable id, ByteString)
deserialiseStringTable bs
  | strLen bs >= 8
  , let lenStrs = fromIntegral (readWord32BE bs 0)
        lenArr  = fromIntegral (readWord32BE bs 4)
        lenTotal= 8 + lenStrs + 4 * lenArr
  , strLen bs >= lenTotal
  , let strs = strTake lenStrs (strDrop 8 bs)
        arr  = array (0, lenArr-1)
                       [ (i, readWord32BE bs off)
                       | (i, off) <- zip [0..lenArr-1]
                                         [offArrS,offArrS+4 .. offArrE]
                       ]
        offArrS = 8 + lenStrs
        offArrE = offArrS + 4 * lenArr - 1
        stringTable = StringTable strs arr
        bs'         = strDrop lenTotal bs
  = Just (stringTable, bs')

  | otherwise
  = Nothing

readWord32BE :: ByteString -> Int -> Word32
readWord32BE bs i =
     fromIntegral (nth bs (i + 0)) `shiftL` 24
   + fromIntegral (nth bs (i + 1)) `shiftL` 16
   + fromIntegral (nth bs (i + 2)) `shiftL` 8
   + fromIntegral (nth bs (i + 3))


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
    [ do tarpath <- either fail return (toTarPath isDir relpath)
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
              -> TarPath  -- ^ Path to use for the tar Entry in the archive
              -> IO TarEntry
packFileEntry filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  file    <- openBinaryFile filepath ReadMode
  size    <- hFileSize file
  content <- strHGetContents file
  return (simpleEntry tarpath (NormalFile content (fromIntegral size))) {
    entryPermissions = if executable perms then executableFilePermissions
                                           else ordinaryFilePermissions,
    entryTime = mtime
  }

-- | Construct a tar 'Entry' based on a local directory (but not its contents).
--
-- The only attribute of the directory that is used is its modification time.
-- Directory ownership and detailed permissions are not preserved.
--
packDirectoryEntry :: FilePath -- ^ Full path to find the file on the local disk
                   -> TarPath  -- ^ Path to use for the tar Entry in the archive
                   -> IO TarEntry
packDirectoryEntry filepath tarpath = do
  mtime   <- getModTime filepath
  return (directoryEntry tarpath) {
    entryTime = mtime
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
getModTime path = do
  -- The directory package switched to the new time package
  t <- getModificationTime path
  return . floor . utcTimeToPOSIXSeconds $ t


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
  deriving (Typeable)

instance Show TarFormatError where
  show TruncatedArchive         = "truncated tar archive"
  show ShortTrailer             = "short tar trailer"
  show BadTrailer               = "bad tar trailer"
  show TrailingJunk             = "tar file has trailing junk"
  show ChecksumIncorrect        = "tar checksum error"
  show NotTarFormat             = "data is not in tar format"
  show UnrecognisedTarFormat    = "tar entry not in a recognised format"
  show HeaderBadNumericEncoding = "tar header is malformed (bad numeric encoding)"

instance Exception TarFormatError


-- | Convert a data stream in the tar file format into an internal data
-- structure. Decoding errors are reported by the 'Fail' constructor of the
-- 'Entries' type.
--
-- * The conversion is done lazily.
--
tarRead :: ByteString -> TarEntries TarFormatError
tarRead = tarUnfoldEntries getEntry

getEntry :: ByteString -> Either TarFormatError (Maybe (TarEntry, ByteString))
getEntry bs
  | strLen header < 512 = Left TruncatedArchive

  -- Tar files end with at least two blocks of all '0'. Checking this serves
  -- two purposes. It checks the format but also forces the tail of the data
  -- which is necessary to close the file if it came from a lazily read file.
  | strHead bs == 0 = case strSplitAt 1024 bs of
      (end, trailing)
        | strLen end /= 1024        -> Left ShortTrailer
        | Nothing == strBrk (/= 0) end      -> Left BadTrailer
        | Nothing == strBrk (/= 0) trailing -> Left TrailingJunk
        | otherwise                    -> Right Nothing

  | otherwise  = partial $ do

  case (chksum_, format_) of
    (Ok chksum, _   ) | correctChecksum header chksum -> return ()
    (Ok _,      Ok _) -> Error ChecksumIncorrect
    _                 -> Error NotTarFormat

  -- These fields are partial, have to check them
  format   <- format_;   mode     <- mode_;
  uid      <- uid_;      gid      <- gid_;
  size     <- size_;     mtime    <- mtime_;
  devmajor <- devmajor_; devminor <- devminor_;

  let content = strTake size (strDrop 512 bs)
      padding = (512 - size) `mod` 512
      bs'     = strDrop (512 + size + padding) bs

      entry = TarEntry {
        entryTarPath     = TarPath name prefix,
        tarEntryContent     = case chr (fromEnum typecode) of
                   '\0' -> NormalFile      content size
                   '0'  -> NormalFile      content size
                   '1'  -> HardLink        (LinkTarget linkname)
                   '2'  -> SymbolicLink    (LinkTarget linkname)
                   '3'  -> CharacterDevice devmajor devminor
                   '4'  -> BlockDevice     devmajor devminor
                   '5'  -> Directory
                   '6'  -> NamedPipe
                   '7'  -> NormalFile      content size
                   _    -> OtherEntryType  (chr (fromEnum typecode)) content size,
        entryPermissions = mode,
        entryOwnership   = Ownership uname gname uid gid,
        entryTime        = mtime,
        entryFormat      = format
    }

  return (Just (entry, bs'))

  where
   header = strTake 512 bs

   name       = getString   0 100 header
   mode_      = getOct    100   8 header
   uid_       = getOct    108   8 header
   gid_       = getOct    116   8 header
   size_      = getOct    124  12 header
   mtime_     = getOct    136  12 header
   chksum_    = getOct    148   8 header
   typecode   = getByte   156     header
   linkname   = getString 157 100 header
   magic      = getChars  257   8 header
   uname      = getString 265  32 header
   gname      = getString 297  32 header
   devmajor_  = getOct    329   8 header
   devminor_  = getOct    337   8 header
   prefix     = getString 345 155 header
-- trailing   = getBytes  500  12 header

   format_ = case magic of
    "\0\0\0\0\0\0\0\0" -> return V7Format
    "ustar\NUL00"      -> return UstarFormat
    "ustar  \NUL"      -> return GnuFormat
    _                  -> Error UnrecognisedTarFormat

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

getOct :: Integral a => Int64 -> Int64 -> ByteString -> Partial TarFormatError a
getOct off len = parseOct
               . asString
               . strTakeWhile (\c -> c /= 0  && c /= (fromIntegral (fromEnum ' ') ) )
               . strDropWhile (== (fromIntegral (fromEnum ' ')))
               . getBytes off len
  where
    parseOct "" = return 0
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
    parseOct ('\128':xs) = return (readBytes xs)
    parseOct ('\255':xs) = return (negate (readBytes xs))
    parseOct s  = case readOct s of
      [(x,[])] -> return x
      _        -> Error HeaderBadNumericEncoding

    readBytes = go 0
      where go acc []     = acc
            go acc (x:xs) = go (acc * 256 + fromIntegral (ord x)) xs

getBytes :: Int64 -> Int64 -> ByteString -> ByteString
getBytes off len = strTake len . strDrop off

getByte :: Int64 -> ByteString -> Word8
getByte off bs = nth bs off

getChars :: Int64 -> Int64 -> ByteString -> String
getChars off len = asString . getBytes off len

getString :: Int64 -> Int64 -> ByteString -> String
getString off len = asString . strTakeWhile (/= 0) . getBytes off len

-- These days we'd just use Either, but in older versions of base there was no
-- Monad instance for Either, it was in mtl with an anoying Error constraint.
--
data Partial e a = Error e | Ok a

partial :: Partial e a -> Either e a
partial (Error msg) = Left msg
partial (Ok x)      = Right x

instance Functor (Partial e) where
    fmap = liftM

instance Applicative (Partial e) where
    pure  = return
    (<*>) = ap

instance Monad (Partial e) where
    return        = Ok
    Error m >>= _ = Error m
    Ok    x >>= k = k x
    fail          = error "fail @(Partial e)"

type FileSize  = Int64
-- | The number of seconds since the UNIX epoch
type EpochTime = Int64
type DevMajor  = Int
type DevMinor  = Int
type TypeCode  = Char
type TarPermissions = FileMode

-- | Tar archive entry.
--
data TarEntry = TarEntry {

    -- | The path of the file or directory within the archive. This is in a
    -- tar-specific form. Use 'entryPath' to get a native 'FilePath'.
    entryTarPath :: TarPath,

    -- | The real content of the entry. For 'NormalFile' this includes the
    -- file data. An entry usually contains a 'NormalFile' or a 'Directory'.
    tarEntryContent :: TarEntryContent,

    -- | File permissions (Unix style file mode).
    entryPermissions :: TarPermissions,

    -- | The user and group to which this file belongs.
    entryOwnership :: Ownership,

    -- | The time the file was last modified.
    entryTime :: EpochTime,

    -- | The tar format the archive is using.
    entryFormat :: Format
  }

-- | Native 'FilePath' of the file or directory within the archive.
--
tarEntryPath :: TarEntry -> FilePath
tarEntryPath = fromTarPath . entryTarPath

-- | The content of a tar archive entry, which depends on the type of entry.
--
-- Portable archives should contain only 'NormalFile' and 'Directory'.
--
data TarEntryContent = NormalFile      ByteString FileSize
                  | Directory
                  | SymbolicLink    LinkTarget
                  | HardLink        LinkTarget
                  | CharacterDevice DevMajor DevMinor
                  | BlockDevice     DevMajor DevMinor
                  | NamedPipe
                  | OtherEntryType  TypeCode ByteString FileSize
    deriving (Eq, Ord)

data Ownership = Ownership {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | Numeric owner user id. Should be set to @0@ if unknown.
    ownerId :: Int,

    -- | Numeric owner group id. Should be set to @0@ if unknown.
    groupId :: Int
  }
    deriving (Eq, Ord)

-- | There have been a number of extensions to the tar file format over the
-- years. They all share the basic entry fields and put more meta-data in
-- different extended headers.
--
data Format =

     -- | This is the classic Unix V7 tar format. It does not support owner and
     -- group names, just numeric Ids. It also does not support device numbers.
     V7Format

     -- | The \"USTAR\" format is an extension of the classic V7 format. It was
     -- later standardised by POSIX. It has some restrictions but is the most
     -- portable format.
     --
   | UstarFormat

     -- | The GNU tar implementation also extends the classic V7 format, though
     -- in a slightly different way from the USTAR format. In general for new
     -- archives the standard USTAR/POSIX should be used.
     --
   | GnuFormat
  deriving (Eq, Ord)

-- | @rw-r--r--@ for normal files
ordinaryFilePermissions :: TarPermissions
ordinaryFilePermissions   = 0o0644

-- | @rwxr-xr-x@ for executable files
executableFilePermissions :: TarPermissions
executableFilePermissions = 0o0755

-- | @rwxr-xr-x@ for directories
directoryPermissions :: TarPermissions
directoryPermissions  = 0o0755

-- | An 'Entry' with all default values except for the file name and type. It
-- uses the portable USTAR/POSIX format (see 'UstarHeader').
--
-- You can use this as a basis and override specific fields, eg:
--
-- > (emptyEntry name HardLink) { linkTarget = target }
--
simpleEntry :: TarPath -> TarEntryContent -> TarEntry
simpleEntry tarpath content = TarEntry {
    entryTarPath     = tarpath,
    tarEntryContent     = content,
    entryPermissions = case content of
                         Directory -> directoryPermissions
                         _         -> ordinaryFilePermissions,
    entryOwnership   = Ownership "" "" 0 0,
    entryTime        = 0,
    entryFormat      = UstarFormat
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
fileEntry :: TarPath -> ByteString -> TarEntry
fileEntry name fileContent =
  simpleEntry name (NormalFile fileContent (fromIntegral (strLen fileContent)))

-- | A tar 'Entry' for a directory.
--
-- Entry fields such as file permissions and ownership have default values.
--
directoryEntry :: TarPath -> TarEntry
directoryEntry name = simpleEntry name Directory

--
-- * Tar paths
--

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
-- The rationale presumably was to make it a bit more compatible with old tar
-- programs that only understand the classic format. A classic tar would be
-- able to extract the file name and possibly some dir prefix, but not the
-- full dir prefix. So the files would end up in the wrong place, but that's
-- probably better than ending up with the wrong names too.
--
-- So it's understandable but rather annoying.
--
-- * Tar paths use Posix format (ie @\'/\'@ directory separators), irrespective
--   of the local path conventions.
--
-- * The directory separator between the prefix and name is /not/ stored.
--
data TarPath = TarPath FilePath -- path name, 100 characters max.
                       FilePath -- path prefix, 155 characters max.
  deriving (Eq, Ord)

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
fromTarPath :: TarPath -> FilePath
fromTarPath (TarPath name prefix) = adjustDirectory $
  joinPath $ splitDirectories prefix
                          ++ splitDirectories name
  where
    adjustDirectory | hasTrailingPathSeparator name
                    = addTrailingPathSeparator
                    | otherwise = id

-- | Convert a 'TarPath' to a Unix/Posix 'FilePath'.
--
-- The difference compared to 'fromTarPath' is that it always returns a Unix
-- style path irrespective of the current operating system.
--
-- This is useful to check how a 'TarPath' would be interpreted on a specific
-- operating system, eg to perform portability checks.
--
fromTarPathToPosixPath :: TarPath -> FilePath
fromTarPathToPosixPath (TarPath name prefix) = adjustDirectory $
  joinPath $ splitDirectories prefix
                         ++ splitDirectories name
  where
    adjustDirectory | hasTrailingPathSeparator name
                    = addTrailingPathSeparator
                    | otherwise = id

-- | Convert a 'TarPath' to a Windows 'FilePath'.
--
-- The only difference compared to 'fromTarPath' is that it always returns a
-- Windows style path irrespective of the current operating system.
--
-- This is useful to check how a 'TarPath' would be interpreted on a specific
-- operating system, eg to perform portability checks.
--
fromTarPathToWindowsPath :: TarPath -> FilePath
fromTarPathToWindowsPath (TarPath name prefix) = adjustDirectory $
  joinPath $ splitDirectories prefix
                           ++ splitDirectories name
  where
    adjustDirectory | hasTrailingPathSeparator name
                    = addTrailingPathSeparator
                    | otherwise = id

-- | Convert a native 'FilePath' to a 'TarPath'.
--
-- The conversion may fail if the 'FilePath' is too long. See 'TarPath' for a
-- description of the problem with splitting long 'FilePath's.
--
toTarPath :: Bool -- ^ Is the path for a directory? This is needed because for
                  -- directories a 'TarPath' must always use a trailing @\/@.
          -> FilePath -> Either String TarPath
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
splitLongPath :: FilePath -> Either String TarPath
splitLongPath path =
  case packName nameMax (reverse (splitPath path)) of
    Left err                 -> Left err
    Right (name, [])         -> Right (TarPath name "")
    Right (name, first:rest) -> case packName prefixMax remainder of
      Left err               -> Left err
      Right (_     , (_:_))  -> Left "File name too long (cannot split)"
      Right (prefix, [])     -> Right (TarPath name prefix)
      where
        -- drop the '/' between the name and prefix:
        remainder = init first : rest

  where
    nameMax, prefixMax :: Int
    nameMax   = 100
    prefixMax = 155

    packName _      []     = Left "File name empty"
    packName maxLen (c:cs)
      | n > maxLen         = Left "File name too long"
      | otherwise          = Right (packName' maxLen n [c] cs)
      where n = length c

    packName' maxLen n ok (c:cs)
      | n' <= maxLen             = packName' maxLen n' (c:ok) cs
                                     where n' = n + length c
    packName' _      _ ok    cs  = (joinPath ok, cs)

-- | The tar format allows just 100 ASCII characters for the 'SymbolicLink' and
-- 'HardLink' entry types.
--
newtype LinkTarget = LinkTarget FilePath
  deriving (Eq, Ord)

-- | Convert a native 'FilePath' to a tar 'LinkTarget'. This may fail if the
-- string is longer than 100 characters or if it contains non-portable
-- characters.
--
toLinkTarget   :: FilePath -> Maybe LinkTarget
toLinkTarget path | length path <= 100 = Just (LinkTarget path)
                  | otherwise          = Nothing

-- | Convert a tar 'LinkTarget' to a native 'FilePath'.
--
fromLinkTarget :: LinkTarget -> FilePath
fromLinkTarget (LinkTarget path) = adjustDirectory $
  joinPath $ splitDirectories path
  where
    adjustDirectory | hasTrailingPathSeparator path
                    = addTrailingPathSeparator
                    | otherwise = id

-- | Convert a tar 'LinkTarget' to a Unix/Posix 'FilePath'.
--
fromLinkTargetToPosixPath :: LinkTarget -> FilePath
fromLinkTargetToPosixPath (LinkTarget path) = adjustDirectory $
  joinPath $ splitDirectories path
  where
    adjustDirectory | hasTrailingPathSeparator path
                    = addTrailingPathSeparator
                    | otherwise = id

-- | Convert a tar 'LinkTarget' to a Windows 'FilePath'.
--
fromLinkTargetToWindowsPath :: LinkTarget -> FilePath
fromLinkTargetToWindowsPath (LinkTarget path) = adjustDirectory $
  joinPath $ splitDirectories path
  where
    adjustDirectory | hasTrailingPathSeparator path
                    = addTrailingPathSeparator
                    | otherwise = id

--
-- * Entries type
--

-- | A tar archive is a sequence of entries.
--
-- The point of this type as opposed to just using a list is that it makes the
-- failure case explicit. We need this because the sequence of entries we get
-- from reading a tarball can include errors.
--
-- It is a concrete data type so you can manipulate it directly but it is often
-- clearer to use the provided functions for mapping, folding and unfolding.
--
-- Converting from a list can be done with just @foldr Next Done@. Converting
-- back into a list can be done with 'foldEntries' however in that case you
-- must be prepared to handle the 'Fail' case inherent in the 'Entries' type.
--
-- The 'Monoid' instance lets you concatenate archives or append entries to an
-- archive.
--
data TarEntries e = TarNext TarEntry (TarEntries e)
               | TarDone
               | TarFail e

infixr 5 `TarNext`

-- | This is like the standard 'unfoldr' function on lists, but for 'Entries'.
-- It includes failure as an extra possibility that the stepper function may
-- return.
--
-- It can be used to generate 'Entries' from some other type. For example it is
-- used internally to lazily unfold entries from a 'ByteString'.
--
tarUnfoldEntries :: (a -> Either e (Maybe (TarEntry, a))) -> a -> TarEntries e
tarUnfoldEntries f = unfold
  where
    unfold x = case f x of
      Left err             -> TarFail err
      Right Nothing        -> TarDone
      Right (Just (e, x')) -> TarNext e (unfold x')

-- | This is like the standard 'foldr' function on lists, but for 'Entries'.
-- Compared to 'foldr' it takes an extra function to account for the
-- possibility of failure.
--
-- This is used to consume a sequence of entries. For example it could be used
-- to scan a tarball for problems or to collect an index of the contents.
--
tarFoldEntries :: (TarEntry -> a -> a) -> a -> (e -> a) -> TarEntries e -> a
tarFoldEntries next done fail' = fold
  where
    fold (TarNext e es) = next e (fold es)
    fold TarDone        = done
    fold (TarFail err)  = fail' err

-- | This is like the standard 'map' function on lists, but for 'Entries'. It
-- includes failure as a extra possible outcome of the mapping function.
--
-- If your mapping function cannot fail it may be more convenient to use
-- 'mapEntriesNoFail'
tarMapEntries :: (TarEntry -> Either e' TarEntry) -> TarEntries e -> TarEntries (Either e e')
tarMapEntries f =
  tarFoldEntries (\entry rest -> either (TarFail . Right) (flip TarNext rest) (f entry)) TarDone (TarFail . Left)

-- | Like 'mapEntries' but the mapping function itself cannot fail.
--
tarMapEntriesNoFail :: (TarEntry -> TarEntry) -> TarEntries e -> TarEntries e
tarMapEntriesNoFail f =
  tarFoldEntries (\entry -> TarNext (f entry)) TarDone TarFail

instance Monoid (TarEntries e) where
  mempty      = TarDone
  mappend a b = tarFoldEntries TarNext b TarFail a

instance Functor TarEntries where
  fmap f = tarFoldEntries TarNext TarDone (TarFail . f)

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
tarUnpack :: Exception e => FilePath -> TarEntries e -> IO ()
tarUnpack baseDir entries = unpackEntries [] (checkSecurity entries)
                     >>= emulateLinks

  where
    -- We're relying here on 'checkSecurity' to make sure we're not scribbling
    -- files all over the place.

    unpackEntries _     (TarFail err)      = either throwIO throwIO err
    unpackEntries links TarDone            = return links
    unpackEntries links (TarNext entry es) = case tarEntryContent entry of
      NormalFile file _ -> extractFile path file
                        >> unpackEntries links es
      Directory         -> extractDir path
                        >> unpackEntries links es
      HardLink     link -> (unpackEntries $! saveLink path link links) es
      SymbolicLink link -> (unpackEntries $! saveLink path link links) es
      _                 -> unpackEntries links es --ignore other file types
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

    saveLink path link links = seq (length path)
                             $ seq (length link')
                             $ (path, link'):links
      where link' = fromLinkTarget link

    emulateLinks = mapM_ $ \(relPath, relLinkTarget) ->
      let absPath   = baseDir </> relPath
          absTarget = takeDirectory absPath </> relLinkTarget
       in copyFile absTarget absPath

-- | Create the external representation of a tar archive by serialising a list
-- of tar entries.
--
-- * The conversion is done lazily.
--
tarWrite :: [TarEntry] -> ByteString
tarWrite es = strConcat $ map putEntry es ++ [strReplicate (512*2) 0]

putEntry :: TarEntry -> ByteString
putEntry entry = case tarEntryContent entry of
  NormalFile       content size -> strConcat [ header, content, padding size ]
  OtherEntryType _ content size -> strConcat [ header, content, padding size ]
  _                             -> header
  where
    header       = putHeader entry
    padding size = strReplicate paddingSize 0 
      where paddingSize = fromIntegral (negate size `mod` 512)

putHeader :: TarEntry -> ByteString
putHeader entry =
     asByteString $ take 148 block
  ++ putOct 7 checksum
  ++ ' ' : drop 156 block
--  ++ putOct 8 checksum
--  ++ drop 156 block
  where
    block    = putHeaderNoChkSum entry
    checksum = foldl' (\x y -> x + ord y) 0 block

putHeaderNoChkSum :: TarEntry -> String
putHeaderNoChkSum TarEntry {
    entryTarPath     = TarPath name prefix,
    tarEntryContent     = content,
    entryPermissions = permissions,
    entryOwnership   = ownership,
    entryTime        = modTime,
    entryFormat      = format
  } =

  concat
    [ putString  100 $ name
    , putOct       8 $ permissions
    , putOct       8 $ ownerId ownership
    , putOct       8 $ groupId ownership
    , putOct      12 $ contentSize
    , putOct      12 $ modTime
    , fill         8 $ ' ' -- dummy checksum
    , putChar8       $ typeCode
    , putString  100 $ linkTarget
    ] ++
  case format of
  V7Format    ->
      fill 255 '\NUL'
  UstarFormat -> concat
    [ putString    8 $ "ustar\NUL00"
    , putString   32 $ ownerName ownership
    , putString   32 $ groupName ownership
    , putOct       8 $ deviceMajor
    , putOct       8 $ deviceMinor
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  GnuFormat -> concat
    [ putString    8 $ "ustar  \NUL"
    , putString   32 $ ownerName ownership
    , putString   32 $ groupName ownership
    , putGnuDev    8 $ deviceMajor
    , putGnuDev    8 $ deviceMinor
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  where
    (typeCode, contentSize, linkTarget,
     deviceMajor, deviceMinor) = case content of
       NormalFile      _ size            -> ('0' , size, [],   0,     0)
       Directory                         -> ('5' , 0,    [],   0,     0)
       SymbolicLink    (LinkTarget link) -> ('2' , 0,    link, 0,     0)
       HardLink        (LinkTarget link) -> ('1' , 0,    link, 0,     0)
       CharacterDevice major minor       -> ('3' , 0,    [],   major, minor)
       BlockDevice     major minor       -> ('4' , 0,    [],   major, minor)
       NamedPipe                         -> ('6' , 0,    [],   0,     0)
       OtherEntryType  code _ size       -> (code, size, [],   0,     0)

    putGnuDev w n = case content of
      CharacterDevice _ _ -> putOct w n
      BlockDevice     _ _ -> putOct w n
      _                   -> replicate w '\NUL'

-- * TAR format primitive output

type FieldWidth = Int

putString :: FieldWidth -> String -> String
putString n s = take n s ++ fill (n - length s) '\NUL'

--TODO: check integer widths, eg for large file sizes
putOct :: (Integral a, Show a) => FieldWidth -> a -> String
putOct n x =
  let octStr = take (n-1) $ showOct x ""
   in fill (n - length octStr - 1) '0'
   ++ octStr
   ++ putChar8 '\NUL'

putChar8 :: Char -> String
putChar8 c = [c]

fill :: FieldWidth -> Char -> String
fill n c = replicate n c
-- | A compact mapping from sequences of small nats to nats.
--
newtype IntTrie k v = IntTrie (UArray Word32 Word32)
    deriving (Eq, Show, Typeable)


-- Compact, read-only implementation of a trie. It's intended for use with file
-- paths, but we do that via string ids.

-- Each node has a size and a sequence of keys followed by an equal length
-- sequnce of corresponding entries. Since we're going to flatten this into
-- a single array then we will need to replace the trie structure with pointers
-- represented as array offsets.

-- Each node is a pair of arrays, one of keys and one of Either value pointer.
-- We need to distinguish values from internal pointers. We use a tag bit:
--
tagLeaf, tagNode, untag :: Word32 -> Word32
tagLeaf = id
tagNode = flip setBit   31
untag   = flip clearBit 31

isNode :: Word32 -> Bool
isNode = flip testBit 31

-- So the overall array form of the above trie is:
--
-- offset:   0   1    2    3   4  5  6    7    8     9     10  11  12
-- array:  [ 1 | N1 | 3 ][ 3 | 2, 3, N4 | 512, 2048, 10 ][ 1 | 5 | 4096 ]
--                     \__/                           \___/

-------------------------------------
-- Decoding the trie array form
--

completionsFrom :: (Enum k, Enum v) => IntTrie k v -> Word32 -> Completions k v
completionsFrom trie@(IntTrie arr) nodeOff =
    [ (word32ToEnum (untag key), next)
    | keyOff <- [keysStart..keysEnd]
    , let key   = arr ! keyOff
          entry = arr ! (keyOff + nodeSize)
          next | isNode key = Completions (completionsFrom trie entry)
               | otherwise  = TrieEntry (word32ToEnum entry)
    ]
  where
    nodeSize  = arr ! nodeOff
    keysStart = nodeOff + 1
    keysEnd   = nodeOff + nodeSize

-------------------------------------
-- Toplevel trie array construction
--

-- So constructing the 'IntTrie' as a whole is just a matter of stringing
-- together all the bits

-- | Build an 'IntTrie' from a bunch of (key, value) pairs, where the keys
-- are sequences.
--
trieConstruct :: (Ord k, Enum k, Enum v) => [([k], v)] -> IntTrie k v
trieConstruct = IntTrie . mkArray . flattenTrie . mkTrie

mkArray :: [Word32] -> UArray Word32 Word32
mkArray xs = listArray (0, fromIntegral (length xs) - 1) xs


---------------------------------
-- Looking up in the trie array
--

data TrieLookup k  v = TrieEntry v | Completions (Completions k v) deriving Show
type Completions k  v = [(k, TrieLookup k v)]

trieLookup :: IntTrie Int TarEntryOffset -> [Int] -> Maybe (TrieLookup Int TarEntryOffset)
trieLookup trie@(IntTrie arr) = go 0
  where
    go :: Word32 -> [Int] -> Maybe (TrieLookup Int TarEntryOffset)
    go nodeOff []     = Just (completions nodeOff)
    go nodeOff (k:ks) = case search nodeOff (tagLeaf k') of
      Just entryOff
        | null ks   -> Just (entry entryOff)
        | otherwise -> Nothing
      Nothing       -> case search nodeOff (tagNode k') of
        Nothing       -> Nothing
        Just entryOff -> go (arr ! entryOff) ks
      where
        k' = enumToWord32 k

    entry       entryOff = TrieEntry (word32ToEnum (arr ! entryOff))
  
    completions nodeOff  = Completions (completionsFrom trie  nodeOff)

    search :: Word32 -> Word32 -> Maybe Word32
    search nodeOff key = fmap (+nodeSize) (bsearch keysStart keysEnd key)
      where
        nodeSize  = arr ! nodeOff
        keysStart = nodeOff + 1
        keysEnd   = nodeOff + nodeSize

    bsearch :: Word32 -> Word32 -> Word32 -> Maybe Word32
    bsearch a b key
      | a > b     = Nothing
      | otherwise = case compare key (arr ! mid) of
          LT -> bsearch a (mid-1) key
          EQ -> Just mid
          GT -> bsearch (mid+1) b key
      where mid = (a + b) `div` 2

enumToWord32 :: Enum n => n -> Word32
enumToWord32 = fromIntegral . fromEnum

word32ToEnum :: Enum n => Word32 -> n
word32ToEnum = toEnum . fromIntegral


-------------------------
-- Intermediate Trie type
--

-- The trie node functor
data TrieNodeF k v x = Leaf k v | Node k x deriving (Eq, Show)

instance Functor (TrieNodeF k v) where
  fmap _ (Leaf k v) = Leaf k v
  fmap f (Node k x) = Node k (f x)

-- The trie functor
type TrieF k v x = [TrieNodeF k v x]

-- Trie is the fixpoint of the 'TrieF' functor
newtype Trie  k v   = Trie (TrieF k v (Trie k v)) deriving (Eq, Show)


unfoldTrieNode :: (s -> TrieNodeF k v [s]) -> s -> TrieNodeF k v (Trie k v)
unfoldTrieNode f = fmap (unfoldTrie f) . f

unfoldTrie :: (s -> TrieNodeF k v [s]) -> [s] -> Trie k v
unfoldTrie f = Trie . map (unfoldTrieNode f)

{-
trieSize :: Trie k v -> Int
trieSize (Trie ts) = 1 + sum (map trieNodeSize ts)

trieNodeSize :: TrieNodeF k v (Trie k v) -> Int
trieNodeSize (Leaf _ _) = 2
trieNodeSize (Node _ t) = 2 + trieSize t
-}

---------------------------------
-- Building and flattening Tries
--

-- | A list of key value pairs. The keys must be distinct and non-empty.
type Paths k v = [([k], v)]


mkTrie :: Ord k => Paths k v -> Trie k v
mkTrie = unfoldTrie (fmap split) . split
       . sortBy (compare `on` fst)
       . filter (not . null . fst)
  where
    split :: Eq k => Paths k v -> TrieF k v (Paths k v)
    split = map mkGroup . groupBy ((==) `on` (head . fst))
      where
        mkGroup = \ksvs@((k0:_,v0):_) ->
          case [ (ks, v) | (_:ks, v) <- ksvs, not (null ks) ] of
            []    -> Leaf k0 v0
            ksvs' -> Node k0 ksvs'

type Offset = Int

-- This is a breadth-first traversal. We keep a list of the tries that we are
-- to write out next. Each of these have an offset allocated to them at the
-- time we put them into the list. We keep a running offset so we know where
-- to allocate next.
--
flattenTrie :: (Enum k, Enum v) => Trie k v -> [Word32]
flattenTrie trie = go (queue [trie]) (size trie)
  where
    size (Trie tns) = 1 + 2 * length tns

    go :: (Enum k, Enum v) => TarQ (Trie k v) -> Offset -> [Word32]
    go todo offset =
      case dequeue todo of
        Nothing                   -> []
        Just (Trie tnodes, tries) ->
            flat ++ go (tries `enqueue` tries') offset'
          where
            count = length tnodes
            flat   = fromIntegral count : keys ++ values
            (keys, values) = unzip (sortBy (compare `on` fst) keysValues)
            (keysValues, tries', offset') = doNodes offset [] [] tnodes

    doNodes off kvs ts' []       = (kvs, reverse ts', off)
    doNodes off kvs ts' (tn:tns) = case tn of
      Leaf k v -> doNodes off            (leafKV k v  :kvs)    ts'  tns
      Node k t -> doNodes (off + size t) (nodeKV k off:kvs) (t:ts') tns

    leafKV k v = (tagLeaf (enum2Word32 k), enum2Word32 v)
    nodeKV k o = (tagNode (enum2Word32 k), int2Word32  o)

data TarQ a = TarQ [a] [[a]]

queue :: [a] -> TarQ a
queue xs = TarQ xs []

enqueue :: TarQ a -> [a] -> TarQ a
enqueue (TarQ front  back) [] = TarQ front       back
enqueue (TarQ front  back) xs = TarQ front (xs : back)

dequeue :: TarQ a -> Maybe (a, TarQ a)
dequeue (TarQ (x:xs) back)    = Just (x, TarQ xs back)
dequeue (TarQ []     back)    = case concat (reverse back) of
                               x:xs -> Just (x, TarQ xs [])
                               []   -> Nothing

int2Word32 :: Int -> Word32
int2Word32 = fromIntegral

enum2Word32 :: Enum n => n -> Word32
enum2Word32 = int2Word32 . fromEnum


-------------------------
-- Correctness property
--


-- | An effecient mapping from strings to a dense set of integers.
--
data StringTable id = StringTable
                        ByteString          -- all the strings concatenated
                        (UArray Int Word32)  -- offset table
  deriving (Eq, Show, Typeable)

-- | Look up a string in the token table. If the string is present, return
-- its corresponding index.
--
tarLookup :: StringTable Int -> String -> Maybe Int
tarLookup (StringTable bs tbl) str =
    binarySearch 0 (topBound-1) (asByteString str)
  where
    (0, topBound) = bounds tbl

    binarySearch a b key
      | a > b     = Nothing
      | otherwise = case compare key (index' bs tbl mid) of
          LT -> binarySearch a (mid-1) key
          EQ -> Just (toEnum mid)
          GT -> binarySearch (mid+1) b key
      where mid = (a + b) `div` 2

index' :: ByteString -> UArray Int Word32 -> Int -> ByteString
index' bs tbl i = strTake len . strDrop start $ bs
  where
    start, end, len :: Int
    start = fromIntegral (tbl ! i)
    end   = fromIntegral (tbl ! (i+1))
    len   = end - start


-- | Given the index of a string in the table, return the string.
--
tarIndex :: StringTable Int -> Int -> String
tarIndex (StringTable bs tbl) = asString . index' bs tbl 


-- | Given a list of strings, construct a 'StringTable' mapping those strings
-- to a dense set of integers.
--
construct :: Enum id => [String] -> StringTable id
construct strs = StringTable bs tbl
  where
    bs      = asByteString (concat strs')
    tbl     = array (0, length strs') (zip [0..] offsets)
    offsets = scanl (\off str -> off + fromIntegral (length str)) 0 strs'
    strs'   = map head . group . sort $ strs


