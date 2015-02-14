
module Preface (module X) where

import Control.Applicative as X ((<|>),(<$>),(<*),(*>),(<*>),(<$))
import Control.Concurrent as X (forkIO, forkOS, ThreadId, threadDelay, killThread, 
                                Chan, newChan, writeChan, readChan, getChanContents,
                                QSem, newQSem, waitQSem, signalQSem,
                                MVar, newEmptyMVar, takeMVar, putMVar, newMVar, withMVar
                               )
import Control.Exception as X (catch, bracket, finally, SomeException, Exception, throwIO)
import Control.Monad as X (mzero, mplus, filterM, msum, forM, foldM, void, join, when, unless, forever)
import Control.Monad.State as X (State, liftM2, liftM, execState)
import Control.Monad.Trans as X (liftIO)

import Data.Binary as X (Binary, Get, Put) 
import Data.Binary as X (get, put) -- I need to export these in order to create Binary classes
import Data.Binary.Get as X (getWord8, getByteString, getWord16be, getWord32be, getWord64be,
                             runGet)
import Data.Binary.Put as X (putWord8, putByteString, putWord16be, putWord32be, putWord64be,
                             runPut)
import Data.Bits as X (Bits(..), (.&.), shiftR, shiftL, (.|.), complement, xor, rotateR, rotateL,
                       testBit, setBit, finiteBitSize)
import Data.ByteString as X (ByteString, useAsCString, packCStringLen)
import Data.ByteString.Unsafe as X (unsafeUseAsCStringLen)

import Data.Char as X (chr, ord, toLower, digitToInt, isDigit, isAlpha,
                       isUpper, isLower, toUpper, toLower, isAlphaNum)
import Data.Int as X (Int8, Int16, Int32, Int64)
import Data.IORef as X (IORef , newIORef, readIORef, writeIORef, 
    atomicWriteIORef, atomicModifyIORef', modifyIORef, modifyIORef', mkWeakIORef)
import Data.Ord as X (comparing)
import Data.List as X (sort, sortBy, inits, tails, unfoldr, foldl', 
                       transpose, zip4, intersect)
import Data.Map as X (Map)
import Data.Maybe as X (listToMaybe, isJust, fromMaybe, isNothing, fromJust, mapMaybe)
import Data.Monoid as X (mconcat, mappend, mempty)
import Data.Set as X (Set, union, member)
import Data.String as X (IsString, fromString)
import Data.Text as X (Text)

import Data.Time as X (UTCTime(..), fromGregorian, secondsToDiffTime, getCurrentTime, iso8601DateFormat)
import Data.Time.Clock as X (NominalDiffTime)
import Data.Time.Clock.POSIX as X (posixSecondsToUTCTime)
import Data.Time.Format as X (formatTime, parseTime, readsTime, TimeLocale, defaultTimeLocale)
import Data.Tuple as X (swap)
import Data.Typeable as X (Typeable)
import Data.Word as X (Word8, Word16, Word32, Word64)

import Foreign.C.Error as X (getErrno, throwErrnoIf, Errno(..))
import Foreign.C.Types as X (CInt(..), CUInt(..), CChar(..), CUShort(..),
                             CDouble(..), CFloat(..), CShort(..), CLong(..), CULong(..), CTime(..) )
import Foreign.C.String as X (CString, withCString, peekCString)
import Foreign.Marshal as X (alloca, allocaBytes, allocaArray, fromBool,copyBytes)
import Foreign.Marshal.Array as X (peekArray)
import Foreign.Ptr as X (Ptr, FunPtr, plusPtr, castPtr, nullPtr, castFunPtrToPtr)
import Foreign.ForeignPtr as X (withForeignPtr, mallocForeignPtr, mallocForeignPtrBytes, castForeignPtr, newForeignPtr, newForeignPtr_, ForeignPtr)
import Foreign.Storable as X (Storable(..), peek, poke)

import GHC.Generics as X

import System.Directory as X (canonicalizePath, doesDirectoryExist, doesFileExist, getDirectoryContents,
                         createDirectoryIfMissing, copyFile, getModificationTime,
                         getHomeDirectory,
                         removeDirectoryRecursive, createDirectory, removeFile )
import System.FilePath as X (addExtension, (</>), replaceExtension, takeDirectory,
                        takeBaseName, takeExtension, takeFileName, joinPath, splitPath,
                        normalise, isAbsolute)
-- import System.Locale as X (TimeLocale, defaultTimeLocale)
import System.IO as X (Handle, hClose, hFlush, hPutStrLn, openFile, IOMode(..), stderr, stdout )
import System.IO.Unsafe as X (unsafePerformIO)

import System.Environment as X (getArgs, getEnvironment, lookupEnv)
import System.Exit as X (ExitCode, exitSuccess, exitFailure, exitWith )
import System.Process as X (StdStream(..), proc, createProcess, waitForProcess,
  readProcessWithExitCode,
  CreateProcess(..))

-- import Text.Regex.TDFA as X ((=~), (=~~))
import Text.Printf as X (printf)


import Network.Socket as X (sClose, withSocketsDo, Socket(..), SockAddr(..),
     addrAddress, defaultProtocol, SocketType(..), Family(..), getAddrInfo, defaultHints,
     socket, addrSocketType, addrFamily, fdSocket, mkSocket ) 

import Network as X (PortID(..), listenOn )
-- import Network.Mime as X (defaultMimeLookup)
import Network.URI as X (unEscapeString)

{-
import qualified Data.ByteString as B
import qualified Data.Map                as M
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P
import qualified Data.Set                as S
import qualified Data.Text as T (pack, unpack, singleton)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
-}

import Stringy as X
import Byter as X

import Debug.Trace as X

import Misc as X
import Math as X

