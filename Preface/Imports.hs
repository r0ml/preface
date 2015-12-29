{- | GHC 7.10 comes with the following builtin libraries:

array  -- Data.Array
base
bin-pacakge-db -- this is an internal package, and should not be used
binary
bytestring
Cabal
containers
deepseq
directory
filepath
ghc
ghc-prim  -- This API is still considered experimental and prone to change
haskeline
hoopl
hpc
integer-gmp
pretty
process
template-haskell
terminfo
time
transformers
unix
Win32
xhtml

    The first thing this preface does is to include most of the functions in most of these packages
to provide a single import / namespace containing all of the commonly used builtin functions.
-}

module Preface.Imports ( module X, module Preface.Imports ) where

import qualified Prelude as XX
import Prelude as X hiding ( (++) )

import qualified Control.Applicative as XX
import Control.Applicative as X ((<|>), optional, liftA, liftA2, liftA3)

import Control.Concurrent as X (ThreadId, myThreadId, forkIO, forkFinally, forkIOWithUnmask,
                                      killThread, throwTo, forkOn, forkOnWithUnmask,
                                      getNumCapabilities, setNumCapabilities, threadCapability,
                                      yield, threadDelay, threadWaitRead, threadWaitWrite,
                                      threadWaitReadSTM, threadWaitWriteSTM,
                                      rtsSupportsBoundThreads, forkOS, isCurrentThreadBound,
                                      runInBoundThread, runInUnboundThread, mkWeakThreadId,
                                Chan, newChan, writeChan, readChan, dupChan,
                                      getChanContents, writeList2Chan,
                                QSem, newQSem, waitQSem, signalQSem,
                                QSemN, newQSemN, waitQSemN, signalQSemN,
                                MVar, newEmptyMVar, newMVar, takeMVar, putMVar, readMVar, swapMVar,
                                      tryTakeMVar, tryPutMVar, isEmptyMVar, withMVar, withMVarMasked,
                                      modifyMVar_, modifyMVar, modifyMVarMasked_, modifyMVarMasked,
                                      tryReadMVar, mkWeakMVar)
import Control.Concurrent.STM as X (TChan, atomically,
     writeTChan, readTChan, newTChanIO, newTChan)

import Control.Concurrent.Async as X
import Control.DeepSeq as X (NFData(..), ($!!), force, deepseq)

import Control.Exception as X (Exception(..), SomeException,
                               IOException, ArithException, ArrayException, AssertionFailed,
                               SomeAsyncException, AsyncException, NonTermination, NestedAtomically,
                               BlockedIndefinitelyOnMVar, BlockedIndefinitelyOnSTM,
                               AllocationLimitExceeded, Deadlock, NoMethodError, PatternMatchFail,
                               RecConError, RecSelError, RecUpdError, ErrorCall, 
                               assert, AssertionFailed,
                               asyncExceptionToException, asyncExceptionFromException,
                               throw, throwIO,  
                               catch, catches, Handler(..), catchJust, handle, handleJust,
                               try, tryJust, evaluate, mapException,
                               mask, mask_, uninterruptibleMask, uninterruptibleMask_,
                               getMaskingState, allowInterrupt, MaskingState(..),
                               assert, bracket, bracket_, bracketOnError, finally, onException)
import Control.Monad as X (
        forM, forM_, 
        (>=>), (<=<), forever, void, 
        join, msum, mplus, mfilter, filterM, mapAndUnzipM, zipWithM, zipWithM_, foldM, foldM_,
        replicateM, replicateM_, guard, when, unless,
        liftM, liftM2, liftM3, liftM4, liftM5, ap, (<$!>) )

import Control.Monad.ST as X ( ST, runST, fixST, stToIO, RealWorld )

import Data.Array.IArray as X (Array, Ix(..), IArray(..), listArray, (!), bounds, array, elems)
import Data.Array.Unboxed as X (UArray)
import Data.Binary as X (Binary)

import Data.Bits as X (Bits(..), (.&.), shiftR, shiftL, (.|.), complement, xor, rotateR, rotateL,
                       testBit, setBit, finiteBitSize)
import Data.ByteString as X (ByteString, useAsCString, packCString, packCStringLen)
import Data.ByteString.Internal as X (toForeignPtr, fromForeignPtr, mallocByteString)
import Data.ByteString.Unsafe as X (unsafeUseAsCStringLen)

import Data.Char as X (chr, ord, toLower, isDigit, isAlpha, isAscii
                       , isHexDigit {- isSpace, -}
                       , isUpper, isLower, toUpper, toLower, isAlphaNum
                       , isSymbol, isPunctuation, intToDigit, digitToInt)
import Data.Either as X (isLeft, isRight, lefts, rights, partitionEithers)
import Data.Function as X (on)
import Data.Int as X (Int8, Int16, Int32, Int64)
import Data.IORef as X (IORef , newIORef, readIORef, writeIORef, 
    atomicWriteIORef, atomicModifyIORef', modifyIORef, modifyIORef', mkWeakIORef)
import Data.Ord as X (comparing)
import Data.List as X (sort, sortBy, nub, inits, tails, unfoldr, foldl' 
                      , find, transpose, zip4, intersect, partition
                      , isPrefixOf, isSuffixOf, isInfixOf, (\\)
                      , stripPrefix, intersperse, elemIndex, group, groupBy )
import Data.Map as X (Map, assocs, findWithDefault, keys)
import Data.Maybe as X (listToMaybe, isJust, fromMaybe, isNothing, fromJust,
                        mapMaybe, catMaybes)
import Data.Set as X (Set, union, member)
import Data.String as X (IsString, fromString)
import Data.Text as X (Text)

import Data.Time as X (UTCTime(..), fromGregorian, secondsToDiffTime, getCurrentTime, 
        addUTCTime, iso8601DateFormat, rfc822DateFormat, diffUTCTime)
import Data.Time.Clock as X (NominalDiffTime)
import Data.Time.Clock.POSIX as X (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format as X (formatTime, parseTimeM, readsTime, TimeLocale, defaultTimeLocale)
import Data.Tuple as X (swap)
import Data.Typeable as X (Typeable, typeOf )

import Data.Version as X (Version(..), showVersion)

import Data.Word as X (Word8, Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)

-- all of the C error codes (e.g. ePIPE, eTIME, eREMOTE, etc. should be 
-- converted into an algebraic type)
import Foreign.C.Error as X (getErrno, Errno(..), ePIPE)
import Foreign.C.Types as X (CInt(..), CUInt(..), CChar(..), CUShort(..)
          , CDouble(..), CFloat(..), CShort(..), CLong(..), CULong(..)
          , CUChar(..)
          , CTime(..), CSize(..) )
import Foreign.C.String as X (CString, CStringLen, withCString, peekCString, peekCStringLen)
import Foreign.Concurrent as X (addForeignPtrFinalizer)
import Foreign.Marshal as X (alloca, allocaBytes, allocaBytesAligned, mallocArray, allocaArray, fromBool,copyBytes, free)
import Foreign.Marshal.Utils as X (new, with)
import Foreign.Marshal.Array as X (peekArray, pokeArray, withArray, newArray0, newArray)
import Foreign.Ptr as X (Ptr, FunPtr, plusPtr, castPtr, castFunPtr, nullPtr, castFunPtrToPtr)
import Foreign.ForeignPtr as X (withForeignPtr, mallocForeignPtr, mallocForeignPtrBytes, castForeignPtr, newForeignPtr, newForeignPtr_, ForeignPtr)
import Foreign.Storable as X (Storable(..), peek, poke)

import GHC.Generics as X hiding (Arity, Fixity)
import GHC.Exts as X (sortWith, groupWith)
import GHC.IO.Exception as X (IOException(..), IOErrorType(..) )
import GHC.IO.Handle as X (hDuplicate)

import Language.Haskell.TH as X hiding (Arity, Fixity)
import Language.Haskell.TH.Quote as X
import Language.Haskell.TH.Syntax as X hiding(Infix)

import Numeric as X (readHex, readSigned, readDec, readFloat
                    , showHex, showOct, readOct
                    , showIntAtBase )

import System.Directory as X (canonicalizePath, doesDirectoryExist, doesFileExist
                       , getDirectoryContents, getAppUserDataDirectory
                       , createDirectoryIfMissing, copyFile, getModificationTime
                       , getHomeDirectory, getCurrentDirectory
                       , removeDirectoryRecursive, createDirectory, removeFile
                       , getPermissions, Permissions(..)
                       , getTemporaryDirectory
                       , setCurrentDirectory )
import System.FilePath as X (addExtension, (</>), replaceExtension, takeDirectory
                        , takeBaseName, takeExtension
                        , dropFileName, takeFileName, joinPath
                        , splitPath, splitExtension, splitDirectories, splitSearchPath
                        , isPathSeparator, isSearchPathSeparator
                        , normalise, isAbsolute, isValid
                        , addTrailingPathSeparator, hasTrailingPathSeparator
                        , dropTrailingPathSeparator )
-- import System.Locale as X (TimeLocale, defaultTimeLocale)
import System.CPUTime as X (getCPUTime)
import System.IO as X (Handle, hClose, hFlush, hPutStrLn, hPutStr, hGetLine, hGetContents
        , hGetBuf, hGetChar, hSetBuffering, BufferMode(..)
        , openFile, withFile, IOMode(..), stdin, stderr, stdout
        , hSeek, SeekMode(..), hFileSize
        , openBinaryFile )
import System.IO.Error as X ( isEOFError, ioeGetErrorType
                            , isAlreadyExistsError )
import System.IO.Unsafe as X (unsafePerformIO, unsafeDupablePerformIO, unsafeInterleaveIO)

import System.Environment as X (getArgs, getEnvironment, lookupEnv, setEnv, getEnv, getProgName)
import System.Exit as X (ExitCode(..), exitSuccess, exitFailure, exitWith )

import System.Process as X (StdStream(..), proc, createProcess, waitForProcess,
  -- Does this work
  readProcessWithExitCode, runInteractiveProcess, terminateProcess,
  CreateProcess(..), ProcessHandle, rawSystem )

import System.Posix as X (getFileStatus, fileSize, getSymbolicLinkStatus, isSymbolicLink)
import System.Posix.Signals as X (installHandler, sigTERM, sigINT)
import System.Posix.Types as X (Fd(..), FileMode(..) )

import System.Random as X (newStdGen, mkStdGen, Random(..), RandomGen(..), StdGen)

import System.Timeout as X (timeout)

-- import Text.Regex.TDFA as X ((=~), (=~~))
import Text.Printf as X (printf)


import Network.Socket as X (sClose, withSocketsDo, Socket(..), SockAddr(..),
     addrAddress, defaultProtocol, SocketType(..), Family(..), getAddrInfo, defaultHints,
     getSocketOption, ShutdownCmd(..), shutdown,
     socket, addrSocketType, addrFamily, fdSocket, mkSocket, getPeerName, getSocketName
   , socketToHandle ) 


import Network as X (PortID(..), listenOn )
-- import Network.Mime as X (defaultMimeLookup)
-- import Network.URI as X (unEscapeString)

import Debug.Trace as X (trace, traceShow, traceIO)

infixr 5 ++
-- | This construction allows me to document Prelude functions
(++) :: [a] -> [a] -> [a]
(++) = (XX.++)

-- | This is the same thing as `<*>` with the arguments reversed
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = (XX.<**>)

