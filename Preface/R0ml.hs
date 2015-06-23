{-| This is r0ml's extended preface for GHC.
    GHC 7.10 comes with the following builtin libraries:

array
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

module Preface.R0ml (module X, module Preface.R0ml
  , Stringy(..), Chary(..) ) where

import Control.Applicative as X ((<|>),(<$>),(<*),(*>),(<*>),(<$),(<**>), optional,
                                 liftA, liftA2, liftA3, pure)
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
import Control.DeepSeq as X (NFData(..), deepseq, ($!!), force)
import Control.Exception as X (Exception(..), SomeException,
                               IOException, ArithException, ArrayException, AssertionFailed,
                               SomeAsyncException, AsyncException, NonTermination, NestedAtomically,
                               BlockedIndefinitelyOnMVar, BlockedIndefinitelyOnSTM,
                               AllocationLimitExceeded, Deadlock, NoMethodError, PatternMatchFail,
                               RecConError, RecSelError, RecUpdError, ErrorCall, 
                               asyncExceptionToException, asyncExceptionFromException,
                               throw, throwIO, ioError,  
                               catch, catches, Handler(..), catchJust, handle, handleJust,
                               try, tryJust, evaluate, mapException,
                               mask, mask_, uninterruptibleMask, uninterruptibleMask_,
                               getMaskingState, allowInterrupt, MaskingState(..),
                               assert, bracket, bracket_, bracketOnError, finally, onException)
import Control.Monad as X (
        mapM, mapM_, forM, forM_, sequence, sequence_,
        (=<<), (>=>), (<=<), forever, void, 
        join, msum, mplus, mfilter, filterM, mapAndUnzipM, zipWithM, zipWithM_, foldM, foldM_,
        replicateM, replicateM_, guard, when, unless,
        liftM, liftM2, liftM3, liftM4, liftM5, ap, (<$!>) )
import Control.Monad.IO.Class as X (liftIO)
import Control.Monad.ST as X ( ST, runST, fixST, stToIO, RealWorld )

-- Look at the differences between the Get/Put monads and using a Builder directly
import Data.Binary as X (Binary, Get, Put) 
import Data.Binary as X (get, put, encode, decode, decodeOrFail) 
import Data.Binary.Get as X (getWord8, getByteString, getWord16be, getWord32be, getWord64be,
                             runGet)
import Data.Binary.Put as X (putWord8, putByteString, putWord16be, putWord32be, putWord64be,
                             runPut)
import Data.Bits as X (Bits(..), (.&.), shiftR, shiftL, (.|.), complement, xor, rotateR, rotateL,
                       testBit, setBit, finiteBitSize)
import Data.ByteString as X (ByteString, useAsCString, packCStringLen)
import Data.ByteString.Unsafe as X (unsafeUseAsCStringLen)

import Data.Char as X (chr, ord, toLower, digitToInt, isDigit, isAlpha,
                       isHexDigit,
                       isUpper, isLower, toUpper, toLower, isAlphaNum)
import Data.Either as X (isLeft, isRight, lefts, rights, partitionEithers)
import Data.Int as X (Int8, Int16, Int32, Int64)
import Data.IORef as X (IORef , newIORef, readIORef, writeIORef, 
    atomicWriteIORef, atomicModifyIORef', modifyIORef, modifyIORef', mkWeakIORef)
import Data.Ord as X (comparing)
import Data.List as X (sort, sortBy, nub, inits, tails, unfoldr, foldl', 
                       find, transpose, zip4, intersect)
import Data.Map as X (Map, assocs)
import Data.Maybe as X (listToMaybe, isJust, fromMaybe, isNothing, fromJust,
                        mapMaybe, catMaybes)
import Data.Monoid as X (mconcat, mappend, mempty)
import Data.Set as X (Set, union, member)
import Data.String as X (IsString, fromString)
import Data.Text as X (Text)

import Data.Time as X (UTCTime(..), fromGregorian, secondsToDiffTime, getCurrentTime, 
        addUTCTime, iso8601DateFormat, rfc822DateFormat)
import Data.Time.Clock as X (NominalDiffTime)
import Data.Time.Clock.POSIX as X (posixSecondsToUTCTime)
import Data.Time.Format as X (formatTime, parseTimeM, readsTime, TimeLocale, defaultTimeLocale)
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
import Numeric as X (readHex, readSigned, readDec, readFloat)

import System.Directory as X (canonicalizePath, doesDirectoryExist, doesFileExist, getDirectoryContents,
                         createDirectoryIfMissing, copyFile, getModificationTime,
                         getHomeDirectory,
                         removeDirectoryRecursive, createDirectory, removeFile )
import System.FilePath as X (addExtension, (</>), replaceExtension, takeDirectory,
                        takeBaseName, splitExtension, takeExtension, takeFileName, joinPath, splitPath,
                        normalise, isAbsolute)
-- import System.Locale as X (TimeLocale, defaultTimeLocale)
import System.IO as X (Handle, hClose, hFlush, hPutStrLn, openFile, IOMode(..), stdin, stderr, stdout )
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
-- import Network.URI as X (unEscapeString)

{-
import qualified Data.ByteString as B
import qualified Data.Map                as M
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P
import qualified Data.Set                as S
import qualified Data.Text as T (pack, unpack, singleton)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
-}

import Preface.Stringy as X hiding()
import Preface.Byter as X hiding ()

import Debug.Trace as X

import Preface.Misc as X
import Preface.Math as X

import Preface.Str as X
import Preface.Symbols as X
import Preface.SecureHash as X

import Preface.Xml as X
import Preface.JSONic as X

import Bindings.Curl as X
import Bindings.Posix as X

-- import Data.Vector as X ( (!), Vector )
-- import qualified Data.Vector as V

import Preface.SCGI as X

-- ----------------------------------------
import Distribution.TestSuite as X ( Progress(..), Test, testGroup)
import qualified Distribution.TestSuite as TS

-- data TestInstance  = TS.TestInstance 
--  { testRun :: IO Progress, testName :: String,
--                   testTags :: [String], testOptions :: [OptionDescr],
--                   testSetOptions :: String -> String -> Either String TestInstance }

-- data TestInstance = TestInstance { TS.TestInstance | run -> testRun }

makeTest :: String -> (String -> IO TestResult) -> Test
makeTest nam dotest = 
  let t = TS.TestInstance { TS.run = do { a <- dotest nam; return (Finished (cvt a)) }
      , TS.name = nam, TS.tags = [], TS.options = []
      , TS.setOption = \_  _ -> Right t }
   in TS.Test t
  where cvt TestPass = TS.Pass
        cvt (TestFail a) = TS.Fail a

data TestResult = TestPass | TestFail String

