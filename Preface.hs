
module Preface (module X, module Preface) where

import Control.Applicative as X ((<|>),(<$>),(<*),(*>),(<*>),(<$))
import Control.Concurrent as X (forkIO, forkOS, ThreadId, threadDelay, killThread, 
                                Chan, newChan, writeChan, readChan,
                                QSem, newQSem, waitQSem, signalQSem,
                                MVar, newEmptyMVar, takeMVar, putMVar
                               )
import Control.Exception as X (catch, bracket, finally, SomeException)
import Control.Monad as X (mzero, mplus, filterM, msum, forM, void, join, when, unless, forever)
import Control.Monad.State as X (State, liftM2, liftM, execState)
import Control.Monad.Trans as X (liftIO)

import Data.Binary as X (Binary, Get, Put) 
import Data.Binary as X (get, put) -- I need to export these in order to create Binary classes
import Data.Binary.Get as X (getWord8, getByteString, getWord16be, getWord32be, getWord64be,
                             runGet)
import Data.Binary.Put as X (putWord8, putByteString, putWord16be, putWord32be, putWord64be,
                             runPut)
import Data.Bits as X ((.&.), shiftR, (.|.), complement, xor, rotateL)
import Data.ByteString as X (ByteString, useAsCString, packCStringLen)

import Data.Char as X (isSpace, chr, ord, toLower, digitToInt, isDigit)
import Data.Int as X (Int8, Int16, Int32, Int64)
import Data.IORef as X (IORef , newIORef, readIORef, writeIORef, 
    atomicWriteIORef, atomicModifyIORef', modifyIORef, modifyIORef', mkWeakIORef)
import Data.Ord as X (comparing)
import Data.List as X (intercalate, intersperse, 
	sortBy, isPrefixOf, isSuffixOf, inits, tails, unfoldr, foldl')
import Data.Map as X (Map)
import qualified Data.Map as M
import Data.Maybe as X (listToMaybe, isJust, fromMaybe, isNothing, fromJust)
import Data.Monoid as X (mconcat, mappend, mempty)
import Data.Set as X (Set)
import Data.String as X (IsString, fromString)
import Data.Time as X (UTCTime(..), fromGregorian, secondsToDiffTime, getCurrentTime)
import Data.Time.Format as X (formatTime, parseTime, readsTime)
import Data.Word as X (Word8, Word16, Word32, Word64)

import Foreign.C.Types as X (CInt, CDouble, CFloat, CShort, CLong)
import Foreign.C.String as X (CString, withCString, peekCString)
import Foreign.Marshal as X (alloca, fromBool)
import Foreign.Marshal.Array as X (peekArray)
import Foreign.Ptr as X (Ptr, plusPtr, castPtr, nullPtr)
import Foreign.ForeignPtr as X (withForeignPtr, mallocForeignPtr, castForeignPtr, ForeignPtr)
import Foreign.Storable as X (peek, poke)
import Foreign.Concurrent as X (newForeignPtr)

import System.Directory as X (canonicalizePath, doesDirectoryExist, doesFileExist, getDirectoryContents,
                         createDirectoryIfMissing, copyFile, getModificationTime,
                         getHomeDirectory,
                         removeDirectoryRecursive, createDirectory, removeFile )
import System.FilePath as X (addExtension, (</>), replaceExtension, takeDirectory,
                        takeBaseName, takeExtension, takeFileName, joinPath, splitPath,
                        normalise, isAbsolute)
import System.Locale as X (TimeLocale, defaultTimeLocale)
import System.IO as X (Handle, hClose, hFlush, hPutStrLn, openFile, IOMode(..), stderr, stdout )
import System.IO.Unsafe as X (unsafePerformIO)

import System.Environment as X (getArgs, getEnvironment, getEnv)
import System.Exit as X (ExitCode, exitSuccess, exitFailure, exitWith )
import System.Process as X (StdStream(..), proc, createProcess, waitForProcess,
  CreateProcess(..))

import Text.Regex.TDFA as X ((=~), (=~~))


import qualified Control.Monad.State as State (get, put)
import qualified Control.Monad.State.Class as State (MonadState)

import Data.Text as X (Text)

import qualified Network.Socket.ByteString as S (recv, send)
import qualified Network.Socket as S (accept, connect)
import Network.Socket as X (sClose, withSocketsDo, Socket(..), SockAddr(..),
     addrAddress, defaultProtocol, SocketType(..), Family(..), getAddrInfo, defaultHints,
     socket, addrSocketType, addrFamily, fdSocket, mkSocket ) 

import Network as X (PortID(..), listenOn )
import Network.Mime as X (defaultMimeLookup)
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

import Debug.Trace as X

getState :: (Monad m, State.MonadState s m) => m s 
getState = State.get

putState :: (Monad m, State.MonadState s m) => s -> m ()
putState = State.put

sktRecv :: Socket -> Int -> IO ByteString
sktRecv = S.recv

sktSend :: Socket -> ByteString -> IO Int
sktSend = S.send

sktAccept :: Socket -> IO (Socket, SockAddr)
sktAccept = S.accept

sktConnect :: Socket -> SockAddr -> IO ()
sktConnect = S.connect

lookupWithDefault :: (Eq a) => b -> a -> [(a,b)] -> b
lookupWithDefault d k l = let r = lookup k l in case r of { Nothing -> d ; Just e -> e }

mapInsert :: (Ord k) => k -> a -> Map k a -> Map k a
mapInsert = M.insert

mapLookup :: (Ord k) => k -> Map k a -> Maybe a
mapLookup = M.lookup

mapFromList :: Ord k => [(k, a)] -> Map k a 
mapFromList = M.fromList
