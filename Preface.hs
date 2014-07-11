
module Preface (module X, module Preface) where

import Control.Applicative as X ((<|>),(<$>),(<*),(*>),(<*>),(<$))
import Control.Concurrent as X (forkIO, forkOS, ThreadId,
                                Chan, newChan, writeChan, readChan,
                                QSem, newQSem, waitQSem, signalQSem,
                                MVar, newEmptyMVar, takeMVar, putMVar
                               )
import Control.Exception as X (catch, bracket, SomeException)
import Control.Monad as X (mzero, mplus, filterM, msum, forM, void, join, when, unless, forever)
import Control.Monad.State as X (State, liftM2, liftM, execState)
import Control.Monad.Trans as X (liftIO)

import Data.Binary as X (Binary, Get, Put, get, put)
import Data.Binary.Get as X (getWord8, getByteString, getWord16be, getWord32be, runGet)
import Data.Binary.Put as X (putWord8, putByteString, putWord16be, putWord32be, runPut)
import Data.Bits as X ((.&.), shiftR, (.|.), complement, xor, rotateL)
import Data.ByteString as X (ByteString)
import qualified Data.ByteString as B (concat, null, empty, readFile)

import Data.Char as X (isSpace, chr, ord, toLower, digitToInt, isDigit)
import Data.Int as X (Int8, Int16, Int32, Int64)
import Data.IORef as X (IORef , newIORef, readIORef, writeIORef)
import Data.Ord as X (comparing)
import Data.List as X (intercalate, intersperse, 
	sortBy, isPrefixOf, isSuffixOf, inits, tails, unfoldr, foldl')
import Data.Map as X (Map)
import Data.Maybe as X (listToMaybe, isJust, fromMaybe, isNothing, fromJust)
import Data.Monoid as X (mconcat, mappend, mempty)
import Data.Set as X (Set)
import Data.String as X (IsString, fromString)
import Data.Time as X (UTCTime(..), fromGregorian, secondsToDiffTime, getCurrentTime)
import Data.Time.Format as X (formatTime, parseTime, readsTime)
import Data.Word as X (Word8, Word16, Word32, Word64)

import Foreign.Marshal.Array as X (peekArray)
import Foreign.Ptr as X (Ptr, plusPtr, castPtr)
import Foreign.ForeignPtr as X (withForeignPtr)

import System.Directory as X (canonicalizePath, doesDirectoryExist, doesFileExist, getDirectoryContents,
                         createDirectoryIfMissing, copyFile, getModificationTime,
                         getHomeDirectory)
import System.FilePath as X (addExtension, (</>), replaceExtension, takeDirectory,
                        takeBaseName, takeExtension, takeFileName, joinPath, splitPath,
                        normalise, isAbsolute)
import System.Locale as X (TimeLocale, defaultTimeLocale)
import System.IO as X (Handle, hClose, hFlush, hPutStrLn, openFile, IOMode(..), stderr, stdout )
import System.IO.Unsafe as X (unsafePerformIO)

import System.Environment as X (getArgs, getEnvironment, getEnv)
import System.Exit as X (ExitCode)
import System.Process as X (StdStream(..), proc, createProcess, waitForProcess,
  CreateProcess(..))

import Text.Regex.TDFA as X ((=~), (=~~))


import qualified Control.Monad.State as State (get, put)
import qualified Control.Monad.State.Class as State (MonadState)

import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import qualified Data.Text as T (pack, unpack)

import Data.Text as X (Text)

import qualified Network.Socket.ByteString as S (recv, send)
import qualified Network.Socket as S (accept)
import Network.Socket as X (sClose, withSocketsDo, Socket(..), SockAddr(..) )
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

byteStringToString :: ByteString -> String
byteStringToString = T.unpack . T.decodeUtf8

stringFromByteString :: ByteString -> String
stringFromByteString = byteStringToString


stringToByteString :: String -> ByteString
stringToByteString = T.encodeUtf8 . T.pack

byteStringFromString :: String -> ByteString
byteStringFromString = stringToByteString

sktRecv :: Socket -> Int -> IO ByteString
sktRecv = S.recv

sktSend :: Socket -> ByteString -> IO Int
sktSend = S.send

sktAccept :: Socket -> IO (Socket, SockAddr)
sktAccept = S.accept

stringToText :: String -> Text
stringToText = T.pack

textFromString :: String -> Text
textFromString = T.pack

byteConcat :: [ByteString] -> ByteString
byteConcat = B.concat

byteNull :: ByteString -> Bool
byteNull = B.null

byteEmpty :: ByteString 
byteEmpty = B.empty

byteReadFile :: FilePath -> IO ByteString
byteReadFile = B.readFile

lookupWithDefault :: (Eq a) => b -> a -> [(a,b)] -> b
lookupWithDefault d k l = let r = lookup k l in case r of { Nothing -> d ; Just e -> e }
