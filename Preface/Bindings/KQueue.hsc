{-# LANGUAGE ForeignFunctionInterface  #-}

-- | This module contains a low-level binding to the kqueue interface.
-- It stays close to the C API, changing the types to more native
-- Haskell types, but not significantly changing it.
-- See the kqueue man page or the examples in @examples/@ for usage
-- information.
-- For a higher-level binding, see "System.KQueue.HighLevel".
module Preface.Bindings.KQueue
-- low level
  ( KQueue
  , kqueue
  , KEvent (..)
  , Filter (..)
  , Flag (..)
  , FFlag (..)
  , kevent
  , KQueueException
-- high level
  , watchDir
  , EventType (..)
  , Watcher(..)
  ) where

-- #undef __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__
-- #undef __BLOCKS__

#include <sys/event.h>
#include <sys/time.h>
#include <signal.h>

import Foreign.Storable (Storable, poke, peekByteOff, pokeByteOff, sizeOf, alignment, peek)
import Control.Exception (Exception, throwIO)
import Foreign ( maybeWith, with, withArray, alloca)
import Foreign.C.Types (CULong, CUShort, CUInt, CInt(..), CShort)
import Foreign.C.Error (Errno(..), getErrno)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, nullPtr)
import Data.Bits ( (.|.), bit, testBit, finiteBitSize)
import Control.Concurrent (ThreadId)
import Data.Maybe (mapMaybe)
import Data.Time.Clock (NominalDiffTime)
import Data.List (foldl')
import Control.Monad (forever)
-- import Debug.Trace

-- for high level
import System.Posix.IO     (OpenMode (..), openFd, defaultFileFlags)
import System.Posix.Types  (Fd(..))

-- | A kernel event queue.
newtype KQueue = KQueue Fd -- The descriptor

-- | Create a new KQueue.
kqueue :: IO KQueue
kqueue = {- traceShow "kqueue" $ -} KQueue <$> c_kqueue

foreign import ccall unsafe "sys/event.h kqueue" c_kqueue :: IO Fd

foreign import ccall unsafe "signal.h sigprocmask" c_sigprocmask :: CInt -> Ptr CUInt -> Ptr CUInt -> IO CInt

blocktimer :: IO CInt
blocktimer = do
  let trm = bit ((#const SIGALRM) - 1) :: CUInt
  z <- alloca $ \bb -> 
       alloca $ \cc -> do
         poke cc trm
         c_sigprocmask ((#const SIG_BLOCK) :: CInt) cc bb
  putStrLn (show z)
  return z
  
-- | A kernel event.
data KEvent = KEvent
  { ident    :: CULong  -- ^ The identifier for the event, often a file descriptor.
  , evfilter :: Filter  -- ^ The kernel filter (type of event).
  , flags    :: [Flag]  -- ^ Actions to perform on the event.
  , fflags   :: [FFlag] -- ^ Filter-specific flags.
  , fdata    ::  Ptr ()   -- ^ Filter-specific data value.
  , udata    :: Ptr ()  -- ^ User-defined data, passed through unchanged.
  } deriving (Show, Eq)

-- TODO: nicer types for ident, data_ and udata.

data Filter = EvfiltRead | EvfiltWrite | EvfiltAio | EvfiltVnode | EvfiltProc
  | EvfiltSignal | EvfiltTimer | EvfiltUnknown
  deriving (Show, Eq)

instance Enum Filter where 
  fromEnum x = case x of 
    EvfiltRead -> (#const EVFILT_READ)
    EvfiltWrite -> (#const EVFILT_WRITE)
    EvfiltAio -> (#const EVFILT_AIO)
    EvfiltVnode -> (#const EVFILT_VNODE)
    EvfiltProc -> (#const EVFILT_PROC)
    EvfiltSignal -> (#const EVFILT_SIGNAL)
    EvfiltTimer -> (#const EVFILT_TIMER)
    EvfiltUnknown -> undefined
    
  toEnum x = case x of 
    (#const EVFILT_READ) -> EvfiltRead
    (#const EVFILT_WRITE) -> EvfiltWrite
    (#const EVFILT_AIO) -> EvfiltAio
    (#const EVFILT_VNODE) -> EvfiltVnode
    (#const EVFILT_PROC) -> EvfiltProc
    (#const EVFILT_SIGNAL) -> EvfiltSignal
    (#const EVFILT_TIMER) -> EvfiltTimer
    _ -> EvfiltUnknown
    
data Flag = EvAdd | EvEnable | EvDisable | EvDelete | EvReceipt | EvOneshot | EvClear 
  | EvEof | EvError | EvUnknown
  deriving (Show, Eq)

instance Enum Flag where
  fromEnum x = case x of 
    EvAdd -> (#const EV_ADD)
    EvEnable -> (#const EV_ENABLE)
    EvDisable -> (#const EV_DISABLE)
    EvDelete -> (#const EV_DELETE)
    EvReceipt -> (#const EV_RECEIPT)
    EvOneshot -> (#const EV_ONESHOT)
    EvClear -> (#const EV_CLEAR)
    EvEof -> (#const EV_EOF)
    EvError -> (#const EV_ERROR)
    EvUnknown -> undefined
  toEnum x = case x of 
    (#const EV_ADD) -> EvAdd
    (#const EV_ENABLE) -> EvEnable
    (#const EV_DISABLE) -> EvDisable
    (#const EV_DELETE) -> EvDelete
    (#const EV_RECEIPT) -> EvReceipt
    (#const EV_ONESHOT) -> EvOneshot
    (#const EV_CLEAR) -> EvClear
    (#const EV_EOF) -> EvEof
    (#const EV_ERROR) -> EvError
    _ -> EvUnknown
    
data FFlag = NoteDelete | NoteWrite | NoteExtend | NoteAttrib | NoteLink 
  | NoteRename | NoteRevoke | NoteExit | NoteFork | NoteExec | NoteSignal | NoteUnknown
  deriving (Eq, Show)

instance Enum FFlag where
  fromEnum x = case x of 
    NoteDelete -> (#const NOTE_DELETE)
    NoteWrite -> (#const NOTE_WRITE)
    NoteExtend -> (#const NOTE_EXTEND)
    NoteAttrib -> (#const NOTE_ATTRIB)
    NoteLink -> (#const NOTE_LINK)
    NoteRename -> (#const NOTE_RENAME)
    NoteRevoke -> (#const NOTE_REVOKE)
    NoteExit -> (#const NOTE_EXIT)
    NoteFork -> (#const NOTE_FORK)
    NoteExec -> (#const NOTE_EXEC)
    NoteSignal -> (#const NOTE_SIGNAL)
    NoteUnknown -> undefined
  toEnum x = case x of 
    (#const NOTE_DELETE) -> NoteDelete
    (#const NOTE_WRITE) -> NoteWrite
    (#const NOTE_EXTEND) -> NoteExtend
    (#const NOTE_ATTRIB) -> NoteAttrib
    (#const NOTE_LINK) -> NoteLink
    (#const NOTE_RENAME) -> NoteRename
    (#const NOTE_REVOKE) -> NoteRevoke
    (#const NOTE_EXIT) -> NoteExit
    (#const NOTE_FORK) -> NoteFork
    (#const NOTE_EXEC) -> NoteExec
    (#const NOTE_SIGNAL) -> NoteSignal
    _ -> {- trace ("note: "++ show x) -} NoteUnknown

-- | Convert a list of enumeration values to an integer by combining
-- them with bitwise 'or'.
enumToBitmask :: Enum a => [a] -> Int
enumToBitmask = foldl' (.|.) 0 . map fromEnum

-- | Convert an integer to a list of enumeration values by testing
-- each bit, and if set, convert it to an enumeration member.
bitmaskToEnum :: Enum a => Int -> [a]
bitmaskToEnum bm = mapMaybe maybeBit [0 .. finiteBitSize bm - 1]
  where
    maybeBit b | testBit bm b = Just . toEnum . bit $ b
               | otherwise    = Nothing

instance Storable KEvent where
  sizeOf _ = (#const sizeof(struct kevent)) 
  alignment _ = 4
  peek e = KEvent <$>                                     (((#peek struct kevent,ident) e ) :: IO (CULong))
                  <*> fmap (toEnum . fromIntegral)        (((#peek struct kevent, filter) e) :: IO CShort)
                  <*> fmap (bitmaskToEnum . fromIntegral) (((#peek struct kevent, flags) e) :: IO CUShort)
                  <*> fmap (bitmaskToEnum . fromIntegral) (((#peek struct kevent, fflags) e) :: IO CUInt)
                  <*>                                     (((#peek struct kevent, data) e) :: IO (Ptr ()))
                  <*>                                     (((#peek struct kevent, udata) e) :: IO (Ptr ()))
  poke e ev =
    do (#poke struct kevent,ident) e (ident                                   ev :: CULong)
       (#poke struct kevent,filter) e (fromIntegral . fromEnum . evfilter    $ ev :: CShort)
       (#poke struct kevent,flags) e (fromIntegral . enumToBitmask . flags  $ ev :: CUShort)
       (#poke struct kevent,fflags) e ( ((#const NOTE_FFCOPY) :: CUInt) .|. ((fromIntegral . enumToBitmask . fflags $ ev) :: CUInt))
       (#poke struct kevent,data) e (fdata                                   ev :: Ptr ())
       (#poke struct kevent,udata) e (udata                                   ev :: Ptr ())

newtype TimeSpec = TimeSpec NominalDiffTime
  deriving (Show, Eq)

-- TODO: waarom krijg ik geen CTime maar een CLong als seconds bij gebruik van #get/#set?
instance Storable TimeSpec where
  sizeOf _ = (#const sizeof(struct timespec))
  alignment _ = 8
  peek t =  mkTimeSpec
        <$> (\ptr -> peekByteOff ptr 0 :: IO CInt)  t
        <*> ((#peek struct timespec,tv_nsec) t :: IO CInt)
    where
      mkTimeSpec s ns = TimeSpec $ realToFrac s + realToFrac ns/1000000000
  poke t (TimeSpec dt) =
    do (\ptr val -> pokeByteOff ptr 0 (val :: CInt)) t (fromInteger s)
       (#poke struct timespec, tv_nsec) t ((floor . (* 1000000000) $ ns) :: CInt)
    where
      (s, ns) = properFraction dt

foreign import ccall unsafe "sys/event.h kevent" c_kevent :: Fd -> Ptr KEvent -> CInt -> Ptr KEvent -> CInt -> Ptr TimeSpec -> IO CInt

data KQueueException = KQueueException deriving (Show)

instance Exception KQueueException

-- | Add events to monitor, or retrieve events from the kqueue. If an
-- error occurs, will throw a 'KQueueException' if there is no room in
-- the returned event list. Otherwise, will set 'EvError' on the event
-- and add it to the returned event list.
kevent ::  KQueue               -- ^ The kernel queue to operate on.
       -> [KEvent]              -- ^ The list of events to start monitoring, or changes to retrieve.
       -> Int                   -- ^ The maximum number of events to retrieve.
       -> Maybe NominalDiffTime -- ^ Timeout. When nothing, blocks until an event has occurred.
       -> IO [KEvent]           -- ^ A list of events that have occurred.
kevent (KQueue kq) changelist nevents mtimeout =
  withArray changelist $ \chArray ->
  allocaArray nevents  $ \evArray ->
  maybeWith with (TimeSpec <$> mtimeout) $ \timeout ->
    do -- putStrLn ("kevent starts: "++show (sizeOf (head changelist)))
       -- z <- peek chArray
       -- putStrLn (show (kq, z, evArray, nevents, timeout))
       -- let zar = castPtr chArray :: Ptr CUShort
       -- clem <- peekArray 20 zar 
       -- mapM_ (\x -> print (showHex x "") ) clem
       ret <- c_kevent kq chArray (fromIntegral . length $ changelist) evArray (fromIntegral nevents) timeout
       -- res <- (peekArray (fromIntegral ret) evArray)
       -- putStrLn ("kevent has "++show ret++ show res)
       case ret of
         -- Error while processing changelist, and no room in return array.
         -1 -> 
           do
             Errno errno <- getErrno
             if (errno == 4) then do
               putStrLn ("error 4")
               return []
             else do 
               putStrLn ("error: "++show errno)
               throwIO KQueueException
         -- Timeout.
         0  -> return []
         -- Returned n events. Can contain errors. The change that
         -- failed will be in the event list. EV_ERROR will be set on the
         -- event.
         n  -> peekArray (fromIntegral n) evArray

----------------------------------

{-
foreign import ccall unsafe "mainx" c_mainx :: CString -> IO CInt
mainx = do
  s <- newCString "/Users/r0ml/Desktop"
  r <- c_mainx s
  print (show r)
  
foreign import ccall unsafe "__hscore_open" c_open :: CString -> CInt -> IO CInt
openF :: String -> IO Fd
openF path = do
  path <- newCString path 
  fd <- c_open path 0x8000 
  return (Fd fd)
-}

-- | The type of file change that occurred.
data EventType = Changed | Created | Deleted deriving Show

-- | An identifier for the watcher of a file. Allows you to stop
-- watching it later.
newtype Watcher = Watcher ThreadId

-- | Watch a file for changes. The file doesn't have to exist, but the
-- directory it is in, does. Returns immediately. You can stop
-- watching by passing the 'Watcher' to 'stopWatching'.
watchDir :: FilePath -> (KEvent -> IO ()) -> IO ()
watchDir dir callback =
  do kq <- kqueue
     watchDirectory kq dir callback

watchDirectory :: KQueue -> FilePath -> (KEvent -> IO ()) -> IO ()
watchDirectory kq dir callback =
  do -- Event structures for the directory and file to monitor
     putStrLn ("watchDir "++dir)
     dfd <- openFd dir ReadOnly Nothing defaultFileFlags
     putStrLn ("opened " ++ (show dfd))
     let dirEvent = KEvent
           { ident = fromIntegral dfd
           , evfilter = EvfiltVnode
           , flags = [EvAdd, EvClear]
           , fflags = [NoteWrite]
           , fdata = nullPtr
           , udata = nullPtr
           }
         {- evw = KEvent
           { ident = fromIntegral dfd
           , evfilter = EvfiltVnode
           , flags = [EvAdd, EvClear]
           , fflags = [NoteWrite]
           , fdata = nullPtr
           , udata = nullPtr
           }
           -}
         
     -- Add the event(s) to the queue.
     -- let eventsToAdd = [dirEvent] 
     -- _ <- kevent kq [evw] 0 Nothing
     
     _ <- blocktimer
     
     putStrLn "moving on"
     
     -- Forever listen to the events, calling the relevant callbacks.
     forever $ monitorChangesIO kq dirEvent callback 
  
-- | Monitor changes on a file and directory. Calls the callback when
-- the file has changed, or has been created or removed. Returns the
-- file descriptor of the file, if it exists.
monitorChangesIO :: KQueue -> KEvent -> (KEvent -> IO ()) -> IO ()
monitorChangesIO kq dirEvent callback =
  do 
     -- Collect all other events that occurred at the same time.
     chgs <- kevent kq [dirEvent] 1 Nothing -- (Just 1000000) -- (map (setFlag EvAdd) eventsToMonitor)
     -- putStrLn ("cleming:  "++(show chgs))

     -- If the file was written to, we call the relevant callback.
     mapM_ callback chgs
     return ()


