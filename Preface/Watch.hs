{-# LANGUAGE QuasiQuotes #-}

module Preface.Watch

where

import Preface.Imports
import Preface.FFITemplates
import Bindings.Darwin

{-
data EventStream = EventStream CEventStreamRef (FunPtr PathEvent)

data CEventStream
type CEventStreamRef = Ptr CEventStream

type PathEvent = CString -> IO ()
-}

type EventStreamCallbackType = FSEventStreamRef -> Ptr () -> CSize -> Ptr CString -> Ptr CUInt -> Ptr CULong -> IO ()

foreign import ccall "wrapper" mkPathEvent :: EventStreamCallbackType -> IO (FunPtr EventStreamCallbackType)

{-
foreign import ccall unsafe 
    "Watcher.h WatcherCreate" 
    c_WatcherCreate :: Ptr CString -> CInt -> FunPtr (CString -> IO ()) -> IO CEventStreamRef

foreign import ccall unsafe 
    "Watcher.h WatcherRelease" 
    c_WatcherRelease :: CEventStreamRef -> IO ()

startEventStream :: [FilePath] -> (FilePath -> IO ()) -> IO EventStream
startEventStream paths f = do
  cStrs <- mapM newCString paths  
  callback <- mkPathEvent $ \c -> peekCString c >>= f
  watcher <- withArrayLen cStrs $ \count pp -> c_WatcherCreate pp (fromIntegral count) callback
  mapM_ free cStrs
  return $ EventStream watcher callback

stopEventStream :: EventStream -> IO ()
stopEventStream (EventStream ref fp) = do
  c_WatcherRelease ref
  freeHaskellFunPtr fp
-}
{-
 stream = FSEventStreamCreate(NULL,
        &myCallbackFunction,
        callbackInfo,
        pathsToWatch,
        kFSEventStreamEventIdSinceNow, /* Or a previous event ID */
        latency,
        kFSEventStreamCreateFlagNone /* Flags explained in reference */
-}

-- FSEventStreamRef FSEventStreamCreate( CFAllocatorRef allocator, FSEventStreamCallback callback, FSEventStreamContext *context, CFArrayRef pathsToWatch, FSEventStreamEventId sinceWhen, CFTimeInterval latency, FSEventStreamCreateFlags flags)

[enumInt|FSEventStreamCreateFlag
   FSEventStreamCreateFlagUseCFTypes 0x00000001
   FSEventStreamCreateFlagNoDefer 0x00000002
   FSEventStreamCreateFlagWatchRoot  0x00000004
   FSEventStreamCreateFlagIgnoreSelf 0x00000008
   FSEventStreamCreateFlagFileEvents 0x00000010 
|]

[enumInt|FSEventStreamEventFlag
   FSEventStreamEventFlagMustScanSubDirs 0x00000001
   FSEventStreamEventFlagUserDropped 0x00000002
   FSEventStreamEventFlagKernelDropped 0x00000004
   FSEventStreamEventFlagEventIdsWrapped 0x00000008
   FSEventStreamEventFlagHistoryDone 0x00000010
   FSEventStreamEventFlagRootChanged 0x00000020
   FSEventStreamEventFlagMount 0x00000040
   FSEventStreamEventFlagUnmount 0x00000080
   FSEventStreamEventFlagItemCreated 0x00000100
   FSEventStreamEventFlagItemRemoved 0x00000200
   FSEventStreamEventFlagItemInodeMetaMod 0x00000400
   FSEventStreamEventFlagItemRenamed 0x00000800
   FSEventStreamEventFlagItemModified 0x00001000
   FSEventStreamEventFlagItemFinderInfoMod 0x00002000
   FSEventStreamEventFlagItemChangeOwner 0x00004000
   FSEventStreamEventFlagItemXattrMod 0x00008000
   FSEventStreamEventFlagItemIsFile 0x00010000
   FSEventStreamEventFlagItemIsDir 0x00020000
   FSEventStreamEventFlagItemIsSymlink 0x00040000
   FSEventStreamEventFlagOwnEvent 0x00080000
   FSEventStreamEventFlagItemIsHardlink 0x00100000
   FSEventStreamEventFlagItemIsLastHardlink 0x00200000
|]

type FSEventStreamContextRef = Ptr FSEventStreamContext
data FSEventStreamContext
type FSEventStreamRef = Ptr FSEventStream
data FSEventStream

foreign import ccall "FSEventStreamCreate" c_FSEventStreamCreate :: Ptr () ->
    FunPtr a -> FSEventStreamContextRef -> CFArrayRef -> CULong -> CDouble -> CUInt -> IO FSEventStreamRef

fsEventStreamCreate :: [String] -> Double -> [FSEventStreamCreateFlag] -> IO (Chan [FSEvent])
fsEventStreamCreate paths latency flags = do
  let since = -1 -- kFSEventStreamEventIdSinceNow
  callback <- newChan
  ff <- mkPathEvent (fsEventStreamCallback callback)
  psx <- cfArrayOfStrings paths
 
  th <- forkOS $ do
    let q = foldr (.|.) 0 (map fromEnum flags)
    esr <- c_FSEventStreamCreate nullPtr ff nullPtr psx since (realToFrac latency) (fromIntegral q)
    dm <- cfStringCreate "kCFRunLoopDefaultMode"
    rlgc <- c_CFRunLoopGetCurrent
    c_FSEventStreamScheduleWithRunLoop esr rlgc dm
    c_FSEventStreamStart esr
    c_CFRunLoopRun rlgc
  return callback

foreign import ccall "FSEventStreamScheduleWithRunLoop" c_FSEventStreamScheduleWithRunLoop ::
  FSEventStreamRef -> CFRunLoopRef -> CFStringRef -> IO ()

foreign import ccall "FSEventStreamStart" c_FSEventStreamStart :: FSEventStreamRef -> IO ()

foreign import ccall "FSEventStreamRelease" c_FSEventStreamRelease :: FSEventStreamRef -> IO ()


type FSEvent = (String, Int {- [FSEventStreamEventFlag] -} , Int)

fsEventStreamCallback :: Chan [FSEvent] -> FSEventStreamRef -> Ptr () -> CSize -> Ptr CString -> Ptr CUInt -> Ptr CULong -> IO ()
fsEventStreamCallback chan stream cb cnum paths flags ids = do
  let num = fromIntegral cnum
  flagl <- peekArray num flags
  idl <- peekArray num ids
  pathl <- peekArray num paths
  pathx <- mapM peekCString pathl
  let ea = zip3 pathx (map fromIntegral flagl) (map fromIntegral idl)
  writeChan chan ea

{-
void destroyWatch(watch* w) {
  pthread_mutex_lock(&w->mut);
  FSEventStreamStop(w->eventStream);
  FSEventStreamInvalidate(w->eventStream);
  CFRunLoopStop(w->runLoop);
  CFRelease(w->runLoop);
  FSEventStreamRelease(w->eventStream);
  close(w->writefd);
  pthread_mutex_unlock(&w->mut);
  pthread_mutex_destroy(&w->mut);
  free(w);
}
-}

