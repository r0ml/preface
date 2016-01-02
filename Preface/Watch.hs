{-# LANGUAGE QuasiQuotes #-}

module Preface.Watch

where

import Preface.Imports
import Preface.FFITemplates
import Bindings.CoreFoundation

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

[enum|FSEventStreamCreateFlag
   UseCFTypes 0x00000001
   NoDefer 0x00000002
   WatchRoot  0x00000004
   IgnoreSelf 0x00000008
   FileEvents 0x00000010 
|]

[enum|FSEventStreamEventFlag
   MustScanSubDirs 0x00000001
   UserDropped 0x00000002
   KernelDropped 0x00000004
   EventIdsWrapped 0x00000008
   HistoryDone 0x00000010
   RootChanged 0x00000020
   Mount 0x00000040
   Unmount 0x00000080
   ItemCreated 0x00000100
   ItemRemoved 0x00000200
   ItemInodeMetaMod 0x00000400
   ItemRenamed 0x00000800
   ItemModified 0x00001000
   ItemFinderInfoMod 0x00002000
   ItemChangeOwner 0x00004000
   ItemXattrMod 0x00008000
   ItemIsFile 0x00010000
   ItemIsDir 0x00020000
   ItemIsSymlink 0x00040000
   OwnEvent 0x00080000
   ItemIsHardlink 0x00100000
   ItemIsLastHardlink 0x00200000
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

