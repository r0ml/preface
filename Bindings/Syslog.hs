{-# LANGUAGE ForeignFunctionInterface, QuasiQuotes #-}

module Bindings.Syslog ( startSyslog, setlogmask
  , SyslogPriority(..),  SyslogFacility(..),  SyslogOption(..)
  ) where

import Preface.FFITemplates (enum)
-- Because it is a QuasiQuoter, @enumIx8@ must be defined in a different file

import Preface.Imports

-- | The SyslogFacility is set when logging begins
[enum|SyslogFacility
KERN        /* kernel messages */
USER        /* random user-level messages */
MAIL        /* mail system */
DAEMON      /* system daemons */
AUTH        /* authorization messages */
SYSLOG      /* messages generated internally by syslogd */
LPR         /* line printer subsystem */
NEWS        /* network news subsystem */
UUCP        /* UUCP subsystem */
CRON        /* clock daemon */
AUTHPRIV    /* authorization messages (private) */
FTP         /* ftp daemon */
NETINFO     /* NetInfo */
REMOTEAUTH  /* remote authentication/authorization */
INSTALL     /* installer subsystem */
RAS         /* Remote Access Service (VPN / PPP) */

LOCAL0      /* reserved for local use */
LOCAL1      /* reserved for local use */
LOCAL2      /* reserved for local use */
LOCAL3      /* reserved for local use */
LOCAL4      /* reserved for local use */
LOCAL5      /* reserved for local use */
LOCAL6      /* reserved for local use */
LOCAL7      /* reserved for local use */

LAUNCHD     /* 24 launchd - general bootstrap daemon */
  |]

-- | The SyslogOptions are set when logging begins
[enum|SyslogOption
 PID         0x01    /* log the pid with each message */
 CONS        0x02    /* log on the console if errors in sending */
 ODELAY      0x04    /* delay open until first syslog() (default) */
 NDELAY      0x08    /* don't delay open */
 NOWAIT      0x10    /* don't wait for console forks: DEPRECATED */
 PERROR      0x20    /* log to stderr as well */
  |]

-- | Each log message identifies its priority.  It is possible to set the allowed
-- syslog priorities, so that the priorities which are not set will be ignored.
[enum|SyslogPriority
EMERG       /* system is unusable */
ALERT       /* action must be taken immediately */
CRIT        /* critical conditions */
ERR         /* error conditions */
WARNING     /* warning conditions */
NOTICE      /* normal but significant condition */
INFO        /* informational */
DEBUG       /* debug-level messages */
|]

-- | Sets the active list of @SyslogPriority@.  Any log messages for priorities outside
-- this list will be ignored.  The implementation doesn't care if the syslog was initialized
-- in a different thread.
setlogmask :: [SyslogPriority] -> IO [SyslogPriority]
setlogmask prios = do
  let prio = foldl setBit 0 (map fromEnum prios)
  ps <- c_setlogmask prio
  return $ filter (testBit ps . fromEnum) [SyslogPriorityEMERG .. SyslogPriorityDEBUG]

-- | returns a function which takes a @SyslogPriority@ and a String to log.
-- The argument is the ident string for this process.
-- This function spawns a logging thread and the returned function writes to a 
-- channel.  The background thread then reads from the channel and calls the system
-- syslog.
startSyslog :: String -> IO (SyslogPriority -> String -> IO ())
startSyslog x = do
  openlog x
  ch <- newChan
  _ <- forkIO $ forever $ do
    ar <- readChan ch
    (uncurry syslog') ar
  return (curry (writeChan ch))
  where
    syslog' :: SyslogPriority -> String -> IO ()
--    syslog' prio msg = withCString msg (\p -> withCString "%s" (\q -> c_syslog (toEnum (fromEnum prio)) q p))
    syslog' = flip withCString . (withCString "%s" .) . flip . c_syslog . toEnum . fromEnum
    openlog :: String -> IO ()
    openlog = openlog_ [SyslogOptionPID, SyslogOptionPERROR] SyslogFacilityUSER

    openlog_ :: [SyslogOption] -> SyslogFacility -> String -> IO ()
    openlog_ opts facil ident = do
       let opt = toEnum . sum . map fromEnum $ opts
           fac = toEnum . fromEnum           $ facil
       withCString ident $ \p -> c_openlog p opt (8*fac)


foreign import ccall unsafe "openlog" c_openlog :: CString -> CInt -> CInt -> IO ()
foreign import ccall unsafe "setlogmask" c_setlogmask :: CInt -> IO CInt
foreign import ccall unsafe "syslog" c_syslog :: CInt -> CString -> CString -> IO ()

-- foreign import ccall unsafe "closelog" c_closelog :: IO ()
-- closelog :: IO ()
-- closelog = c_closelog

