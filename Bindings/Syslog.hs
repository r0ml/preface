{-# LANGUAGE ForeignFunctionInterface, QuasiQuotes #-}

module Bindings.Syslog ( startSyslog, setlogmask
  , SyslogPriority(..),  SyslogFacility(..),  SyslogOption(..)
  ) where

import Preface.FFITemplates (enumInt)
-- Because it is a QuasiQuoter, @enumIx8@ must be defined in a different file
import Bindings.Util (enumIx8)

import Preface.Imports

-- | The SyslogFacility is set when logging begins
[enumIx8|SyslogFacility
LOG_KERN        0  /* kernel messages */
LOG_USER        1  /* random user-level messages */
LOG_MAIL        2  /* mail system */
LOG_DAEMON      3  /* system daemons */
LOG_AUTH        4  /* authorization messages */
LOG_SYSLOG      5  /* messages generated internally by syslogd */
LOG_LPR         6  /* line printer subsystem */
LOG_NEWS        7  /* network news subsystem */
LOG_UUCP        8  /* UUCP subsystem */
LOG_CRON        9  /* clock daemon */
LOG_AUTHPRIV    10 /* authorization messages (private) */
LOG_FTP         11 /* ftp daemon */
LOG_NETINFO     12 /* NetInfo */
LOG_REMOTEAUTH  13 /* remote authentication/authorization */
LOG_INSTALL     14 /* installer subsystem */
LOG_RAS         15 /* Remote Access Service (VPN / PPP) */

LOG_LOCAL0      16 /* reserved for local use */
LOG_LOCAL1      17 /* reserved for local use */
LOG_LOCAL2      18 /* reserved for local use */
LOG_LOCAL3      19 /* reserved for local use */
LOG_LOCAL4      20 /* reserved for local use */
LOG_LOCAL5      21 /* reserved for local use */
LOG_LOCAL6      22 /* reserved for local use */
LOG_LOCAL7      23 /* reserved for local use */

LOG_LAUNCHD     24 /* launchd - general bootstrap daemon */
  |]

-- | The SyslogOptions are set when logging begins
[enumInt|SyslogOption
 LOG_PID         0x01    /* log the pid with each message */
 LOG_CONS        0x02    /* log on the console if errors in sending */
 LOG_ODELAY      0x04    /* delay open until first syslog() (default) */
 LOG_NDELAY      0x08    /* don't delay open */
 LOG_NOWAIT      0x10    /* don't wait for console forks: DEPRECATED */
 LOG_PERROR      0x20    /* log to stderr as well */
  |]

-- | Each log message identifies its priority.  It is possible to set the allowed
-- syslog priorities, so that the priorities which are not set will be ignored.
[enumInt|SyslogPriority
LOG_EMERG       0       /* system is unusable */
LOG_ALERT       1       /* action must be taken immediately */
LOG_CRIT        2       /* critical conditions */
LOG_ERR         3       /* error conditions */
LOG_WARNING     4       /* warning conditions */
LOG_NOTICE      5       /* normal but significant condition */
LOG_INFO        6       /* informational */
LOG_DEBUG       7       /* debug-level messages */
|]

-- | Sets the active list of @SyslogPriority@.  Any log messages for priorities outside
-- this list will be ignored.  The implementation doesn't care if the syslog was initialized
-- in a different thread.
setlogmask :: [SyslogPriority] -> IO [SyslogPriority]
setlogmask prios = do
  let prio = foldl setBit 0 (map fromEnum prios)
  ps <- c_setlogmask prio
  return $ filter (testBit ps . fromEnum) [LOG_EMERG .. LOG_DEBUG]

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
    openlog = openlog_ [LOG_PID, LOG_PERROR] LOG_USER

    openlog_ :: [SyslogOption] -> SyslogFacility -> String -> IO ()
    openlog_ opts facil ident = do
       let opt = toEnum . sum . map fromEnum $ opts
           fac = toEnum . fromEnum           $ facil
       withCString ident $ \p -> c_openlog p opt fac


foreign import ccall unsafe "openlog" c_openlog :: CString -> CInt -> CInt -> IO ()
foreign import ccall unsafe "setlogmask" c_setlogmask :: CInt -> IO CInt
foreign import ccall unsafe "syslog" c_syslog :: CInt -> CString -> CString -> IO ()

-- foreign import ccall unsafe "closelog" c_closelog :: IO ()
-- closelog :: IO ()
-- closelog = c_closelog

