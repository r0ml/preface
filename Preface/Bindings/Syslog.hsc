{-# LANGUAGE ForeignFunctionInterface #-}

module Preface.Bindings.Syslog ( startLog,
  openlog,  openlog_,  closelog,  syslog,  setlogmask,  SyslogPriority(..),  SyslogFacility(..),  SyslogOption(..)
  ) where

import Control.Concurrent (newChan, writeChan, getChanContents, forkIO)
import Data.Maybe (fromJust)
import Data.Bits (testBit, setBit)
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString, withCString)
import Data.Tuple (swap)

#include <syslog.h>

data SyslogFacility = LOG_KERN | LOG_USER | LOG_MAIL | LOG_DAEMON | LOG_AUTH | LOG_SYSLOG | LOG_LPR | LOG_NEWS | LOG_UUCP | LOG_CRON
  | LOG_AUTHPRIV | LOG_FTP | LOG_LOCAL0 | LOG_LOCAL1 | LOG_LOCAL2 | LOG_LOCAL3 | LOG_LOCAL4 | LOG_LOCAL5 | LOG_LOCAL6 | LOG_LOCAL7
  deriving (Eq, Show)

facilityMap :: [(SyslogFacility, Int)]
facilityMap = [(LOG_KERN, #const LOG_KERN), (LOG_USER, #const LOG_USER), (LOG_MAIL, #const LOG_MAIL), (LOG_DAEMON, #const LOG_DAEMON),
  (LOG_AUTH, #const LOG_AUTH), (LOG_SYSLOG, #const LOG_SYSLOG), (LOG_LPR, #const LOG_LPR), (LOG_NEWS, #const LOG_NEWS),
  (LOG_UUCP, #const LOG_UUCP),(LOG_CRON, #const LOG_CRON), (LOG_AUTHPRIV, #const LOG_AUTHPRIV), (LOG_FTP, #const LOG_FTP),
  (LOG_LOCAL0, #const LOG_LOCAL0), (LOG_LOCAL1, #const LOG_LOCAL1), (LOG_LOCAL2, #const LOG_LOCAL2), (LOG_LOCAL3, #const LOG_LOCAL3),
  (LOG_LOCAL4, #const LOG_LOCAL4), (LOG_LOCAL5, #const LOG_LOCAL5), (LOG_LOCAL6, #const LOG_LOCAL6), (LOG_LOCAL7, #const LOG_LOCAL7)
  ]

instance Enum SyslogFacility where
  toEnum = fromJust . flip lookup (map swap facilityMap)
  fromEnum = fromJust . flip lookup facilityMap

data SyslogOption = LOG_PID | LOG_CONS | LOG_ODELAY | LOG_NDELAY | LOG_NOWAIT | LOG_PERROR deriving (Eq, Show)

optionMap :: [(SyslogOption, Int)]
optionMap = [(LOG_PID, #const LOG_PID), (LOG_CONS, #const LOG_CONS), (LOG_ODELAY, #const LOG_ODELAY), (LOG_NDELAY, #const LOG_NDELAY),
  (LOG_NOWAIT, #const LOG_NOWAIT), (LOG_PERROR, #const LOG_PERROR)
  ]
instance Enum SyslogOption where
  toEnum = fromJust . flip lookup (map swap optionMap)
  fromEnum = fromJust . flip lookup optionMap

openlog :: String -> IO ()
openlog x = openlog_ x [LOG_PID, LOG_PERROR] LOG_USER

openlog_ :: String -> [SyslogOption] -> SyslogFacility -> IO ()
openlog_ ident opts facil = do
  let opt = toEnum . sum . map fromEnum $ opts
      fac = toEnum . fromEnum           $ facil
  withCString ident $ \p -> c_openlog p opt fac

closelog :: IO ()
closelog = c_closelog

data SyslogPriority = SyslogEmergency | SyslogAlert | SyslogCritical | SyslogError | SyslogWarning | SyslogNotice | SyslogInfo | SyslogDebug
  deriving ( Eq, Show )

prioMap :: [(SyslogPriority, Int)]
prioMap = [(SyslogEmergency, #const LOG_EMERG), (SyslogAlert, #const LOG_ALERT), (SyslogCritical, #const LOG_CRIT), (SyslogError, #const LOG_ERR),
  (SyslogWarning, #const LOG_WARNING), (SyslogNotice, #const LOG_NOTICE), (SyslogInfo, #const LOG_INFO), (SyslogDebug, #const LOG_DEBUG)
  ]
instance Enum SyslogPriority where
  toEnum = fromJust . flip lookup (map swap prioMap)
  fromEnum = fromJust . flip lookup prioMap

syslog :: SyslogPriority -> String -> IO ()
syslog prio msg = withCString msg (\p -> withCString "%s" (\q -> c_syslog (toEnum (fromEnum prio)) q p))

setlogmask :: [SyslogPriority] -> IO [SyslogPriority]
setlogmask prios = do
  let prio = foldl setBit 0 (map fromEnum prios)
  ps <- c_setlogmask prio
  return $ filter (testBit ps . fromEnum) [SyslogEmergency,SyslogAlert,SyslogCritical,SyslogError,SyslogWarning,SyslogNotice,SyslogInfo,SyslogDebug]

startLog :: String -> IO (SyslogPriority -> String -> IO ())
startLog x = do
  openlog x
  ch <- newChan
  _ <- forkIO $ do
    ar <- getChanContents ch
    mapM_ (uncurry syslog) ar
  return (curry (writeChan ch))

foreign import ccall unsafe "closelog" c_closelog :: IO ()
foreign import ccall unsafe "openlog" c_openlog :: CString -> CInt -> CInt -> IO ()
foreign import ccall unsafe "setlogmask" c_setlogmask :: CInt -> IO CInt
foreign import ccall unsafe "syslog" c_syslog :: CInt -> CString -> CString -> IO ()
