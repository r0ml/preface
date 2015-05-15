{-# LANGUAGE ForeignFunctionInterface #-}

module Preface.Bindings.Syslog ( startLog,
  openlog,  openlog_,  closelog,  syslog,  setlogmask,  Priority(..),  Facility(..),  Option(..)
  ) where

import Control.Concurrent (newChan, writeChan, getChanContents, forkIO)
import Data.Maybe (fromJust)
import Data.Bits (testBit, setBit)
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString, withCString)
import Data.Tuple (swap)

#include <syslog.h>

data Facility = KERN | USER | MAIL | DAEMON | AUTH | SYSLOG | LPR | NEWS | UUCP | CRON
  | AUTHPRIV | FTP | LOCAL0 | LOCAL1 | LOCAL2 | LOCAL3 | LOCAL4 | LOCAL5 | LOCAL6 | LOCAL7
  deriving (Eq, Show)

facilityMap :: [(Facility, Int)]
facilityMap = [(KERN, #const LOG_KERN), (USER, #const LOG_USER), (MAIL, #const LOG_MAIL), (DAEMON, #const LOG_DAEMON),
  (AUTH, #const LOG_AUTH), (SYSLOG, #const LOG_SYSLOG), (LPR, #const LOG_LPR), (NEWS, #const LOG_NEWS),
  (UUCP, #const LOG_UUCP),(CRON, #const LOG_CRON), (AUTHPRIV, #const LOG_AUTHPRIV), (FTP, #const LOG_FTP),
  (LOCAL0, #const LOG_LOCAL0), (LOCAL1, #const LOG_LOCAL1), (LOCAL2, #const LOG_LOCAL2), (LOCAL3, #const LOG_LOCAL3),
  (LOCAL4, #const LOG_LOCAL4), (LOCAL5, #const LOG_LOCAL5), (LOCAL6, #const LOG_LOCAL6), (LOCAL7, #const LOG_LOCAL7)
  ]

instance Enum Facility where
  toEnum = fromJust . flip lookup (map swap facilityMap)
  fromEnum = fromJust . flip lookup facilityMap

data Option = PID | CONS | ODELAY | NDELAY | NOWAIT | PERROR deriving (Eq, Show)

optionMap :: [(Option, Int)]
optionMap = [(PID, #const LOG_PID), (CONS, #const LOG_CONS), (ODELAY, #const LOG_ODELAY), (NDELAY, #const LOG_NDELAY),
  (NOWAIT, #const LOG_NOWAIT), (PERROR, #const LOG_PERROR)
  ]
instance Enum Option where
  toEnum = fromJust . flip lookup (map swap optionMap)
  fromEnum = fromJust . flip lookup optionMap

openlog :: String -> IO ()
openlog x = openlog_ x [PID, PERROR] USER

openlog_ :: String -> [Option] -> Facility -> IO ()
openlog_ ident opts facil = do
  let opt = toEnum . sum . map fromEnum $ opts
      fac = toEnum . fromEnum           $ facil
  withCString ident $ \p -> c_openlog p opt fac

closelog :: IO ()
closelog = c_closelog

data Priority = Emergency | Alert | Critical | Error | Warning | Notice | Info | Debug
  deriving ( Eq, Show )

prioMap :: [(Priority, Int)]
prioMap = [(Emergency, #const LOG_EMERG), (Alert, #const LOG_ALERT), (Critical, #const LOG_CRIT), (Error, #const LOG_ERR),
  (Warning, #const LOG_WARNING), (Notice, #const LOG_NOTICE), (Info, #const LOG_INFO), (Debug, #const LOG_DEBUG)
  ]
instance Enum Priority where
  toEnum = fromJust . flip lookup (map swap prioMap)
  fromEnum = fromJust . flip lookup prioMap

syslog :: Priority -> String -> IO ()
syslog prio msg = withCString msg (\p -> withCString "%s" (\q -> c_syslog (toEnum (fromEnum prio)) q p))

setlogmask :: [Priority] -> IO [Priority]
setlogmask prios = do
  let prio = foldl setBit 0 (map fromEnum prios)
  ps <- c_setlogmask prio
  return $ filter (testBit ps . fromEnum) [Emergency,Alert,Critical,Error,Warning,Notice,Info,Debug]

startLog :: String -> IO (Priority -> String -> IO ())
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
