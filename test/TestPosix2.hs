
{-# LANGUAGE CPP #-}

import Preface.R0ml

import System.Posix.Terminal (queryTerminal)
import System.Posix.Signals (Handler(CatchOnce), softwareTermination, installHandler, keyboardSignal)
import System.Posix.IO (stdInput)

{-
tests :: IO [T.Test]
tests = return $ map (uncurry H.test) testCases

testCases = [ ("one", TestCase $ do
#if defined(linux_HOST_OS)
    inotify <- initINotify
    print inotify
    home <- getHomeDirectory
    wd <- addWatch inotify [Open,Close,Access,Modify,Move] home print
    print wd
    putStrLn "Listens to your home directory. Hit enter to terminate."
    getLine
    removeWatch inotify wd
#endif
    assertEqual "inotify" 1 1
    )
    
    ]
-}


waitForTermination :: IO ()
waitForTermination = do
  istty <- queryTerminal stdInput
  mv <- newEmptyMVar
  _ <- installHandler softwareTermination (CatchOnce (putMVar mv ())) Nothing
  case istty of
    True  -> installHandler keyboardSignal (CatchOnce (putMVar mv ())) Nothing >> return ()
    False -> return ()
  takeMVar mv

{-  
test :: String -> Result -> Test
test name r = Test t
  where t = TestInstance { run = return (Finished r), name = name, tags = [], options = [], setOption = \_ _ -> Right t }
-}

main :: IO ()
main = do
{-
  let dirnam = "/Users/r0ml/Desktop"
  putStrLn "start testing"
  watcher <- watchDir dirnam (\chg -> putStrLn (show chg))
  -- waitForTermination
-}

  -- arg <- getStatFS
  -- mapM_ print arg
  -- I'll have to eyeball the results
    
  exitSuccess

{-
  let filename = "/tmp/clem"
  -- watcher <- watchFile filename (\chg -> putStrLn ("File " ++ filename ++ " was " ++ show chg))
  waitForTermination
  -- stopWatching watcher
  assertEqual "kqueue" 1 1
-}

