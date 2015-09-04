{-# LANGUAGE DeriveDataTypeable #-}

module Preface.Runner

where 

import Preface.Imports

data Timeout = Timeout deriving(Typeable, Show)
instance Exception Timeout

runWithTimeout :: Double -> String -> [String] -> String -> IO (Either String String)

runWithTimeout tim cmd args indat = do
  -- let cmd = "/usr/bin/wc"
  (inp, outp, errp, ph) <- runInteractiveProcess cmd args Nothing Nothing
  
  if not (null indat) then hPutStr inp indat else return ()

  hClose inp

  res <- hGetContents outp
  -- hGetContents errp

  tot <- forkIO $ (threadDelay . floor . (1000000*) . toRational) tim >> terminateProcess ph
  
  -- ec <- getProcessExitCode ph
  ec <- waitForProcess ph
  throwTo tot Timeout

  case ec of 
     ExitSuccess -> return $ Right res
     _ -> hGetContents errp >>= return . Left 
  
