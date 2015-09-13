
module Preface.IO (
    withBinaryTempFile, autotype, autotypeLn
) where

import Preface.Imports
import System.Directory (getTemporaryDirectory)
import System.IO (openBinaryTempFile)

-- Creates a new temporary file making use of the template.
-- The temp file is deleted after use. For example:
--
-- > withTempFile "sdist." $ \tmpFile hFile -> do ...
--
-- The @tmpFlie@ will be file in the given directory, e.g.
-- @src/sdist.342@.
withBinaryTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withBinaryTempFile template action = do
  tmpDir <- getTemporaryDirectory
  bracket
    (openBinaryTempFile tmpDir template)
    (\(name, h) -> (hClose h >> ignoringIOErrors (removeFile name)))
    (uncurry action)

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `catch` (\e -> const (return ()) (e :: IOError))

autotype :: String -> IO ()
autotype ss = if null ss then return () else 
   putChar (head ss) >> threadDelay 60000 >> autotype (tail ss)
  
autotypeLn :: String -> IO ()
autotypeLn = (>> putStrLn "") . (>> threadDelay 500000) . autotype
