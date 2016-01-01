
module Preface.IO (
    withBinaryTempFile
) where

import Preface.Imports
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
    (\(name, handle) -> (hClose handle >> ignoringIOErrors (removeFile name)))
    (uncurry action)

ignoringIOErrors ioe = ioe `catch` (\e -> const (return ()) (e :: IOError))

