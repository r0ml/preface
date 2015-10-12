{-# LANGUAGE TemplateHaskell #-}

-- These are required for the NSShow definitions
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Preface.IOQuotes (
    file
  , sh
  , shbg
  , shebang
  , shin
  , python
  , zsh
  , perl
  , ruby
  , rlang
  , psql
  , opts
  , ShellError
) where
 
-- import Preface.R0ml
import Preface.Imports
import Data.Char (isSpace)

import Preface.QuasiQuotes (interpolate)

type ShellError = (Int,String)

quasiQuoter :: (String -> ExpQ) -> QuasiQuoter
quasiQuoter x = QuasiQuoter { quoteExp = x, quotePat = undefined, quoteDec = undefined, quoteType = undefined }

{-

-- | 'quoteFile' takes a 'QuasiQuoter' and lifts it into one that read
-- the data out of a file.  For example, suppose 'asmq' is an 
-- assembly-language quoter, so that you can write [asmq| ld r1, r2 |]
-- as an expression. Then if you define @asmq_f = quoteFile asmq@, then
-- the quote [asmq_f| foo.s |] will take input from file "foo.s" instead
-- of the inline text

quoteFile :: QuasiQuoter -> QuasiQuoter
quoteFile (QuasiQuoter { quoteExp = qe, quotePat = qp, quoteType = qt, quoteDec = qd }) 
  = QuasiQuoter { quoteExp = get qe, quotePat = get qp, quoteType = get qt, quoteDec = get qd }
  where
   get :: (String -> Q a) -> String -> Q a
   get old_quoter file_name = [| do { file_cts <- [| runIO (readFile $(interpolate file_name)) |]
                                 ; old_quoter $(interpolate file_cts) } |]

-}

{- | interpolates the filename then reads the file at runtime

>   let a = ".profile" in [file|$HOME/$a|]

-}
file :: QuasiQuoter
file = quasiQuoter pt
  where pt :: String->ExpQ
        pt x = [| do { let a = $(interpolate x) in try (readFile a) :: IO (Either SomeException String) } |]



{- find an alternative for eval_ (plugins)
-- scriptqq :: String -> ExpQ
-- scriptqq x = [| do {fn <- $(interpolate x); s <- readFile fn; eval_ s []


--         ["-XDeriveDataTypeable","-XNoMonomorphismRestriction","-XFlexibleInstances","-XUndecidableInstances",
--         "-XExistentialQuantification","-XDeriveGeneric","-XOverlappingInstances","-XQuasiQuotes","-XTypeSynonymInstances",
--         "-XOverloadedStrings"
           [
         ] [] []} |]

-}

{- | Opts is a QuasiQuoter which uses the text of the quotation as the name of the file to be read.
     The file name is interpolated, so that environment variables or computed names can be used.

     The file that is read in is assumed to have option definitions of the form   Key=Value  or  Key:Value
     one per line.  The result will be a list of String pairs showing the key/value mappings.
-}
opts :: QuasiQuoter
opts = quasiQuoter $ \x -> [| do {s <- readFile $(interpolate x); return $ strToMap s } |]

{- | sh is a quasiquoter which interpolates the string and evaluates it with the system shell.  The result is either an
     error (the error code and contents of stderr), or the contents of stdout.
-}
sh :: QuasiQuoter
sh = quasiQuoter $ \x -> [| shell $( interpolate x) |]

{- | shin is a quasiquoter which interpolates everything up to the first @|@ as the shell command, and then
     interpolates everything after the @|@ to be the standard input to be passed to the shell command.

     Example:
@
>>> [shin|wc|"This is the standard input"|]
Right "       0       5      28"
@
-}
shin :: QuasiQuoter
shin = quasiQuoter $ \x -> [| shell3 "/bin/sh" ("-c" : words $(interpolate x)) |]

shbg :: QuasiQuoter
shbg = quasiQuoter $ \x -> [| let (z:a) = words $(interpolate x) in coproc z a |]  

shebang :: QuasiQuoter
shebang = quasiQuoter (\x -> let (a,_:b) = break (=='|') x in intShebang a b)
python :: QuasiQuoter
python = quasiQuoter (intShebang "python -c")
zsh :: QuasiQuoter
zsh = quasiQuoter (intShebang "zsh -c")
perl :: QuasiQuoter
perl = quasiQuoter (intShebang "perl -e")
ruby :: QuasiQuoter
ruby = quasiQuoter (intShebang "ruby -e")
rlang :: QuasiQuoter
rlang = quasiQuoter (intShebang "R -q -e") 
psql :: QuasiQuoter
psql = quasiQuoter (intShebang "psql -c")

intShebang :: String -> String -> ExpQ
intShebang a b = [| shell2 (words a) $(interpolate b) |]


shell :: String -> IO (Int, String, String)
shell cmd = do { shellEx <- fromMaybe "/bin/sh" <$> lookupEnv "SHELL"; shell2 [shellEx , "-c"] cmd }

shell2 :: [String] -> String -> IO (Int, String, String)
shell2 x y = shell3 (head x) ( tail x ++ [y]) ""

shell3 :: String -> [String] -> String -> IO (Int, String, String)
shell3 int cmd inp = do
  (ex,out,err) <- 
     catch (readProcessWithExitCode int cmd inp) (\x -> return (ExitFailure 101, "", (show (x::SomeException))))
  let outx = if null out || last out /= '\n' then out else init out 
  return $ case ex of 
    ExitSuccess -> (0, "", outx)
    ExitFailure x -> (x,err, out)


-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.

{-
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid
-}

cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanupProcess (Just si, Just so, Just se, ph) = do
    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    ignoreSigPipe (hClose si)
    hClose so
    hClose se
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops.

-- readProcess a b c = coproc a b ( flip hPutStr c ) hGetContents
ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e

readUpTo :: Handle -> Int -> IO String
readUpTo h n = do
   fp <- mallocForeignPtrBytes n
   withForeignPtr fp $ \buf -> do
     len <- hGetBuf h buf n
     str <- peekCStringLen (buf, len)
     return $ if len == n then str ++ " ..." else str

coproc :: (Show a, NFData a) => String -> [String] -> Int -> (Handle -> IO ()) -> (Handle -> IO a) -> IO (ExitCode, Double, String, a)
coproc cmd args timeoutval inf outf = do
  let aa = proc cmd args
      b = aa { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

  bracketOnError (createProcess b) cleanupProcess $
    \(Just inh, Just outh, Just errh, ph) -> do
        t1 <- getCurrentTime
        f1 <- forkIO ( ignoreSigPipe (inf inh) >> ignoreSigPipe (hClose inh)  ) 
        res <- newEmptyMVar
        f2 <- forkIO ( do { a <- outf outh; deepseq a (putMVar res a); hClose outh } )
        errg <- newEmptyMVar
        f3 <- forkIO ( do { a <- readUpTo errh 200; deepseq a (putMVar errg a); hClose errh } )
        f4 <- forkIO ( threadDelay (1000000 * timeoutval) >> terminateProcess ph >> putMVar errg ("timed out after "++(show timeoutval)++" seconds"))

        ex <- waitForProcess ph
        t2 <- getCurrentTime
        r1 <- takeMVar res
        r2 <- takeMVar errg

        killThread f1
        killThread f2
        killThread f3
        killThread f4
        return (ex, 1000.0 * fromRational ( toRational (diffUTCTime t2 t1) ), r2, r1)

-- trim :: String -> String
-- trim x = let f = dropWhile isSpace x in reverse (dropWhile isSpace (reverse f))

-- tkc x = spaces >> char x

{-
key :: P.Parsec String u String
key = P.spaces >> P.many ( P.noneOf ":=,\n \t\r" ) 
-}

strToMap :: String -> [(String,String)]
strToMap q = filter (\(x,_) -> not (null x) ) (map parsePair (lines q))
  where parsePair y = let y1 = dropWhile isSpace y
                          (y2,y3) = break (`elem` ":=,\n \t\r") y1
                          y4 = if null y3 then y3 else dropWhile isSpace (tail y3)
                       in (y2, y4)


