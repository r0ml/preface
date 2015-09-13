{-# LANGUAGE TemplateHaskell #-}

-- These are required for the NSShow definitions
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Preface.IOQuotes (
    file
  , sh
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

import Preface.StrUtils (NSShow(..))

type ShellError = (Int,String)

quasiQuoter :: (String -> ExpQ) -> QuasiQuoter
quasiQuoter x = QuasiQuoter { quoteExp = x, quotePat = undefined, quoteDec = undefined, quoteType = undefined }

pt :: String->ExpQ
pt x = [| do { let a = $(interpolate x) in try (readFile a) :: IO (Either SomeException String) } |]

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


shell :: String -> IO (Either ShellError String)
shell cmd = do { shellEx <- fromMaybe "/bin/sh" <$> lookupEnv "SHELL"; shell2 [shellEx , "-c"] cmd }

shell2 :: [String] -> String -> IO (Either ShellError String)
shell2 x y = shell3 (head x) ( tail x ++ [y]) ""

shell3 :: String -> [String] -> String -> IO (Either ShellError String)
shell3 int cmd inp = do
  (ex,out,err) <- 
     catch (readProcessWithExitCode int cmd inp) (\x -> return (ExitFailure 101, "", (show (x::SomeException))))
  return $ case ex of 
    ExitSuccess -> Right (if null out || last out /= '\n' then out else init out )
    ExitFailure x -> Left (x,err)

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


-- this can be used as $(interpolate x) 
-- I need the environment if the interpolation includes environment variables, but not otherwise
interpolate :: String -> ExpQ
interpolate q = [| let _v_v x = maybe x id (unsafePerformIO (lookupEnv x)) in concat $(listE (interp q)) |]
   -- [| concat $(listE (interp q)) |]
   where interp x =
           let -- v_v x = maybe x id (unsafePerformIO (lookupEnv x))
               (s1, s2) = break (=='$') x
               s3 = case s2 of
                      [] -> []
                      '$':s4 -> case s4 of 
                                  [] -> []
                                  '$':s5 -> stringE "$" : interp s5
                                  s6@(c1:_s7) | isAlpha c1 && isLower c1 -> 
                                          let (s8, s9) = span (\x2 -> isAlphaNum x2 || '_' == x2 ) s6
                                           in (appE [|nsShow|] (varE (mkName s8))): interp s9
                                             | isAlpha c1 && isUpper c1 ->
                                          let (s10, s11) = span (\x2 -> isAlphaNum x2 || '_' == x2) s6
                                           in appE (varE (mkName "_v_v")) (stringE s10) : interp s11
                                     --      in [| $(_v s10) |] : interp s11
                                  _ -> fail "can't get here"
                      _ -> fail "can't get here either"
            in stringE s1 : s3

