{-# LANGUAGE TemplateHaskell #-}

-- These are required for the NSShow definitions
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Preface.Str (
  str
  , iostr
  , template
  , file
  , sh
  , shebang
  , shin
  , python
  , zsh
  , perl
  , ruby
  , rlang
  , psql
  , interpolate
  , opts
  , ShellError

-- these are defined but (shouldn't / don't need to be) exported
  , NSShow(..)
  , enum
  , enumI
  , enumIr
  , storable
  , mkEnum
  , mkEnumI
  , stripComments
  , genEnum
  , genRec
  , KValic(..)

-- these are no longer defined
--  , hash
--  , script,
--  , url
) where
 
-- import Preface.R0ml
import Preface.Imports
import Data.Char (isSpace)

import qualified Data.ByteString.Char8 as BC (unpack)
import qualified Data.Text as T (unpack)

type ShellError = (Int,String)

quasiQuoter :: (String -> ExpQ) -> QuasiQuoter
quasiQuoter x = QuasiQuoter { quoteExp = x, quotePat = undefined, quoteDec = undefined, quoteType = undefined }

pt :: String->ExpQ
pt x = [| do { let a = $(interpolate x) in try (readFile a) :: IO (Either SomeException String) } |]

-- The QuasiQuoters
str :: QuasiQuoter
str = quasiQuoter $ \x -> [| x |]

iostr :: QuasiQuoter
iostr = quasiQuoter interpolate

-- Note: this will be the compile time file contents ?
-- | The compile time contents of the quoted filename.  The filename will be interpolated
template :: QuasiQuoter
template = str

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

-- qurl x = [| readUrl x |]

{- | url is a QuasiQuoter that reads the contents of a URL.

   Example: 

>  [url|http://169.254.169.254/latest/meta-data/|]

     when executed in an Amazon instance, returns the types of available metadata in a JSON string.

     A useful extension would be for the handler to inspect the Content-Type header and convert accordingly.
     
     This QuasiQuoter returns the contents of the URL as a String.  To retrieve the contents of the URL as a
     ProtocolResponse with the associated headers and unconverted bytes, you should be using the functions provided
     by "Twisted".
-}
-- url = quasiQuoter qurl

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

-- | For many interpolations, we want to use Strings as-is, but convert non-Strings via Show.  We do not, however,
-- want to use Show on Strings.  The class NSShow (and the function nsShow) do that.
-- This business requires OverlappingInstances and the order of the definitions matters
class Show a => NSShow a where
  nsShow :: NSShow a => a -> String
  nsShow = show

instance {-# OVERLAPPABLE #-} Show a => NSShow a where nsShow = show
instance {-# OVERLAPPING #-} NSShow String where nsShow x = x
instance NSShow ByteString where nsShow = BC.unpack
instance NSShow Text where nsShow = T.unpack

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
{-
  as <- P.many1 interpiece
  let a = map snd as
      b = map fst as
  if or b then return [| do { _env <- getEnvironment; return $ concat $(listE a) } |]
  -- This is very subtle.  In order to get around the OverloadedStrings and the Environment, 
  -- the 'return' along with the fromString . concat are required

  else return [| return $ (fromString . concat) $(listE a) |]
--  else return [| return $ concat $(listE a) |]

-- interpolates a haskell variable (of the form $lowercase) 
-- returns a bool (indicating whether the global environment is required (no) and the ExprQ)
interlit :: P.Parsec String () (Bool, ExpQ)
interlit = do
         _ <- P.lookAhead (P.satisfy (\x -> (isAlpha x && isLower x ) || ('_' == x ) ) ) 
         nam <- P.many1 (P.satisfy isAlphaNum <|> P.char '_')
         return (False, [| $(varE (mkName nam)) |] )

-- interpolates an environment variable (of the form $Uppercase) 
-- returns a bool (indicating whether the global environment is required (yes) and the ExprQ)
interenv :: P.Parsec String () (Bool, ExpQ)
interenv = do 
         _ <- P.lookAhead (P.satisfy (\x -> isAlpha x && isUpper x) )
         a <- P.many1 (P.satisfy isAlphaNum <|> P.char '_')
         let b = [| fromMaybe ("$" ++ a) (lookup a $( varE (mkName "_env"))) |]
         return (True, b)
-}
{-
-- interpolates an expression (of the form ${expr}
-- returns a bool (indicating whether the global environment is required (no) and the ExprQ)
interexpr :: P.Parsec String () (Bool, ExpQ)
interexpr = do
  _ <- P.char '{'
  b <- P.many ( (P.char '\\' >> P.anyChar) <|> P.noneOf "}\\" ) 
  _ <- P.char '}'
  return (False, parseQExp b )
-}
         
-- piece of interpolation
-- returns a bool (indicating whether the global environment is required, and the ExprQ)
{-
interpiece :: P.Parsec String () (Bool, ExpQ)
interpiece = do
  _ <- P.lookAhead P.anyChar -- need this to fail to terminate
  stringPrefix <- P.many ( P.noneOf "$\\" <|> ( P.char '\\' >> (P.anyChar <|> (P.eof >> return '\\') ) ) ) -- the string up to an interpolation
  P.option (False, [| $(stringE stringPrefix) |] ) $ do
    _ <- P.char '$'
    (g, c) <- {- interexpr <|> -} interlit <|> interenv <|> interfail
    return (g, [| stringPrefix ++ nsShow $(c)|] )
         
-- this ugliness tries to avoid failing if the $ is not followed by { or a name
interfail :: P.Parsec String () (Bool, ExpQ)
interfail = (P.eof >> return loneDollar ) <|> ( P.lookAhead P.anyChar >> return loneDollar) where loneDollar = (False, [| "$" |])
-}

enum :: QuasiQuoter
enum =  QuasiQuoter { quoteExp = undefined, quotePat = undefined, quoteDec = qd, quoteType = undefined } where
  qd s = let (hm : tm) = words (deComma (stripComments s))
          in genEnum hm tm

enumIr :: QuasiQuoter
enumIr = enumI read

enumI :: (String -> Int) -> QuasiQuoter
enumI f =  QuasiQuoter { quoteExp = undefined, quotePat = undefined, quoteDec = qd, quoteType = undefined } where
  qd s = let m = words (deComma (stripComments s))
         in genEnumI (head m) (zip (stride 2 (tail m)) (map f (stride 2 (drop 2 m))))

stride :: Int -> [a] -> [a]
stride _ [] = []
stride n (x:xs) = x : stride n (drop (n-1) xs)

mkEnum :: String -> Q [Dec]
mkEnum s = let m = words s
           in genEnum (head m) (tail m)

mkEnumI :: String -> Q [Dec]
mkEnumI s = let m = words s
            in genEnumI (head m) (zip (stride 2 (tail m)) (map read (stride 2 (drop 2 m))))

genEnum :: String -> [String] -> Q [Dec]
genEnum name vals = do
    dd <- dataD (cxt[]) nam [] dv [''Eq, ''Bounded, ''Show, ''Enum]
    return [dd]
  where dv = map (\n -> normalC (mkName n) []) vals
        nam = mkName name
        
genEnumI :: String -> [(String, Int)] -> Q [Dec]
genEnumI name vals = do
    dd <- dataD (cxt[]) nam [] dv [''Eq, ''Bounded, ''Show]
    fe <- instanceD (cxt [])
            (appT (conT ''Enum) (conT nam))
            [funD (mkName "fromEnum") $ map genClause vals,
             funD (mkName "toEnum") $ map (genClause2 . swap) vals]
    return [dd, fe]
  where dv = map (\(n,_) -> normalC (mkName n) []) vals
        nam = mkName name
        genClause (k,v) = clause [conP (mkName k) []] (normalB [|v|]) []
        genClause2 (k,v) = clause [litP (integerL (fromIntegral k))] (normalB  (conE (mkName v))) []

storable :: QuasiQuoter
storable = QuasiQuoter { quoteExp = undefined, quotePat = undefined, quoteDec = qd, quoteType = undefined }
  where qd s = let (hm : tmx) = words (deComma (stripComments s))
                   nms = stride 2 tmx
                   tm = map xl (stride 2 (tail tmx))
                in {- traceShow (hm, tm) $ -} genStorable hm (zip nms (map f tm ))
        f (x,y) = ( mkName x, y)
        xl x = let (y,z) = break (=='/') x
                in (y, if null z then 1 else (read (drop 1 z) :: Int))
genStorable :: String -> [(String, (Name,Int))] -> Q [Dec]
genStorable name vals = do
        dd <- dataD (cxt[]) nam [] [dv] [''Show]
        let so = [|foldl (+) 0 $(lens)|]
            msv = map snd vals
            lens = listE $ map ( \(tn, nx) -> [|nx * sizeOf (undefined :: $(conT tn))|] ) msv
            namo n = mkName ("o_"++ show n)
            nbx (n, (tn,nx)) = [ bindS (varP (mkName ("b_"++show n)))
                                    [|peekByteOff $(varE (mkName "a")) $(varE (namo n))  |],
                                 letS [ valD (varP (namo (n+1)))
                                    (normalB [|$(varE (namo n)) + (nx * sizeOf (undefined :: $(conT tn)))|] 
                                    ) [] ]
                               ]
            smsx = map nbx (zip [0..] msv)
            inx = letS [ valD (varP (mkName "o_0")) (normalB [|0|]) [] ]
            smsa = inx : concat smsx
            cns = foldl appE (conE nam) (map (varE . mkName . ("b_"++) . show) [0.. (length vals - 1) ])
            retc = noBindS (appE [|return|] $ cns)
        fe <- instanceD (cxt [])
                (appT (conT ''Storable) (conT nam))
                [
                 funD (mkName "peek") [clause [varP (mkName "a") ] (normalB (doE (smsa ++ [retc]))) []],
                      -- (snd (foldl tpm (0, (appE [|pure|] (conE nam))) vals )))  []],
                 funD (mkName "poke") [clause [wildP, wildP] (normalB [| undefined |]) []],
                 funD (mkName "sizeOf") [clause [wildP] (normalB so) []],
                 funD (mkName "alignment") [clause [wildP] (normalB (litE (integerL 4))) []]
                 ]
        return [dd, fe]
  where dv = recC nam ( map (\(n,(t,ll)) -> varStrictType (mkName (lnam++"_"++n)) (strictType notStrict (conT t))) vals)
        nam = mkName name
        lnam = toLower (head name) : tail name

        frag :: (Int, (Name, Int)) -> ExpQ
        frag (n, b@(bb,x)) 
             | bb == ''String = let offset = litE (integerL (fromIntegral n)) 
                                    caster = appT [t|IO|] (conT (cType b) )
                                    parm = varE (mkName "a")
                                    slen = litE (integerL (fromIntegral x))
                                    dopeek = [|peekCStringLen (plusPtr $(parm) $(offset), $(slen))|]
                                 in sigE dopeek caster
             | otherwise = let offset = litE (integerL (fromIntegral n))
                               caster = appT [t|IO|] (conT (cType b) )
                               dopeek = appE ( appE [|peekByteOff|] (varE (mkName "a"))) offset 
                            in sigE dopeek caster
 
        tpm :: (Int, ExpQ) -> (String, (Name, Int) ) -> (Int, ExpQ) 
        tpm (n,x) (_a,b) = (n+bLen b, (infixApp x [|(<*>)|] (frag (n, b))))
        cType (x,n) = x
        bLen (x,n)
          | x == mkName "CUInt" = n * sizeOf (0 :: CUInt)  
          | x == mkName "CULong" = n * sizeOf (0 :: CULong) 
          | x == mkName "CUShort" = n * sizeOf (0 :: CUShort)
          | x == mkName "String" = n
          | otherwise = error ("unknown type: " ++ show x) 

class KValic a where
        toKVL :: a -> [(String, String)]
--         fromKVL :: [(String, String)] -> Either String a  -- might be an error

genRec :: String -> String -> [(String, TypeQ)] -> Q [Dec]
genRec name pfx flds = do
    dd <- dataD (cxt[]) mn [] [recC mn rc] [''Show]
-- the above defines the data definition (rec)
-- then we define an instance for KValic and a functions toKVL / fromKVL
--   to convert the record to a key/value pair
    fe <- instanceD (cxt [])
            (appT (conT ''KValic) (conT mn))
            [funD (mkName "toKVL") $ [clause [conP mn (map (varP . mkName . ("a"++) . show) [1..length flds])] (normalB (foldl tpm (listE []) (zip [(1::Int)..] flds))) [] ]
            ]
    return [dd, fe]
  where rc = map (\(n,t)->varStrictType (mkName (pfx ++ n) ) (strictType notStrict t)) flds
        mn = mkName name
        tpm x (a,(b,c)) = -- tupE [litE (stringL b), appE [|nsShow|] (varE (mkName ("a"++show a)))]
           do 
             cc <- c
             case cc of
               AppT (ConT aa) _bb | aa == ''Maybe ->
                 [|consUnlessNothing|] `appE` (litE (stringL b))
                 `appE` (varE (mkName ("a"++show a)))
                 `appE` x
               _ -> infixApp ( tupE [litE (stringL b),
                 appE [|nsShow|] (varE (mkName ("a"++show a))) ]) [|(:)|] x

consUnlessNothing :: NSShow a => t -> Maybe a -> [(t, String)] -> [(t, String)]
consUnlessNothing _a Nothing c = c
consUnlessNothing a (Just b) c = (a,nsShow b) : c

stripComments :: String -> String
stripComments = stripComments' True where
  stripComments' _ [] = []
  stripComments' True s = if take 2 s == "/*" then ' ' : stripComments' False (drop 2 s)
                                             else head s : stripComments' True (tail s)
  stripComments' False s = if take 2 s == "*/" then stripComments' True (drop 2 s)
                                              else stripComments' False (tail s)

deComma :: String -> String
deComma = map (\x -> if x == ',' then ' ' else x) 

