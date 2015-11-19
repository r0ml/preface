{-# LANGUAGE TemplateHaskell #-}

module Preface.QuasiQuotes (
  qqstr
  , istr
  , interpolate
) where
 
-- import Preface.R0ml
import Preface.Imports
import Preface.StrUtils

quasiQuoter :: (String -> ExpQ) -> QuasiQuoter
quasiQuoter x = QuasiQuoter { quoteExp = x, quotePat = undefined, quoteDec = undefined, quoteType = undefined }

-- The QuasiQuoters
qqstr :: QuasiQuoter
qqstr = quasiQuoter $ \x -> [| x |]

istr :: QuasiQuoter
istr = quasiQuoter interpolate

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

{-
interpolateOld :: String -> ExpQ
interpolateOld q = [| let _v_v x = maybe x id (unsafePerformIO (lookupEnv x)) in concat $(listE (interp q)) |]
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
-}

-- this can be used as $(interpolate x) 
-- I need the environment if the interpolation includes environment variables, but not otherwise
interpolate :: String -> ExpQ
interpolate q = [| let _v_v x = maybe ("$"++x) id (unsafePerformIO (lookupEnv x)) in $(interp q) |]
   where interp :: String -> ExpQ
         interp x =
           let (s1, s2) = break (=='$') x
               s3 :: ExpQ
               s3 = case s2 of
                      [] -> stringE "" 
                      '$': [] -> stringE "\n" 
                      '$':'$': s5 -> [| "$" ++ $(interp s5) |]
                      '$': s6@(c1:_) | isAlpha c1 && isLower c1 -> 
                                 let (s8, s9) = namspan s6 in [| nsShow $(varE (mkName s8)) ++ $(interp s9) |]
                                     | isAlpha c1 && isUpper c1 ->
                                 let (s10, s11) = namspan s6
                                  in [| $(appE (varE (mkName "_v_v")) (stringE s10) ) ++ $(interp s11) |]
                      _ -> fail "can't get here either"
            in [| $(stringE s1) ++ $(s3) |] 
         namspan = span ( (||) <$> isAlphaNum <*> ('_' == ) )

