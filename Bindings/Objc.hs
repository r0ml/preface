{-# LANGUAGE TemplateHaskell #-}

module Bindings.Objc (
  objc
) where

import Preface.Imports
import Bindings.Darwin
import Preface.Stringy

{- TODO:
 -
 - The quasiQuoter needs to handle  "super"
 - 
 - The quasiQuoter should generate the type of the result? (the :: IO xxx )
 -
 - The quasiQuoter should generate the assignment to the result?
 -  (e.g add  xxx <- x message: arg )
 -
 - Can the quasiQuoter introspect the Cocoa framework at compile time
 - and generate the types automagically?  It not, the type annotation is 
 - required anyway.  If the Cocoa introspection can happen at compile time,
 - what about other frameworks?  What about dynamically generated methods?
 -
 - Is there a better way to deal with the "Argumentative" because Storables
 - should be automagic.  Non-storables need to be converted to storables --
 - and immediates likewise.  
 -}

objc :: QuasiQuoter
objc =  QuasiQuoter { quoteExp = qx, quotePat = undefined
                    , quoteDec = undefined, quoteType = undefined }
  where qx :: String->ExpQ
        qx x = nsParse x 

parseRcvr s = if isUpper (head s)
                 then [| objcClassObject ( objcClass $(stringE s) ) |]
                 else varE (mkName s)

-- parseArg needs to return (and take a argument) the same thing that 
--    parseMoreArgs take

type PAccum = (String, [StmtQ], [ExpQ])

parseArg :: String -> PAccum
parseArg s =
  let s2 = trim s
      s3 = head s2
      pa
        | isDigit s3 = if all isDigit s2 then 
            let [(a,b)] = readDec s2 in parseMoreArgs ("", [],[litE (integerL a)]) b
          else let [(a,b)] = readFloat s2 in parseMoreArgs ("", [], [litE (rationalL a)] ) b
        | isUpper s3 = let (s4,s5)=break isSpace s2
                        in parseMoreArgs ("", [], [appE [|fromEnum|] $ conE (mkName s4)] ) s5
        | s3 == '@' = let (sl2, a1,a2) = parseArg (tail (traceShow ("nsparse1",s2) s2))
                          n1 = mkName ("x_"++show (length a1))
                          az = head a2
                          ay = tail (traceShow ("nsparse2",length a2) a2)
                           -- BindS (VarP x_0) (AppE (VarE Bindings.Darwin.nsString) (LitE (StringL "asdf")))
                       in (sl2, [bindS (varP n1) (appE [|nsString|] az) ], (varE n1) : ay )
        | otherwise = let (s4, s5) = break isSpace s2
                       in parseMoreArgs ("", [], [varE (mkName s4)] ) s5
   in pa

-- parseMoreArgs takes the selector partial, the do statements prefix, and the array of args so far
-- it returns the updated selector partial, the updated do statements prefix, and the updated array of args so far.
parseMoreArgs accum@(sel, dox, args) s = 
  let s1 = trimL s
   in if null s1 then accum
      else let (s5, s6) = break (==':') s1
               (sl, dx, a3) = parseArg (tail (traceShow ("parseMoreArgs",s6) s6))
            in (sel++s5++":"++sl, dox++dx, args++a3)

nsParse :: String -> ExpQ
nsParse q = let s1 = trimL q
                (s2, s3) = break isSpace s1
                s4 = trimL s3
                (s5, s6) = break (==':') s4
                rcvr = parseRcvr s2
                (sel, pfxs, args) = if null s6
                         then ([| objcSel $(stringE s5) |] , [], [])
                         else let (a1, a2, a3) = parseArg (tail (traceShow ("nsParse", s6) s6))
                               in ([| objcSel $(stringE (s5++":"++a1)) |], a2, map (appE [|MkObjcArg|]) a3 )
             in doE ( pfxs ++ [noBindS [| nsInvoke $(rcvr) $(sel) ( $(listE args) ) |] ])

{-
            if isUpper (head s1)
             then ( 
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
-}


