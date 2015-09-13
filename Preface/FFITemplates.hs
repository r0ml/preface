{-# LANGUAGE TemplateHaskell #-}

-- These are required for the NSShow definitions
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Preface.FFITemplates (

-- these are defined but (shouldn't / don't need to be) exported
    enum
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
--  , url
) where
 
-- import Preface.R0ml
import Preface.Imports
import Preface.StrUtils

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
            smsx = map nbx (zip [(0::Int)..] msv)
            inx = letS [ valD (varP (mkName "o_0")) (normalB [|0|]) [] ]
            smsa = init $ inx : concat smsx
            
            nby (n, (tn, nx)) = [ noBindS
                                    [|pokeByteOff $(varE (mkName "a")) $(varE (namo n)) $(varE (mkName ("b_"++show n))) |],
                                 letS [ valD (varP (namo (n+1)))
                                    (normalB [|$(varE (namo n)) + (nx * sizeOf (undefined :: $(conT tn)))|] 
                                    ) []
                                  ]
                                ]
            smsy = map nby (zip [(0::Int)..] msv)
            smsb = init $ inx : concat smsy

            cns = foldl appE (conE nam) (map (varE . mkName . ("b_"++) . show) [0.. (length vals - 1) ])
            
            cny = conP nam (map (varP . mkName . ("b_"++) . show) [0.. (length vals - 1) ])
            retc = noBindS (appE [|return|] $ cns)
            retk = noBindS [|return ()|]
        fe <- instanceD (cxt [])
                (appT (conT ''Storable) (conT nam))
                [
                 funD (mkName "peek") [clause [varP (mkName "a") ] (normalB (doE (smsa ++ [retc]))) []],
                      -- (snd (foldl tpm (0, (appE [|pure|] (conE nam))) vals )))  []],
                 -- if read-only, I could do this:
                 -- funD (mkName "poke") [clause [wildP, wildP] (normalB [| undefined |]) []],
                 funD (mkName "poke") [clause [varP (mkName "a"), cny]
                    (normalB (doE (smsb ++ [retk] ))) [] ],
                 funD (mkName "sizeOf") [clause [wildP] (normalB so) []],
                 funD (mkName "alignment") [clause [wildP] (normalB (litE (integerL 4))) []]
                 ]
        return [dd, fe]
  where dv = recC nam ( map (\(n,(t,_ll)) -> varStrictType (mkName (lnam++"_"++n)) (strictType notStrict (conT t))) vals)
        nam = mkName name
        lnam = toLower (head name) : tail name
{-
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
-}

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


{-  It is possible to generate foreign function calls via Template Haskell
 -  This would be useful in generating bindings more easily
runQ [d|foreign import ccall unsafe "alert" c_alert :: IO () |]
[ForeignD (ImportF CCall Unsafe "static alert" c_alert_2 (AppT (ConT GHC.Types.IO) (TupleT 0)))]

-}

