{-# LANGUAGE TemplateHaskell #-}

-- These are required for the NSShow definitions
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Preface.FFITemplates (
    enum , {- enumInt , enumI, -} toMask, toCMask, bitsToList, cToEnum
    , orList
  , genStorable
  , storable
  , genRec
  , KValic(..)
) where
 
import Preface.Imports
import Preface.StrUtils (deComma, stripComments, nsShow, NSShow(..) )
import Preface.Misc (stride)

enum :: QuasiQuoter
enum =  QuasiQuoter { quoteExp = undefined, quotePat = undefined
                    , quoteDec = qd, quoteType = undefined } where
  qd s = let (hm : tm) = words (deComma (stripComments s))
             nmvs = enump tm 0
          in genEnumI hm nmvs
         where enump ws n = if null ws then []
                  else let (a:as) = ws
                        in if null as then [(a,n)]
                           else let (b:bs) = as
                                 in if isDigit (head b) || (head b == '-') then
                                       let bb = read b
                                        in (a,bb):enump bs (bb+1)
                                    else (a,n):enump as (n+1)

enumInt :: QuasiQuoter
enumInt = enumI read

enumI :: (String -> Int) -> QuasiQuoter
enumI f =  QuasiQuoter { quoteExp = undefined, quotePat = undefined
                       , quoteDec = qd, quoteType = undefined } where
  qd s = let (m1:ms) = words (deComma (stripComments s))
          in genEnumI m1 (zip (stride 2 ms) (map f (stride 2 (drop 1 ms))))

-- | Given the name of a type, and an array of name/value pairs, construct an
-- Enum type which maps the names (as constructors) to the values (as the toEnum)
-- This implementation generates the declarations required both for the datatype
-- and the instance of Enum which implements `toEnum` and `fromEnum` for all values.
genEnumI :: String -> [(String, Int)] -> Q [Dec]
genEnumI name vals = do
    dd <- dataD (cxt[]) nam [] dv [''Eq, ''Bounded, ''Show]
    fe <- instanceD (cxt []) (appT (conT ''Enum) (conT nam))
            [fd "fromEnum" genClause, fd "toEnum" genClause2]
    return [dd, fe]
  where valsx = map (\(k,v) -> (name++k,v)) vals
        dv = map (\(n,_) -> normalC (mkName n) []) valsx
        nam = mkName name
        genClause (k,v) = clause [conP (mkName k) []] (normalB [|v|]) []
        genClause2 (k,v) = clause [litP (integerL (fromIntegral v))] (normalB  (conE (mkName k))) []
        fd x y = funD (mkName x) $ map y valsx

{- 
-- | Create an Enum using the supplied array of strings as the constructors 
genEnum :: String -> [String] -> Q [Dec]
genEnum name vals = (:[]) <$> dataD (cxt[]) nam [] dv [''Eq, ''Bounded, ''Show, ''Enum]
  where dv = map (\n -> normalC (mkName n) []) vals
        nam = mkName name
  -}

orList :: Enum a => [a] -> CUInt
orList = fromIntegral . (foldr ((.|.) . fromEnum) 0)

toMask :: Enum a => [a] -> Word32
toMask = foldr (\x y -> y .|.  (shiftL 1 (fromEnum x))) 0

toCMask :: Enum a => [a] -> CUInt
toCMask = foldr (\x y -> y .|.  (shiftL 1 (fromEnum x))) 0

bitsToList :: Enum a => CUInt -> [a]
bitsToList x = btl x 0
  where btl x n = {- trace ("btl "++ show x++" "++ show n) $ -} if x == 0 then []
            else if testBit x n then (toEnum (shiftL 1 n)) : btl (clearBit x n) (n+1)
                     else btl x (n+1)

cToEnum :: Enum a => CInt -> a
cToEnum = toEnum . fromEnum

-- -----------------------

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

-- ------------------------------------

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

{-  It is possible to generate foreign function calls via Template Haskell
 -  This would be useful in generating bindings more easily
runQ [d|foreign import ccall unsafe "alert" c_alert :: IO () |]
[ForeignD (ImportF CCall Unsafe "static alert" c_alert_2 (AppT (ConT GHC.Types.IO) (TupleT 0)))]

-}

