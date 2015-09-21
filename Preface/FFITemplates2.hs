{-# LANGUAGE TemplateHaskell #-}

-- These are required for the NSShow definitions
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Preface.FFITemplates2 (
    genStorable
  , storable
) where
 
-- import Preface.R0ml
import Preface.Imports
import Preface.StrUtils (deComma, stripComments)
import Preface.Misc (stride)

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

