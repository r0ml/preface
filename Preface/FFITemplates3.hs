{-# LANGUAGE TemplateHaskell #-}

-- These are required for the NSShow definitions
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Preface.FFITemplates3 (
    genRec
  , KValic(..)
) where
 
import Preface.Imports
import Preface.StrUtils

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

