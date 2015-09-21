{-# LANGUAGE TemplateHaskell #-}

module Preface.FFITemplates (
    enum , enumInt , enumI
) where
 
import Preface.Imports
import Preface.StrUtils (deComma, stripComments)
import Preface.Misc (stride)

enum :: QuasiQuoter
enum =  QuasiQuoter { quoteExp = undefined, quotePat = undefined
                    , quoteDec = qd, quoteType = undefined } where
  qd s = let (hm : tm) = words (deComma (stripComments s))
          in genEnumI hm (zip tm [0..])

enumInt :: QuasiQuoter
enumInt = enumI read

enumI :: (String -> Int) -> QuasiQuoter
enumI f =  QuasiQuoter { quoteExp = undefined, quotePat = undefined
                       , quoteDec = qd, quoteType = undefined } where
  qd s = let m = words (deComma (stripComments s))
          in genEnumI (head m) (zip (stride 2 (tail m)) (map f (stride 2 (drop 2 m))))

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
  where dv = map (\(n,_) -> normalC (mkName n) []) vals
        nam = mkName name
        genClause (k,v) = clause [conP (mkName k) []] (normalB [|v|]) []
        genClause2 (k,v) = clause [litP (integerL (fromIntegral v))] (normalB  (conE (mkName k))) []
        fd x y = funD (mkName x) $ map y vals

{- 
-- | Create an Enum using the supplied array of strings as the constructors 
genEnum :: String -> [String] -> Q [Dec]
genEnum name vals = (:[]) <$> dataD (cxt[]) nam [] dv [''Eq, ''Bounded, ''Show, ''Enum]
  where dv = map (\n -> normalC (mkName n) []) vals
        nam = mkName name
  -}

