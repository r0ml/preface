{-# LANGUAGE TemplateHaskell #-}

module Preface.FFITemplates (
    enum , {- enumInt , enumI, -} toMask, toCMask, bitsToList, cToEnum
    , orList
) where
 
import Preface.Imports
import Preface.StrUtils (deComma, stripComments)
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

