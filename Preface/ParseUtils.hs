
module Preface.ParseUtils

where 

import Preface.Imports

-- Parse Utils
--
jfy :: (a,t) -> (Maybe a, t)
jfy (a,b) = (Just a,b)

lastOf :: (Maybe [a], s) -> (Maybe a, s)
lastOf (a,s) = case a of
                  Nothing -> (Nothing,s)
                  Just b -> ( Just (last b), s)

oneOf :: [s->(Maybe t,s)] -> s -> (Maybe t, s)
oneOf (f:fl) s = let (a,b) = f s in case a of { Nothing -> oneOf fl s; Just _ -> (a,b) }
oneOf [] s = (Nothing, s)

sequenceOf :: [s -> (Maybe a, s)] -> s -> (Maybe [a], s)
sequenceOf (f:fl) s  = let (a,b) = f s in case a of { Nothing -> (Nothing, s);
              Just aa -> let (c,d) = sequenceOf fl b in case c of { Nothing -> (Nothing ,b);
                 Just cc -> (Just (aa:cc), d) } }
sequenceOf [] s = (Just [], s)

catOf :: [s -> (Maybe [a], s)] -> s -> (Maybe [a], s)
catOf f s = let (a,b) = sequenceOf f s in (maybe Nothing (Just . concat) a, b)

satisfy :: (Char -> Bool) -> String -> (Maybe Char, String)
satisfy f sa@(c:s) = if f c then (Just c, s) else (Nothing, sa)
satisfy _ "" = (Nothing, "")

enString :: (Maybe Char, String) -> (Maybe String, String) 
enString (a,b) = case a of 
                   Nothing -> (Nothing, b)
                   Just aa -> (Just [aa], b)
-- first (maybe Nothing (Just . (:[])))

stringOf :: String -> String -> (Maybe String, String)
stringOf (c:p) s = if null s || c /= head s then (Nothing, s)
                   else let (a,b) = stringOf p (tail s)
                         in case a of {Nothing -> (Nothing, s); Just aa -> (Just (c:aa), b) }
stringOf "" s = (Just "", s)

manyOf :: (s -> (Maybe a, s)) -> s -> (Maybe [a], s)
manyOf f s = let (a,b) = f s
              in case a of 
                   Nothing -> (Just [], b)
                   Just aa -> let (Just c, d) = manyOf f b in (Just (aa:c), d)

sepBy :: (s -> (Maybe a, s)) -> (s -> (Maybe b, s)) -> s -> (Maybe [a], s)
sepBy f g s = let (a,s2) = f s
               in case a of 
                    Nothing -> (Nothing, s)
                    Just aa -> let (c,s3) = g s2
                                in case c of
                                     Nothing -> (Just [aa], s2)
                                     Just _ -> let (j,s4) = sepBy f g s3
                                                 in case j of
                                                      Nothing -> (Just [aa], s2)
                                                      Just jj -> (Just (aa:jj), s4)

catManyOf :: (String -> (Maybe String, String)) -> String -> (Maybe String, String)
catManyOf f s = let (a,b) = manyOf f s in (maybe (Just "") (Just . concat) a, b)

popWhile :: (Char -> Bool) -> String -> (String, String)
popWhile f = break (not . f)

countMinMax :: Int -> Int -> (String -> (Maybe a, String)) -> String -> (Maybe [a], String)
countMinMax n x f s = let (a,s2) = f s
                       in case a of 
                            Just aa -> 
                                if x > 0
                                   then let (b, s3) = countMinMax (n-1) (x-1) f s2
                                         in case b of
                                              Nothing -> (Nothing, s)
                                              Just bb -> (Just (aa:bb), s3)
                                   else (Just [aa], s2)
                            Nothing -> if n <= 0 then (Just [], s)
                                       else (Nothing, s)
                          
concOf :: ((Maybe [String]), s) -> (Maybe String, s)
concOf (Nothing, s) = (Nothing, s)
concOf (Just a, s) = (Just (concat a), s)



