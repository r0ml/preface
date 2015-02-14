
module Math (
    primes
  , fractionalPart
)
where 

setMinus :: Ord a => [a] -> [a] -> [a]
setMinus (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : setMinus  xs  (y:ys)
          EQ ->     setMinus  xs     ys 
          GT ->     setMinus (x:xs)  ys
setMinus  xs     _     = xs

setUnion :: Ord a => [a] -> [a] -> [a]
setUnion (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : setUnion  xs  (y:ys)
          EQ -> x : setUnion  xs     ys 
          GT -> y : setUnion (x:xs)  ys
setUnion  xs     ys    = xs ++ ys

primes :: [Integer]
primes = 2 : ([3,5..] `setMinus` unionAll [[p*p,p*p+2*p..] | p <- primes']) 
 where
   primes' = 3 : ([5,7..] `setMinus` unionAll [[p*p,p*p+2*p..] | p <- primes'])   
   unionAll ((x:xs):t) = x : setUnion xs (unionAll (pairs t))
   unionAll [] = []
   unionAll ([]:t) = unionAll t
   pairs ((x:xs):ys:t) = (x : setUnion xs ys) : pairs t 
   pairs [] = []
   pairs (_:_) = undefined

fractionalPart :: RealFrac a => a -> a
fractionalPart n = n - fromIntegral ( floor n )

