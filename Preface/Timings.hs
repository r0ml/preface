{-# LANGUAGE BangPatterns #-}

-- | Functions related to timing other functions.
-- I expect to use these functions to compare different implementations.
module Preface.Timings (
  timings, timings', randomList
) where

import Preface.Imports

strictShow :: Show a => Handle -> a -> IO ()
strictShow h !x = (hPutStr h $! (show x)) >> hFlush h

gt :: (Fractional a, Integral b) => b -> b -> a
gt x y = fromIntegral (x - y) / 1000000000

-- | This times two IO functions.  The third argument is the test data to pass to both functions.
-- Currently, the functions are run alternately, twice each.
-- The result is a list of tuples containing the CPU time for each run.
-- (i.e. each tuple is a value for the first and second function timings, and the list contains
-- as many entries as number of trials (which is currently hard-wired at 2)
--
-- The functions must return an instance of @Show@ because, in order to ensure that the 
-- evaluation has occurred, the results of the functions are "shown" and written to /dev/null. 
timings :: (Show a, Show c) => ( b -> IO a) -> ( b -> IO c) -> b -> IO [(Double, Double)]
timings f g !b = do
  t <- withFile "/dev/null" WriteMode $ \h -> do
    strictShow h "start"
    t1 <- getCPUTime
    f b >>= strictShow h 
    t2 <- getCPUTime
    g b >>= strictShow h
    t3 <- getCPUTime
    f b >>= strictShow h
    t4 <- getCPUTime
    g b >>= strictShow h
    t5 <- getCPUTime
    let d1 = gt t2 t1
        d2 = gt t3 t2
        d3 = gt t4 t3 
        d4 = gt t5 t4 
    return [(d1, d2),(d3, d4)]
  return t

-- | The same as for @timings@, but the two functions are pure.
timings' :: (Show a, Show c) => ( b -> a) -> ( b -> c) -> b -> IO [(Double, Double)]
timings' f g !b = do
  t <- withFile "/dev/null" WriteMode $ \h -> do
    strictShow h "start"
    t1 <- getCPUTime
    strictShow h (f b)
    t2 <- getCPUTime
    strictShow h (g b)
    t3 <- getCPUTime
    strictShow h (f b)
    t4 <- getCPUTime
    strictShow h (g b)
    t5 <- getCPUTime
    let d1 = gt t2 t1
        d2 = gt t3 t2
        d3 = gt t4 t3
        d4 = gt t5 t4
    return [(d1, d2), (d3, d4)]
  return t


randomList :: (Integral a, Random b, Integral b) => Int -> b -> b -> IO [a]
randomList n l h = do
  a <- newStdGen
  return $ map (fromIntegral . (l +) . mod (h-l)) (take n (randoms a))

-- sum . map fromIntegral . concat  .  map unroll . take 100 . map abs . randoms <$> newStdGen
-- timings 100 (sum . map fromIntegral . map head .  map unroll . take 100 . map abs . randoms <$> newStdGen)
-- timings 100 (sum . map (fromIntegral . head . unroll . abs) . take 100 . randoms <$> newStdGen)
-- timings (return . map (unroll . abs) ) =<< (take 10000 <$> randoms <$> newStdGen)
-- timings (return . map unroll  ) =<< randomList 1000 1 1000000000 
-- timings (return . map putWord32be  ) =<< randomList 1000 1 1000000000
-- timings (return . map (unroll . fromIntegral) ) (return . map (putWord32be . fromIntegral) ) =<< randomList 1000 1 1000000000
--
-- import Data.Binary.Put
-- timings (return . map (\x -> runPut (Data.Binary.Put.putWord32be x)) ) (return . map (Preface.R0ml.putWord32be . fromIntegral) ) =<< randomList 1000 1 1000000000
--
-- flip timings  -- reverses the order (and values ?)
