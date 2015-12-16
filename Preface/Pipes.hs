{-# OPTIONS_GHC -threaded #-}

module Preface.Pipes (
  pipeSource
  , (|->)
  , (-->) , (--<)
  , (--|)
  , while
  , first
  ) where

import Preface.Imports

while :: (a -> Bool) -> [IO a] -> IO [a]
while _f [] = return []
while f (x:xs) = do { y <- x; if f y then fmap (y:) (while f xs) else return [] }

first :: (a -> Bool) -> [IO a] -> IO (Maybe a)
first _f [] = return Nothing
first f (x:xs) = do { y <- x; if f y then return (Just y) else first f xs }

{-
untilM :: (a -> Bool) -> [IO a] -> IO [a]
untilM f [] = return []
untilM f (x:xs) = do
  y <- x
  if f y then return [] else fmap  (y:) (untilM f xs)

cond :: a -> a -> Bool -> a
cond x y t = if t then x else y

untilM2 :: (a->Bool) -> [IO a] -> IO [a]
untilM2 f [] = return []
untilM2 f (x:xs) = x >>= 
   -- \y -> cond (return []) (fmap (y:) (untilM2 f xs)) (f y)
   ap (cond (return ([])) . flip fmap (untilM2 f xs) . (:)) f

untilM3 :: (a->Bool) -> IO a -> IO [a]
untilM3 f g = g >>= 
   -- \y -> cond (return []) ( (y :) <$> ( untilM3 f g)) (f y)
   ap (cond (return ([])) . (<$> untilM3 f g) . (:)) f

untilM4 :: (a->Bool) -> IO a -> IO [a]
untilM4 f g = unsafeInterleaveIO $ do
  y <- g
  if f y then return [] else  fmap (y :)  (untilM4 f g)
   -- ap (cond (return ([])) . (<$> untilM3 f g) . (:)) f
-}

untilM5 :: (a->Bool) -> IO a -> IO (Chan (Maybe a))
untilM5 f g = do
    a <- newChan
    _t <- forkIO $ u a
    putStrLn "Starting..."
    return a 
  where u ch = do
              y<-g
              if f y then writeChan ch Nothing else writeChan ch (Just y) >> u ch

whileC :: (a -> IO b) -> Chan (Maybe a) -> IO ()
whileC f ch = do
  a <- readChan ch
  case a of
    Nothing -> return ()
    Just x -> f x >> whileC f ch

-- untilM5 null getLine >>= whileC print 
--
--
--

infixl 3 -->
infixl 3 --<
infixl 3 |->
infixl 3 --|

-- null |-> getLine --> reverse --< print

-- | Initiate a pipeline by creating a pipeSource from the second argument,
-- and writing the result of the second argument function to the pipe until
-- the termination condition specified in the first argument is true
(|->) :: (a->Bool) -> IO a -> IO (Chan (Maybe a))
(|->) = untilM5

-- | The beginning of a pipe
pipeSource :: IO a -> IO (Chan (Maybe a))
pipeSource a = do
  r <- newChan
  _t <- forkIO $ u r a
  return r
  where u r j = do
                 aa <- j
                 writeChan r (Just aa)
                 u r j

-- | this is a support function to implement a pipeline which will take
-- either a non-IO function (Left) or an IO function (Right)
enPipe :: IO (Chan (Maybe a)) -> Either (a->b) (a->IO b) -> IO (Chan (Maybe b))
enPipe a b = do
    aa <- a
    r <- newChan
    _t <- forkIO $ u aa r
    return r
  where u x y = do
           z <- readChan x
           case z of 
                Nothing -> writeChan y Nothing
                Just q -> case b of 
                    Left f -> writeChan y (Just (f q)) >> u x y
                    Right f ->  do j <- f q
                                   writeChan y (Just j)
                                   u x y
(-->) :: IO (Chan (Maybe a)) -> (a->b) -> IO (Chan (Maybe b))
a --> b = enPipe a (Left b)

(--<) :: IO (Chan (Maybe a)) -> (a->IO b) -> IO (Chan (Maybe b))
a --< b = enPipe a (Right b)

{-
-- | read from a channel, transform what was read, and pass the output to
--  the next channel ( a Unix pipe)
(-->) :: IO (Chan (Maybe a)) -> (a->b) -> IO (Chan (Maybe b))
a --> b = do
    aa <- a
    r <- newChan
    _t <- forkIO $ u aa r
    return r
  where u x y = do
           z <- readChan x
           case z of 
                Nothing -> writeChan y Nothing
                Just q -> writeChan y (Just (b q)) >> u x y

(--<) :: IO (Chan (Maybe a)) -> (a->IO b) -> IO (Chan (Maybe b))
a --< b = do
    aa <- a
    r <- newChan
    _t <- forkIO $ u aa r
    return r
  where u x y = do
           z <- readChan x
           case z of 
                Nothing -> writeChan y Nothing
                Just q -> do z <- b q
                             writeChan y (Just z)
                             u x y
-}
(--|) :: IO (Chan (Maybe a)) -> (a -> IO b) -> IO [b]
a --| b = do
    aa <- a
    u aa
  where u aa = do
          x <- readChan aa
          case x of 
             Nothing -> return []
             Just q -> do 
                     z <- b q 
                     fmap (z :) (u aa)

-- null |-> getLine --> reverse --| print
-- null |-> getLine --> map toUpper --| print
-- (=="exit") |-> getLine --> map toUpper --| print

rmain :: IO ()
rmain = do
  a <- getLine
  if (null a)
     then return ()
     else do
          putStrLn (map toUpper a)
          rmain

  
