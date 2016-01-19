
module Preface.Misc (
  module X, module Preface.Misc)
where

import Preface.Imports

import qualified Network.Socket as S (accept, connect, listen)
import Network.Socket as X (iNADDR_ANY, setSocketOption, bindSocket, SocketOption(..)
                            , maxListenQueue)
import Network.BSD as X (getProtocolNumber)

import System.Posix.Signals as P (Handler(..))

import qualified Network.Socket.ByteString as S (send, recv)
import qualified Data.Map as M (Map, insert, lookup, fromList, alter
                               , empty, map, size, insertWith
                               , elems, keys)
import qualified Foreign.Concurrent as Concurrent (newForeignPtr)

import qualified Data.Binary as DB (encode, decode)
import qualified Data.ByteString.Lazy as BL

-- getState :: (Monad m, State.MonadState s m) => m s 
-- getState = State.get

-- putState :: (Monad m, State.MonadState s m) => s -> m ()
-- putState = State.put

import qualified System.Random as Rand

import qualified System.Process

type CBool = CChar

-- | A functional equivalent to if/then/else syntax (CONDitional)
-- The arguments are in the order condition, then, else
cond :: Bool -> a -> a -> a
cond t x y = if t then x else y

-- | A functional equivalent to if/then/else syntax (CONDitional)
-- The arguments are in the order then, else, condition
cond' :: a -> a-> Bool -> a
cond' x y t = if t then x else y

-- | @flip@ takes a function and reverses the first and second arguments
-- Another way of stating that is that moves the second argument into the first position
-- @fflip@ takes the third argument and moves it into the first position
fflip :: (c -> a -> b -> d) -> a -> b -> c -> d
fflip f a b c = f c a b

{-
-- | Apply f to the second element of a tuple
second :: (b->c) -> (a,b) -> (a,c)
second f (a,b) = (a,f b)

-- | Apply f to the first element of a tuple
first :: (a->c) -> (a,b) -> (c,b)
first f (a,b) = (f a, b)
-}


prepend :: [a] -> [a] -> [a]
prepend = (++)

instance Show Errno where show (Errno a) = "Errno " ++ show a

sktRecv :: Socket -> Int -> IO ByteString
sktRecv = S.recv

sktSend :: Socket -> ByteString -> IO Int
sktSend = S.send

sktAccept :: Socket -> IO (Socket, SockAddr)
sktAccept = S.accept

sktConnect :: Socket -> SockAddr -> IO ()
sktConnect = S.connect

sktListen :: Socket -> Int -> IO () 
sktListen = S.listen

posixCatch :: IO () -> P.Handler
posixCatch = P.Catch

lookupWithDefault :: (Eq a) => b -> a -> [(a,b)] -> b
lookupWithDefault d k l = let r = lookup k l in case r of { Nothing -> d ; Just e -> e }

mapInsert :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
mapInsert = M.insert

mapLookup :: (Ord k) => k -> M.Map k a -> Maybe a
mapLookup = M.lookup

mapFromList :: Ord k => [(k, a)] -> M.Map k a 
mapFromList = M.fromList

mapAlter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
mapAlter = M.alter

mapEmpty :: Map k a
mapEmpty = M.empty

mapSize :: Map k a -> Int
mapSize = M.size

mapMap :: (a -> b) -> Map k a -> Map k b
mapMap = M.map

mapInsertWith :: Ord k0 => (a -> a -> a) -> k0 -> a -> Map k0 a -> Map k0 a
mapInsertWith = M.insertWith

mapElems :: Map k a -> [a]
mapElems = M.elems

mapKeys :: Map k a -> [k]
mapKeys = M.keys

newConcurrentForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
newConcurrentForeignPtr = Concurrent.newForeignPtr

stride :: Int -> [a] -> [a]
stride _ [] = []
stride n (x:xs) = x : stride n (drop (n-1) xs)

binaryEncode :: Binary a => a -> ByteString 
binaryEncode = BL.toStrict . DB.encode

binaryDecode :: Binary a => ByteString -> a
binaryDecode = DB.decode . BL.fromStrict

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = do
    v <- p x
    if v then return $ Just x else findM p xs

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)

split :: Eq a => a -> [a] -> [[a]]
split x [] = []
split x xs = if null b then [a] else a : split x (tail b)
               where (a,b) = break (== x) xs

groupOn :: Eq a => (b -> a) -> [b] -> [[b]]
groupOn f = groupBy ((==) `on` f)

nubOn :: Eq a => (b -> a) -> [b] -> [b]
nubOn f = nubBy ((==) `on` f)

merge ::  (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

-- | Merge two sorted lists using into a single, sorted whole,
-- allowing the programmer to specify the comparison function.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] ys = ys
mergeBy _ xs [] = xs
mergeBy f (ax@(x:xs)) (ay@(y:ys)) 
        -- Ordering derives Eq, Ord, so the comparison below is valid.
    | f x y /= GT = x : mergeBy f xs ay
    | otherwise = y : mergeBy f ax ys

systemShell :: String -> System.Process.CreateProcess
systemShell = System.Process.shell

