
module Preface.Misc
where

-- import Control.Monad.State as State

import Preface.Imports

import qualified Network.Socket as S (accept, connect)
import qualified Network.Socket.ByteString as S (send, recv)
import qualified Data.Map as M (Map, insert, lookup, fromList)
import qualified Foreign.Concurrent as Concurrent (newForeignPtr)

-- getState :: (Monad m, State.MonadState s m) => m s 
-- getState = State.get

-- putState :: (Monad m, State.MonadState s m) => s -> m ()
-- putState = State.put

sktRecv :: Socket -> Int -> IO ByteString
sktRecv = S.recv

sktSend :: Socket -> ByteString -> IO Int
sktSend = S.send

sktAccept :: Socket -> IO (Socket, SockAddr)
sktAccept = S.accept

sktConnect :: Socket -> SockAddr -> IO ()
sktConnect = S.connect

lookupWithDefault :: (Eq a) => b -> a -> [(a,b)] -> b
lookupWithDefault d k l = let r = lookup k l in case r of { Nothing -> d ; Just e -> e }

mapInsert :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
mapInsert = M.insert

mapLookup :: (Ord k) => k -> M.Map k a -> Maybe a
mapLookup = M.lookup

mapFromList :: Ord k => [(k, a)] -> M.Map k a 
mapFromList = M.fromList

newConcurrentForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
newConcurrentForeignPtr = Concurrent.newForeignPtr

