{-# LANGUAGE ForeignFunctionInterface, QuasiQuotes #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Bindings.Zlib (
        Zlib, deflater, inflater, zPut, zGet, zDone
        , ZStream(..)
        , inflater2
        , zlibVersion
        , ungzip
        )
where

import Preface.Imports
import Data.ByteString (empty)
import Preface.FFITemplates (enum, storable)
import Preface.Stringy

[storable|ZStream
  next_in CString
  avail_in CULong
  total_in CULong
  next_out CString
  avail_out CULong
  total_out CULong
  msg CString
  state CString
  zalloc CString
  zfree CString
  opaque CString
  data_type CULong
  adler CULong
  reserved CULong
|]

zStreamNew :: ZStream
zStreamNew = ZStream nullPtr 0 0 nullPtr 0 0 nullPtr nullPtr nullPtr nullPtr nullPtr 0 0 0

type Zresult = Either ZlibException ByteString
type Zlib = (Chan (Maybe ByteString), Chan Zresult)

zPut :: Zlib -> ByteString -> IO ()
zPut (a,_) b = writeChan a (Just b)

zDone :: (Chan (Maybe a), t) -> IO ()
zDone (a,_) = writeChan a Nothing

zGet :: Zlib -> IO Zresult
zGet (_,a) = readChan a

type Feeder = ZMem -> ByteString -> Bool -> Chan Zresult -> IO ()

zStart :: (IO ZMem) -> Feeder -> IO Zlib
zStart xinit doz = do
  input <- newChan
  response <- newChan
  _ <- forkIO $ xinit >>= doOuterLoop input response
  return (input, response)
  where doOuterLoop input response zs = do
           dat <- readChan input
           case dat of 
              Nothing -> do
                  -- I'm going to assume that the final call doesnt need to inner-loop
                  b <- doz zs Data.ByteString.empty True response
                  -- writeChan response b
                  return ()
              Just x -> do
                  b <- doz zs x False response
                  -- writeChan response b
                  doOuterLoop input response zs

inflater :: IO Zlib
inflater = zStart (zInit (-15) Nothing Nothing) doInflate where doInflate = feed c_inflate

inflater2 :: IO Zlib
inflater2 = zStart (zInit (31) Nothing Nothing) doInflate where doInflate = feed c_inflate

-- the -1 means default compression level
deflater :: IO Zlib
deflater = zStart (zInit (-15) (Just (-1)) Nothing) doDeflate where doDeflate = feed c_deflate        
data ZMem = ZMem (ForeignPtr ZStream) (ForeignPtr CChar)

-- | Initialize an inflation or deflation process with the given windowBits. You will need
-- to call 'feed' to feed compressed data to this and
-- 'finish' to extract the final chunk of decompressed data.
zInit :: Int -> Maybe Int -> Maybe ByteString -> IO ZMem
zInit wb lev bs = do
    let n = (sizeOf (undefined :: ZStream))
        vers = "1.2.5"
        nflater = isNothing lev
    zsx <- mallocForeignPtrBytes n
    let ii = if nflater then (\zs x -> c_inflateInit2_ zs (fromIntegral wb) x (fromIntegral n))
             else (\zs x -> c_deflateInit2_ zs (fromIntegral (fromJust lev))
                            (fromIntegral (fromEnum ZConstantDEFLATED))
                            (fromIntegral wb) (fromIntegral (8::Int)) -- memLevel  8 is the default
                            (fromIntegral $ fromEnum ZStrategyDEFAULT )
                            x (fromIntegral n))
    _res <- withForeignPtr zsx $ (\zs -> poke zs zStreamNew >> withCString vers (ii zs) )
    let sd = if nflater then c_inflateSetDictionary else c_deflateSetDictionary
    case bs of
         Nothing -> return ()
         Just bss -> withForeignPtr zsx $ \zstr ->
           unsafeUseAsCStringLen bss $ \(cstr, len) -> sd zstr cstr (fromIntegral len)
    fb <- setOutBuff zsx
    return (ZMem zsx fb)
  
[enum|ZConstant NO_FLUSH PARTIAL_FLUSH SYNC_FLUSH FULL_FLUSH FINISH BLOCK DEFLATED 8 |]

[enum|ZStrategy DEFAULT FILTERED HUFFMAN_ONLY RLE FIXED|]

[enum|ZError Z_OK Z_STREAM_END Z_NEED_DICT 
  Z_ERRNO -1 Z_STREAM_ERROR -2 Z_DATA_ERROR -3
  Z_MEM_ERROR -4 Z_BUF_ERROR -5 Z_VERSION_ERROR -6|]

data ZlibException = ZlibException ZError deriving (Show, Typeable)
instance Exception ZlibException

zBufError :: CInt
zBufError = -5

defaultChunkSize :: Int
-- defaultChunkSize = 32752
defaultChunkSize = 1048576

setOutBuff :: ForeignPtr ZStream -> IO (ForeignPtr CChar)
setOutBuff zsx = do
  fbuff <- mallocForeignPtrBytes defaultChunkSize
  -- fbuffa <- mallocBytes defaultChunkSize
  -- fbuff <- newForeignPtr_ fbuffa
  withForeignPtr zsx $ \zstr -> 
    withForeignPtr fbuff $ \buff -> do
      zz <- peek zstr
      poke zstr zz {zStream_next_out = buff, zStream_avail_out = toEnum defaultChunkSize }
  return fbuff

type Flater = Ptr ZStream -> CInt -> IO CInt

-- | Feed the given 'ByteString' 
feed :: Flater -> ZMem -> ByteString -> Bool -> Chan Zresult -> IO ()
feed f (ZMem zsx _zbuff) bs bool chan = do
  withForeignPtr zsx $ \zstr -> do
    unsafeUseAsCStringLen bs $ \(cstr, len) -> do
      zz <- peek zstr
      poke zstr zz { zStream_next_in = cstr, zStream_avail_in = fromIntegral len }
      innerl zstr
  where innerl zstr = do zoz <- peek zstr
                         let obuff = zStream_next_out zoz
                         res <- f zstr (fromIntegral (fromEnum (if bool then ZConstantFINISH else ZConstantNO_FLUSH)))
                         if (res /= 0 && res /= 1 && res /= zBufError)
                         then do
                                 writeChan chan (Left (ZlibException (toEnum (fromEnum res))))
                         else do zy <- peek zstr
                                 let avail = fromIntegral $ zStream_avail_out zy
                                     buff = zStream_next_out zy
                                     siz = defaultChunkSize - avail
                                     _zin = zStream_avail_in zy
                                     _zti = zStream_total_in zy
                                 rs <- packCStringLen (plusPtr buff (-siz), siz)
                                 let zx = zy { zStream_next_out=obuff, zStream_avail_out = fromIntegral defaultChunkSize }
                                 poke zstr zx
                                 writeChan chan (Right rs)

                                 if (zStream_avail_in zy > 0) then innerl zstr else return ()

zlibVersion :: IO ByteString
zlibVersion = c_zlibVersion >>= packCString

foreign import ccall unsafe "zlibVersion" c_zlibVersion :: IO CString

foreign import ccall unsafe "deflateInit2_"
  c_deflateInit2_ :: Ptr ZStream -> CInt -> CInt -> CInt -> CInt -> CInt -> 
                     CString -> CInt -> IO CInt 

foreign import ccall unsafe "inflateInit2_"
  c_inflateInit2_ :: Ptr ZStream -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe "inflate" c_inflate :: Flater
foreign import ccall unsafe "deflate" c_deflate :: Flater

foreign import ccall unsafe "deflateSetDictionary"
  c_deflateSetDictionary :: Ptr ZStream -> Ptr CChar -> CUInt -> IO ()
foreign import ccall unsafe "inflateSetDictionary"
  c_inflateSetDictionary :: Ptr ZStream -> Ptr CChar -> CUInt -> IO ()

ungzip :: ByteString -> IO (Either String ByteString)
ungzip c = do 
  a <- inflater2
  zPut a c
  zDone a
  g <- go a
  pure $ either Left (Right . strConcat) g
  where go :: Zlib -> IO (Either String [ByteString]) 
        go a = do
                b <- zGet a
                case b of 
                     Left e -> pure $ Left (show e)
                     Right r -> do
                        if strNull r
                           then pure $ Right [r]
                           else do d <- go a
                                   pure $ either Left (Right . (r:)) d


