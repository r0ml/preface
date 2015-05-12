
-- |A concise Haskell implementation of the SHA suite of hash functions. 
-- Optimized for brevity, not performance

module Preface.SecureHash (
         Digest
       , sha
       , md5
       , hmacSha
       , stringDigest
       )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Bits (xor, (.&.), rotateL, (.|.), rotateR, complement, shiftR, Bits )
import Data.Word (Word32, Word64)
import Data.Int (Int64)
import Preface.Byter
import Data.List (zip4, foldl', transpose)

type Digest = ByteString

sha :: Int -> ByteString -> Digest
sha 1 = sha1
sha 224 = sha224
sha 256 = sha256
sha 384 = sha384
sha 512 = sha512
sha x@_ = error . (\y -> "not a valid sha type: " ++ y ++ " (valid types are 1, 224, 256, 384, 512)" ) . show . const x

type SHA1State = (Word32, Word32,  Word32,  Word32,  Word32)
type SHA256State = (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32) 
type SHA512State = (Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64)

blk :: Int -> Bool -> ByteString -> [ByteString]
blk k se bs = chunkify bs 0 
  where 
    chunkify :: ByteString -> Int64 -> [ByteString]
    chunkify x n = if B.length x >= k then B.take k x : chunkify (B.drop k x) (n+fromIntegral k)
                       else let ln = fromIntegral $ B.length x 
                                tpad = k - fromIntegral ln
                                kx = 1 + (k `div` 8)
                                pad = if tpad < kx then tpad + k else tpad
                                xbs = B.concat [x, B.singleton 0x80, B.replicate (pad - kx) 0, bc ]
                                sdp = fromIntegral (8 * (n + ln ))
                                sdc = if se then swapEndian64 sdp else sdp
                                bc = longsToBytes ( (if k == 128 then [0] else [] ) ++ [ sdc ] )
                             in if tpad < kx then [B.take k xbs, B.drop k xbs] else [xbs]


ch :: Bits a => a -> a -> a -> a
ch x y z = (x .&. y) `xor` (complement x .&. z)

maj :: Bits a => a -> a -> a -> a
maj x y z = (x .&. (y .|. z)) .|. (y .&. z)


-- --------------------------------------------------------------------------

processSHA256Block :: SHA256State -> ByteString -> SHA256State
processSHA256Block s00@(a00, b00, c00, d00, e00, f00, g00, h00) bb =
  let ws = getSHA256Sched bb 
      ss = map (\(s,r,w) -> step256 s r w) (zip3 (s00 : ss) sha256rotor ws)
      (a64, b64, c64, d64, e64, f64, g64, h64) = last ss
   in (a00+a64, b00+b64, c00+c64, d00+d64, e00+e64, f00+f64, g00+g64, h00+h64)
  where
    step256 (a, b, c, d, e, f, g, h) k w = let t1 = h + bsig256_1 e + ch e f g + k + w
                                               t2 = bsig256_0 a + maj a b c
                                            in ( t1+t2, a, b, c, d+t1, e, f, g)
    bsig256_0 x = rotateR x 2 `xor` rotateR x 13 `xor` rotateR x 22
    bsig256_1 x = rotateR x 6 `xor` rotateR x 11 `xor` rotateR x 25
    lsig256_0 x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3
    lsig256_1 x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10

    getSHA256Sched :: ByteString -> [Word32]
    getSHA256Sched bs =
      let initx = map swapEndian32 (bytesToWords bs) 
          full = initx ++ rest
          rest = map (\(d,c,b,a) -> lsig256_1 a + b + lsig256_0 c + d) 
                 (zip4 full (drop 1 full) (drop 9 full) (drop 14 full))
       in take 64 full

-- cuberoot n = n ** (1/3)
-- floor $ 0x10000000000000000 * (frac $ cuberoot ( fromIntegral x) :: CReal ) | x <- take 64 primes]
    sha256rotor :: [Word32]
    sha256rotor = [
      0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
      0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
      0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
      0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
      0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
      0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
      0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
      0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 ]

processSHA512Block :: SHA512State -> ByteString -> SHA512State
processSHA512Block s00@(a00, b00, c00, d00, e00, f00, g00, h00) bb = 
  let ws = sked bb
      ss = map (\(s,r,w) -> step512 s r w) (zip3 ( s00 : ss) sha512rotor ws)
      (a80, b80, c80, d80, e80, f80, g80, h80) = last ss
   in (a00+a80, b00+b80, c00+c80, d00+d80, e00+e80, f00+f80, g00+g80, h00+h80)
  where 
    sked bs =
      let initx = map swapEndian64 (bytesToLongs bs)
          full = initx ++ rest
          rest = map (\(a,b,c,d) -> lsig512_1 d + c + lsig512_0 b + a)
                     (zip4 full (drop 1 full) (drop 9 full) (drop 14 full))
       in take 80 full

    step512 (a, b, c, d, e, f, g, h) k w = let t1 = h + bsig512_1 e + ch e f g + k + w
                                               t2 = bsig512_0 a + maj a b c
                                            in (t1+t2, a, b, c, d+t1, e, f, g)
    bsig512_0 x = rotateR x 28 `xor` rotateR x 34 `xor` rotateR x 39
    bsig512_1 x = rotateR x 14 `xor` rotateR x 18 `xor` rotateR x 41
    lsig512_0 x = rotateR x 1 `xor` rotateR x 8 `xor` shiftR x 7
    lsig512_1 x = rotateR x 19 `xor` rotateR x 61 `xor` shiftR x 6

    sha512rotor = [
     0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc,
     0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118,
     0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2,
     0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694,
     0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65,
     0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5,
     0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4,
     0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70,
     0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df,
     0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b,
     0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30,
     0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8,
     0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8,
     0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3,
     0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec,
     0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b,
     0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178,
     0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b,
     0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c,
     0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817 ]

-- |Compute the SHA-1 hash of the given ByteString. The digest is 20 bytes long. 
sha1 :: ByteString -> Digest 
sha1 bs = let (a, b, c, d, e) = foldl' doBlock initialState (blk 64 True bs)
           in wordsToBytes (map swapEndian32 [a,b,c,d,e])
  where 
    initialState = ( 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0)
    doBlock :: SHA1State -> ByteString -> SHA1State
    doBlock s00 bb =
      let ws = sked bb
          ss = map (\(s,w) -> step1_ch s 0x5a827999 w) (zip (s00 : ss) (take 20 ws) )
          ss2 = map (\(s,w) -> step1_par s 0x6ed9eba1 w) (zip (last ss : ss2) (take 20 (drop 20 ws)))
          ss3 = map (\(s,w) -> step1_maj s 0x8f1bbcdc w) (zip (last ss2 : ss3) (take 20 (drop 40 ws)))
          ss4 = map (\(s,w) -> step1_par s 0xca62c1d6 w) (zip (last ss3 : ss4) (take 20 (drop 60 ws)))
          (a00, b00, c00, d00, e00) = s00
          (a80, b80, c80, d80, e80) = last ss4
       in ( a00 + a80, b00 + b80, c00 + c80, d00 + d80, e00 + e80)
    sked bsx = let fn = flip rotateL 1 . foldl1 xor
                   full = map swapEndian32 (bytesToWords bsx) ++ map fn (transpose (map (flip drop full) [0, 2, 8, 13]) )
                in take 80 full
    step1_ch :: SHA1State -> Word32 -> Word32 -> SHA1State
    step1_ch (a,b,c,d,e) k w = ( rotateL a 5 + ((b .&. c) `xor` (complement b .&. d)) + e + k + w, a, rotateL b 30, c, d )

    step1_par :: SHA1State -> Word32 -> Word32 -> SHA1State
    step1_par (a,b,c,d,e) k w = ( rotateL a 5 + (b `xor` c `xor` d) + e + k + w, a, rotateL b 30, c, d )

    step1_maj :: SHA1State -> Word32 -> Word32 -> SHA1State
    step1_maj (a,b,c,d,e) k w = ( rotateL a 5 + ((b .&. (c .|. d)) .|. (c .&. d)) + e + k + w, a, rotateL b 30, c, d )

-- [ hexify $ (floor $ 0x10000000000000000 * (frac $ (sqrt ( fromIntegral x ) :: CReal ) ) ) `mod` 0x100000000 | x <- [23::Int, 29, 31, 37, 41, 43, 47, 53]]
-- |Compute the SHA-224 hash of the given ByteString. The digest is 28 bytes long.
sha224 :: ByteString -> Digest
sha224 bs = let initState = (0xc1059ed8, 0x367cd507, 0x3070dd17, 0xf70e5939, 0xffc00b31, 0x68581511, 0x64f98fa7, 0xbefa4fa4)
                (a, b, c, d, e, f, g, _) = foldl' processSHA256Block initState (blk 64 True bs)
             in wordsToBytes (map swapEndian32 [a,b,c,d,e,f,g])

{- the 1 * n works around a bug in Data.Number.CReal -}
-- frac :: RealFrac a => a -> a
-- frac n = n - (fromIntegral ( floor (1 * n) ) )
-- init256 :: SHA256State
-- init256 = let [a,b,c,d,e,f,g,h] = [ floor $ 0x100000000 * (frac $ sqrt ( fromIntegral x) ) | x <- take 8 primes ]
           -- in (a,b,c,d,e,f,g,h)
-- (0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19)
-- |Compute the SHA-256 hash of the given ByteString. The digest is 32 bytes long.
sha256 :: ByteString -> Digest
sha256 bs = let init256 = (0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19)
                (a, b, c, d, e, f, g, h) = foldl' processSHA256Block init256 (blk 64 True bs)
             in wordsToBytes (map swapEndian32 [a,b,c,d,e,f,g,h])

--  [ hexify $ (floor $ 0x10000000000000000 * (frac $ (sqrt ( fromIntegral x ) :: CReal ) ) ) | x <- [23::Int, 29, 31, 37, 41, 43, 47, 53]]
-- |Compute the SHA-384 hash of the given ByteString. The digest is 48 bytes long.
sha384 :: ByteString -> Digest
sha384 bs = let initialState = (0xcbbb9d5dc1059ed8, 0x629a292a367cd507, 0x9159015a3070dd17, 0x152fecd8f70e5939,
                                0x67332667ffc00b31, 0x8eb44a8768581511, 0xdb0c2e0d64f98fa7, 0x47b5481dbefa4fa4)
                (a, b, c, d, e, f, _, _) = foldl' processSHA512Block initialState (blk 128 True bs)
             in longsToBytes (map swapEndian64 [a,b,c,d,e,f])

-- [ hexify $ floor $ 0x10000000000000000 * (frac $ sqrt ( fromIntegral x) :: CReal ) | x <- [2::Int,3,5,7,11,13,17,19]]
-- |Compute the SHA-512 hash of the given ByteString. The digest is 64 bytes long.
sha512 :: ByteString -> Digest
sha512 bs = let initialState = (0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
                                0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179)
                (a, b, c, d, e, f, g, h) = foldl' processSHA512Block initialState (blk 128 True bs)
             in longsToBytes (map swapEndian64 [a,b,c,d,e,f,g,h])
-- --------------------------------------------------------------------------

hmacSha :: Int -> ByteString -> ByteString -> Digest
hmacSha sn k m = let bn = if sn > 256 then 128 else 64
                     f = sha sn
                     kt  = if B.length k > bn then f k else k
                     pad = B.replicate (bn - B.length kt) 0
                     k' = B.append kt pad
                     ipad = B.map (xor 0x36) k'
                     opad = B.map (xor 0x5c) k'
                  in f (B.append opad (f (B.append ipad m)))

-- --------------------------------------------------------------------------

md5 :: ByteString -> Digest
md5 bs = let initialState = (0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476)
             (a, b, c, d) = foldl' md5Update initialState (blk 64 False bs)
          in wordsToBytes [a,b,c,d]
  where
    md5Update (a,b,c,d) w = 
      let ws = cycle (bytesToWords w)
          ma = [ floor (0x100000000 * abs ( sin x)) | x <- [1..64::Double]]
          m1 = take 16 ma
          m2 = take 16 (drop 16 ma)
          m3 = take 16 (drop 32 ma)
          m4 = take 16 (drop 48 ma)
          (z1,z2,z3,z4) = foldl ch2 (a,b,c,d) [
             (\x y z -> z `xor` (x .&. (y `xor` z)) , ws,                     [ 7, 12, 17, 22], m1),
             (\x y z -> y `xor` (z .&. (x `xor` y)) , everynth 4 (tail ws),   [ 5,  9, 14, 20], m2),
             (\x y z -> x `xor` y `xor` z ,           everynth 2 (drop 5 ws), [ 4, 11, 16, 23], m3),
             (\x y z -> y `xor` (x .|. complement z), everynth 6 ws         , [ 6, 10, 15, 21], m4) ]
       in (z1+a, z2+b, z3+c, z4+d)
    mch fn h i j k x s ac = i + rotateL (fn i j k + (x + ac + h)) s
    ch1 fn (h,i,j,k) ws rs ms = let r = mch fn h i j k (head ws) (head rs) (head ms)
                 in r : (if null (tail ms) then [] else ch1 fn (k,r,i,j) (tail ws) (tail rs) (tail ms) )
    ch2 e (f,w1,r,m) = let [h,i,j,k] = drop 12 (ch1 f e w1 (cycle r) m) in (h,k,j,i)
    everynth n ys = head ys : everynth n (drop (n+1) ys )

-- should this be base16 ?
stringDigest :: Digest -> String
stringDigest = concatMap shex . B.unpack 
  where shex :: Integral a => a -> String
        shex n = let (a,b) = divMod (fromIntegral n) 16 in [BC.index chars a, BC.index chars b]
        chars = BC.pack ("0123456789abcdef" :: String)

