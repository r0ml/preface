--
-- A module containing ASN1 BER and DER specification encoding/decoding.
--
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
module Preface.ASN1
    ( BER(..)
    , DER(..)
    -- * incremental parsing interfaces
    , runParseState
    , isParseDone
    , newParseState
    , ParseState
    , ParseCursor
    -- * simple parsing interfaces
    , parseLBS
    , parseBS
    -- * types
    , ASN1Header(..)
    , ASN1Class(..)
    , ASN1Tag
    , ASN1Length(..)
    , ASN1Event(..)

    , toByteString
    , toLazyByteString
    -- * generic class for decoding and encoding stream
    , ASN1Decoding(..)
    , ASN1DecodingRepr(..)
    , ASN1Encoding(..)
    -- * strict bytestring version
    , decodeASN1'
    , decodeASN1Repr'
    , encodeASN1'
    -- * Errors types
    , ASN1Error(..)

    , ASN1Repr
    , getConstructedEnd
    , getConstructedEndRepr

    , BitArray(..)
    , BitArrayOutOfBound(..)
    , bitArrayLength
    , bitArrayGetBit
    , bitArraySetBitValue
    , bitArraySetBit
    , bitArrayClearBit
    , bitArrayGetData
    , toBitArray
    , OID
    -- * classes
    , OIDable(..)
    , OIDNameable(..)
    , pretty
    , PrettyType(..)

    -- for test
    , mkSmallestLength
    , runGet
    , getHeader
    , putHeader
    , Result(..)

{- no? -}
    , ASN1(..)
    , ASN1S
    , ASN1ConstructionType(..)
    , ASN1TimeType(..)
    , ASN1Object(..)
    -- * Events types
    , ASN1StringEncoding(..)
    , ASN1CharacterString(..)
    , asn1CharacterString
    , asn1CharacterToString
    ) where

import Preface.Imports
import Preface.SecureHash (stringDigest)
import qualified Control.Exception as E
import Control.Arrow (first)
-- import Data.ByteString (ByteString)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- import Data.Word
-- import Data.Maybe (fromJust, fromMaybe)
-- import Data.Typeable

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))

-- import Foreign

import qualified Data.ByteString.Unsafe   as B
import Data.Time (TimeZone)
-- import Data.List (unfoldr)
-- import Data.Char (ord)
-- import Data.Hourglass
-- import Control.Monad
-- import Control.Exception (Exception, throw)
-- import           Numeric (showHex)
-- import Data.String
import qualified Data.ByteString.Char8 as BC



-- | Basic Encoding Rules (BER)
data BER = BER

-- | Distinguished Encoding Rules (DER)
data DER = DER

instance ASN1DecodingRepr BER where
    decodeASN1Repr _ lbs = decodeEventASN1Repr (const Nothing) `fmap` parseLBS lbs

instance ASN1Decoding BER where
    decodeASN1 _ lbs = (map fst . decodeEventASN1Repr (const Nothing)) `fmap` parseLBS lbs

instance ASN1DecodingRepr DER where
    decodeASN1Repr _ lbs = decodeEventASN1Repr checkDER `fmap` parseLBS lbs

instance ASN1Decoding DER where
    decodeASN1 _ lbs = (map fst . decodeEventASN1Repr checkDER) `fmap` parseLBS lbs

instance ASN1Encoding DER where
    encodeASN1 _ l = toLazyByteString $ encodeToRaw l

decodeConstruction :: ASN1Header -> ASN1ConstructionType
decodeConstruction (ASN1Header Universal 0x10 _ _) = Sequence
decodeConstruction (ASN1Header Universal 0x11 _ _) = Set
decodeConstruction (ASN1Header c t _ _)            = Container c t

decodeEventASN1Repr :: (ASN1Header -> Maybe ASN1Error) -> [ASN1Event] -> [ASN1Repr]
decodeEventASN1Repr checkHeader l = loop [] l
    where loop _ []     = []
          loop acc (h@(Header hdr@(ASN1Header _ _ True _)):ConstructionBegin:xs) =
                let ctype = decodeConstruction hdr in
                case checkHeader hdr of
                    Nothing  -> (Start ctype,[h,ConstructionBegin]) : loop (ctype:acc) xs
                    Just err -> E.throw err
          loop acc (h@(Header hdr@(ASN1Header _ _ False _)):p@(Primitive prim):xs) =
                case checkHeader hdr of
                    Nothing -> case decodePrimitive hdr prim of
                        Left err  -> E.throw err
                        Right obj -> (obj, [h,p]) : loop acc xs
                    Just err -> E.throw err
          loop (ctype:acc) (ConstructionEnd:xs) = (End ctype, [ConstructionEnd]) : loop acc xs
          loop _ (x:_) = E.throw $ StreamUnexpectedSituation (show x)

-- | DER header need to be all of finite size and of minimum possible size.
checkDER :: ASN1Header -> Maybe ASN1Error
checkDER (ASN1Header _ _ _ len) = checkLength len
    where checkLength :: ASN1Length -> Maybe ASN1Error
          checkLength LenIndefinite = Just $ PolicyFailed "DER" "indefinite length not allowed"
          checkLength (LenShort _)  = Nothing
          checkLength (LenLong n i)
              | n == 1 && i < 0x80  = Just $ PolicyFailed "DER" "long length should be a short length"
              | n == 1 && i >= 0x80 = Nothing
              | otherwise           = if i >= 2^((n-1)*8) && i < 2^(n*8)
                  then Nothing
                  else Just $ PolicyFailed "DER" "long length is not shortest"

encodeToRaw :: [ASN1] -> [ASN1Event]
encodeToRaw = concatMap writeTree . mkTree
    where writeTree (p@(Start _),children) = snd $ encodeConstructed p children
          writeTree (p,_)                  = snd $ encodePrimitive p

          mkTree []           = []
          mkTree (x@(Start _):xs) =
              let (tree, r) = spanEnd 0 xs
               in (x,tree):mkTree r
          mkTree (p:xs)       = (p,[]) : mkTree xs

          spanEnd :: Int -> [ASN1] -> ([ASN1], [ASN1])
          spanEnd _ []             = ([], [])
          spanEnd 0 (x@(End _):xs) = ([x], xs)
          spanEnd lvl (x:xs)       = case x of
                    Start _ -> let (ys, zs) = spanEnd (lvl+1) xs in (x:ys, zs)
                    End _   -> let (ys, zs) = spanEnd (lvl-1) xs in (x:ys, zs)
                    _       -> let (ys, zs) = spanEnd lvl xs in (x:ys, zs)


-- | nothing means the parser stop this construction on
-- an ASN1 end tag, otherwise specify the position
-- where the construction terminate.
type ConstructionEndAt = Maybe Word64

data ParseExpect = ExpectHeader (Maybe (B.ByteString -> Result ASN1Header))
                 | ExpectPrimitive Word64 (Maybe (B.ByteString -> Result ByteString))

type ParsePosition = Word64

-- | represent the parsing state of an ASN1 stream.
--
-- * the stack of constructed elements.
-- * the next expected type.
-- * the position in the stream.
--
data ParseState = ParseState [ConstructionEndAt] ParseExpect ParsePosition

-- | create a new empty parse state. position is 0
newParseState :: ParseState
newParseState = ParseState [] (ExpectHeader Nothing) 0

isEOC :: ASN1Header -> Bool
isEOC (ASN1Header cl t _ _) = cl == Universal && t == 0

asn1LengthToConst :: ASN1Length -> Maybe Word64
asn1LengthToConst (LenShort n)  = Just $ fromIntegral n
asn1LengthToConst (LenLong _ n) = Just $ fromIntegral n
asn1LengthToConst LenIndefinite = Nothing

-- in the future, drop this for the `mplus` with Either.
mplusEither :: Either b a -> (a -> Either b c) -> Either b c
mplusEither (Left e) _  = Left e
mplusEither (Right e) f = f e

-- | Represent the events and state thus far.
type ParseCursor = ([ASN1Event], ParseState)

-- | run incrementally the ASN1 parser on a bytestring.
-- the result can be either an error, or on success a list
-- of events, and the new parsing state.
runParseState :: ParseState -- ^ parser state
              -> ByteString -- ^ input data as bytes
              -> Either ASN1Error ParseCursor
runParseState = loop
     where
           loop iniState bs
                | B.null bs = terminateAugment (([], iniState), bs) `mplusEither` (Right . fst)
                | otherwise = go iniState bs `mplusEither` terminateAugment
                                             `mplusEither` \((evs, newState), nbs) -> loop newState nbs
                                             `mplusEither` (Right . first (evs ++))

           terminateAugment ret@((evs, ParseState stackEnd pe pos), r) =
                case stackEnd of
                    Just endPos:xs
                         | pos > endPos  -> Left StreamConstructionWrongSize
                         | pos == endPos -> terminateAugment ((evs ++ [ConstructionEnd], ParseState xs pe pos), r)
                         | otherwise     -> Right ret 
                    _                    -> Right ret

           -- go get one element (either a primitive or a header) from the bytes
           -- and returns the new cursor and the remaining byte.
           go :: ParseState -> ByteString -> Either ASN1Error (ParseCursor, ByteString)
           go (ParseState stackEnd (ExpectHeader cont) pos) bs =
                case runGetHeader cont pos bs of
                     Fail s                 -> Left $ ParsingHeaderFail s
                     Partial f              -> Right (([], ParseState stackEnd (ExpectHeader $ Just f) pos), B.empty)
                     Done hdr nPos remBytes
                        | isEOC hdr -> case stackEnd of
                                           []                  -> Right (([], ParseState [] (ExpectHeader Nothing) nPos), remBytes)
                                           Just _:_            -> Left StreamUnexpectedEOC
                                           Nothing:newStackEnd -> Right ( ( [ConstructionEnd]
                                                                          , ParseState newStackEnd (ExpectHeader Nothing) nPos)
                                                                        , remBytes)
                        | otherwise -> case hdr of
                                       (ASN1Header _ _ True len)  ->
                                           let nEnd = (nPos +) `fmap` asn1LengthToConst len
                                           in Right ( ( [Header hdr,ConstructionBegin]
                                                      , ParseState (nEnd:stackEnd) (ExpectHeader Nothing) nPos)
                                                    , remBytes)
                                       (ASN1Header _ _ False LenIndefinite) -> Left StreamInfinitePrimitive
                                       (ASN1Header _ _ False len) ->
                                           let pLength = fromJust $ asn1LengthToConst len
                                           in if pLength == 0
                                                 then Right ( ( [Header hdr,Primitive B.empty]
                                                              , ParseState stackEnd (ExpectHeader Nothing) nPos)
                                                            , remBytes)
                                                 else Right ( ( [Header hdr]
                                                              , ParseState stackEnd (ExpectPrimitive pLength Nothing) nPos)
                                                            , remBytes)
           go (ParseState stackEnd (ExpectPrimitive len cont) pos) bs =
                case runGetPrimitive cont len pos bs of
                     Fail _               -> error "primitive parsing failed"
                     Partial f            -> Right (([], ParseState stackEnd (ExpectPrimitive len $ Just f) pos), B.empty)
                     Done p nPos remBytes -> Right (([Primitive p], ParseState stackEnd (ExpectHeader Nothing) nPos), remBytes)

           runGetHeader Nothing  = \pos -> runGetPos pos getHeader
           runGetHeader (Just f) = const f

           runGetPrimitive Nothing  n = \pos -> runGetPos pos (getBytes $ fromIntegral n)
           runGetPrimitive (Just f) _ = const f

-- | when no more input is available, it's important to check that the parser is
-- in a finish state too.
isParseDone :: ParseState -> Bool
isParseDone (ParseState [] (ExpectHeader Nothing) _) = True
isParseDone _                                        = False

-- | Parse one lazy bytestring and returns on success all ASN1 events associated.
parseLBS :: L.ByteString -> Either ASN1Error [ASN1Event]
parseLBS lbs = foldrEither process ([], newParseState) (L.toChunks lbs) `mplusEither` onSuccess
    where 
          onSuccess (allEvs, finalState)
                  | isParseDone finalState = Right $ concat $ reverse allEvs
                  | otherwise              = Left ParsingPartial

          process :: ([[ASN1Event]], ParseState) -> ByteString -> Either ASN1Error ([[ASN1Event]], ParseState)
          process (pevs, cState) bs = runParseState cState bs `mplusEither` \(es, cState') -> Right (es : pevs, cState')

          foldrEither :: (a -> ByteString -> Either ASN1Error a) -> a -> [ByteString] -> Either ASN1Error a
          foldrEither _ acc []     = Right acc
          foldrEither f acc (x:xs) = f acc x `mplusEither` \nacc -> foldrEither f nacc xs

-- | Parse one strict bytestring and returns on success all ASN1 events associated.
parseBS :: ByteString -> Either ASN1Error [ASN1Event]
parseBS bs = runParseState newParseState bs `mplusEither` onSuccess
    where onSuccess (evs, pstate)
                    | isParseDone pstate = Right evs
                    | otherwise          = Left ParsingPartial

-- | transform a list of ASN1 Events into a strict bytestring
toByteString :: [ASN1Event] -> ByteString
toByteString = B.concat . L.toChunks . toLazyByteString

-- | transform a list of ASN1 Events into a lazy bytestring
toLazyByteString :: [ASN1Event] -> L.ByteString
toLazyByteString evs = L.fromChunks $ loop [] evs
    where loop _ [] = []
          loop acc (x@(Header (ASN1Header _ _ pc len)):xs) = toBs x : loop (if pc then (len == LenIndefinite):acc else acc) xs
          loop acc (ConstructionEnd:xs) = case acc of
                                              []        -> error "malformed stream: end before construction"
                                              (True:r)  -> toBs ConstructionEnd : loop r xs
                                              (False:r) -> loop r xs
          loop acc (x:xs) = toBs x : loop acc xs

          toBs (Header hdr)      = putHeader hdr
          toBs (Primitive bs)    = bs
          toBs ConstructionBegin = B.empty
          toBs ConstructionEnd   = B.empty

-- | Describe an ASN1 decoding, that transform a bytestream into an asn1stream
class ASN1Decoding a where
    -- | decode a lazy bytestring into an ASN1 stream
    decodeASN1 :: a -> L.ByteString -> Either ASN1Error [ASN1]

-- | transition class.
class ASN1DecodingRepr a where
    -- | decode a lazy bytestring into an ASN1 stream
    decodeASN1Repr :: a -> L.ByteString -> Either ASN1Error [ASN1Repr]

-- | Describe an ASN1 encoding, that transform an asn1stream into a bytestream
class ASN1Encoding a where
    -- | encode a stream into a lazy bytestring
    encodeASN1 :: a -> [ASN1] -> L.ByteString

-- | decode a strict bytestring into an ASN1 stream
decodeASN1' :: ASN1Decoding a => a -> B.ByteString -> Either ASN1Error [ASN1]
decodeASN1' encoding bs = decodeASN1 encoding $ L.fromChunks [bs]

-- | decode a strict bytestring into an ASN1Repr stream
decodeASN1Repr' :: ASN1DecodingRepr a => a -> B.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr' encoding bs = decodeASN1Repr encoding $ L.fromChunks [bs]

-- | encode a stream into a strict bytestring
encodeASN1' :: ASN1Encoding a => a -> [ASN1] -> B.ByteString
encodeASN1' encoding = B.concat . L.toChunks . encodeASN1 encoding

-- | Possible errors during parsing operations
data ASN1Error = StreamUnexpectedEOC         -- ^ Unexpected EOC in the stream.
               | StreamInfinitePrimitive     -- ^ Invalid primitive with infinite length in a stream.
               | StreamConstructionWrongSize -- ^ A construction goes over the size specified in the header.
               | StreamUnexpectedSituation String -- ^ An unexpected situation has come up parsing an ASN1 event stream.
               | ParsingHeaderFail String    -- ^ Parsing an invalid header.
               | ParsingPartial              -- ^ Parsing is not finished, there is construction unended.
               | TypeNotImplemented String   -- ^ Decoding of a type that is not implemented. Contribution welcome.
               | TypeDecodingFailed String   -- ^ Decoding of a knowed type failed.
               | PolicyFailed String String -- ^ Policy failed including the name of the policy and the reason.
               deriving (Typeable, Show, Eq)

instance Exception ASN1Error

-- | The result of a parse.
data Result r = Fail String
              -- ^ The parse failed. The 'String' is the
              --   message describing the error, if any.
              | Partial (B.ByteString -> Result r)
              -- ^ Supply this continuation with more input so that
              --   the parser can resume. To indicate that no more
              --   input is available, use an 'B.empty' string.
              | Done r Position B.ByteString
              -- ^ The parse succeeded.  The 'B.ByteString' is the
              --   input that had not yet been consumed (if any) when
              --   the parse succeeded.

instance Show r => Show (Result r) where
    show (Fail msg)  = "Fail " ++ show msg
    show (Partial _) = "Partial _"
    show (Done r pos bs) = "Done " ++ show r ++ " " ++ show pos ++ " " ++ show bs

instance Functor Result where
    fmap _ (Fail msg)  = Fail msg
    fmap f (Partial k) = Partial (fmap f . k)
    fmap f (Done r p bs) = Done (f r) p bs

type Input  = B.ByteString
type Buffer = Maybe B.ByteString

type Failure   r = Input -> Buffer -> More -> Position -> String -> Result r
type Success a r = Input -> Buffer -> More -> Position -> a      -> Result r
type Position    = Word64

-- | Have we read all available input?
data More = Complete
          | Incomplete (Maybe Int)
          deriving (Eq)

-- | The Get monad is an Exception and State monad.
newtype Get a = Get
    { unGet :: forall r. Input -> Buffer -> More -> Position -> Failure r -> Success a r -> Result r }

append :: Buffer -> Buffer -> Buffer
append l r = B.append `fmap` l <*> r
{-# INLINE append #-}

bufferBytes :: Buffer -> B.ByteString
bufferBytes  = fromMaybe B.empty
{-# INLINE bufferBytes #-}

instance Functor Get where
    fmap p m =
      Get $ \s0 b0 m0 p0 kf ks ->
        let ks' s1 b1 m1 p1 a = ks s1 b1 m1 p1 (p a)
         in unGet m s0 b0 m0 p0 kf ks'

instance Applicative Get where
    pure  = return
    (<*>) = ap

instance Alternative Get where
    empty = failDesc "empty"
    (<|>) = mplus

-- Definition directly from Control.Monad.State.Strict
instance Monad Get where
    return a = Get $ \ s0 b0 m0 p0 _ ks -> ks s0 b0 m0 p0 a

    m >>= g  = Get $ \s0 b0 m0 p0 kf ks ->
        let ks' s1 b1 m1 p1 a = unGet (g a) s1 b1 m1 p1 kf ks
         in unGet m s0 b0 m0 p0 kf ks'

    fail     = failDesc

instance MonadPlus Get where
    mzero     = failDesc "mzero"
    mplus a b =
      Get $ \s0 b0 m0 p0 kf ks ->
        let kf' _ b1 m1 p1 _ = unGet b (s0 `B.append` bufferBytes b1)
                                       (b0 `append` b1) m1 p1 kf ks
         in unGet a s0 (Just B.empty) m0 p0 kf' ks

------------------------------------------------------------------------

put :: Position -> B.ByteString -> Get ()
put pos s = Get (\_ b0 m p0 _ k -> k s b0 m (p0+pos) ())
{-# INLINE put #-}

finalK :: B.ByteString -> t -> t1 -> Position -> r -> Result r
finalK s _ _ p a = Done a p s

failK :: Failure a
failK _ _ _ p s = Fail (show p ++ ":" ++ s)

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGetPos :: Position -> Get a -> B.ByteString -> Result a
runGetPos pos m str = unGet m str Nothing (Incomplete Nothing) pos failK finalK
{-# INLINE runGetPos #-}

runGet :: Get a -> B.ByteString -> Result a
runGet = runGetPos 0
{-# INLINE runGet #-}

-- | If at least @n@ bytes of input are available, return the current
--   input, otherwise fail.
ensure :: Int -> Get B.ByteString
ensure n = n `seq` Get $ \ s0 b0 m0 p0 kf ks ->
    if B.length s0 >= n
    then ks s0 b0 m0 p0 s0
    else unGet (demandInput >> ensureRec n) s0 b0 m0 p0 kf ks
{-# INLINE ensure #-}

-- | If at least @n@ bytes of input are available, return the current
--   input, otherwise fail.
ensureRec :: Int -> Get B.ByteString
ensureRec n = Get $ \s0 b0 m0 p0 kf ks ->
    if B.length s0 >= n
    then ks s0 b0 m0 p0 s0
    else unGet (demandInput >> ensureRec n) s0 b0 m0 p0 kf ks

-- | Immediately demand more input via a 'Partial' continuation
--   result.
demandInput :: Get ()
demandInput = Get $ \s0 b0 m0 p0 kf ks ->
  case m0 of
    Complete      -> kf s0 b0 m0 p0 "too few bytes"
    Incomplete mb -> Partial $ \s ->
      if B.null s
      then kf s0 b0 m0 p0 "too few bytes"
      else let update l = l - B.length s
               s1 = s0 `B.append` s
               b1 = b0 `append` Just s
            in ks s1 b1 (Incomplete (update `fmap` mb)) p0 ()

failDesc :: String -> Get a
failDesc err = Get (\s0 b0 m0 p0 kf _ -> kf s0 b0 m0 p0 ("Failed reading: " ++ err))

------------------------------------------------------------------------
-- Utility with ByteStrings

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input. This function creates a fresh
-- copy of the underlying bytes.
getBytesCopy :: Int -> Get B.ByteString
getBytesCopy n = do
  bs <- getBytes n
  return $! B.copy bs

------------------------------------------------------------------------
-- Helpers

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get B.ByteString
getBytes n = do
     s <- ensure n
     put (fromIntegral n) $ B.unsafeDrop n s
     return $ B.unsafeTake n s

getWord8 :: Get Word8
getWord8 = do
     s <- ensure 1
     put 1 $ B.unsafeTail s
     return $ B.unsafeHead s

{- | uintOfBytes returns the number of bytes and the unsigned integer represented by the bytes -}
uintOfBytes :: ByteString -> (Int, Integer)
uintOfBytes b = (B.length b, B.foldl (\acc n -> (acc `shiftL` 8) + fromIntegral n) 0 b)

--bytesOfUInt i = B.unfoldr (\x -> if x == 0 then Nothing else Just (fromIntegral (x .&. 0xff), x `shiftR` 8)) i
bytesOfUInt :: Integer -> [Word8]
bytesOfUInt x = reverse (list x)
    where list i = if i <= 0xff then [fromIntegral i] else (fromIntegral i .&. 0xff) : list (i `shiftR` 8)

{- | intOfBytes returns the number of bytes in the list and
   the represented integer by a two's completement list of bytes -}
intOfBytes :: ByteString -> (Int, Integer)
intOfBytes b
    | B.length b == 0   = (0, 0)
    | otherwise         = (len, if isNeg then -(maxIntLen - v + 1) else v)
    where
        (len, v)  = uintOfBytes b
        maxIntLen = 2 ^ (8 * len) - 1
        isNeg     = testBit (B.head b) 7

{- | bytesOfInt convert an integer into a two's completemented list of bytes -}
bytesOfInt :: Integer -> [Word8]
bytesOfInt i
    | i > 0      = if testBit (head uints) 7 then 0 : uints else uints
    | i == 0     = [0]
    | otherwise  = if testBit (head nints) 7 then nints else 0xff : nints
    where
        uints = bytesOfUInt (abs i)
        nints = reverse $ plusOne $ reverse $ map complement $ uints
        plusOne []     = [1]
        plusOne (x:xs) = if x == 0xff then 0 : plusOne xs else (x+1) : xs

{- ASN1 often uses a particular kind of 7-bit encoding of integers like
   in the case of long tags or encoding of integer component of OID's.
   Use this function for such an encoding. Assumes a positive integer.

   Here is the description of the algorithm of the above encoding:

   1. The integer is chunked up into 7-bit groups. Each of these 7bit
      chunks are encoded as a single octet.

   2. All the octets except the last one has its 8th bit set.
-}
putVarEncodingIntegral :: (Bits i, Integral i) => i -> ByteString
putVarEncodingIntegral i = B.reverse $ B.unfoldr genOctets (i,True)
    where genOctets (x,first)
            | x > 0     =
                let out = fromIntegral (x .&. 0x7F) .|. (if first then 0 else 0x80) in
                Just (out, (shiftR x 7, False))
            | otherwise = Nothing

encodeHeader :: Bool -> ASN1Length -> ASN1 -> ASN1Header
encodeHeader pc len (Boolean _)                = ASN1Header Universal 0x1 pc len
encodeHeader pc len (IntVal _)                 = ASN1Header Universal 0x2 pc len
encodeHeader pc len (BitString _)              = ASN1Header Universal 0x3 pc len
encodeHeader pc len (OctetString _)            = ASN1Header Universal 0x4 pc len
encodeHeader pc len Null                       = ASN1Header Universal 0x5 pc len
encodeHeader pc len (OID _)                    = ASN1Header Universal 0x6 pc len
encodeHeader pc len (Real _)                   = ASN1Header Universal 0x9 pc len
encodeHeader pc len (Enumerated _)             = ASN1Header Universal 0xa pc len
encodeHeader pc len (ASN1String cs)            = ASN1Header Universal (characterStringType $ characterEncoding cs) pc len
  where characterStringType UTF8      = 0xc
        characterStringType Numeric   = 0x12
        characterStringType Printable = 0x13
        characterStringType T61       = 0x14
        characterStringType VideoTex  = 0x15
        characterStringType IA5       = 0x16
        characterStringType Graphic   = 0x19
        characterStringType Visible   = 0x1a
        characterStringType General   = 0x1b
        characterStringType UTF32     = 0x1c
        characterStringType Character = 0x1d
        characterStringType BMP       = 0x1e
encodeHeader pc len (ASN1Time TimeUTC _ _)     = ASN1Header Universal 0x17 pc len
encodeHeader pc len (ASN1Time TimeGeneralized _ _) = ASN1Header Universal 0x18 pc len
encodeHeader pc len (Start Sequence)           = ASN1Header Universal 0x10 pc len
encodeHeader pc len (Start Set)                = ASN1Header Universal 0x11 pc len
encodeHeader pc len (Start (Container tc tag)) = ASN1Header tc tag pc len
encodeHeader pc len (Other tc tag _)           = ASN1Header tc tag pc len
encodeHeader _ _ (End _)                       = error "this should not happen"

encodePrimitiveHeader :: ASN1Length -> ASN1 -> ASN1Header
encodePrimitiveHeader = encodeHeader False

encodePrimitiveData :: ASN1 -> ByteString
encodePrimitiveData (Boolean b)         = B.singleton (if b then 0xff else 0)
encodePrimitiveData (IntVal i)          = putInteger i
encodePrimitiveData (BitString bits)    = putBitString bits
encodePrimitiveData (OctetString b)     = putString b
encodePrimitiveData Null                = B.empty
encodePrimitiveData (OID oidv)          = putOID oidv
encodePrimitiveData (Real _)            = B.empty -- not implemented
encodePrimitiveData (Enumerated i)      = putInteger $ fromIntegral i
encodePrimitiveData (ASN1String cs)     = getCharacterStringRawData cs
encodePrimitiveData (ASN1Time ty ti tz) = putTime ty ti tz
encodePrimitiveData (Other _ _ b)       = b
encodePrimitiveData o                   = error ("not a primitive " ++ show o)

encodePrimitive :: ASN1 -> (Int, [ASN1Event])
encodePrimitive a =
    let b = encodePrimitiveData a
        blen = B.length b
        len = makeLength blen
        hdr = encodePrimitiveHeader len a
     in (B.length (putHeader hdr) + blen, [Header hdr, Primitive b])
  where
        makeLength len
            | len < 0x80 = LenShort len
            | otherwise  = LenLong (nbBytes len) len
        nbBytes nb = if nb > 255 then 1 + nbBytes (nb `div` 256) else 1

encodeOne :: ASN1 -> (Int, [ASN1Event])
encodeOne (Start _) = error "encode one cannot do start"
encodeOne t         = encodePrimitive t

encodeList :: [ASN1] -> (Int, [ASN1Event])
encodeList []               = (0, [])
encodeList (End _:xs)       = encodeList xs
encodeList (t@(Start _):xs) =
    let (ys, zs)    = getConstructedEnd 0 xs
        (llen, lev) = encodeList zs
        (len, ev)   = encodeConstructed t ys
     in (llen + len, ev ++ lev)

encodeList (x:xs)           =
    let (llen, lev) = encodeList xs
        (len, ev)   = encodeOne x
     in (llen + len, ev ++ lev)

encodeConstructed :: ASN1 -> [ASN1] -> (Int, [ASN1Event])
encodeConstructed c@(Start _) children =
    (tlen, Header h : ConstructionBegin : events ++ [ConstructionEnd])
  where (clen, events) = encodeList children
        len  = mkSmallestLength clen
        h    = encodeHeader True len c
        tlen = B.length (putHeader h) + clen

encodeConstructed _ _ = error "not a start node"

mkSmallestLength :: Int -> ASN1Length
mkSmallestLength i
    | i < 0x80  = LenShort i
    | otherwise = LenLong (nbBytes i) i
        where nbBytes nb = if nb > 255 then 1 + nbBytes (nb `div` 256) else 1

type ASN1Ret = Either ASN1Error ASN1

decodePrimitive :: ASN1Header -> B.ByteString -> ASN1Ret
decodePrimitive (ASN1Header Universal 0x1 _ _) p   = getBoolean False p
decodePrimitive (ASN1Header Universal 0x2 _ _) p   = getInteger p
decodePrimitive (ASN1Header Universal 0x3 _ _) p   = getBitString p
decodePrimitive (ASN1Header Universal 0x4 _ _) p   = getOctetString p
decodePrimitive (ASN1Header Universal 0x5 _ _) p   = getNull p
decodePrimitive (ASN1Header Universal 0x6 _ _) p   = getOID p
decodePrimitive (ASN1Header Universal 0x7 _ _) _   = Left $ TypeNotImplemented "Object Descriptor"
decodePrimitive (ASN1Header Universal 0x8 _ _) _   = Left $ TypeNotImplemented "External"
decodePrimitive (ASN1Header Universal 0x9 _ _) _   = Left $ TypeNotImplemented "real"
decodePrimitive (ASN1Header Universal 0xa _ _) p   = getEnumerated p
decodePrimitive (ASN1Header Universal 0xb _ _) _   = Left $ TypeNotImplemented "EMBEDDED PDV"
decodePrimitive (ASN1Header Universal 0xc _ _) p   = getCharacterString UTF8 p
decodePrimitive (ASN1Header Universal 0xd _ _) _   = Left $ TypeNotImplemented "RELATIVE-OID"
decodePrimitive (ASN1Header Universal 0x10 _ _) _  = error "sequence not a primitive"
decodePrimitive (ASN1Header Universal 0x11 _ _) _  = error "set not a primitive"
decodePrimitive (ASN1Header Universal 0x12 _ _) p  = getCharacterString Numeric p
decodePrimitive (ASN1Header Universal 0x13 _ _) p  = getCharacterString Printable p
decodePrimitive (ASN1Header Universal 0x14 _ _) p  = getCharacterString T61 p
decodePrimitive (ASN1Header Universal 0x15 _ _) p  = getCharacterString VideoTex p
decodePrimitive (ASN1Header Universal 0x16 _ _) p  = getCharacterString IA5 p
decodePrimitive (ASN1Header Universal 0x17 _ _) p  = getTime TimeUTC p
decodePrimitive (ASN1Header Universal 0x18 _ _) p  = getTime TimeGeneralized p
decodePrimitive (ASN1Header Universal 0x19 _ _) p  = getCharacterString Graphic p
decodePrimitive (ASN1Header Universal 0x1a _ _) p  = getCharacterString Visible p
decodePrimitive (ASN1Header Universal 0x1b _ _) p  = getCharacterString General p
decodePrimitive (ASN1Header Universal 0x1c _ _) p  = getCharacterString UTF32 p
decodePrimitive (ASN1Header Universal 0x1d _ _) p  = getCharacterString Character p
decodePrimitive (ASN1Header Universal 0x1e _ _) p  = getCharacterString BMP p
decodePrimitive (ASN1Header tc        tag  _ _) p  = Right $ Other tc tag p


getBoolean :: Bool -> ByteString -> Either ASN1Error ASN1
getBoolean isDer s =
    if B.length s == 1
        then case B.head s of
            0    -> Right (Boolean False)
            0xff -> Right (Boolean True)
            _    -> if isDer then Left $ PolicyFailed "DER" "boolean value not canonical" else Right (Boolean True)
        else Left $ TypeDecodingFailed "boolean: length not within bound"

{- | getInteger, parse a value bytestring and get the integer out of the two complement encoded bytes -}
getInteger :: ByteString -> Either ASN1Error ASN1
{-# INLINE getInteger #-}
getInteger s = IntVal <$> getIntegerRaw "integer" s

{- | getEnumerated, parse an enumerated value the same way that integer values are parsed. -}
getEnumerated :: ByteString -> Either ASN1Error ASN1
{-# INLINE getEnumerated #-}
getEnumerated s = Enumerated <$> getIntegerRaw "enumerated" s

{- | According to X.690 section 8.4 integer and enumerated values should be encoded the same way. -}
getIntegerRaw :: String -> ByteString -> Either ASN1Error Integer
getIntegerRaw typestr s
    | B.length s == 0 = Left . TypeDecodingFailed $ typestr ++ ": null encoding"
    | B.length s == 1 = Right $ snd $ intOfBytes s
    | otherwise       =
        if (v1 == 0xff && testBit v2 7) || (v1 == 0x0 && (not $ testBit v2 7))
            then Left . TypeDecodingFailed $ typestr ++ ": not shortest encoding"
            else Right $ snd $ intOfBytes s
        where
            v1 = s `B.index` 0
            v2 = s `B.index` 1

getBitString :: ByteString -> Either ASN1Error ASN1
getBitString s =
    let toSkip = B.head s in
    let toSkip' = if toSkip >= 48 && toSkip <= 48 + 7 then toSkip - (fromIntegral $ ord '0') else toSkip in
    let xs = B.tail s in
    if toSkip' >= 0 && toSkip' <= 7
        then Right $ BitString $ toBitArray xs (fromIntegral toSkip')
        else Left $ TypeDecodingFailed ("bitstring: skip number not within bound " ++ show toSkip' ++ " " ++  show s)

getCharacterString :: ASN1StringEncoding -> ByteString -> Either ASN1Error ASN1
getCharacterString encoding bs = Right $ ASN1String (ASN1CharacterString encoding bs)

getOctetString :: ByteString -> Either ASN1Error ASN1
getOctetString = Right . OctetString

getNull :: ByteString -> Either ASN1Error ASN1
getNull s
    | B.length s == 0 = Right Null
    | otherwise       = Left $ TypeDecodingFailed "Null: data length not within bound"

{- | return an OID -}
getOID :: ByteString -> Either ASN1Error ASN1
getOID s = Right $ OID $ (fromIntegral (x `div` 40) : fromIntegral (x `mod` 40) : groupOID xs)
  where
        (x:xs) = B.unpack s

        groupOID :: [Word8] -> [Integer]
        groupOID = map (foldl (\acc n -> (acc `shiftL` 7) + fromIntegral n) 0) . groupSubOID

        groupSubOIDHelper [] = Nothing
        groupSubOIDHelper l  = Just $ spanSubOIDbound l

        groupSubOID :: [Word8] -> [[Word8]]
        groupSubOID = unfoldr groupSubOIDHelper

        spanSubOIDbound [] = ([], [])
        spanSubOIDbound (a:as) = if testBit a 7 then (clearBit a 7 : ys, zs) else ([a], as)
            where (ys, zs) = spanSubOIDbound as

getTime :: ASN1TimeType -> ByteString -> Either ASN1Error ASN1
getTime = undefined
{-
getTime timeType bs
    | hasNonASCII bs = decodingError "contains non ASCII characters"
    | otherwise      =
        case timeParseE format (BC.unpack bs) of -- BC.unpack is safe as we check ASCIIness first
            Left _  ->
                case timeParseE formatNoSeconds (BC.unpack bs) of
                    Left _  -> decodingError ("cannot convert string " ++ BC.unpack bs)
                    Right r -> parseRemaining r
            Right r -> parseRemaining r
  where
        parseRemaining r =
            case parseTimezone $ parseMs $ first adjustUTC r of
                Left err        -> decodingError err
                Right (dt', tz) -> Right $ ASN1Time timeType dt' tz

        adjustUTC dt@(DateTime (Date y m d) tod)
            | timeType == TimeGeneralized = dt
            | y > 2050                    = DateTime (Date (y - 100) m d) tod
            | otherwise                   = dt
        formatNoSeconds = init format
        format | timeType == TimeGeneralized = 'Y':'Y':baseFormat
               | otherwise                   = baseFormat
        baseFormat = "YYMMDDHMIS"

        parseMs (dt,s) =
            case s of
                '.':s' -> let (ns, r) = first toNano $ spanToLength 3 isDigit s'
                           in (dt { dtTime = (dtTime dt) { todNSec = ns } }, r)
                _      -> (dt,s)
        parseTimezone (dt,s) =
            case s of
                '+':s' -> Right (dt, parseTimezoneFormat id s')
                '-':s' -> Right (dt, parseTimezoneFormat ((-1) *) s')
                'Z':[] -> Right (dt, Just timezone_UTC)
                ""     -> Right (dt, Nothing)
                _      -> Left ("unknown timezone format: " ++ s)

        parseTimezoneFormat transform s
            | length s == 4  = Just $ toTz $ toInt $ fst $ spanToLength 4 isDigit s
            | otherwise      = Nothing
          where toTz z = let (h,m) = z `divMod` 100 in TimezoneOffset $ transform (h * 60 + m)

        toNano :: String -> NanoSeconds
        toNano l = fromIntegral (toInt l * order * 1000000)
          where len   = length l
                order = case len of
                            1 -> 100
                            2 -> 10
                            3 -> 1
                            _ -> 1

        spanToLength :: Int -> (Char -> Bool) -> String -> (String, String)
        spanToLength len p l = loop 0 l
          where loop i z
                    | i >= len  = ([], z)
                    | otherwise = case z of
                                    []   -> ([], [])
                                    x:xs -> if p x
                                                then let (r1,r2) = loop (i+1) xs
                                                      in (x:r1, r2)
                                                else ([], z)

        toInt :: String -> Int
        toInt = foldl (\acc w -> acc * 10 + (ord w - ord '0')) 0

        decodingError reason = Left $ TypeDecodingFailed ("time format invalid for " ++ show timeType ++ " : " ++ reason)
        hasNonASCII = maybe False (const True) . B.find (\c -> c > 0x7f)
-}

-- FIXME need msec printed
putTime :: ASN1TimeType -> UTCTime -> Maybe TimeZone -> ByteString
putTime = undefined
{-
putTime ty dt mtz = BC.pack etime
  where
        etime
            | ty == TimeUTC = timePrint "YYMMDDHMIS" dt ++ tzStr
            | otherwise     = timePrint "YYYYMMDDHMIS" dt ++ msecStr ++ tzStr
        msecStr = []
        tzStr = case mtz of
                     Nothing                      -> ""
                     Just tz | tz == timezone_UTC -> "Z"
                             | otherwise          -> show tz
-}

putInteger :: Integer -> ByteString
putInteger i = B.pack $ bytesOfInt i

putBitString :: BitArray -> ByteString
putBitString (BitArray n bits) =
    B.concat [B.singleton (fromIntegral i),bits]
  where i = (8 - (n `mod` 8)) .&. 0x7

putString :: ByteString -> ByteString
putString l = l

{- no enforce check that oid1 is between [0..2] and oid2 is between [0..39] -}
putOID :: [Integer] -> ByteString
putOID oids = case oids of
    (oid1:oid2:suboids) ->
        let eoidclass = fromIntegral (oid1 * 40 + oid2)
            subeoids  = B.concat $ map encode suboids
         in B.cons eoidclass subeoids
    _                   -> error ("invalid OID format " ++ show oids)
  where
        encode x | x == 0    = B.singleton 0
                 | otherwise = putVarEncodingIntegral x


-- | parse an ASN1 header
getHeader :: Get ASN1Header
getHeader = do
    (cl,pc,t1) <- parseFirstWord <$> getWord8
    tag        <- if t1 == 0x1f then getTagLong else return t1
    len        <- getLength
    return $ ASN1Header cl tag pc len

-- | Parse the first word of an header
parseFirstWord :: Word8 -> (ASN1Class, Bool, ASN1Tag)
parseFirstWord w = (cl,pc,t1)
  where cl = toEnum $ fromIntegral $ (w `shiftR` 6)
        pc = testBit w 5
        t1 = fromIntegral (w .&. 0x1f)

{- when the first tag is 0x1f, the tag is in long form, where
 - we get bytes while the 7th bit is set. -}
getTagLong :: Get ASN1Tag
getTagLong = do
    t <- fromIntegral <$> getWord8
    when (t == 0x80) $ error "not canonical encoding of tag"
    if testBit t 7
        then loop (clearBit t 7)
        else return t
  where loop n = do
            t <- fromIntegral <$> getWord8
            if testBit t 7
                then loop (n `shiftL` 7 + clearBit t 7)
                else return (n `shiftL` 7 + t)


{- get the asn1 length which is either short form if 7th bit is not set,
 - indefinite form is the 7 bit is set and every other bits clear,
 - or long form otherwise, where the next bytes will represent the length
 -}
getLength :: Get ASN1Length
getLength = do
    l1 <- fromIntegral <$> getWord8
    if testBit l1 7
        then case clearBit l1 7 of
            0   -> return LenIndefinite
            len -> do
                lw <- getBytes len
                return (LenLong len $ uintbs lw)
        else
            return (LenShort l1)
  where
        {- uintbs return the unsigned int represented by the bytes -}
        uintbs = B.foldl (\acc n -> (acc `shiftL` 8) + fromIntegral n) 0

-- | putIdentifier encode an ASN1 Identifier into a marshalled value
putHeader :: ASN1Header -> B.ByteString
putHeader (ASN1Header cl tag pc len) = B.concat
    [ B.singleton word1
    , if tag < 0x1f then B.empty else tagBS
    , lenBS]
  where cli   = shiftL (fromIntegral $ fromEnum cl) 6
        pcval = shiftL (if pc then 0x1 else 0x0) 5
        tag0  = if tag < 0x1f then fromIntegral tag else 0x1f
        word1 = cli .|. pcval .|. tag0
        lenBS = B.pack $ putLength len
        tagBS = putVarEncodingIntegral tag

{- | putLength encode a length into a ASN1 length.
 - see getLength for the encoding rules -}
putLength :: ASN1Length -> [Word8]
putLength (LenShort i)
    | i < 0 || i > 0x7f = error "putLength: short length is not between 0x0 and 0x80"
    | otherwise         = [fromIntegral i]
putLength (LenLong _ i)
    | i < 0     = error "putLength: long length is negative"
    | otherwise = lenbytes : lw
        where
            lw       = bytesOfUInt $ fromIntegral i
            lenbytes = fromIntegral (length lw .|. 0x80)
putLength (LenIndefinite) = [0x80]

{- associate a list of asn1 event with an ASN1 type.
 - it's sometimes required to know the exact byte sequence leading to an ASN1 type:
 - eg: cryptographic signature -}
type ASN1Repr = (ASN1, [ASN1Event])

getConstructedEnd :: Int -> [ASN1] -> ([ASN1],[ASN1])
getConstructedEnd _ xs@[]                = (xs, [])
getConstructedEnd i ((x@(Start _)):xs)   = let (yz, zs) = getConstructedEnd (i+1) xs in (x:yz,zs)
getConstructedEnd i ((x@(End _)):xs)
    | i == 0    = ([], xs)
    | otherwise = let (ys, zs) = getConstructedEnd (i-1) xs in (x:ys,zs)
getConstructedEnd i (x:xs)               = let (ys, zs) = getConstructedEnd i xs in (x:ys,zs)

getConstructedEndRepr :: [ASN1Repr] -> ([ASN1Repr],[ASN1Repr])
getConstructedEndRepr = g
    where g []                 = ([], [])
          g (x@(Start _,_):xs) = let (ys, zs) = getEnd 1 xs in (x:ys, zs)
          g (x:xs)             = ([x],xs)

          getEnd :: Int -> [ASN1Repr] -> ([ASN1Repr],[ASN1Repr])
          getEnd _ []                    = ([], [])
          getEnd 0 xs                    = ([], xs)
          getEnd i ((x@(Start _, _)):xs) = let (ys, zs) = getEnd (i+1) xs in (x:ys,zs)
          getEnd i ((x@(End _, _)):xs)   = let (ys, zs) = getEnd (i-1) xs in (x:ys,zs)
          getEnd i (x:xs)                = let (ys, zs) = getEnd i xs in (x:ys,zs)
{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

-- | throwed in case of out of bounds in the bitarray.
data BitArrayOutOfBound = BitArrayOutOfBound Word64
    deriving (Show,Eq,Typeable)
instance Exception BitArrayOutOfBound

-- | represent a bitarray / bitmap
--
-- the memory representation start at bit 0
data BitArray = BitArray Word64 ByteString
    deriving (Show,Eq)

-- | returns the length of bits in this bitarray
bitArrayLength :: BitArray -> Word64
bitArrayLength (BitArray l _) = l

bitArrayOutOfBound :: Word64 -> a
bitArrayOutOfBound n = throw $ BitArrayOutOfBound n

-- | get the nth bits
bitArrayGetBit :: BitArray -> Word64 -> Bool
bitArrayGetBit (BitArray l d) n
    | n >= l    = bitArrayOutOfBound n
    | otherwise = flip testBit (7-fromIntegral bitn) $ B.index d (fromIntegral offset)
        where (offset, bitn) = n `divMod` 8

-- | set the nth bit to the value specified
bitArraySetBitValue :: BitArray -> Word64 -> Bool -> BitArray
bitArraySetBitValue (BitArray l d) n v
    | n >= l    = bitArrayOutOfBound n
    | otherwise =
        let (before,after) = B.splitAt (fromIntegral offset) d in
        -- array bound check before prevent fromJust from failing.
        let (w,remaining) = fromJust $ B.uncons after in
        BitArray l (before `B.append` (setter w (7-fromIntegral bitn) `B.cons` remaining))
  where
        (offset, bitn) = n `divMod` 8
        setter = if v then setBit else clearBit

-- | set the nth bits
bitArraySetBit :: BitArray -> Word64 -> BitArray
bitArraySetBit bitarray n = bitArraySetBitValue bitarray n True

-- | clear the nth bits
bitArrayClearBit :: BitArray -> Word64 -> BitArray
bitArrayClearBit bitarray n = bitArraySetBitValue bitarray n False

-- | get padded bytestring of the bitarray
bitArrayGetData :: BitArray -> ByteString
bitArrayGetData (BitArray _ d) = d

-- | number of bit to skip at the end (padding)
toBitArray :: ByteString -> Int -> BitArray
toBitArray l toSkip =
    BitArray (fromIntegral (B.length l * 8 - fromIntegral toSkip)) l

-- | Standard ASN.1 Object ID (OID)
type OID = [Integer]

-- | Class of things that have an Object ID
class OIDable a where
    -- | return the object ID of an Object from the ObjectIdentifiable class.
    getObjectID :: a -> OID

-- | Class of things that can be named by Object ID
class OIDNameable a where
    -- | Try to convert an OID into an Object
    fromObjectID :: OID -> Maybe a

data PrettyType = Multiline Int -- Offset where to start
                | SingleLine
    deriving (Show,Eq)

-- | Pretty Print a list of ASN.1 element
pretty :: PrettyType -- ^ indent level in space character
       -> [ASN1]     -- ^ stream of ASN1
       -> String
pretty (Multiline at) = prettyPrint at
  where
    indent n = replicate n ' '

    prettyPrint _ []                 = ""
    prettyPrint n (x@(Start _) : xs) = indent n     ++ p9 id x ++ prettyPrint (n+1) xs
    prettyPrint n (x@(End _) : xs)   = indent (n-1) ++ p9 id x ++ prettyPrint (n-1) xs
    prettyPrint n (x : xs)           = indent n     ++ p9 id x ++ prettyPrint n xs

pretty SingleLine = prettyPrint
  where
    prettyPrint []                 = ""
    prettyPrint (x@(Start _) : xs) = p9 id x ++ "," ++ prettyPrint xs
    prettyPrint (x@(End _) : xs)   = p9 id x ++ "," ++ prettyPrint xs
    prettyPrint (x : xs)           = p9 id x ++ "," ++ prettyPrint xs

p9 :: ([Char] -> t) -> ASN1 -> t
p9 put9 (Boolean b)                        = put9 ("bool: " ++ show b)
p9 put9 (IntVal i)                         = put9 ("int: " ++ showHex i "")
p9 put9 (BitString bits)                   = put9 ("bitstring: " ++ (hexdump $ bitArrayGetData bits))
p9 put9 (OctetString bs)                   = put9 ("octetstring: " ++ hexdump bs)
p9 put9 (Null)                             = put9 "null"
p9 put9 (OID is)                           = put9 ("OID: " ++ show is)
p9 put9 (Real d)                           = put9 ("real: " ++ show d)
p9 put9 (Enumerated _)                     = put9 "enum"
p9 put9 (Start Sequence)                   = put9 "{"
p9 put9 (End Sequence)                     = put9 "}"
p9 put9 (Start Set)                        = put9 "["
p9 put9 (End Set)                          = put9 "]"
p9 put9 (Start (Container x y))            = put9 ("< " ++ show x ++ " " ++ show y)
p9 put9 (End (Container x y))              = put9 ("> " ++ show x ++ " " ++ show y)
p9 put9 (ASN1String cs)                    = putCS put9 cs
p9 put9 (ASN1Time TimeUTC time tz)         = put9 ("utctime: " ++ show time ++ " " ++ show tz)
p9 put9 (ASN1Time TimeGeneralized time tz) = put9 ("generalizedtime: " ++ show time ++ " " ++ show tz)
p9 put9 (Other tc tn x)                    = put9 ("other(" ++ show tc ++ "," ++ show tn ++ "," ++ show x ++ ")")

putCS :: ([Char] -> t) -> ASN1CharacterString -> t
putCS put9 (ASN1CharacterString UTF8 t)         = put9 ("utf8string:" ++ show t)
putCS put9 (ASN1CharacterString Numeric bs)     = put9 ("numericstring:" ++ hexdump bs)
putCS put9 (ASN1CharacterString Printable t)    = put9 ("printablestring: " ++ show t)
putCS put9 (ASN1CharacterString T61 bs)         = put9 ("t61string:" ++ show bs)
putCS put9 (ASN1CharacterString VideoTex bs)    = put9 ("videotexstring:" ++ hexdump bs)
putCS put9 (ASN1CharacterString IA5 bs)         = put9 ("ia5string:" ++ show bs)
putCS put9 (ASN1CharacterString Graphic bs)     = put9 ("graphicstring:" ++ hexdump bs)
putCS put9 (ASN1CharacterString Visible bs)     = put9 ("visiblestring:" ++ hexdump bs)
putCS put9 (ASN1CharacterString General bs)     = put9 ("generalstring:" ++ hexdump bs)
putCS put9 (ASN1CharacterString UTF32 t)        = put9 ("universalstring:" ++ show t)
putCS put9 (ASN1CharacterString Character bs)   = put9 ("characterstring:" ++ hexdump bs)
putCS put9 (ASN1CharacterString BMP t)          = put9 ("bmpstring: " ++ show t)

hexdump :: ByteString -> String
hexdump = stringDigest

-- | Define the type of container
data ASN1ConstructionType = Sequence
                          | Set
                          | Container ASN1Class ASN1Tag
                          deriving (Show,Eq)

-- | Different ASN1 time representation
data ASN1TimeType = TimeUTC         -- ^ ASN1 UTCTime Type: limited between 1950-2050
                  | TimeGeneralized -- ^ ASN1 GeneralizedTime Type
                  deriving (Show,Eq,Ord)

-- | Define high level ASN1 object.
data ASN1 =
      Boolean Bool
    | IntVal  Integer
    | BitString BitArray
    | OctetString ByteString
    | Null
    | OID  OID
    | Real Double
    | Enumerated Integer
    | ASN1String ASN1CharacterString
    | ASN1Time ASN1TimeType UTCTime (Maybe TimeZone)
    | Other ASN1Class ASN1Tag ByteString
    | Start ASN1ConstructionType
    | End   ASN1ConstructionType
    deriving (Show, Eq)

-- | represent a chunk of ASN1 Stream.
-- this is equivalent to ShowS but for an ASN1 Stream.
type ASN1S = [ASN1] -> [ASN1]

-- | Define an object that can be converted to and from ASN.1
class ASN1Object a where
    -- | transform an object into a chunk of ASN1 stream.
    toASN1   :: a      -> ASN1S

    -- | returns either an object along the remaining ASN1 stream,
    -- or an error.
    fromASN1 :: [ASN1] -> Either String (a, [ASN1])

-- | Element class
data ASN1Class = Universal
               | Application
               | Context
               | Private
               deriving (Show,Eq,Ord,Enum)

-- | ASN1 Tag
type ASN1Tag = Int

-- | ASN1 Length with all different formats
data ASN1Length = LenShort Int      -- ^ Short form with only one byte. length has to be < 127.
                | LenLong Int Int   -- ^ Long form of N bytes
                | LenIndefinite     -- ^ Length is indefinite expect an EOC in the stream to finish the type
                deriving (Show,Eq)

-- | ASN1 Header with the class, tag, constructed flag and length.
data ASN1Header = ASN1Header !ASN1Class !ASN1Tag !Bool !ASN1Length
    deriving (Show,Eq)

-- | represent one event from an asn1 data stream
data ASN1Event = Header ASN1Header     -- ^ ASN1 Header
               | Primitive !ByteString -- ^ Primitive
               | ConstructionBegin     -- ^ Constructed value start
               | ConstructionEnd       -- ^ Constructed value end
               deriving (Show,Eq)

-- a note on T61 encodings. The actual specification of a T61 character set seems
-- to be lost in time, as such it will be considered an ascii like encoding.
--
-- <http://www.mail-archive.com/asn1@asn1.org/msg00460.html>
-- "sizable volume of software in the world treats TeletexString (T61String)
-- as a simple 8-bit string with mostly Windows Latin 1"

-- | Define all possible ASN1 String encoding.
data ASN1StringEncoding =
      IA5       -- ^ 128 characters equivalent to the ASCII alphabet
    | UTF8      -- ^ UTF8
    | General   -- ^ all registered graphic and character sets (see ISO 2375) plus SPACE and DELETE.
    | Graphic   -- ^ all registered G sets and SPACE
    | Numeric   -- ^ encoding containing numeric [0-9] and space
    | Printable -- ^ printable [a-z] [A-Z] [()+,-.?:/=] and space.
    | VideoTex  -- ^ CCITT's T.100 and T.101 character sets
    | Visible   -- ^ International ASCII printing character sets
    | T61       -- ^ teletext
    | UTF32     -- ^ UTF32
    | Character -- ^ Character
    | BMP       -- ^ UCS2
    deriving (Show,Eq,Ord)

-- | provide a way to possibly encode or decode character string based on character encoding
stringEncodingFunctions :: ASN1StringEncoding
                        -> Maybe (ByteString -> String, String -> ByteString)
stringEncodingFunctions encoding
    | encoding == UTF8                   = Just (decodeUTF8, encodeUTF8)
    | encoding == BMP                    = Just (decodeBMP, encodeBMP)
    | encoding == UTF32                  = Just (decodeUTF32, encodeUTF32)
    | encoding `elem` asciiLikeEncodings = Just (decodeASCII, encodeASCII)
    | otherwise                          = Nothing
  where asciiLikeEncodings = [IA5,Numeric,Printable,Visible,General,Graphic,T61]

-- | encode a string into a character string
asn1CharacterString :: ASN1StringEncoding -> String -> ASN1CharacterString
asn1CharacterString encoding s =
    case stringEncodingFunctions encoding of
        Just (_, e) -> ASN1CharacterString encoding (e s)
        Nothing     -> error ("cannot encode ASN1 Character String " ++ show encoding ++ " from string")

-- | try to decode an 'ASN1CharacterString' to a String
asn1CharacterToString :: ASN1CharacterString -> Maybe String
asn1CharacterToString (ASN1CharacterString encoding bs) =
    case stringEncodingFunctions encoding of
        Just (d, _) -> Just (d bs)
        Nothing     -> Nothing

-- | ASN1 Character String with encoding
data ASN1CharacterString = ASN1CharacterString
    { characterEncoding         :: ASN1StringEncoding
    , getCharacterStringRawData :: ByteString
    } deriving (Show,Eq,Ord)

instance IsString ASN1CharacterString where
    fromString s = ASN1CharacterString UTF8 (encodeUTF8 s)

decodeUTF8 :: ByteString -> String
decodeUTF8 b = loop 0 $ B.unpack b
  where loop :: Int -> [Word8] -> [Char]
        loop _   []     = []
        loop pos (x:xs)
            | x `isClear` 7 = toEnum (fromIntegral x) : loop (pos+1) xs
            | x `isClear` 6 = error "continuation byte in heading context"
            | x `isClear` 5 = uncont 1 (x .&. 0x1f) pos xs
            | x `isClear` 4 = uncont 2 (x .&. 0xf)  pos xs
            | x `isClear` 3 = uncont 3 (x .&. 0x7)  pos xs
            | otherwise     = error "too many byte"
        uncont :: Int -> Word8 -> Int -> [Word8] -> [Char]
        uncont 1 iniV pos xs =
            case xs of
                c1:xs' -> decodeCont iniV [c1] : loop (pos+2) xs'
                _      -> error "truncated continuation, expecting 1 byte"
        uncont 2 iniV pos xs =
            case xs of
                c1:c2:xs' -> decodeCont iniV [c1,c2] : loop (pos+3) xs'
                _         -> error "truncated continuation, expecting 2 bytes"
        uncont 3 iniV pos xs =
            case xs of
                c1:c2:c3:xs' -> decodeCont iniV [c1,c2,c3] : loop (pos+4) xs'
                _            -> error "truncated continuation, expecting 3 bytes"
        uncont _ _ _ _ = error "invalid number of bytes for continuation"
        decodeCont :: Word8 -> [Word8] -> Char
        decodeCont iniV l
            | all isContByte l = toEnum $ foldl (\acc v -> (acc `shiftL` 6) + fromIntegral v) (fromIntegral iniV) $ map (\v -> v .&. 0x3f) l
            | otherwise        = error "continuation bytes invalid"
        isContByte v = v `testBit` 7 && v `isClear` 6
        isClear v i = not (v `testBit` i)

encodeUTF8 :: String -> ByteString
encodeUTF8 s = B.pack $ concatMap (toUTF8 . fromEnum) s
  where toUTF8 e
            | e < 0x80      = [fromIntegral e]
            | e < 0x800     = [fromIntegral (0xc0 .|. (e `shiftR` 6)), toCont e]
            | e < 0x10000   = [fromIntegral (0xe0 .|. (e `shiftR` 12))
                              ,toCont (e `shiftR` 6)
                              ,toCont e]
            | e < 0x200000  = [fromIntegral (0xf0 .|. (e `shiftR` 18))
                              , toCont (e `shiftR` 12)
                              , toCont (e `shiftR` 6)
                              , toCont e]
            | otherwise     = error "not a valid value"
        toCont v = fromIntegral (0xc0 .&. (v .&. 0x3f))

decodeASCII :: ByteString -> String
decodeASCII = BC.unpack

encodeASCII :: String -> ByteString
encodeASCII = BC.pack

decodeBMP :: ByteString -> String
decodeBMP b
    | odd (B.length b) = error "not a valid BMP string"
    | otherwise        = undefined
encodeBMP :: String -> ByteString
encodeBMP s = B.pack $ concatMap (toUCS2 . fromEnum) s
  where toUCS2 v = [b0,b1]
            where b0 = fromIntegral (v `shiftR` 8)
                  b1 = fromIntegral (v .&. 0xff)

decodeUTF32 :: ByteString -> String
decodeUTF32 b
    | (B.length b `mod` 4) /= 0 = error "not a valid UTF32 string"
    | otherwise                 = undefined
encodeUTF32 :: String -> ByteString
encodeUTF32 s = B.pack $ concatMap (toUTF32 . fromEnum) s
  where toUTF32 v = [b0,b1,b2,b3]
            where b0 = fromIntegral (v `shiftR` 24)
                  b1 = fromIntegral ((v `shiftR` 16) .&. 0xff)
                  b2 = fromIntegral ((v `shiftR` 8)  .&. 0xff)
                  b3 = fromIntegral (v .&. 0xff)
