{-# Language CPP, BangPatterns, MagicHash, ForeignFunctionInterface, UnliftedFFITypes #-}
-- |
-- Module    : System.Random.TF.Gen
-- Copyright : (c) 2012-2013 Michał Pałka
-- License   : BSD3
--
-- Maintainer  : michal.palka@chalmers.se
-- Stability   : experimental
-- Portability : portable
--
-- This module provides the 'TFGen' generator and the alternative 'RandomGen' class.
--  'TFGen' also implements the standard 'System.Random.RandomGen' class.

module System.Random.TF.Gen
 (TFGen, RandomGen(..), seedTFGen)
 where

import qualified System.Random as R

import System.IO.Unsafe

import Data.Bits
import Data.Char (toUpper, isSpace)
import Data.Maybe (isJust, fromJust)
import Data.Int
import Data.Word
import Data.Primitive.ByteArray

import Numeric

#if !MIN_VERSION_base(4,4,0)
unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = unsafePerformIO
#endif

foreign import ccall unsafe "skein.h Threefish_256_Process_Block"
  threefish256EncryptBlock ::
    ByteArray# -> ByteArray# -> MutableByteArray# s -> Int -> IO ()

createBlock256 :: Word64 -> Word64 -> Word64 -> Word64 -> IO ByteArray
createBlock256 !a !b !c !d = do
  ma <- newByteArray 32
  writeByteArray ma 0 a
  writeByteArray ma 1 b
  writeByteArray ma 2 c
  writeByteArray ma 3 d
  unsafeFreezeByteArray ma

readBlock256 :: ByteArray -> (Word64, Word64, Word64, Word64)
readBlock256 ba =
  ( indexByteArray ba 0
  , indexByteArray ba 1
  , indexByteArray ba 2
  , indexByteArray ba 3 )

-- | The generator type
data TFGen =
  TFGen
    {-# UNPACK #-} !ByteArray -- Key, four Word64s in host endian format
    {-# UNPACK #-} !Word64    -- Tree level
    {-# UNPACK #-} !Word64    -- Tree position bits
    {-# UNPACK #-} !Int16     -- Index in tree position bits
    {-# UNPACK #-} !Int16     -- Index in the block
    ByteArray                 -- The block, eight Word32s in host endian
                              -- format (this field is lazy)

newtype Hex = Hex ByteArray

instance Show Hex where
  showsPrec _ (Hex ba) =
    map toUpper .
    showHex' x1 .
    showHex' x2 .
    showHex' x3 .
    showHex' x4
    where
    (x1, x2, x3, x4) = readBlock256 ba
    showHex' x c = (pad $ showHex x "") ++ c
    pad s = take (16 - l) (repeat '0') ++ s
      where l = length s

instance Read Hex where
  readsPrec _ = map (\(l, s) -> (Hex $ makeBA l, s)) .
      filter (\(l, _) -> length l <= 4) .
      map (\(x, s) -> (toList x, s)) . readHex . dropWhile isSpace
    where makeBA l = unsafeDupablePerformIO $ do
            b <- newByteArray 32
            sequence_ [ writeByteArray b i x | (x, i) <- zip (l ++ repeat 0) [3,2..0] ]
            unsafeFreezeByteArray b
          toList :: Integer -> [Word64]
          toList 0 = []
          toList n = fromIntegral m : toList d
            where (d, m) = n `divMod` (2^64)

data TFGenR = TFGenR Hex Word64 Word64 Int16 Int16
  deriving (Show, Read)

toTFGenR :: TFGen -> TFGenR
toTFGenR (TFGen k i b bi blki _) = TFGenR (Hex k) i b bi blki

fromTFGenR :: TFGenR -> Maybe TFGen
fromTFGenR (TFGenR (Hex k@(ByteArray k')) i b bi blki)
  | bi >= 0 && bi <= 64 && blki >= 0 && blki < 8
              = Just $ TFGen k i b bi blki (mash k' (i-fromIntegral blki) b 0 1)
  | otherwise = Nothing

instance Show TFGen where
  showsPrec n g = showsPrec n (toTFGenR g)

instance Read TFGen where
  readsPrec n =
    map (\(g, s) -> (fromJust g, s)) .
    filter (\(g, _) -> isJust g) .
    map (\(g, s) -> (fromTFGenR g, s)) . readsPrec n

mash :: ByteArray# -> Word64 -> Word64 -> Word64 -> Int -> ByteArray
mash k' i b m o32 =
  -- We use unsafeDupablePerformIO here because the cost
  -- of locking in unsafePerformIO is much higher
  -- than any gains it could bring.
  unsafeDupablePerformIO $ do
      (ByteArray c') <- createBlock256 b i m 0
      -- Allocate array for cipher result
      o@(MutableByteArray o') <- newByteArray 32
      threefish256EncryptBlock k' c' o' o32
      unsafeFreezeByteArray o

mash' :: TFGen -> Word64 -> Int -> ByteArray
mash' (TFGen (ByteArray k') i b _ _ _) m o32 =
  mash k' i b m o32

mkTFGen :: ByteArray -> Word64 -> Word64 -> Int16 -> TFGen
mkTFGen k@(ByteArray k') i b bi =
  TFGen k i b bi 0 (mash k' i b 0 1)

extract :: ByteArray -> Int -> Word32
extract b i = indexByteArray b i

{-# INLINE tfGenNext #-}
tfGenNext :: TFGen -> (Word32, TFGen)
tfGenNext (TFGen k@(ByteArray k') i b bi blki blk) =
  (val,
   if blki == 7
    then
      if i < maxBound - 1
        then mkTFGen k (i+1) b bi
        else
          if bi < 64
            then mkTFGen k 0 (setBit b $ fromIntegral bi) (bi+1)
            else mkTFGen (mash k' maxBound b 0 0) 0 0 0
    else TFGen k (i+1) b bi (blki+1) blk)
  where
  val :: Word32
  val = extract blk (fromIntegral blki)

tfGenNext' :: TFGen -> (Int, TFGen)
tfGenNext' g
  -- We force the result into StdGen's range
  | val' <=  2147483562 = (fromIntegral val', g')
  | otherwise           = tfGenNext' g'
  where
  (val, g') = tfGenNext g
  val'      = 0x7FFFFFFF .&. val

tfGenSplit :: TFGen -> (TFGen, TFGen)
tfGenSplit g@(TFGen k i b bi _ _)
  | bi == maxb = (mkTFGen k' 0 0 1,   mkTFGen k' 0 1   1)
  | otherwise  = (mkTFGen k  i b bi', mkTFGen k  i b'' bi')
  where
  maxb = 64
  bi'  = bi + 1
  k'   = mash' g 0 0
  b''  = setBit b (fromIntegral bi)

instance R.RandomGen TFGen where
  next = tfGenNext'
  -- Current Random instances assume that the generator
  -- must have this range.
  genRange _ = (0, 2147483562)
  split = tfGenSplit

-- | Create a generator from a random seed.
seedTFGen :: (Word64, Word64, Word64, Word64) -> TFGen
seedTFGen (a1, a2, a3, a4) =
  mkTFGen
    (unsafeDupablePerformIO $ createBlock256 a1 a2 a3 a4)
    0 0 0

-- | Alternative 'RandomGen' class with a modified 'next' operation, and added 'splitn'
-- and 'level' operations.
-- 
-- Using the generator requires that no more than one operation is called
-- on the same generator state, as the implementation does not guarantee pseudorandomness
-- otherwise. As an exception, calling 'splitn' many times on the same generator state is
-- allowed as long as the \'bits\' argument is the same for all the calls.
class RandomGen g where
  -- | 'next' returns a 'Word32' that appears to have been chosen uniformly at random, and a 
  -- new generator state.
  next  :: g -> (Word32, g)
  -- | 'split' returns two derived generator states that appear to be independent pseudorandom
  -- number generators.
  split :: g -> (g, g)
  -- | 'splitn' is the n-way split operation used to create many derived generator states
  -- in one go. Application of 'splitn' to two first arguments should be shared between
  -- different applications of the index argument to avoid unnecessary repeated computations.
  --
  -- The following code creates ten \'independent\' generator states. Number \'4\' comes
  -- from the fact that at least
  -- four bits are needed to encode ten different indices.
  --
  -- @
  --    f :: RandomGen g => g -> [g]
  --    f r = map (splitn r 4) [0..9]
  -- @
  splitn :: g -- ^ Original generator state.
    -> Int    -- ^ Number of bits that will be used to index the derived states.
              -- Must be between 0 and 32.
    -> Word32 -- ^ Index of the derived state. Call to @splitn r n i@ must
              -- satisfy @0 <= i < 2^n@.
    -> g
  -- | 'level' is a \'hint\' operation that may cause an iteration of work
  -- of the generator be performed prematurely in order to
  -- prevent the subsequent operations from being expensive. It is meant to be
  -- called before a 'splitn' operation, which is expected to be evaluated
  -- a very large number indices. Calling 'level' in such case might decrease
  -- the total amount of work performed.
  level :: g -> g

tfGenSplitN :: TFGen -> Int -> Word32 -> TFGen
tfGenSplitN (TFGen k@(ByteArray ku) i b bi _ _) nbits
  | nbits < 0          = error "tfGenSplitN called with nbits < 0"
  | nbits > 32         = error "tfGenSplitN called with nbits > 32"
  | bi' + nbits > maxb = \n ->
     let k' = mash ku i (b .|. shiftL (fromIntegral $ clip n) (fromIntegral bi)) 0 0 in
     mkTFGen k' 0 (shiftR (fromIntegral $ clip n) (bi' + nbits - maxb)) (bi - fromIntegral (maxb - nbits))
  | otherwise          = \n ->
     mkTFGen k i (b .|. shiftL (fromIntegral $ clip n) bi') (bi + fromIntegral nbits)
  where
  bi' = fromIntegral bi
  maxb = 64
  clip n = (0xFFFFFFFF `shiftR` (32 - nbits)) .&. n

tfGenLevel :: TFGen -> TFGen
tfGenLevel g@(TFGen k@(ByteArray ku) i b bi _ _)
  | bi + 40 > maxb = mkTFGen k' 0 0 0
  | otherwise      = g
  where
  maxb = 64
  k'   = mash ku i b 0 0

instance RandomGen TFGen where
  {-# INLINE next #-}
  next   = tfGenNext
  split  = tfGenSplit
  splitn = tfGenSplitN
  level  = tfGenLevel

