{-# Language CPP, BangPatterns, ScopedTypeVariables #-}
-- |
-- Module    : System.Random.TF.Instances
-- Copyright : (c) 2012-2013 Michał Pałka
-- License   : BSD3
--
-- Maintainer  : michal.palka@chalmers.se
-- Stability   : experimental
-- Portability : portable
--
-- This module defines alternative 'Random' instances for
-- common integral types, which make use of 
-- the 'System.Random.TF.Gen.RandomGen' class from "System.Random.TF.Gen".

module System.Random.TF.Instances
 (Random (..), randomEnum) where

import Data.Bits
import Data.Int
import Data.Word

import System.Random.TF.Gen

#if !MIN_VERSION_base(4,5,0)
unsafeShiftR :: Bits a => a -> Int -> a
unsafeShiftR = shiftR

unsafeShiftL :: Bits a => a -> Int -> a
unsafeShiftL = shiftL
#endif

myUnfoldr :: (t -> (a, t)) -> t -> [a]
myUnfoldr f g = x' : myUnfoldr f g'
  where
  (x', g') = f g

class Random a where
  randomR :: RandomGen g => (a,a) -> g -> (a,g)

  random  :: RandomGen g => g -> (a, g)

  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randomRs ival g = myUnfoldr (randomR ival) g

  randoms  :: RandomGen g => g -> [a]
  randoms  g      = myUnfoldr random g
{-
  randomRIO :: (a,a) -> IO a

  randomIO  :: IO a
-}

boundsWrap :: Integral a => (a -> g -> (a, g)) -> (a, a) -> g -> (a, g)
boundsWrap f (l, h) rng
  | l == h    = (l, rng)
  | l > h     = mapFst (h+) $ f (l - h) rng
  | otherwise = mapFst (l+) $ f (h - l) rng
  where mapFst g (x, y) = (g x, y)

randomWord32 :: RandomGen g => (Word32, Word32) -> g -> (Word32, g)
randomWord32 (l, h) rng = boundsWrap randomWord32' (l, h) rng

randomInt32 :: RandomGen g => (Int32, Int32) -> g -> (Int32, g)
randomInt32 (l, h) rng = boundsWrap randomInt32' (l, h) rng
  where
  randomInt32' m r = case randomWord32' (fromIntegral m) r of
                       (x, r') -> (fromIntegral x, r')

word32Mask :: Word32 -> Word32
word32Mask w =
  (((((w .>. 1) .>. 2) .>. 4) .>. 8) .>. 16)
  where
  w .>. n = w .|. (w `unsafeShiftR` n)

-- Inspired by Java's java.util.Random.
-- This version avoids division modulo.
-- Returns a random number from range [0..k-1], or from the full range if k = 0.
{-# INLINE randomWord32' #-}
randomWord32' :: RandomGen g => Word32 -> g -> (Word32, g)
randomWord32' k
  -- Case 1: k is the maxBound.
  | k' == 0       = next
  -- Case 2: k' is a power of two; k is a bit mask.
  | k' .&. k == 0 = \rng ->
      case next rng of
        (x, rng') -> (x .&. k, rng')
  -- Case 3: The general case. Case 3 subsumes Case 2,
  -- and Case 2 subsumes Case 1. Cases 1 and 2 are
  -- there for efficiency.
  | otherwise     = loop
  where
  k' = k + 1
  mask = word32Mask k
  loop rng
    | x' <= k   = (x', rng')
    | otherwise = loop rng'
    where
    (x, rng') = next rng
    x' = x .&. mask

makeWord64 :: Word32 -> Word32 -> Word64
makeWord64 w1 w2 = w1' `unsafeShiftL` 32 .|. w2'
  where
  w1', w2' :: Word64
  w1' = fromIntegral w1
  w2' = fromIntegral w2

randomWord64 :: RandomGen g => (Word64, Word64) -> g -> (Word64, g)
randomWord64 (l, h) rng = boundsWrap randomWord64' (l, h) rng

randomInt64 :: RandomGen g => (Int64, Int64) -> g -> (Int64, g)
randomInt64 (l, h) rng = boundsWrap randomInt64' (l, h) rng
  where
  randomInt64' m r = case randomWord64' (fromIntegral m) r of
                       (x, r') -> (fromIntegral x, r')

-- Works similarly to randomWord32'
randomWord64' :: RandomGen g => Word64 -> g -> (Word64, g)
randomWord64' k
  -- Case 1: The range fits in 32 bits.
  | k <= m32      = \rng ->
    case randomWord32' (fromIntegral k) rng of
      (x, rng') -> (fromIntegral x, rng')
  -- Case 2: (l,h) is the full range. This case should
  -- probably be removed
  | k' == 0       = \rng ->
    let !(x1, rng')  = next rng
        !(x2, rng'') = next rng'
    in (makeWord64 x1 x2, rng'')
  -- Case 3: k' is a power of two; k is a bit mask.
  | k' .&. k == 0 = \rng ->
    let !(x1, rng')  = next rng
        !(x2, rng'') = next rng'
    in (makeWord64 x1 x2 .&. k, rng'')
  -- Case 4: The general case. Case 4 subsumes Cases 1 and 3,
  -- and Case 3 subsumes Case 2. Cases 1, 2 and 3 are
  -- there for efficiency.
  | otherwise     = loop
  where
  m32 :: Word64
  m32 = fromIntegral (maxBound :: Word32)
  k' = k + 1
  mask = word32Mask (fromIntegral $ k `unsafeShiftR` 32)
  loop rng
    | x <= k    = (x, rng'')
    | otherwise = loop rng''
    where
    (x1, rng')  = next rng
    (x2, rng'') = next rng'
    x  = makeWord64 (x1 .&. mask) x2

-- Returns the most significant word and the number of extra words.
-- x must be non-negative
getShiftAndLead :: (Integral a, Bits a) => a -> (Int, Word32)
getShiftAndLead !x = cWords x 0
  where
  cWords !x !c
    | x' == 0   = (c, fromIntegral x)
    | otherwise = cWords x' (c+1)
    where
    x' = x `unsafeShiftR` 32

randomInteger :: RandomGen g => (Integer, Integer) -> g -> (Integer, g)
randomInteger (l, h) rng = boundsWrap randomInteger' (l, h) rng

{-# INLINE randomInteger' #-}
randomInteger' :: forall g. RandomGen g => Integer -> g -> (Integer, g)
randomInteger' k rng
  | k < 2^64  = case randomWord64' (fromIntegral k) rng of
                  (x, rng') -> (fromIntegral x, rng')
  | otherwise = loop rng
  where
  (w, l) = getShiftAndLead k
  -- Constructing Integers is very expensive, so it is better
  -- to do it from Word64s than from Word32s.
  construct rng
    | even w    = construct' (fromIntegral lx) w   rng'
    | otherwise = construct' (fromIntegral x) (w-1) rng''
    where
    (lx, rng')  = randomWord32' l rng
    (x2, rng'') = next rng'
    x  = makeWord64 lx x2
  construct' :: Integer -> Int -> g -> (Integer, g)
  construct' !a 0 rng = (a, rng)
  construct' !a n rng =
    construct' (a `shiftL` 64 .|. fromIntegral x) (n-2) rng''
    where
    (x1, rng')  = next rng
    (x2, rng'') = next rng'
    x  = makeWord64 x1 x2
  loop rng
    | x <= k   = (x, rng')
    | otherwise = loop rng'
    where
    (x, rng') = construct rng

randomBounded :: (RandomGen g, Random a, Bounded a) => g -> (a, g)
randomBounded = randomR (minBound, maxBound)

instance Random Int where
  randomR (a, b) rng = (fromIntegral x, rng')
    where !(x, rng') = randomR (fromIntegral a :: Int64, fromIntegral b) rng
  random  = randomBounded

randomEnum :: (Enum a, RandomGen g) => (a, a) -> g -> (a, g)
randomEnum (a,b) g = 
  case randomR (fromEnum a, fromEnum b) g of
    (x, g') -> (toEnum x, g')

instance Random Char where
  randomR = randomEnum
  random  = randomBounded

instance Random Bool where
  randomR = randomEnum
  random  = randomBounded

-- For random Integers we use the range of Int
instance Random Integer where
  randomR = randomInteger
  random  = randomR (toInteger (minBound::Int), toInteger (maxBound::Int))

instance Random Word32 where
  randomR = randomWord32
  -- Optimised version
  random  = next

instance Random Word64 where
  randomR = randomWord64
  random  = randomBounded

instance Random Int32 where
  randomR = randomInt32
  -- Optimised version
  random g = let (x, g') = next g in (fromIntegral x, g')

instance Random Int64 where
  randomR = randomInt64
  random  = randomBounded

instance Random Word8 where
  randomR (l, h) g =
    let (x, g') = randomWord32 (fromIntegral l, fromIntegral h) g
    in (fromIntegral x, g')
  -- Optimised version
  random g = let (x, g') = next g in (fromIntegral x, g')

instance Random Int8 where
  randomR (l, h) g =
    let (x, g') = randomInt32 (fromIntegral l, fromIntegral h) g
    in (fromIntegral x, g')
  -- Optimised version
  random g = let (x, g') = next g in (fromIntegral x, g')

instance Random Word16 where
  randomR (l, h) g =
    let (x, g') = randomWord32 (fromIntegral l, fromIntegral h) g
    in (fromIntegral x, g')
  -- Optimised version
  random g = let (x, g') = next g in (fromIntegral x, g')

instance Random Int16 where
  randomR (l, h) g =
    let (x, g') = randomInt32 (fromIntegral l, fromIntegral h) g
    in (fromIntegral x, g')
  -- Optimised version
  random g = let (x, g') = next g in (fromIntegral x, g')
