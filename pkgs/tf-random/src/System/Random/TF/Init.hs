{-# LANGUAGE CPP #-}
-- Module    : System.Random.TF.Init
-- Copyright : (c) 2013 Michał Pałka
-- License   : BSD3
--
-- Maintainer  : michal.palka@chalmers.se
-- Stability   : experimental
-- Portability : portable
--
module System.Random.TF.Init
 (newTFGen, mkTFGen, mkSeedTime, mkSeedUnix, initTFGen)
 where

import System.Random.TF.Gen (TFGen, seedTFGen, split)

import Control.Monad (when)

import Data.Bits (bitSize)
import Data.IORef
import Data.Word

import Foreign (allocaBytes, peekArray)

import Data.Ratio (numerator, denominator)
import Data.Time
import System.CPUTime
import System.IO
import System.IO.Unsafe (unsafePerformIO)

-- | Use system time create the random seed.
-- This method of seeding may not be relible.
mkSeedTime :: IO (Word64, Word64, Word64, Word64)
mkSeedTime = do
  utcTm <- getCurrentTime
  cpu <- getCPUTime
  let daytime = toRational $ utctDayTime utcTm
      t1, t2 :: Word64
      t1 = fromIntegral $ numerator daytime
      t2 = fromIntegral $ denominator daytime
      day = toModifiedJulianDay $ utctDay utcTm
      d1 :: Word64
      d1 = fromIntegral day
      c1 :: Word64
      c1 = fromIntegral cpu
  return (t1, t2, d1, c1)

-- | Use the UNIX special file @\/dev\/urandom@ to create the seed.
-- Inspired by @random-mwc@.
mkSeedUnix :: IO (Word64, Word64, Word64, Word64)
mkSeedUnix = do
  let bytes = 32
      rfile = "/dev/urandom"
  l <- allocaBytes bytes $ \buf -> do
    nread <- withBinaryFile rfile ReadMode $ \h ->
      hGetBuf h buf bytes
    when (nread /= bytes) $
      fail $ "mkSeedUnix: Failed to read " ++
        show bytes ++ " from " ++ rfile
    peekArray 4 buf
  let [x1, x2, x3, x4] = l
  return (x1, x2, x3, x4)

-- | Create a seed and used it to seed an instance of TFGen.
-- Uses 'mkSeedUnix' on UNIX, and 'mkSeedTime' otherwise.
initTFGen :: IO TFGen
initTFGen = do
#ifdef UNIX
  s <- mkSeedUnix
#else
  s <- mkSeedTime
#endif
  return $ seedTFGen s

-- | Derive a new generator instance from the global RNG using split.
-- This is the default way of obtaining a new RNG instance.
-- Initial generator is seeded using 'mkSeedUnix' on UNIX,
-- and 'mkSeedTime' otherwise. This should be eventually
-- replaced with proper seeding.

-- Inspired by System.Random
newTFGen :: IO TFGen
newTFGen = atomicModifyIORef theTFGen split

{-# NOINLINE theTFGen #-}
theTFGen :: IORef TFGen
theTFGen  = unsafePerformIO $ do
   rng <- initTFGen
   newIORef rng

-- | Quick and dirty way of creating a deterministically
-- seeded generator.
mkTFGen :: Int -> TFGen
mkTFGen n
  | bitSize n > 64 = error "mkTFGen: case where size of Int > 64 not implemented"
  | otherwise      = seedTFGen (fromIntegral n, 0, 0, 0)
