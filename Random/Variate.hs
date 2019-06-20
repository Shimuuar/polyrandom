{-# LANGUAGE UnboxedTuples #-}
-- |
module Random.Variate (
    -- * Type classes
    Uniform(..)
  , UniformR(..)
  , Uniform01(..)
    -- * References
    -- $references
  ) where

import Data.Bits
import Data.Word
import Data.Int

import Random.Class

----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Generate uniformly distributed value. In other words every
--   inhabitant of data type will be generated with equal probability.
--
--   Note that neither @Uniform@ nor @UniformR@ could be superclass of
--   each other. For example 'Integer' could be instance of @UniformR@
--   but not @Uniform@ and @(Word,Word)@ is other way around.
class Uniform a where
  uniform :: MonadRandom m => m a


-- | Generate value in inclusive range.
class UniformR a where
  uniformR :: MonadRandom m => (a,a) -> m a


-- | Sample numbers uniformly in interval (0,1).
class Uniform01 a where
  -- | Generate random number in range (0,1]. Thus it's safe to use in
  --   computation where non-zero values are required, e.g. logarithm
  --   of value
  uniform01  :: MonadRandom m => m a
  -- | Generate random number in range [0,1).
  uniform01Z :: MonadRandom m => m a


----------------------------------------------------------------
-- Uniform instances
----------------------------------------------------------------

instance Uniform Word8 where
  uniform = do
    n <- uniformWord32
    return $! fromIntegral n

instance Uniform Word16 where
  uniform = do
    n <- uniformWord32
    return $! fromIntegral n

instance Uniform Word32 where
  uniform = uniformWord32

instance Uniform Word64 where
  uniform = uniformWord64

-- FIXME: is this correct?! Otherwise different platform will produce
--        different results
instance Uniform Word where
  uniform = do
    n <- uniformWord64
    return $! fromIntegral n

instance Uniform Int8 where
  uniform = do
    n <- uniformWord32
    return $! fromIntegral n

instance Uniform Int16 where
  uniform = do
    n <- uniformWord32
    return $! fromIntegral n

instance Uniform Int32 where
  uniform = do
    n <- uniformWord32
    return $! fromIntegral n

instance Uniform Int64 where
  uniform = do
    n <- uniformWord64
    return $! fromIntegral n

-- FIXME: see comment about Word
instance Uniform Int where
  uniform = do
    n <- uniformWord64
    return $! fromIntegral n

instance (Uniform a, Uniform b) => Uniform (a,b) where
  uniform = do
    a <- uniform
    b <- uniform
    return (a,b)


----------------------------------------------------------------
-- Uniform range
----------------------------------------------------------------

instance UniformR Word32 where
  uniformR (x1,x2)
    | n == 0    = uniform
    | otherwise = do x <- uniformRWord32 n
                     return $! x + a
    where
      (# a , b #) | x1 < x2   = (# x1, x2 #)
                  | otherwise = (# x2, x1 #)
      n = b - a + 1

instance UniformR Word64 where
  uniformR (x1,x2)
    | n == 0    = uniform
    | otherwise = do x <- uniformRWord64 n
                     return $! x + a
    where
      (# a , b #) | x1 < x2   = (# x1, x2 #)
                  | otherwise = (# x2, x1 #)
      n = b - a + 1

----------------------------------------------------------------
-- Uniform01
----------------------------------------------------------------

instance Uniform01 Float where
  uniform01  = uniformFloat01
  uniform01Z = uniformFloat01Z

instance Uniform01 Double where
  uniform01  = uniformDouble01
  uniform01Z = uniformDouble01Z


-- $references
--
