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

instance (Uniform a, Uniform b, Uniform c) => Uniform (a,b,c) where
  uniform = do
    a <- uniform
    b <- uniform
    c <- uniform
    return (a,b,c)


----------------------------------------------------------------
-- Uniform range
----------------------------------------------------------------

instance UniformR Word8 where
  uniformR (x1,x2) = do
    x <- uniformR (fromIntegral x1 :: Word32, fromIntegral x2 :: Word32)
    return $! fromIntegral x

instance UniformR Word16 where
  uniformR (x1,x2) = do
    x <- uniformR (fromIntegral x1 :: Word32, fromIntegral x2 :: Word32)
    return $! fromIntegral x

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

instance UniformR Word where
  uniformR (x1,x2) = do
    x <- uniformR (fromIntegral x1 :: Word64, fromIntegral x2 :: Word64)
    return $! fromIntegral x



instance UniformR Int8 where
  uniformR (x1,x2) = do
    x <- uniformR (fromIntegral x1 :: Int32, fromIntegral x2 :: Int32)
    return $! fromIntegral x

instance UniformR Int16 where
  uniformR (x1,x2) = do
    x <- uniformR (fromIntegral x1 :: Int32, fromIntegral x2 :: Int32)
    return $! fromIntegral x

instance UniformR Int32 where
  uniformR (x1,x2)
    | n == 0    = uniform
    | otherwise = do x <- uniformRWord32 n
                     return $! fromIntegral x + a
    where
      (# a , b #) | x1 < x2   = (# x1, x2 #)
                  | otherwise = (# x2, x1 #)
      n = fromIntegral $! b - a + 1

instance UniformR Int64 where
  uniformR (x1,x2)
    | n == 0    = uniform
    | otherwise = do x <- uniformRWord64 n
                     return $! fromIntegral x + a
    where
      (# a , b #) | x1 < x2   = (# x1, x2 #)
                  | otherwise = (# x2, x1 #)
      n = fromIntegral $! b - a + 1

instance UniformR Int where
  uniformR (x1,x2) = do
    x <- uniformR (fromIntegral x1 :: Int64, fromIntegral x2 :: Int64)
    return $! fromIntegral x


----------------------------------------------------------------
-- Uniform01
----------------------------------------------------------------

-- instance Uniform01 Float where
--   uniform01  = uniformFloat01
--   uniform01Z = uniformFloat01Z

-- instance Uniform01 Double where
--   uniform01  = uniformDouble01
--   uniform01Z = uniformDouble01Z


-- $references
--
