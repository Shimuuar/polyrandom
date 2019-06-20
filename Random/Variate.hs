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
  uniform01 = do
    w <- uniformWord32
    return $! wordToFloat w
  uniform01Z = do
    x <- uniform01
    return $ x - 2**(-33)

instance Uniform01 Double where
  uniform01 = do
    w <- uniformWord64
    return $! wordToDouble w
  uniform01Z = do
    x <- uniform01
    return $ x - 2**(-53)


wordToDouble :: Word64 -> Double
wordToDouble w    = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16  -- 2^{-52} 
          m_inv_53 = 1.1102230246251565404236316680908203125e-16 -- 2^{-53}
          m_inv_32 = 2.3283064365386962890625e-10                -- 2^{-32}
          u        = fromIntegral w               :: Int32
          v        = fromIntegral (w `shiftR` 32) :: Int32
{-# INLINE wordToDouble #-}


wordToFloat :: Word32 -> Float
wordToFloat x      = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
    where m_inv_33 = 1.16415321826934814453125e-10
          m_inv_32 = 2.3283064365386962890625e-10
          i        = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}



-- $references
--
-- * Doornik, J.A. (2007) Conversion of high-period random numbers to
--   floating point.
--   /ACM Transactions on Modeling and Computer Simulation/ 17(1).
--   <http://www.doornik.com/research/randomdouble.pdf>
