-- |
module Random.Variate where

import Data.Word
import Data.Int

import Random.Class

----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | 
class UniformRange a where
  uniformR :: MonadRandom m => (a,a) -> m a

-- | 
class Uniform a where
  uniform :: MonadRandom m => m a

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
