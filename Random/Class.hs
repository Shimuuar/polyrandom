{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
-- |
module Random.Class where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Word

import qualified Random.PRNG as PRNG

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | API for using PRNGs
class Monad m => MonadRandom m where
  -- | Underlying PRNG
  type PRNG m
  -- | Generate uniformly distributed 32-bit word
  uniformWord32    :: m Word32
  -- | Generate uniformly distributed 64-bit word
  uniformWord64    :: m Word64
  -- | Generate uniformly distributed 32-bit word in range [0,n]
  uniformRWord32   :: Word32 -> m Word32
  -- | Generate uniformly distributed 32-bit word in range [0,n]
  uniformRWord64   :: Word64 -> m Word64
  -- | Generate @Float@ in the range (0,1]
  uniformFloat01   :: m Float
  -- | Generate @Float@ in the range [0,1)
  uniformFloat01Z  :: m Float
  -- | Generate @Double@ in the range (0.1]
  uniformDouble01  :: m Double
  -- | Generate @Double@ in the range [0.1)
  uniformDouble01Z :: m Double
  -- | Reset generator state to value provided in the seed
  restoreSeed      :: PRNG.Seed (PRNG m) -> m ()
  -- | Save current state of generator as seed
  saveSeed         :: m (PRNG.Seed (PRNG m))


----------------------------------------------------------------
-- Concrete implementations for pure PRNG
----------------------------------------------------------------

instance PRNG.Pure g => MonadRandom (PRNG.Rand g) where
  type PRNG (PRNG.Rand g) = g
  uniformWord32    = PRNG.step32
  uniformWord64    = PRNG.step64
  uniformRWord32   = PRNG.step32R
  uniformRWord64   = PRNG.step64R
  uniformFloat01   = PRNG.stepFloat01
  uniformFloat01Z  = PRNG.stepFloat01Z
  uniformDouble01  = PRNG.stepDouble01
  uniformDouble01Z = PRNG.stepDouble01Z
  restoreSeed seed = PRNG.Rand $ \_ -> (# PRNG.restore seed, () #)
  saveSeed         = PRNG.Rand $ \g -> (# g , PRNG.save g #)


newtype RandT g m a = RandT
  { unRandT :: StateT g m a }
  deriving (Functor, Applicative, Monad)

liftRand :: Monad m => PRNG.Rand g a -> RandT g m a
{-# INLINE liftRand #-}
liftRand rnd = RandT $ do
  g <- get
  let (# g', a #) = PRNG.unRand rnd g
  put g'
  return a

instance (PRNG.Pure g, Monad m) => MonadRandom (RandT g m) where
  type PRNG (RandT g m) = g
  uniformWord32    = liftRand uniformWord32
  uniformWord64    = liftRand uniformWord64
  uniformRWord32   = liftRand . uniformRWord32
  uniformRWord64   = liftRand . uniformRWord64
  uniformFloat01   = liftRand uniformFloat01
  uniformFloat01Z  = liftRand uniformFloat01Z
  uniformDouble01  = liftRand uniformDouble01
  uniformDouble01Z = liftRand uniformDouble01Z
  restoreSeed = RandT . put . PRNG.restore
  saveSeed    = RandT $ do
    g <- get
    return $! PRNG.save g


----------------------------------------------------------------
--
----------------------------------------------------------------

newtype MRandT g m a = MRandT
  { unMRandT :: ReaderT (PRNG.Ref g (PrimState m)) m a }
  deriving (Functor, Applicative, Monad)

instance (PRNG.Stateful g, PrimMonad m) => MonadRandom (MRandT g m) where
  type PRNG (MRandT g m) = g
  --
  uniformWord32        = MRandT $ ReaderT $ PRNG.stepSt32
  uniformWord64        = MRandT $ ReaderT $ PRNG.stepSt64
  uniformRWord32 n     = MRandT $ ReaderT $ \g -> PRNG.stepSt32R g n
  uniformRWord64 n     = MRandT $ ReaderT $ \g -> PRNG.stepSt64R g n
  uniformFloat01       = MRandT $ ReaderT $ PRNG.stepStFloat01
  uniformFloat01Z      = MRandT $ ReaderT $ PRNG.stepStFloat01Z
  uniformDouble01      = MRandT $ ReaderT $ PRNG.stepStDouble01
  uniformDouble01Z     = MRandT $ ReaderT $ PRNG.stepStDouble01Z
  restoreSeed     seed = MRandT $ ReaderT $ \g -> PRNG.restoreSt g seed
  saveSeed             = MRandT $ ReaderT $ PRNG.saveSt
