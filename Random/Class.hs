{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
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

newtype RandT g m a = RandT
  { unRandT :: StateT g m a }
  deriving (Functor, Applicative, Monad)


instance (PRNG.Pure g, Monad m) => MonadRandom (RandT g m) where
  type PRNG (RandT g m) = g
  --
  uniformWord32 = RandT $ do
    g <- get
    let (!g', !w) = PRNG.step32 g
    put g'
    return w
  --
  uniformWord64 = RandT $ do
    g <- get
    let (!g', !w) = PRNG.step64 g
    put g'
    return w
  --
  uniformRWord32 n = RandT $ do
    g <- get
    let (!g', !w) = PRNG.step32R g n
    put g'
    return w
  --
  uniformRWord64 n = RandT $ do
    g <- get
    let (!g', !w) = PRNG.step64R g n
    put g'
    return w
  --
  uniformFloat01 = RandT $ do
    g <- get
    let (!g', !x) = PRNG.stepFloat01 g
    put g'
    return x
  --
  uniformFloat01Z = RandT $ do
    g <- get
    let (!g', !x) = PRNG.stepFloat01Z g
    put g'
    return x
  --
  uniformDouble01 = RandT $ do
    g <- get
    let (!g', !x) = PRNG.stepDouble01 g
    put g'
    return x
  --
  uniformDouble01Z = RandT $ do
    g <- get
    let (!g', !x) = PRNG.stepDouble01Z g
    put g'
    return x
  --
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
