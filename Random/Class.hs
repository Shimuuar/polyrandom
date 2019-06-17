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
import Data.Proxy
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
  uniformWord32 :: m Word32
  -- | Generate uniformly distributed 64-bit word
  uniformWord64 :: m Word64
  -- | Primitive for choosing 
  wordWidthChoice :: (PRNG.W -> m a) -> m a
  -- | Reset generator state to value provided in the seed
  restoreSeed :: PRNG.Seed (PRNG m) -> m ()
  -- | Save current state of generator as seed
  saveSeed :: m (PRNG.Seed (PRNG m))


----------------------------------------------------------------
-- Concrete implementations for pure PRNG
----------------------------------------------------------------

newtype RandT g m a = RandT
  { unRandT :: StateT (PRNG.State g) m a }
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
  wordWidthChoice cont = cont (PRNG.reify (Proxy :: Proxy (PRNG.OutputWidth g)))
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
  wordWidthChoice cont = cont (PRNG.reify (Proxy :: Proxy (PRNG.OutputWidth g)))
  restoreSeed     seed = MRandT $ ReaderT $ \g -> PRNG.restoreSt g seed
  saveSeed             = MRandT $ ReaderT $ PRNG.saveSt
