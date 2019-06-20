{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-- |
-- Low level API for PRNG. Concrete implementations of generators are
-- expected to implement there interfaces. Unfortunately these
-- interfaces are rather large. For example if PRNG generates uniform
-- 32-bit words 'step32' will be basic primitive but if we're using
-- LCG which generates numbers in interval [0,2^31-1) @step32@ will
-- have to use sampling with rejection and algorithms for generating
-- numbers in the interval will be different as well.
module Random.PRNG (
    -- * Word sizes
    W
  , WordTy
  , Reify(..)
    -- * PRNG API
  , OutputWidth
  , StateSize
  , SeedSize
  , Seed(..)
    -- ** Two APIs
  , Pure(..)
  , Stateful(..)
  ) where

import Control.Monad.Primitive
import Data.Proxy
import Data.ByteString (ByteString)
import Data.Word       (Word32,Word64)
import GHC.TypeLits


----------------------------------------------------------------
-- Word sizes
----------------------------------------------------------------

-- | Word size. We support two word sizes.
data W
  = W32
  | W64

-- | Mapping from word size to haskell data types
type family WordTy w where
  WordTy 'W32 = Word32
  WordTy 'W64 = Word64

class Reify (w :: W) where
  reify :: Proxy w -> W

instance Reify 'W32 where reify _ = W32
instance Reify 'W64 where reify _ = W64

----------------------------------------------------------------
-- PRNG API
----------------------------------------------------------------

-- | Width of output of PRNG
type family OutputWidth g :: W

-- | State size of PRNG in bytes.
type family StateSize g :: Nat

-- | Size of initial seed. It could be less than @StateSize@
type family SeedSize g :: Nat

-- | Serialized state of PRNG.
newtype Seed g = Seed ByteString


-- | PRNG with state as pure value. Such PRNGs are meant to be used in
--   the state monads.
class Reify (OutputWidth g) => Pure g where
  -- | State of PRNG
  data State g
  -- | Generate single uniformly distributed 32-bit word
  step32  :: State g -> (State g, Word32)
  -- | Generate single uniformly distributed 64-bit word
  step64  :: State g -> (State g, Word64)
  -- | @step32R g n@ generates number in range @[0,n]@
  step32R :: State g -> Word32 -> (State g, Word32)
  -- | @step64R g n@ generates number in range @[0,n]@
  step64R :: State g -> Word64 -> (State g, Word64)
  -- | Generate @Float@ in the range (0,1]
  stepFloat01  :: State g -> (State g, Float)
  -- | Generate @Float@ in the range [0,1)
  stepFloat01Z :: State g -> (State g, Float)
  -- | Generate @Double@ in the range (0,1]
  stepDouble01  :: State g -> (State g, Double)
  -- | Generate @Double@ in the range [0,1)
  stepDouble01Z :: State g -> (State g, Double)
  -- | Save state of PRNG as bytestring.
  save    :: State g -> Seed g
  -- | Restore state from seed. Seed of any length should be
  --   accepted. If seed is too long it's OK to use only initial
  --   information
  restore :: Seed g -> State g


-- | RNGS with mutable state. As a general rule it should be assumed
--   that it's not safe to use single generator from multiple threads.
class Reify (OutputWidth g) => Stateful g where
  -- | Reference to mutable state of generator
  data Ref g :: * -> *
  -- | Generate uniformly distributed 32-bit word
  stepSt32        :: PrimMonad m => Ref g (PrimState m) -> m Word32
  -- | Generate uniformly distributed 64-bit word
  stepSt64        :: PrimMonad m => Ref g (PrimState m) -> m Word64
  -- | @step32R g n@ generates number in range @[0,n]@
  stepSt32R       :: PrimMonad m => Ref g (PrimState m) -> Word32 -> m Word32
  -- | @step32R g n@ generates number in range @[0,n]@
  stepSt64R       :: PrimMonad m => Ref g (PrimState m) -> Word64 -> m Word64
  -- | Generate @Float@ in the range (0,1]
  stepStFloat01   :: PrimMonad m => Ref g (PrimState m) -> m Float
  -- | Generate @Float@ in the range [0.1)
  stepStFloat01Z  :: PrimMonad m => Ref g (PrimState m) -> m Float
  -- | Generate @Double@ in the range (0.1]
  stepStDouble01  :: PrimMonad m => Ref g (PrimState m) -> m Double
  -- | Generate @Double@ in the range [0.1)
  stepStDouble01Z :: PrimMonad m => Ref g (PrimState m) -> m Double
  -- | Save state of PRNG in bytestring
  saveSt          :: PrimMonad m => Ref g (PrimState m) -> m (Seed g)
  -- | Restore state of PRNG
  restoreSt       :: PrimMonad m => Ref g (PrimState m) -> Seed g -> m ()
