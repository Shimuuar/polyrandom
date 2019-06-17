{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Low level API for PRNG. Concrete implementations of generators are
-- expected to implement there interfaces. 
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
--   the state monads
class Reify (OutputWidth g) => Pure g where
  -- | State of PRNG
  data State g
  -- | Generate single uniformly distributed 32-bit word
  step32 :: State g -> (State g, Word32)
  -- | Generate single uniformly distributed 64-bit word
  step64 :: State g -> (State g, Word64)
  -- | Save state of PRNG.
  save    :: State g -> Seed g
  -- | Restore state from seed. Any seed 
  restore :: Seed g -> State g


-- | RNGS with mutable state. As a general rule it should be assumed
--   that it's not safe to use single generator from multiple threads.
class Reify (OutputWidth g) => Stateful g where
  -- | Reference to mutable state of generator
  data Ref g :: * -> *
  -- | Generate single uniformly distributed word and mutate state of
  --   generator in process.
  stepSt32  :: PrimMonad m => Ref g (PrimState m) -> m Word32
  stepSt64  :: PrimMonad m => Ref g (PrimState m) -> m Word64
  -- | Save state of PRNG in
  saveSt    :: PrimMonad m => Ref g (PrimState m) -> m (Seed g)
  -- | Restore state of PRNG
  restoreSt :: PrimMonad m => Ref g (PrimState m) -> Seed g -> m ()
