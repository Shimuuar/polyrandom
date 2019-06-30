{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnboxedTuples    #-}
-- |
-- Low level API for PRNG. Concrete implementations of generators are
-- expected to implement there interfaces. Unfortunately these
-- interfaces are rather large. For example if PRNG generates uniform
-- 32-bit words 'step32' will be basic primitive but if we're using
-- LCG which generates numbers in interval [0,2^31-1) @step32@ will
-- have to use sampling with rejection and algorithms for generating
-- numbers in the interval will be different as well.
module Random.PRNG (
    -- * Low level API
    StateSize
  , SeedSize
  , Seed(..)
    -- ** Two APIs
  , Rand(..)
  , MRand(..)
  , Pure(..)
  , Stateful(..)
    -- * Primitive combinators
    -- ** Floating point
  , wordToFloat
  , wordToFloatZ
  , wordToDouble
  , wordToDoubleZ
    -- ** Integral ranges
  , uniformWithRejection
    -- * References
    -- $references
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Int
import Data.ByteString (ByteString)
import Data.Word       (Word32,Word64)
import GHC.TypeLits


----------------------------------------------------------------
-- PRNG API
----------------------------------------------------------------

-- | State size of PRNG in bytes. Note that it's size of serialized
--   representation of state.
type family StateSize g :: Nat

-- | Size of initial seed. It could be less than @StateSize@
type family SeedSize g :: Nat

-- | Serialized state of PRNG.
newtype Seed g = Seed ByteString

newtype Rand g a = Rand { unRand :: g -> (# g , a #) }
  deriving (Functor)

instance Applicative (Rand g) where
  pure a = Rand $ \g -> (# g, a #)
  Rand f <*> Rand x = Rand $ \g -> let (# g' , f' #) = f g
                                       (# g'', x' #) = x g'
                                   in  (# g'', f' x' #)

instance Monad (Rand g) where
  return  = pure
  m >>= f = Rand $ \g -> let (# g', a #) = unRand m g
                         in  unRand (f a) g'

newtype MRand g s a = MRand { unMRandT :: Ref g s -> ST s a }
  deriving (Functor)

instance Applicative (MRand g s) where
  pure a = MRand $ \_ -> pure a
  MRand f <*> MRand x = MRand $ \g -> f g <*> x g

instance Monad (MRand g s) where
  return = pure
  MRand m >>= f = MRand $ \g -> m g >>= \a -> unMRandT (f a) g



-- | PRNG with state as pure value. Such PRNGs are meant to be used in
--   the state monads.
class Pure g where
  -- | Generate single uniformly distributed 32-bit word
  step32  :: Rand g Word32
  -- | Generate single uniformly distributed 64-bit word
  step64  :: Rand g Word64
  -- | @step32R g n@ generates number in range @[0,n]@
  step32R :: Word32 -> Rand g Word32
  -- | @step64R g n@ generates number in range @[0,n]@
  step64R :: Word64 -> Rand g Word64
  -- | Save state of PRNG as bytestring.
  save    :: g -> Seed g
  -- | Restore state from seed. Seed of any length should be
  --   accepted. If seed is too long it's OK to use only initial
  --   information
  restore :: Seed g -> g


-- | RNGS with mutable state. As a general rule it should be assumed
--   that it's not safe to use single generator from multiple threads.
class Stateful g where
  -- | Reference to mutable state of generator
  data Ref g :: * -> *
  -- | Generate uniformly distributed 32-bit word
  stepSt32        :: MRand g s Word32
  -- | Generate uniformly distributed 64-bit word
  stepSt64        :: MRand g s Word64
  -- | @step32R g n@ generates number in range @[0,n]@
  stepSt32R       :: Word32 -> MRand g s Word32
  -- | @step32R g n@ generates number in range @[0,n]@
  stepSt64R       :: Word64 -> MRand g s Word64
  -- | Save state of PRNG in bytestring
  saveSt          :: MRand g s (Seed g)
  -- | Create new generator
  createSt        :: Seed g -> ST s (Ref g s)
  -- | Restore state of PRNG
  restoreSt       :: Seed g -> MRand g s ()



----------------------------------------------------------------
-- Primitive combinators
----------------------------------------------------------------

wordToFloat :: Word32 -> Float
wordToFloat x = wordToFloatZ x + f_inv_33
{-# INLINE wordToFloat #-}

wordToFloatZ :: Word32 -> Float
wordToFloatZ x = (fromIntegral i * f_inv_32) + 0.5
  where
    i = fromIntegral x :: Int32
{-# INLINE wordToFloatZ #-}

wordToDouble :: Word64 -> Double
wordToDouble x = wordToDoubleZ x + d_inv_65
{-# INLINE wordToDouble #-}

wordToDoubleZ :: Word64 -> Double
wordToDoubleZ x = (fromIntegral i * d_inv_64) + 0.5
  where
    i = fromIntegral x :: Int64
{-# INLINE wordToDoubleZ #-}


d_inv_64, d_inv_65 :: Double
d_inv_64 = 5.421010862427522e-20 -- 2^{-64}
d_inv_65 = 2.710505431213761e-20 -- 2^{-65}

f_inv_32, f_inv_33 :: Float
f_inv_32 = 2.3283064365386962890625e-10   -- 2^{-32}
f_inv_33 = 1.16415321826934814453125e-10  -- 2^{-33}


----------------------------------------------------------------
-- Integral numbers
----------------------------------------------------------------

-- | Generate uniform random number with rejection. This method could
--   not be used if PRNG generates uniform Word32\/Word64 since number
--   of generated states is not representable.
uniformWithRejection
  :: (Integral a, Monad m)
  => a    -- ^ Number of distinct values that could be generated. In
          --   other words generator can produce values in range @[0,N)@
  -> a    -- ^ @n@ we want to generate numbers in range @[0,n]@. Note
          --   that range in inclusive in this case!
  -> m a  -- ^ PRNG generator
  -> m a
uniformWithRejection genRange range generator
  -- nGen == 0 if we need to generate full range of word.
  | nGen > genRange || nGen == 0 = upscaledRej
  | nGen < genRange              = simpleRej

  | otherwise                    = generator
  where
    -- Number of values we want to generate. Will overflow if range is maxBound
    nGen      = range + 1
    -- Simple sampling with rejection.
    buckets   = genRange `div` nGen
    maxN      = nGen * buckets
    simpleRej = do x <- generator
                   if x < maxN then return $! x `div` buckets
                               else simpleRej
    -- We need to upscale generator. Note that for every
    --   x \in [0, range]
    --
    -- it could be uniquely represented as
    --
    --   x = genRange * j + i
    --
    --   i \in [0, genRange)
    --   j \in [0, range / genRange]
    --
    -- Alternatively j could be written as
    --
    --   j \in [0, roundup(nGen / genRange))
    --
    -- We need to watch for overflows as well
    upscaledRej = do
      i <- generator
      j <- uniformWithRejection genRange (range `div` genRange) generator
      case j + i of
        r | r < j     -> upscaledRej -- Overflow
          | r > range -> upscaledRej -- Rejection
          | otherwise -> return r


----------------------------------------------------------------
--
----------------------------------------------------------------

-- $references
--
-- * Doornik, J.A. (2007) Conversion of high-period random numbers to
--   floating point.
--   /ACM Transactions on Modeling and Computer Simulation/ 17(1).
--   <http://www.doornik.com/research/randomdouble.pdf>
