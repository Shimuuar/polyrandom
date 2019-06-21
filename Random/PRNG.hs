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
  , Pure(..)
  , Stateful(..)
    -- * Primitive combinators
    -- ** Floating point
  , wordsToDouble
  , wordsToDoubleZ
  , word64ToDouble
  , word64ToDoubleZ
  , wordToFloat
  , wordToFloatZ
    -- ** Integral ranges
  , uniformWithRejection
    -- * References
    -- $references
  ) where

import Control.Monad.Primitive
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
  -- | Generate @Float@ in the range (0,1]
  stepFloat01  :: Rand g Float
  -- | Generate @Float@ in the range [0,1)
  stepFloat01Z :: Rand g Float
  -- | Generate @Double@ in the range (0,1]
  stepDouble01  :: Rand g Double
  -- | Generate @Double@ in the range [0,1)
  stepDouble01Z :: Rand g Double
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



----------------------------------------------------------------
-- Primitive combinators
----------------------------------------------------------------

wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble u v = (fromIntegral u * d_inv_32 + (0.5 + d_inv_53)
                 + fromIntegral (v .&. 0xFFFFF) * d_inv_52)
{-# INLINE wordsToDouble #-}

wordsToDoubleZ :: Word32 -> Word32 -> Double
wordsToDoubleZ u v = wordsToDouble u v - d_inv_53
{-# INLINE wordsToDoubleZ #-}


word64ToDouble :: Word64 -> Double
word64ToDouble w
  = wordsToDouble (fromIntegral w) (fromIntegral (w `shiftR` 32))
{-# INLINE word64ToDouble #-}

word64ToDoubleZ :: Word64 -> Double
word64ToDoubleZ w
  = wordsToDoubleZ (fromIntegral w) (fromIntegral (w `shiftR` 32))
{-# INLINE word64ToDoubleZ #-}


wordToFloat :: Word32 -> Float
wordToFloat x = (fromIntegral i * f_inv_32) + 0.5 + f_inv_33
  where
    i = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

wordToFloatZ :: Word32 -> Float
wordToFloatZ x = wordToFloat x - f_inv_33
{-# INLINE wordToFloatZ #-}

d_inv_52, d_inv_53, d_inv_32 :: Double
d_inv_52 = 2.220446049250313080847263336181640625e-16  -- 2^{-52}
d_inv_53 = 1.1102230246251565404236316680908203125e-16 -- 2^{-53}
d_inv_32 = 2.3283064365386962890625e-10                -- 2^{-32}

f_inv_33, f_inv_32 :: Float
f_inv_33 = 1.16415321826934814453125e-10
f_inv_32 = 2.3283064365386962890625e-10


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
