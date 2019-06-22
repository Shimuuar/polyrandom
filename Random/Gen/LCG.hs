{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnboxedTuples       #-}
-- |
-- Linear congruential generators (LCG). They're defined as recurrence relation:
--
-- \[
-- x_{n+1} = ax_n + c \quad\operatorname{mod}\quad m
-- \]
--
-- If constant @c@ is 0 such generator is called multiplicative linear
-- congruential generator.
module Random.Gen.LCG (
    -- * MLCG
    MLCG231
  , MLCG231G(..)
    -- * Reference implementations
  , MLCGRef(..)
    -- * References
    -- $references
  ) where

import Data.Bits
import Data.Proxy
import Data.Word
import Numeric.Natural
import GHC.TypeLits

import qualified Random.PRNG as PRNG

----------------------------------------------------------------
-- MLCG: m = 2^31 - 1
----------------------------------------------------------------

-- | MLCG specialized for \[ m = 2^{31}-1 \]. This allows for
--   efficient computation of remander
newtype MLCG231G (a :: Nat) = MLCG231 Word32
  deriving (Show,Eq)

type MLCG231 = MLCG231G 1583458089

stepMLCG231 :: forall a i. (KnownNat a, Integral i) => PRNG.Rand (MLCG231G a) i
{-# INLINE stepMLCG231 #-}
stepMLCG231 = PRNG.Rand $ \(MLCG231 w) ->
  let m31  = 0x7fffffff -- 2^31 - 1
      a    = fromIntegral $ natVal (Proxy :: Proxy a)
      prod = fromIntegral w * a :: Word64
      w'   = case (prod  .&. m31) + (prod `shiftR` 31) of
               r | r >= m31  -> r - m31
                 | otherwise -> r
  in (# MLCG231 (fromIntegral w'), fromIntegral w' #)

instance (1 <= a, a <= 2147483646, KnownNat a) => PRNG.Pure (MLCG231G a) where
  step32R w     = PRNG.uniformWithRejection 0x7fffffff w stepMLCG231
  step64R w     = PRNG.uniformWithRejection 0x7fffffff w stepMLCG231
  -- Derived
  step32        = PRNG.step32R maxBound
  step64        = PRNG.step64R maxBound
  stepFloat01   = withWord32 PRNG.wordToFloat
  stepFloat01Z  = withWord32 PRNG.wordToFloatZ
  stepDouble01  = withWord64 PRNG.word64ToDouble
  stepDouble01Z = withWord64 PRNG.word64ToDoubleZ
  -- State
  save    = undefined
  restore = undefined


----------------------------------------------------------------
-- Reference implementation
----------------------------------------------------------------

-- | Reference implementation of multiplicative linear congruential
--   generator. It uses arbitrary precision arithmetic and shouldn't
--   have stellar performance. But its implementation is very
--   straightforward and should be free of any problems brought by
--   fancy tricks.
newtype MLCGRef (a :: Nat) (m :: Nat) = MLCGRef Natural
  deriving (Show,Eq)

-- | Perform single step of
stepMLCGRef
  :: forall a m. (KnownNat a, KnownNat m)
  => PRNG.Rand (MLCGRef a m) Natural
{-# INLINE stepMLCGRef #-}
stepMLCGRef = PRNG.Rand $ \(MLCGRef w) ->
  let w' = (w * a) `mod` m
  in  (# MLCGRef w', w' #)
  where
    a  = fromInteger $ natVal (Proxy :: Proxy a)
    m  = fromInteger $ natVal (Proxy :: Proxy m)


instance (KnownNat a, KnownNat m) => PRNG.Pure (MLCGRef a m) where
  step32R w     = do
    x <- PRNG.step64R (fromIntegral w)
    return $! fromIntegral x
  step64R w     = do
    x <- PRNG.uniformWithRejection m (fromIntegral w) stepMLCGRef
    return $! fromIntegral x
    where
      m = fromInteger $ natVal (Proxy :: Proxy m)
  -- Derived
  step32        = PRNG.step32R maxBound
  step64        = PRNG.step64R maxBound
  stepFloat01   = withWord32 PRNG.wordToFloat
  stepFloat01Z  = withWord32 PRNG.wordToFloatZ
  stepDouble01  = withWord64 PRNG.word64ToDouble
  stepDouble01Z = withWord64 PRNG.word64ToDoubleZ
  -- State
  save    = undefined
  restore = undefined


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

withWord32 :: PRNG.Pure g => (Word32 -> a) -> PRNG.Rand g a
{-# INLINE withWord32 #-}
withWord32 f = do
  w <- PRNG.step32
  return $! f w

with2Word32 :: PRNG.Pure g => (Word32 -> Word32 -> a) -> PRNG.Rand g a
{-# INLINE with2Word32 #-}
with2Word32 f = do
  u <- PRNG.step32
  v <- PRNG.step32
  return $! f u v

withWord64 :: PRNG.Pure g => (Word64 -> a) -> PRNG.Rand g a
{-# INLINE withWord64 #-}
withWord64 f = do
  w <- PRNG.step64
  return $! f w


-- $references
--
-- * Schrage, L. (1979). A more portable Fortran random number
--   generator.
--   /ACM Transactions on Mathematical Software (TOMS),/ 5(2), 132-138.
