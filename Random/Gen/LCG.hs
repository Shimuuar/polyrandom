{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
-- |
-- Linear congruential generators
module Random.Gen.LCG (
  MLCG(..)
  ) where

import Data.Proxy
import Data.Word
import GHC.TypeLits

import qualified Random.PRNG as PRNG

----------------------------------------------------------------
--
----------------------------------------------------------------


-- FIXME: We need to constrain possible values of a and m and their
--        relation to word width

-- | Multiplicative linear congruential generator
newtype MLCG w (a :: Nat) (m :: Nat) = MLCG w
  deriving (Show,Eq)

-- | Perform single step of
stepMLCG
  :: forall w w' a m. ( Integral w, Integral w'
                      , KnownNat a, KnownNat m
                      )
  => PRNG.Rand (MLCG w a m) w'
{-# INLINE stepMLCG #-}
stepMLCG = PRNG.Rand $ \(MLCG w) ->
  let w' = w * a `mod` m
  in  (# MLCG w', fromIntegral w' #)
  where
    a  = fromInteger $ natVal (Proxy :: Proxy a)
    m  = fromInteger $ natVal (Proxy :: Proxy m)


instance (KnownNat a, KnownNat m) => PRNG.Pure (MLCG Word32 a m) where
  step32R w     = PRNG.uniformWithRejection m w stepMLCG
    where m = fromInteger $ natVal (Proxy :: Proxy m)
  step64R w     = PRNG.uniformWithRejection m w stepMLCG
    where m = fromInteger $ natVal (Proxy :: Proxy m)
  -- Derived
  step32        = PRNG.step32R maxBound
  step64        = PRNG.step64R maxBound
  stepFloat01   = withWord32  PRNG.wordToFloat
  stepFloat01Z  = withWord32  PRNG.wordToFloatZ
  stepDouble01  = with2Word32 PRNG.wordsToDouble
  stepDouble01Z = with2Word32 PRNG.wordsToDoubleZ
  -- State
  save    = undefined
  restore = undefined

instance (KnownNat a, KnownNat m) => PRNG.Pure (MLCG Word64 a m) where
  step32R w     = do
    x <- PRNG.step64R (fromIntegral w)
    return $! fromIntegral x
  step64R w     = PRNG.uniformWithRejection m w stepMLCG
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
