{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
-- |
-- Linear congruential generators
module Random.Gen.LCG where

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
  step32        = PRNG.step32R maxBound
  step64        = PRNG.step64R maxBound
  step32R w     = PRNG.uniformWithRejection m w stepMLCG
    where
      m = fromInteger $ natVal (Proxy :: Proxy m)
  step64R w     = PRNG.uniformWithRejection m w stepMLCG
    where
      m = fromInteger $ natVal (Proxy :: Proxy m)
  stepFloat01   = do
    w <- PRNG.step32
    return $! PRNG.wordToFloat w
  stepFloat01Z  = do
    w <- PRNG.step32
    return $! PRNG.wordToFloatZ w
  stepDouble01  = do
    w1 <- PRNG.step32
    w2 <- PRNG.step32
    return $! PRNG.wordsToDouble w1 w2
  stepDouble01Z = do
    w1 <- PRNG.step32
    w2 <- PRNG.step32
    return $! PRNG.wordsToDoubleZ w1 w2
  save    = undefined
  restore = undefined

instance (KnownNat a, KnownNat m) => PRNG.Pure (MLCG Word64 a m) where
  step32        = PRNG.step32R maxBound
  step64        = PRNG.step64R maxBound
  step32R w     = do
    x <- PRNG.step64R (fromIntegral w)
    return $! fromIntegral x
  step64R w     = PRNG.uniformWithRejection m w stepMLCG
    where
      m = fromInteger $ natVal (Proxy :: Proxy m)
  stepFloat01   = do
    w <- PRNG.step32
    return $! PRNG.wordToFloat w
  stepFloat01Z  = do
    w <- PRNG.step32
    return $! PRNG.wordToFloatZ w
  stepDouble01  = do
    w <- PRNG.step64
    return $! PRNG.word64ToDouble w
  stepDouble01Z = do
    w <- PRNG.step64
    return $! PRNG.word64ToDoubleZ w
  save    = undefined
  restore = undefined
  
