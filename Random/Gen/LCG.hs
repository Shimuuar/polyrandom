{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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
  :: forall w a m. ( Integral w
                   , KnownNat a, KnownNat m
                   )
  => MLCG w a m -> (MLCG w a m, w)
{-# INLINE stepMLCG #-}
stepMLCG (MLCG w) = (MLCG w', w')
  where
    w' = w * a `mod` m
    a  = fromInteger $ natVal (Proxy :: Proxy a)
    m  = fromInteger $ natVal (Proxy :: Proxy m)


instance (Integral w, KnownNat a, KnownNat m) => PRNG.Pure (MLCG w a m) where
  step32        = undefined
  step64        = undefined
  step32R       = undefined
  step64R       = undefined
  stepFloat01   = undefined
  stepFloat01Z  = undefined
  stepDouble01  = undefined
  stepDouble01Z = undefined
  save    = undefined
  restore = undefined
