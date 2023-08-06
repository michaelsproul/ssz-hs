module TestTypes where

import GHC.Generics
import Ssz (U64, U8, Ssz, deriveSszEncodeDecode, isSszFixedLen, sszFixedLen)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

-- Simple type with fixed fields.
data Checkpoint = Checkpoint
  {
    epoch :: U64,
    root :: U64
  } deriving (Generic, Show, Eq)

instance Arbitrary Checkpoint where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriveSszEncodeDecode [t|Checkpoint|]

-- Complex type with interleaved fixed and variable fields.
data Mixed = Mixed
  {
    f1 :: U8,
    f2 :: U64,
    f3 :: [U8],
    f4 :: [[U8]],
    f5 :: U64,
    f6 :: [[U64]],
    f7 :: Checkpoint
  } deriving (Generic, Show, Eq)

instance Arbitrary Mixed where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriveSszEncodeDecode [t|Mixed|]
