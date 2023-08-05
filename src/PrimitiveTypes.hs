module PrimitiveTypes where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BSB
import Data.Binary.Get (getWord64le, getWord8)

import Common
import Encode
import Decode

instance Ssz U64 where
    isSszFixedLen = True
    sszFixedLen = 8

instance Encode U64 where
    sszEncodeBuilder = BSB.word64LE
    sszBytesLen _ = 8

instance Decode U64 where
    sszDecode = decodeOrError getWord64le

instance Ssz U8 where
    isSszFixedLen = True
    sszFixedLen = 1

instance Encode U8 where
    sszEncodeBuilder = BSB.word8
    sszBytesLen _ = 1

instance Decode U8 where
    sszDecode = decodeOrError getWord8
