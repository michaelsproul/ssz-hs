module Ssz
  (
    Ssz,
    Encode,
    Decode,
    isSszFixedLen,
    sszFixedLen,
    sszEncode,
    sszDecode,
    deriveSszEncodeDecode,
    U8,
    U64,
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BSB
import Data.Monoid (mempty, mappend)
import Common
import Encode
import Decode
import Encoder (sszEncoderNew, sszEncoderAppend, sszEncoderFinalize)
import Derive (deriveSszEncodeDecode)

instance (Ssz a) => Ssz [a] where
    isSszFixedLen = False
    sszFixedLen = bytesPerLengthOffset

sequenceSszBytesLen :: (Traversable t, Encode a) => t a -> U64
sequenceSszBytesLen (seq :: t a) =
    let len = fromIntegral (length seq) in
    if isSszFixedLen @a then
        len * sszFixedLen @a
    else
        sum (fmap sszBytesLen seq) + bytesPerLengthOffset * len

sequenceSszEncodeBuilder :: (Traversable t, Encode a) => t a -> BSB.Builder
sequenceSszEncodeBuilder (seq :: t a) =
    if isSszFixedLen @a then
        foldl mappend mempty (fmap sszEncodeBuilder seq)
    else
        let len = fromIntegral (length seq)
            offset = len * fromIntegral bytesPerLengthOffset
        in
            sszEncoderFinalize (foldl sszEncoderAppend (sszEncoderNew offset) seq)

instance (Encode a) => Encode [a] where
    sszEncodeBuilder = sequenceSszEncodeBuilder
    sszBytesLen = sequenceSszBytesLen

instance (Decode a) => Decode [a] where
    sszDecode bytes =
        if isSszFixedLen @a then
            traverse sszDecode (chunkedBy (fromIntegral (sszFixedLen @a)) bytes)
        else
            decodeOrError (getListOfVariableLengthItems Nothing) bytes
