module Ssz where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BSB
import Data.Binary (encode, decodeOrFail)
import Data.Binary.Get (runGetOrFail, getWord64le)
import Data.Either (Either, fromRight)
import Data.Monoid (mempty, mappend)
import Common
import Encode
import Encoder (sszEncoderNew, sszEncoderAppend, sszEncoderFinalize)

data DecodeError = InvalidBytes String deriving (Show)

class (Ssz a) => Decode a where
    sszDecode :: BS.ByteString -> Either DecodeError a

instance Ssz U64 where
    isSszFixedLen = True
    sszFixedLen = 8

instance Encode U64 where
    sszEncodeBuilder = BSB.word64LE
    sszBytesLen _ = 8

instance Decode U64 where
    sszDecode bytes = case runGetOrFail getWord64le bytes of
        Left (_, _, err) -> Left (InvalidBytes err)
        Right (_, _, val) -> Right val

instance Ssz U8 where
    isSszFixedLen = True
    sszFixedLen = 1

instance Encode U8 where
    sszEncodeBuilder = BSB.word8
    sszBytesLen _ = 1

instance Decode U8 where
    sszDecode bytes = case decodeOrFail bytes of
        Left (_, _, err) -> Left (InvalidBytes err)
        Right (_, _, val) -> Right val

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
