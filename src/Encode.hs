module Encode where

import Common
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Builder (Builder, toLazyByteString)

class (Ssz a) => Encode a where
    sszEncodeBuilder :: a -> Builder
    sszBytesLen :: a -> U64

sszEncode :: (Encode a) => a -> BS.ByteString
sszEncode = toLazyByteString . sszEncodeBuilder
