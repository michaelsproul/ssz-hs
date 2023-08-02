module Encode where

import Common
import qualified Data.ByteString.Lazy as BS

class (Ssz a) => Encode a where
    sszEncode :: a -> BS.ByteString
    sszBytesLen :: a -> U64
