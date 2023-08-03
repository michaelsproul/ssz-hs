module Common where

import qualified Data.ByteString.Lazy as BS
import Data.Word (Word8, Word32, Word64)
import Data.Int (Int64)
import Data.Binary.Get (Get, runGetOrFail)
import Data.List (tails, transpose)

type U8 = Word8
type U32 = Word32
type U64 = Word64

bytesPerLengthOffset :: U64
bytesPerLengthOffset = 4

class Ssz a where
    isSszFixedLen :: Bool
    sszFixedLen :: U64

-- Borrowed from hw-prim
chunkedBy :: Int64 -> BS.ByteString -> [BS.ByteString]
chunkedBy n bs = if BS.length bs == 0
  then []
  else case BS.splitAt n bs of
    (as, zs) -> as : chunkedBy n zs

pairs :: [a] -> [[a]]
pairs =  map (take 2) . transpose . tails
