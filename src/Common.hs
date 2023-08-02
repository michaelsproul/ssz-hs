module Common where

import Data.Word (Word8, Word32, Word64)

type U8 = Word8
type U32 = Word32
type U64 = Word64

bytesPerLengthOffset :: U64
bytesPerLengthOffset = 4

class Ssz a where
    isSszFixedLen :: Bool
    sszFixedLen :: U64
