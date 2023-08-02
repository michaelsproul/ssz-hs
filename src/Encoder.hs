module Encoder where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Builder
import Data.Monoid (mempty, mappend)

import Common
import Encode

data SszEncoder = SszEncoder {
    offset :: U32,
    buffer :: Builder,
    variableBytes:: Builder,
    numVariableBytes :: U32 }

-- TODO: take a non-empty builder?
sszEncoderNew :: U32 -> SszEncoder
sszEncoderNew offset = SszEncoder {
  offset = offset,
  buffer = mempty,
  variableBytes = mempty,
  numVariableBytes = 0 }

sszEncoderAppend :: (Encode a) => SszEncoder -> a -> SszEncoder
sszEncoderAppend enc (val :: a) =
    if isSszFixedLen @a then
        enc { buffer = mappend (buffer enc) (lazyByteString (sszEncode val)) }
    else
        let variableOffset = offset enc + numVariableBytes enc
            valueBytes = sszEncode val
            newNumVariableBytes = numVariableBytes enc + fromIntegral (BS.length valueBytes)
        in
            enc { buffer = mappend (buffer enc) (word32LE variableOffset),
                  variableBytes = mappend (variableBytes enc) (lazyByteString valueBytes),
                  numVariableBytes = newNumVariableBytes }

sszEncoderFinalize :: SszEncoder -> Builder
sszEncoderFinalize enc = mappend (buffer enc) (variableBytes enc)
