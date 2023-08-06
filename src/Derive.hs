module Derive where

import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import Control.Monad (when)
import Language.Haskell.TH

import Decode
import Encode
import Encoder
import Common
import PrimitiveTypes

-- Tree which mimics the structure of the type's generic representation
data SszDecoder = SszDecoderProduct SszDecoder SszDecoder |
    SszDecoderFixedLen BS.ByteString |
    SszDecoderVariableLen U32

-- After building the SszDecoder tree, compute the list of lengths of variable-length values.
getVariableLengths :: SszDecoder -> [U32]
getVariableLengths decoder = snd (go Nothing [] decoder)
    where
        -- Recursive case.
        go latestOffset lengths (SszDecoderProduct l r) =
            let (leftLatest, leftLengths) = go latestOffset lengths l
                (rightLatest, rightLengths) = go leftLatest leftLengths r
            in
                (rightLatest, rightLengths)
        -- Fixed-len portions are no-ops.
        go latestOffset lengths (SszDecoderFixedLen _) = (latestOffset, lengths)
        -- First offset (latestOffset not set yet)
        go Nothing lengths (SszDecoderVariableLen offset) = (Just offset, lengths)
        -- Subsequent offsets: compute length and add to list.
        go (Just latestOffset) lengths (SszDecoderVariableLen offset) =
            let length = offset - latestOffset -- TODO: checked arith
            in (Just offset, lengths ++ [length]) -- TODO: expensive concat

class GSsz (f :: * -> *) where
    gisSszFixedLen :: Bool
    gsszFixedLen :: U64

class GEncode f where
    gsszEncoderAppend :: SszEncoder -> f a -> SszEncoder

class GDecode f where
    -- Decode an offset or fixed-length field, and return the remainder of the input.
    gsszBeginDecode :: BS.ByteString -> Either DecodeError (SszDecoder, BS.ByteString)
    -- Expand the variable-length fields from the remaining bytes.
    --
    -- Arguments are the decoder, the variable-length bytes, and the list of variable lengths.
    --
    -- Return values are decoded repr, remaining variable-length bytes, and remaining list of
    -- variable lengths.
    gsszEndDecode :: SszDecoder -> BS.ByteString -> [U32] -> Either DecodeError (f a, BS.ByteString, [U32])

-- | Unit: used for constructors without arguments
-- TODO: SSZ unit type
instance GSsz U1 where
    gisSszFixedLen = undefined
    gsszFixedLen = undefined

instance GEncode U1 where
    gsszEncoderAppend _ U1 = error "unsupported: U1"

instance GDecode U1 where
    gsszBeginDecode _ = error "unsupported: U1"
    gsszEndDecode _ _ = error "unsupported: U1"

-- | Products: encode multiple arguments to constructors
instance (GSsz a, GSsz b) => GSsz (a :*: b) where
    gisSszFixedLen = gisSszFixedLen @a && gisSszFixedLen @b
    gsszFixedLen =
        if gisSszFixedLen @(a :*: b) then
            gsszFixedLen @a + gsszFixedLen @b
        else
            bytesPerLengthOffset

instance (GEncode a, GEncode b) => GEncode (a :*: b) where
    gsszEncoderAppend enc (a :*: b) = gsszEncoderAppend (gsszEncoderAppend enc a) b

instance (GDecode a, GDecode b) => GDecode (a :*: b) where
    gsszBeginDecode bytes = do
        (da, bytesRemaining) <- gsszBeginDecode @a bytes
        (db, bytesRemaining') <- gsszBeginDecode @b bytesRemaining
        return (SszDecoderProduct da db, bytesRemaining')
    gsszEndDecode (SszDecoderProduct da db) variableBytes variableLengths = do
        (xa, variableBytesRemaining, lengthsRemaining) <- gsszEndDecode da variableBytes variableLengths
        (xb, variableBytesRemaining', lengthsRemaining') <- gsszEndDecode db variableBytesRemaining lengthsRemaining
        return (xa :*: xb, variableBytesRemaining', lengthsRemaining')

-- | Sums: encode choice between constructors
instance (GSsz a, GSsz b) => GSsz (a :+: b) where
    gisSszFixedLen = error "SSZ unions not implemented"
    gsszFixedLen = error "SSZ unions not implemented"

instance (GEncode a, GEncode b) => GEncode (a :+: b) where
    gsszEncoderAppend _ _ = error "SSZ unions not implemented"

instance (GDecode a, GDecode b) => GDecode (a :+: b) where
    gsszBeginDecode _ = error "SSZ unions not implemented"
    gsszEndDecode _ _ = error "SSZ unions not implemented"

-- | Meta-information (constructor names, etc.)
instance (GSsz a) => GSsz (M1 i c a) where
    gisSszFixedLen = gisSszFixedLen @a
    gsszFixedLen = gsszFixedLen @a

instance (GEncode a) => GEncode (M1 i c a) where
    gsszEncoderAppend enc (M1 x) = gsszEncoderAppend enc x

instance (GDecode a) => GDecode (M1 i c a) where
    gsszBeginDecode bytes = gsszBeginDecode @a bytes
    gsszEndDecode enc bytes lengths = do
        (xa, bytesRemaining, lengthsRemaining) <- gsszEndDecode enc bytes lengths
        return (M1 xa, bytesRemaining, lengthsRemaining)

-- | Constants, additional parameters and recursion of kind *
instance (Ssz a) => GSsz (K1 i a) where
    gisSszFixedLen = isSszFixedLen @a
    gsszFixedLen = sszFixedLen @a

instance (Encode a) => GEncode (K1 i a) where
    gsszEncoderAppend enc (K1 x) = sszEncoderAppend enc x

instance (Decode a) => GDecode (K1 i a) where
    gsszBeginDecode bytes =
        if isSszFixedLen @a then
            let (fixedBytes, bytesRemaining) = BS.splitAt (fromIntegral $ sszFixedLen @a) bytes
            in return (SszDecoderFixedLen fixedBytes, bytesRemaining)
        else do
            offset <- decodeOrError getOffset bytes
            let bytesRemaining = BS.drop (fromIntegral bytesPerLengthOffset) bytes
            return (SszDecoderVariableLen offset, bytesRemaining)

    -- Decoding fixed-length item doesn't consume any variable-length bytes/lengths.
    gsszEndDecode (SszDecoderFixedLen bytes) variableBytes lengths = do
        value <- sszDecode bytes
        return (K1 value, variableBytes, lengths)
    -- Decoding a variable-length item consumes a number of bytes depending on the length.
    gsszEndDecode (SszDecoderVariableLen offset) variableBytes (l:ls) = do
        let (bytes, bytesRemaining) = BS.splitAt (fromIntegral l) variableBytes
        value <- sszDecode bytes
        return (K1 value, bytesRemaining, ls)
    -- If no length is left in the list, then this must be the last variable-length item, so
    -- let it consume the remaining variable-length bytes.
    gsszEndDecode (SszDecoderVariableLen _) variableBytes [] = do
        value <- sszDecode variableBytes
        return (K1 value, BS.empty, [])

deriveSsz :: Q Type -> Q [Dec]
deriveSsz ty =
  [d|
    instance Ssz $(ty) where
        isSszFixedLen = gisSszFixedLen @(Rep $(ty))
        sszFixedLen = gsszFixedLen @(Rep $(ty))
  |]

deriveEncode :: Q Type -> Q [Dec]
deriveEncode ty =
  [d|
    instance Encode $(ty) where
        sszEncodeBuilder x = sszEncoderFinalize (gsszEncoderAppend (sszEncoderNew 0) (from x))
  |]

deriveDecode :: Q Type -> Q [Dec]
deriveDecode ty =
  [d|
    instance Decode $(ty) where
        sszDecode bytes = do
            (decoder, variableBytes) <- gsszBeginDecode @(Rep $(ty)) bytes
            let variableLengths = getVariableLengths decoder
            (repr, bytesRemaining, lengthsRemaining) <- gsszEndDecode decoder variableBytes variableLengths
            when (not (BS.null bytesRemaining)) $
                Left ("not all bytes consumed, remaining: " ++ show (BS.length bytesRemaining))
            when (not (null lengthsRemaining)) $
                Left ("not all lengths consumed, remaining: " ++ show (length lengthsRemaining))
            return (to repr)
  |]

deriveSszEncodeDecode :: Q Type -> Q [Dec]
deriveSszEncodeDecode ty = deriveSsz ty <> deriveEncode ty <> deriveDecode ty
