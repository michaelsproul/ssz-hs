module Decode where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get (Get, getWord32le, getLazyByteString, getRemainingLazyByteString, lookAhead, runGetOrFail, isEmpty)
import Data.Int (Int64)
import Control.Monad (forM, foldM, when)
import Data.Maybe (fromMaybe)

import Common (Ssz, U32, U64, bytesPerLengthOffset, pairs)

type DecodeError = String

class (Ssz a) => Decode a where
    sszDecode :: BS.ByteString -> Either DecodeError a

decodeOrError :: Get a -> BS.ByteString -> Either DecodeError a
decodeOrError parser bytes = case runGetOrFail parser bytes of
    Left (_, _, err) -> Left err
    Right (_, _, val) -> Right val

getOffset :: Get U32
getOffset = getWord32le

getSszItem :: (Decode a) => Maybe Int64 -> Get a
getSszItem maybeLen = do
    byteString <- case maybeLen of
        Just len -> getLazyByteString len
        Nothing -> getRemainingLazyByteString
    case sszDecode byteString of
        Left err -> fail (show err)
        Right val -> return val

-- TODO: checked subtraction
offsetDiff :: [U32] -> Maybe Int64
offsetDiff [x] = Nothing
offsetDiff [x, y] = Just (fromIntegral (y - x))
offsetDiff _ = Nothing

getListOfVariableLengthItems :: (Traversable t, Applicative t, Monoid (t a), Decode a) => Maybe U32 -> Get (t a)
getListOfVariableLengthItems maxItems = do
    empty <- isEmpty
    if empty then
        return mempty
    else do
        firstOffset <- lookAhead getOffset
        when (firstOffset `mod` fromIntegral bytesPerLengthOffset /= 0 || firstOffset == 0) $
            fail $ "invalid first offset: " ++ show firstOffset
        let numItems = firstOffset `div` fromIntegral bytesPerLengthOffset
        when (fromMaybe False (fmap (numItems>) maxItems)) $
            fail $ "too many items: " ++ show numItems
        offsets <- forM [1..numItems] (const getOffset)
        let lengths = map offsetDiff (pairs offsets)
        items <- foldM (\xs maybeLen -> do
            x <- getSszItem maybeLen
            return (mappend xs (pure x))) mempty lengths
        return items
