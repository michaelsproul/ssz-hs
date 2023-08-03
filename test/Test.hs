import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord
import Data.Word (Word8)
import Data.Either (fromRight)
import qualified Data.ByteString.Lazy as BS

import Ssz (sszEncode, sszDecode, U8, U64, Encode, Decode)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

listToByteString :: [Word8] -> BS.ByteString
listToByteString = foldl BS.snoc BS.empty

roundtripTest :: (Encode a, Decode a, Arbitrary a, Eq a, Show a) => a -> String -> TestTree
roundtripTest (_ :: a) desc = QC.testProperty ("roundtrip " ++ desc) $
  \xs -> (xs :: a) == fromRight undefined (sszDecode (sszEncode xs))

randomTest :: (Encode a, Decode a, Eq a, Show a) => a -> String -> TestTree
randomTest (_ :: a) desc = QC.testProperty ("random bytes " ++ desc) $
  \bytes -> case sszDecode @[[U64]] (listToByteString bytes) of
    Left _ -> True
    Right value -> sszEncode value == listToByteString bytes

testsForType :: (Encode a, Decode a, Arbitrary a, Eq a, Show a) => a -> String -> TestTree
testsForType witness desc = testGroup desc
  [
    roundtripTest witness desc,
    randomTest witness desc
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [
    testsForType @U8 0 "U8",
    testsForType @U64 0 "U64",
    testsForType @[U8] [] "[U8]",
    testsForType @[U64] [] "[U64]",
    testsForType @[[U8]] [] "[[U8]]",
    testsForType @[[U64]] [] "[[U64]]"
  ]
