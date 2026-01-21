{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Char8 as Char8ByteString
import qualified Data.Text.Encoding as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import IsomorphismClass
import qualified Numeric.Limits as NumericLimits
import qualified PtrPoker.Size as Size
import qualified PtrPoker.Write as Write
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude

main :: IO ()
main = hspec $ do
  describe "Size" $ do
    it "word64AsciiDec size matches show length" $ hedgehog $ do
      a <- forAll (Gen.word64 (Range.exponential minBound maxBound))
      Size.word64AsciiDec a === length (show a)

    it "int64AsciiDec size matches show length" $ hedgehog $ do
      a <- forAll (Gen.int64 (Range.exponential minBound maxBound))
      Size.int64AsciiDec a === length (show a)

    it "textUtf8 size matches encoded length" $ hedgehog $ do
      a <- forAll (Gen.text (Range.exponential 0 9999) (Gen.choice [Gen.ascii, Gen.unicode]))
      Size.textUtf8 a === Char8ByteString.length (Text.encodeUtf8 a)

    it "textUtf8 ASCII size matches encoded length" $ hedgehog $ do
      a <- forAll (Gen.text (Range.exponential 0 9999) Gen.ascii)
      Size.textUtf8 a === Char8ByteString.length (Text.encodeUtf8 a)

  describe "Write - ASCII decimal" $ do
    it "wordAsciiDec produces parseable output" $ hedgehog $ do
      a <- forAll (Gen.word (Range.exponential minBound maxBound))
      let string = Char8ByteString.unpack (Write.toByteString (Write.wordAsciiDec a))
      annotate string
      read string === a

    it "intAsciiDec produces parseable output" $ hedgehog $ do
      a <- forAll (Gen.int (Range.exponential minBound maxBound))
      let string = Char8ByteString.unpack (Write.toByteString (Write.intAsciiDec a))
      annotate string
      read string === a

    it "doubleAsciiDec produces parseable output" $ hedgehog $ do
      a <- forAll realFloatGen
      let string = Char8ByteString.unpack (Write.toByteString (Write.doubleAsciiDec a))
      annotate string
      if isNaN a
        then string === "NaN"
        else read string === a

    it "zeroNonRealDoubleAsciiDec with real numbers" $ hedgehog $ do
      a <- forAll realRealFloatGen
      let string = Char8ByteString.unpack (Write.toByteString (Write.zeroNonRealDoubleAsciiDec a))
      annotate string
      read string === a

    modifyMaxSuccess (const 99)
      $ it "zeroNonRealDoubleAsciiDec with non-real numbers"
      $ hedgehog
      $ do
        a <- forAll nonRealRealFloatGen
        let string = Char8ByteString.unpack (Write.toByteString (Write.zeroNonRealDoubleAsciiDec a))
        annotate string
        read @Integer string === 0

  describe "Write - Text" $ do
    it "textUtf8 ASCII matches encoded text" $ hedgehog $ do
      a <- forAll (Gen.text (Range.exponential 0 9999) Gen.ascii)
      Write.toByteString (Write.textUtf8 a) === Text.encodeUtf8 a

    it "textUtf8 Unicode matches encoded text" $ hedgehog $ do
      a <- forAll (Gen.text (Range.exponential 0 9999) (Gen.choice [Gen.ascii, Gen.unicode]))
      Write.toByteString (Write.textUtf8 a) === Text.encodeUtf8 a

  describe "Write - Word operations" $ do
    it "word8 matches ByteString.Builder" $ hedgehog $ do
      a <- forAll (Gen.word8 Range.constantBounded)
      Write.toByteString (Write.word8 a) === to (ByteStringBuilder.word8 a)

    it "lWord16 matches ByteString.Builder LE" $ hedgehog $ do
      a <- forAll (Gen.word16 Range.constantBounded)
      Write.toByteString (Write.lWord16 a) === to (ByteStringBuilder.word16LE a)

    it "bWord16 matches ByteString.Builder BE" $ hedgehog $ do
      a <- forAll (Gen.word16 Range.constantBounded)
      Write.toByteString (Write.bWord16 a) === to (ByteStringBuilder.word16BE a)

    it "lWord32 matches ByteString.Builder LE" $ hedgehog $ do
      a <- forAll (Gen.word32 Range.constantBounded)
      Write.toByteString (Write.lWord32 a) === to (ByteStringBuilder.word32LE a)

    it "bWord32 matches ByteString.Builder BE" $ hedgehog $ do
      a <- forAll (Gen.word32 Range.constantBounded)
      Write.toByteString (Write.bWord32 a) === to (ByteStringBuilder.word32BE a)

    it "lWord64 matches ByteString.Builder LE" $ hedgehog $ do
      a <- forAll (Gen.word64 Range.constantBounded)
      Write.toByteString (Write.lWord64 a) === to (ByteStringBuilder.word64LE a)

    it "bWord64 matches ByteString.Builder BE" $ hedgehog $ do
      a <- forAll (Gen.word64 Range.constantBounded)
      Write.toByteString (Write.bWord64 a) === to (ByteStringBuilder.word64BE a)

  describe "Write - Int operations" $ do
    it "lInt16 matches ByteString.Builder LE" $ hedgehog $ do
      a <- forAll (Gen.int16 Range.constantBounded)
      Write.toByteString (Write.lInt16 a) === to (ByteStringBuilder.int16LE a)

    it "bInt16 matches ByteString.Builder BE" $ hedgehog $ do
      a <- forAll (Gen.int16 Range.constantBounded)
      Write.toByteString (Write.bInt16 a) === to (ByteStringBuilder.int16BE a)

    it "lInt32 matches ByteString.Builder LE" $ hedgehog $ do
      a <- forAll (Gen.int32 Range.constantBounded)
      Write.toByteString (Write.lInt32 a) === to (ByteStringBuilder.int32LE a)

    it "bInt32 matches ByteString.Builder BE" $ hedgehog $ do
      a <- forAll (Gen.int32 Range.constantBounded)
      Write.toByteString (Write.bInt32 a) === to (ByteStringBuilder.int32BE a)

    it "lInt64 matches ByteString.Builder LE" $ hedgehog $ do
      a <- forAll (Gen.int64 Range.constantBounded)
      Write.toByteString (Write.lInt64 a) === to (ByteStringBuilder.int64LE a)

    it "bInt64 matches ByteString.Builder BE" $ hedgehog $ do
      a <- forAll (Gen.int64 Range.constantBounded)
      Write.toByteString (Write.bInt64 a) === to (ByteStringBuilder.int64BE a)

realFloatGen :: (MonadGen m, RealFloat a, Read a) => m a
realFloatGen =
  Gen.frequency
    [ (99, realRealFloatGen),
      (1, nonRealRealFloatGen)
    ]

nonRealRealFloatGen :: (MonadGen m, Fractional a) => m a
nonRealRealFloatGen =
  Gen.element [0 / 0, 1 / 0, (-1) / 0, -0]

realRealFloatGen :: (MonadGen m, RealFloat a, Read a) => m a
realRealFloatGen =
  Gen.frequency
    [ (50, fullRangeExponentialRealFloatGen),
      (50, simpleZeroToOneRealFloatGen)
    ]

fullRangeExponentialRealFloatGen :: (MonadGen m, RealFloat a) => m a
fullRangeExponentialRealFloatGen =
  Gen.realFloat (Range.exponentialFloat NumericLimits.minValue NumericLimits.maxValue)

simpleZeroToOneRealFloatGen :: (MonadGen m, Read b) => m b
simpleZeroToOneRealFloatGen =
  do
    int <- Gen.int (Range.exponential 0 999999)
    return (read ("0." <> show int))
