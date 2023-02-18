{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Char8 as Char8ByteString
import qualified Data.Text.Encoding as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range
import IsomorphismClass
import qualified Numeric.Limits as NumericLimits
import qualified PtrPoker.Size as Size
import qualified PtrPoker.Write as Write
import Prelude

main =
  defaultMain $ pure $ checkParallel $ $$(discover)

prop_word64Size =
  property $ do
    a <- forAll (Gen.word64 (Range.exponential minBound maxBound))
    Size.word64AsciiDec a
      === length (show a)

prop_int64Size =
  property $ do
    a <- forAll (Gen.int64 (Range.exponential minBound maxBound))
    Size.int64AsciiDec a
      === length (show a)

prop_wordAsciiDec =
  property $ do
    a <- forAll (Gen.word (Range.exponential minBound maxBound))
    let string =
          Char8ByteString.unpack (Write.writeToByteString (Write.wordAsciiDec a))
    annotate string
    read string === a

prop_intAsciiDec =
  property $ do
    a <- forAll (Gen.int (Range.exponential minBound maxBound))
    let string =
          Char8ByteString.unpack (Write.writeToByteString (Write.intAsciiDec a))
    annotate string
    read string === a

prop_doubleAsciiDec =
  property $ do
    a <- forAll realFloatGen
    let string =
          Char8ByteString.unpack (Write.writeToByteString (Write.doubleAsciiDec a))
    annotate string
    if isNaN a
      then string === "NaN"
      else read string === a

prop_realZeroNonRealDoubleAsciiDec =
  property $ do
    a <- forAll realRealFloatGen
    let string =
          Char8ByteString.unpack (Write.writeToByteString (Write.zeroNonRealDoubleAsciiDec a))
    annotate string
    read string === a

prop_nonRealZeroNonRealDoubleAsciiDec =
  withTests 99 $
    property $ do
      a <- forAll nonRealRealFloatGen
      let string =
            Char8ByteString.unpack (Write.writeToByteString (Write.zeroNonRealDoubleAsciiDec a))
      annotate string
      read @Integer string === 0

prop_sizeOfTextUtf8 =
  property $ do
    a <- forAll (Gen.text (Range.exponential 0 9999) (Gen.choice [Gen.ascii, Gen.unicode]))
    Size.textUtf8 a
      === Char8ByteString.length (Text.encodeUtf8 a)

prop_sizeOfTextASCII =
  property $ do
    a <- forAll (Gen.text (Range.exponential 0 9999) Gen.ascii)
    Size.textUtf8 a
      === Char8ByteString.length (Text.encodeUtf8 a)

prop_textASCII =
  property $ do
    a <- forAll (Gen.text (Range.exponential 0 9999) Gen.ascii)
    Write.writeToByteString (Write.textUtf8 a)
      === Text.encodeUtf8 a

prop_textUtf8 =
  property $ do
    a <- forAll (Gen.text (Range.exponential 0 9999) (Gen.choice [Gen.ascii, Gen.unicode]))
    Write.writeToByteString (Write.textUtf8 a)
      === Text.encodeUtf8 a

prop_word8 =
  property $ do
    a <- forAll (Gen.word8 Range.constantBounded)
    Write.writeToByteString (Write.word8 a)
      === to (ByteStringBuilder.word8 a)

prop_lWord16 =
  property $ do
    a <- forAll (Gen.word16 Range.constantBounded)
    Write.writeToByteString (Write.lWord16 a)
      === to (ByteStringBuilder.word16LE a)

prop_bWord16 =
  property $ do
    a <- forAll (Gen.word16 Range.constantBounded)
    Write.writeToByteString (Write.bWord16 a)
      === to (ByteStringBuilder.word16BE a)

prop_lWord32 =
  property $ do
    a <- forAll (Gen.word32 Range.constantBounded)
    Write.writeToByteString (Write.lWord32 a)
      === to (ByteStringBuilder.word32LE a)

prop_bWord32 =
  property $ do
    a <- forAll (Gen.word32 Range.constantBounded)
    Write.writeToByteString (Write.bWord32 a)
      === to (ByteStringBuilder.word32BE a)

prop_lWord64 =
  property $ do
    a <- forAll (Gen.word64 Range.constantBounded)
    Write.writeToByteString (Write.lWord64 a)
      === to (ByteStringBuilder.word64LE a)

prop_bWord64 =
  property $ do
    a <- forAll (Gen.word64 Range.constantBounded)
    Write.writeToByteString (Write.bWord64 a)
      === to (ByteStringBuilder.word64BE a)

prop_lInt16 =
  property $ do
    a <- forAll (Gen.int16 Range.constantBounded)
    Write.writeToByteString (Write.lInt16 a)
      === to (ByteStringBuilder.int16LE a)

prop_bInt16 =
  property $ do
    a <- forAll (Gen.int16 Range.constantBounded)
    Write.writeToByteString (Write.bInt16 a)
      === to (ByteStringBuilder.int16BE a)

prop_lInt32 =
  property $ do
    a <- forAll (Gen.int32 Range.constantBounded)
    Write.writeToByteString (Write.lInt32 a)
      === to (ByteStringBuilder.int32LE a)

prop_bInt32 =
  property $ do
    a <- forAll (Gen.int32 Range.constantBounded)
    Write.writeToByteString (Write.bInt32 a)
      === to (ByteStringBuilder.int32BE a)

prop_lInt64 =
  property $ do
    a <- forAll (Gen.int64 Range.constantBounded)
    Write.writeToByteString (Write.lInt64 a)
      === to (ByteStringBuilder.int64LE a)

prop_bInt64 =
  property $ do
    a <- forAll (Gen.int64 Range.constantBounded)
    Write.writeToByteString (Write.bInt64 a)
      === to (ByteStringBuilder.int64BE a)

-- * Gens

-------------------------

realFloatGen =
  Gen.frequency
    [ (99, realRealFloatGen),
      (1, nonRealRealFloatGen)
    ]

nonRealRealFloatGen =
  Gen.element [0 / 0, 1 / 0, (-1) / 0, -0]

realRealFloatGen =
  Gen.frequency
    [ (50, fullRangeExponentialRealFloatGen),
      (50, simpleZeroToOneRealFloatGen)
    ]

fullRangeExponentialRealFloatGen =
  Gen.realFloat (Range.exponentialFloat NumericLimits.minValue NumericLimits.maxValue)

simpleZeroToOneRealFloatGen =
  do
    int <- Gen.int (Range.exponential 0 999999)
    return (read ("0." <> show int))
