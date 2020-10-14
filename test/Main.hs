module Main where

import Prelude
import Hedgehog
import Hedgehog.Main
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString.Char8 as Char8ByteString
import qualified PtrPoker.Write as Write
import qualified Numeric.Limits as NumericLimits


main =
  defaultMain $ pure $ checkParallel $ $$(discover)

prop_doubleAsciiDec =
  withTests 999 $
  property $ do
    a <- forAll realFloatGen
    let
      string =
        Char8ByteString.unpack (Write.writeToByteString (Write.doubleAsciiDec a))
    annotate string
    if isNaN a
      then string === "NaN"
      else read string === a

prop_realZeroNonRealDoubleAsciiDec =
  withTests 999 $
  property $ do
    a <- forAll realRealFloatGen
    let
      string =
        Char8ByteString.unpack (Write.writeToByteString (Write.zeroNonRealDoubleAsciiDec a))
    annotate string
    read string === a

prop_nonRealZeroNonRealDoubleAsciiDec =
  withTests 99 $
  property $ do
    a <- forAll nonRealRealFloatGen
    let
      string =
        Char8ByteString.unpack (Write.writeToByteString (Write.zeroNonRealDoubleAsciiDec a))
    annotate string
    read string === 0


-- * Gens
-------------------------

realFloatGen =
  Gen.frequency [
    (99, realRealFloatGen)
    ,
    (1, nonRealRealFloatGen)
    ]

nonRealRealFloatGen =
  Gen.element [0 / 0, 1 / 0, (-1) / 0, -0]

realRealFloatGen =
  Gen.realFloat (Range.exponentialFloat NumericLimits.minValue NumericLimits.maxValue)
