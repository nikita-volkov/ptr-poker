module PtrPoker.Write
where

import PtrPoker.Prelude
import qualified PtrPoker.IO.ByteString as ByteStringIO
import qualified PtrPoker.IO.Prim as PrimIO
import qualified PtrPoker.IO.Ascii as AsciiIO
import qualified PtrPoker.Poke as Poke
import qualified PtrPoker.Size as Size
import qualified PtrPoker.Ffi as Ffi
import qualified PtrPoker.ByteString as ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString


writeToByteString :: Write -> ByteString
writeToByteString Write{..} =
  ByteString.unsafeCreate writeSize (void . Poke.pokePtr writePoke)

{-|
Specification of how much bytes to allocate and how to populate them.

Useful for creating strict bytestrings and tasks like that.
-}
data Write =
  Write {
    writeSize :: Int,
    writePoke :: Poke.Poke
    }

instance Semigroup Write where
  {-# INLINE (<>) #-}
  Write lSize lPoke <> Write rSize rPoke =
    Write (lSize + rSize) (lPoke <> rPoke)

instance Monoid Write where
  {-# INLINE mempty #-}
  mempty =
    Write 0 mempty

instance IsString Write where
  {-# INLINE fromString #-}
  fromString =
    byteString . fromString

{-# INLINE word8 #-}
word8 :: Word8 -> Write
word8 a =
  Write 1 (Poke.word8 a)

{-# INLINE intAsciiDec #-}
intAsciiDec :: Int -> Write
intAsciiDec =
  int64AsciiDec . fromIntegral

{-# INLINE int64AsciiDec #-}
int64AsciiDec :: Int64 -> Write
int64AsciiDec a =
  Write size poke
  where
    size =
      Size.int64Dec a
    poke =
      Poke.sizedReverse size (Ffi.revPokeInt64 (fromIntegral a))

{-|
Render double interpreting non real values,
such as @NaN@, @Infinity@, @-Infinity@,
as zero.
-}
{-# INLINE zeroNonRealDoubleAsciiDec #-}
zeroNonRealDoubleAsciiDec :: Double -> Write
zeroNonRealDoubleAsciiDec a =
  if isNaN a || isInfinite a || isNegativeZero a
    then word8 48
    else if a < 0
      then word8 45 <> byteString (ByteString.double (negate a))
      else byteString (ByteString.double a)

{-# INLINE scientificAsciiDec #-}
scientificAsciiDec :: Scientific -> Write
scientificAsciiDec =
  byteString . ByteString.scientific

{-# INLINE byteString #-}
byteString :: ByteString -> Write
byteString a =
  Write (ByteString.length a) (Poke.byteString a)
