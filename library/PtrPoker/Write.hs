module PtrPoker.Write
where

import PtrPoker.Prelude
import qualified PtrPoker.IO.ByteString as ByteStringIO
import qualified PtrPoker.IO.Prim as PrimIO
import qualified PtrPoker.Poke as Poke
import qualified PtrPoker.Size as Size
import qualified PtrPoker.Ffi as Ffi
import qualified PtrPoker.ByteString as ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString


{-# INLINABLE writeToByteString #-}
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

{-# INLINE word64AsciiDec #-}
word64AsciiDec :: Word64 -> Write
word64AsciiDec a =
  Write size poke
  where
    size =
      Size.word64AsciiDec a
    poke =
      Poke.sizedReverse size (Ffi.revPokeUInt64 (fromIntegral a))

{-# INLINE wordAsciiDec #-}
wordAsciiDec :: Word -> Write
wordAsciiDec =
  word64AsciiDec . fromIntegral

{-# INLINE int64AsciiDec #-}
int64AsciiDec :: Int64 -> Write
int64AsciiDec a =
  Write size poke
  where
    size =
      Size.int64AsciiDec a
    poke =
      Poke.sizedReverse size (Ffi.revPokeInt64 (fromIntegral a))

{-# INLINE intAsciiDec #-}
intAsciiDec :: Int -> Write
intAsciiDec =
  int64AsciiDec . fromIntegral

{-|
Render double interpreting non-real values,
such as @NaN@, @Infinity@, @-Infinity@,
as is.
-}
{-# INLINE doubleAsciiDec #-}
doubleAsciiDec :: Double -> Write
doubleAsciiDec a =
  if a == 0
    then word8 48
    else if isNaN a
      then "NaN"
      else if isInfinite a
        then if a < 0
          then "-Infinity"
          else "Infinity"
        else if a < 0
          then word8 45 <> byteString (ByteString.double (negate a))
          else byteString (ByteString.double a)

{-|
Render double interpreting non real values,
such as @NaN@, @Infinity@, @-Infinity@,
as zero.
-}
{-# INLINE zeroNonRealDoubleAsciiDec #-}
zeroNonRealDoubleAsciiDec :: Double -> Write
zeroNonRealDoubleAsciiDec a =
  if a == 0 || isNaN a || isInfinite a
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

{-|
Benchmark results in comparison to @Data.Text.Encoding.'Data.Text.Encoding.decodeUtf8'@.

>ptr-poker/latin/1               mean 50.85 ns  ( +- 1.037 ns  )
>ptr-poker/latin/10              mean 218.9 ns  ( +- 1.608 ns  )
>ptr-poker/latin/100             mean 1.748 μs  ( +- 24.90 ns  )
>ptr-poker/greek/1               mean 103.7 ns  ( +- 1.326 ns  )
>ptr-poker/greek/10              mean 564.1 ns  ( +- 4.206 ns  )
>ptr-poker/greek/100             mean 5.164 μs  ( +- 37.16 ns  )
>text/latin/1                    mean 63.20 ns  ( +- 4.031 ns  )
>text/latin/10                   mean 208.7 ns  ( +- 15.24 ns  )
>text/latin/100                  mean 1.327 μs  ( +- 60.74 ns  )
>text/greek/1                    mean 99.27 ns  ( +- 5.202 ns  )
>text/greek/10                   mean 487.4 ns  ( +- 7.031 ns  )
>text/greek/100                  mean 4.313 μs  ( +- 109.0 ns  )
-}
{-# INLINE textUtf8 #-}
textUtf8 :: Text -> Write
textUtf8 a =
  Write (Size.textUtf8 a) (Poke.textUtf8 a)
