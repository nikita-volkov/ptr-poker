module PtrPoker.Write where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified PtrPoker.ByteString as ByteString
import qualified PtrPoker.Ffi as Ffi
import qualified PtrPoker.Poke as Poke
import PtrPoker.Prelude hiding (concat)
import qualified PtrPoker.Size as Size

-- |
-- Execute Write, producing strict ByteString.
{-# INLINEABLE writeToByteString #-}
writeToByteString :: Write -> ByteString
writeToByteString Write {..} =
  ByteString.unsafeCreate writeSize (void . Poke.pokePtr writePoke)

-- |
-- Specification of how many bytes to allocate and how to populate them.
--
-- Useful for creating strict bytestrings and tasks like that.
data Write = Write
  { writeSize :: Int,
    writePoke :: Poke.Poke
  }

instance Semigroup Write where
  {-# INLINE (<>) #-}
  Write lSize lPoke <> Write rSize rPoke =
    Write (lSize + rSize) (lPoke <> rPoke)
  {-# INLINE sconcat #-}
  sconcat =
    concat

instance Monoid Write where
  {-# INLINE mempty #-}
  mempty =
    Write 0 mempty
  {-# INLINE mconcat #-}
  mconcat =
    concat

-- |
-- Reuses the IsString instance of 'ByteString'.
instance IsString Write where
  {-# INLINE fromString #-}
  fromString =
    byteString . fromString

-- |
-- Concatenate a foldable of writes.
{-# INLINE concat #-}
concat :: Foldable f => f Write -> Write
concat f =
  Write
    (foldl' (\a b -> a + writeSize b) 0 f)
    (Poke.Poke (\p -> foldM (\p write -> Poke.pokePtr (writePoke write) p) p f))

-- |
-- Render Word8 as byte.
{-# INLINE word8 #-}
word8 :: Word8 -> Write
word8 a =
  Write 1 (Poke.word8 a)

-- |
-- Render Word16 in Little-endian.
{-# INLINE lWord16 #-}
lWord16 :: Word16 -> Write
lWord16 a =
  Write 2 (Poke.lWord16 a)

-- |
-- Render Word16 in Big-endian.
{-# INLINE bWord16 #-}
bWord16 :: Word16 -> Write
bWord16 a =
  Write 2 (Poke.bWord16 a)

-- |
-- Render Word32 in Little-endian.
{-# INLINE lWord32 #-}
lWord32 :: Word32 -> Write
lWord32 a =
  Write 4 (Poke.lWord32 a)

-- |
-- Render Word32 in Big-endian.
{-# INLINE bWord32 #-}
bWord32 :: Word32 -> Write
bWord32 a =
  Write 4 (Poke.bWord32 a)

-- |
-- Render Word64 in Little-endian.
{-# INLINE lWord64 #-}
lWord64 :: Word64 -> Write
lWord64 a =
  Write 8 (Poke.lWord64 a)

-- |
-- Render Word64 in Big-endian.
{-# INLINE bWord64 #-}
bWord64 :: Word64 -> Write
bWord64 a =
  Write 8 (Poke.bWord64 a)

-- |
-- Render Int16 in Little-endian.
{-# INLINE lInt16 #-}
lInt16 :: Int16 -> Write
lInt16 a =
  Write 2 (Poke.lInt16 a)

-- |
-- Render Int16 in Big-endian.
{-# INLINE bInt16 #-}
bInt16 :: Int16 -> Write
bInt16 a =
  Write 2 (Poke.bInt16 a)

-- |
-- Render Int32 in Little-endian.
{-# INLINE lInt32 #-}
lInt32 :: Int32 -> Write
lInt32 a =
  Write 4 (Poke.lInt32 a)

-- |
-- Render Int32 in Big-endian.
{-# INLINE bInt32 #-}
bInt32 :: Int32 -> Write
bInt32 a =
  Write 4 (Poke.bInt32 a)

-- |
-- Render Int64 in Little-endian.
{-# INLINE lInt64 #-}
lInt64 :: Int64 -> Write
lInt64 a =
  Write 8 (Poke.lInt64 a)

-- |
-- Render Int64 in Big-endian.
{-# INLINE bInt64 #-}
bInt64 :: Int64 -> Write
bInt64 a =
  Write 8 (Poke.bInt64 a)

-- |
-- Render Word64 in ASCII decimal.
{-# INLINE word64AsciiDec #-}
word64AsciiDec :: Word64 -> Write
word64AsciiDec a =
  Write size poke
  where
    size =
      Size.word64AsciiDec a
    poke =
      Poke.sizedReverse size (Ffi.revPokeUInt64 (fromIntegral a))

-- |
-- Render Word in ASCII decimal.
{-# INLINE wordAsciiDec #-}
wordAsciiDec :: Word -> Write
wordAsciiDec =
  word64AsciiDec . fromIntegral

-- |
-- Render Int64 in ASCII decimal.
{-# INLINE int64AsciiDec #-}
int64AsciiDec :: Int64 -> Write
int64AsciiDec a =
  Write size poke
  where
    size =
      Size.int64AsciiDec a
    poke =
      Poke.sizedReverse size (Ffi.revPokeInt64 (fromIntegral a))

-- |
-- Render Int in ASCII decimal.
{-# INLINE intAsciiDec #-}
intAsciiDec :: Int -> Write
intAsciiDec =
  int64AsciiDec . fromIntegral

-- |
-- Render double interpreting non-real values,
-- such as @NaN@, @Infinity@, @-Infinity@,
-- as is.
{-# INLINE doubleAsciiDec #-}
doubleAsciiDec :: Double -> Write
doubleAsciiDec a =
  if a == 0
    then word8 48
    else
      if isNaN a
        then "NaN"
        else
          if isInfinite a
            then
              if a < 0
                then "-Infinity"
                else "Infinity"
            else
              if a < 0
                then word8 45 <> byteString (ByteString.double (negate a))
                else byteString (ByteString.double a)

-- |
-- Render double interpreting non real values,
-- such as @NaN@, @Infinity@, @-Infinity@,
-- as zero.
{-# INLINE zeroNonRealDoubleAsciiDec #-}
zeroNonRealDoubleAsciiDec :: Double -> Write
zeroNonRealDoubleAsciiDec a =
  if a == 0 || isNaN a || isInfinite a
    then word8 48
    else
      if a < 0
        then word8 45 <> byteString (ByteString.double (negate a))
        else byteString (ByteString.double a)

-- |
-- Render Scientific in ASCII decimal.
{-# INLINE scientificAsciiDec #-}
scientificAsciiDec :: Scientific -> Write
scientificAsciiDec =
  byteString . ByteString.scientific

-- |
-- Efficiently copy the contents of ByteString using @memcpy@.
{-# INLINE byteString #-}
byteString :: ByteString -> Write
byteString a =
  Write (ByteString.length a) (inline Poke.byteString a)

-- |
-- Render Text in UTF8.
--
-- Does pretty much the same as 'Data.Text.Encoding.encodeUtf8',
-- while allowing you to avoid redundant @memcpy@
-- compared to @('byteString' . 'Data.Text.Encoding.encodeUtf8')@.
{-# INLINEABLE textUtf8 #-}
textUtf8 :: Text -> Write
textUtf8 =
  byteString . ByteString.textUtf8
