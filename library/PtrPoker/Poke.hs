module PtrPoker.Poke
where

import PtrPoker.Prelude hiding (concat)
import qualified PtrPoker.IO.ByteString as ByteStringIO
import qualified PtrPoker.IO.Prim as PrimIO
import qualified PtrPoker.Ffi as Ffi
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array as TextArray


{-# RULES
  "foldMap" forall f foldable. foldMap f foldable =
    Poke $ \ p -> foldM (\ p (Poke poker) -> poker p) p foldable
  #-}

newtype Poke =
  Poke { pokePtr :: Ptr Word8 -> IO (Ptr Word8) }

instance Semigroup Poke where
  {-# INLINE[1] (<>) #-}
  Poke lIO <> Poke rIO =
    Poke (lIO >=> rIO)
  sconcat =
    concat

instance Monoid Poke where
  {-# INLINE[1] mempty #-}
  mempty =
    Poke return
  mconcat =
    concat

instance IsString Poke where
  fromString = byteString . fromString

{-# INLINE[1] concat #-}
concat :: Foldable f => f Poke -> Poke
concat pokers =
  Poke (\ p -> foldM (\ p (Poke io) -> io p) p pokers)

{-# INLINE[1] byteString #-}
byteString :: ByteString -> Poke
byteString bs =
  Poke $ \ ptr -> ByteStringIO.pokeByteString ptr bs

{-# INLINE[1] word8 #-}
word8 :: Word8 -> Poke
word8 a =
  Poke (\ p -> PrimIO.pokeWord8 p a $> plusPtr p 1)

{-| Little-endian Word64 poker. -}
{-# INLINE[1] lWord64 #-}
lWord64 :: Word64 -> Poke
lWord64 a =
  Poke (\ p -> PrimIO.pokeLEWord64 p a $> plusPtr p 8)

{-| Big-endian Word64 poker. -}
{-# INLINE[1] bWord64 #-}
bWord64 :: Word64 -> Poke
bWord64 a =
  Poke (\ p -> PrimIO.pokeBEWord64 p a $> plusPtr p 8)

{-# INLINE textUtf8 #-}
textUtf8 :: Text -> Poke
textUtf8 (Text.Text arr off len) =
  Poke (\ p -> Ffi.encodeText p (TextArray.aBA arr) (fromIntegral off) (fromIntegral len))


-- * ASCII integers
-------------------------

{-# INLINE[1] int8AsciiDec #-}
int8AsciiDec :: Int8 -> Poke
int8AsciiDec a =
  Poke (Ffi.pokeIntInDec (fromIntegral a))

{-# INLINE[1] int16AsciiDec #-}
int16AsciiDec :: Int16 -> Poke
int16AsciiDec a =
  Poke (Ffi.pokeIntInDec (fromIntegral a))

{-# INLINE[1] int32AsciiDec #-}
int32AsciiDec :: Int32 -> Poke
int32AsciiDec a =
  Poke (Ffi.pokeIntInDec (fromIntegral a))

{-# INLINE[1] int64AsciiDec #-}
int64AsciiDec :: Int64 -> Poke
int64AsciiDec a =
  Poke (Ffi.pokeLongLongIntInDec (fromIntegral a))

{-# INLINE[1] intAsciiDec #-}
intAsciiDec :: Int -> Poke
intAsciiDec a =
  Poke (Ffi.pokeLongLongIntInDec (fromIntegral a))

{-# INLINE[1] word8AsciiDec #-}
word8AsciiDec :: Word8 -> Poke
word8AsciiDec a =
  Poke (Ffi.pokeUIntInDec (fromIntegral a))

{-# INLINE[1] word16AsciiDec #-}
word16AsciiDec :: Word16 -> Poke
word16AsciiDec a =
  Poke (Ffi.pokeUIntInDec (fromIntegral a))

{-# INLINE[1] word32AsciiDec #-}
word32AsciiDec :: Word32 -> Poke
word32AsciiDec a =
  Poke (Ffi.pokeUIntInDec (fromIntegral a))

{-# INLINE[1] word64AsciiDec #-}
word64AsciiDec :: Word64 -> Poke
word64AsciiDec a =
  Poke (Ffi.pokeLongLongUIntInDec (fromIntegral a))

{-# INLINE[1] wordAsciiDec #-}
wordAsciiDec :: Word -> Poke
wordAsciiDec a =
  Poke (Ffi.pokeLongLongUIntInDec (fromIntegral a))

{-# INLINE doubleAsciiDec #-}
doubleAsciiDec :: Double -> Poke
doubleAsciiDec a =
  Poke $ \ ptr ->
    Ffi.pokeDouble a ptr
      & fmap (plusPtr ptr . fromIntegral)


-- * Low level
-------------------------

{-|
Having the amount of bytes to be written precomputed,
executes an action, which fills the pointer going downward.
-}
{-# INLINE sizedReverse #-}
sizedReverse :: Int -> (Ptr Word8 -> IO a) -> Poke
sizedReverse size action =
  Poke $ \ ptr ->
    let
      afterPtr =
        plusPtr ptr size
      in action afterPtr $> afterPtr
