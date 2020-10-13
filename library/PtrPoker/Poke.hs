module PtrPoker.Poke
where

import PtrPoker.Prelude hiding (concat)
import qualified PtrPoker.IO.ByteString as ByteStringIO
import qualified PtrPoker.IO.Prim as PrimIO
import qualified PtrPoker.IO.Ascii as AsciiIO


{-# RULES
  "foldMap" forall f foldable. foldMap f foldable =
    Poke $ \ p -> foldM (\ p (Poke poker) -> poker p) p foldable
  #-}

newtype Poke =
  Poke { run :: Ptr Word8 -> IO (Ptr Word8) }

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


-- * ASCII integers
-------------------------

{-# INLINE[1] asciiDecInt8 #-}
asciiDecInt8 :: Int8 -> Poke
asciiDecInt8 a =
  Poke (AsciiIO.pokeIntInDec (fromIntegral a))

{-# INLINE[1] asciiDecInt16 #-}
asciiDecInt16 :: Int16 -> Poke
asciiDecInt16 a =
  Poke (AsciiIO.pokeIntInDec (fromIntegral a))

{-# INLINE[1] asciiDecInt32 #-}
asciiDecInt32 :: Int32 -> Poke
asciiDecInt32 a =
  Poke (AsciiIO.pokeIntInDec (fromIntegral a))

{-# INLINE[1] asciiDecInt64 #-}
asciiDecInt64 :: Int64 -> Poke
asciiDecInt64 a =
  Poke (AsciiIO.pokeLongLongIntInDec (fromIntegral a))

{-# INLINE[1] asciiDecInt #-}
asciiDecInt :: Int -> Poke
asciiDecInt a =
  Poke (AsciiIO.pokeLongLongIntInDec (fromIntegral a))

{-# INLINE[1] asciiDecWord8 #-}
asciiDecWord8 :: Word8 -> Poke
asciiDecWord8 a =
  Poke (AsciiIO.pokeUIntInDec (fromIntegral a))

{-# INLINE[1] asciiDecWord16 #-}
asciiDecWord16 :: Word16 -> Poke
asciiDecWord16 a =
  Poke (AsciiIO.pokeUIntInDec (fromIntegral a))

{-# INLINE[1] asciiDecWord32 #-}
asciiDecWord32 :: Word32 -> Poke
asciiDecWord32 a =
  Poke (AsciiIO.pokeUIntInDec (fromIntegral a))

{-# INLINE[1] asciiDecWord64 #-}
asciiDecWord64 :: Word64 -> Poke
asciiDecWord64 a =
  Poke (AsciiIO.pokeLongLongUIntInDec (fromIntegral a))

{-# INLINE[1] asciiDecWord #-}
asciiDecWord :: Word -> Poke
asciiDecWord a =
  Poke (AsciiIO.pokeLongLongUIntInDec (fromIntegral a))

{-# INLINE asciiDouble #-}
asciiDouble :: Double -> Poke
asciiDouble a =
  Poke $ \ ptr ->
    AsciiIO.pokeDouble a ptr
      & fmap (plusPtr ptr . fromIntegral)
