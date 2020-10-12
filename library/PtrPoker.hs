module PtrPoker
where

import PtrPoker.Prelude hiding (concat)
import qualified PtrPoker.IO.ByteString as ByteStringIO
import qualified PtrPoker.IO.Prim as PrimIO
import qualified PtrPoker.IO.Ascii as AsciiIO


{-# RULES
  "foldMap" forall f foldable. foldMap f foldable =
    Poker $ \ p -> foldM (\ p (Poker poker) -> poker p) p foldable
  #-}

newtype Poker =
  Poker { run :: Ptr Word8 -> IO (Ptr Word8) }

instance Semigroup Poker where
  {-# INLINE[1] (<>) #-}
  Poker lIO <> Poker rIO =
    Poker (lIO >=> rIO)
  sconcat =
    concat

instance Monoid Poker where
  {-# INLINE [1] mempty #-}
  mempty =
    Poker return
  mconcat =
    concat

{-# INLINE[1] concat #-}
concat :: Foldable f => f Poker -> Poker
concat pokers =
  Poker (\ p -> foldM (\ p (Poker io) -> io p) p pokers)

{-# INLINE byteString #-}
byteString :: ByteString -> Poker
byteString bs =
  Poker $ \ ptr -> ByteStringIO.pokeByteString ptr bs

{-# INLINE word8 #-}
word8 :: Word8 -> Poker
word8 a =
  Poker (\ p -> PrimIO.pokeWord8 p a $> plusPtr p 1)

{-| Little-endian Word64 poker. -}
{-# INLINE lWord64 #-}
lWord64 :: Word64 -> Poker
lWord64 a =
  Poker (\ p -> PrimIO.pokeLEWord64 p a $> plusPtr p 8)

{-| Big-endian Word64 poker. -}
{-# INLINE bWord64 #-}
bWord64 :: Word64 -> Poker
bWord64 a =
  Poker (\ p -> PrimIO.pokeBEWord64 p a $> plusPtr p 8)


-- * ASCII integers
-------------------------

{-# INLINE asciiDecInt8 #-}
asciiDecInt8 :: Int8 -> Poker
asciiDecInt8 a =
  Poker (AsciiIO.pokeIntInDec (fromIntegral a))

{-# INLINE asciiDecInt16 #-}
asciiDecInt16 :: Int16 -> Poker
asciiDecInt16 a =
  Poker (AsciiIO.pokeIntInDec (fromIntegral a))

{-# INLINE asciiDecInt32 #-}
asciiDecInt32 :: Int32 -> Poker
asciiDecInt32 a =
  Poker (AsciiIO.pokeIntInDec (fromIntegral a))

{-# INLINE asciiDecInt64 #-}
asciiDecInt64 :: Int64 -> Poker
asciiDecInt64 a =
  Poker (AsciiIO.pokeLongLongIntInDec (fromIntegral a))

{-# INLINE asciiDecInt #-}
asciiDecInt :: Int -> Poker
asciiDecInt a =
  Poker (AsciiIO.pokeLongLongIntInDec (fromIntegral a))

{-# INLINE asciiDecWord8 #-}
asciiDecWord8 :: Word8 -> Poker
asciiDecWord8 a =
  Poker (AsciiIO.pokeUIntInDec (fromIntegral a))

{-# INLINE asciiDecWord16 #-}
asciiDecWord16 :: Word16 -> Poker
asciiDecWord16 a =
  Poker (AsciiIO.pokeUIntInDec (fromIntegral a))

{-# INLINE asciiDecWord32 #-}
asciiDecWord32 :: Word32 -> Poker
asciiDecWord32 a =
  Poker (AsciiIO.pokeUIntInDec (fromIntegral a))

{-# INLINE asciiDecWord64 #-}
asciiDecWord64 :: Word64 -> Poker
asciiDecWord64 a =
  Poker (AsciiIO.pokeLongLongUIntInDec (fromIntegral a))

{-# INLINE asciiDecWord #-}
asciiDecWord :: Word -> Poker
asciiDecWord a =
  Poker (AsciiIO.pokeLongLongUIntInDec (fromIntegral a))
