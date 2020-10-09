module PtrPoker
where

import PtrPoker.Prelude hiding (concat)
import qualified PtrPoker.IO.ByteString as ByteStringIO
import qualified PtrPoker.IO.Prim as PrimIO


newtype Poker =
  Poker { run :: Ptr Word8 -> IO (Ptr Word8) }

instance Semigroup Poker where
  {-# INLINE (<>) #-}
  Poker lIO <> Poker rIO =
    Poker (lIO >=> rIO)
  sconcat =
    concat

instance Monoid Poker where
  mempty =
    Poker return
  mconcat =
    concat

{-# INLINE concat #-}
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
