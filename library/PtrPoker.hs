module PtrPoker
where

import PtrPoker.Prelude
import qualified PtrPoker.IO.ByteString as ByteStringIO


newtype Poker =
  Poker { run :: Ptr Word8 -> IO (Ptr Word8) }

instance Semigroup Poker where
  {-# INLINE (<>) #-}
  Poker lIO <> Poker rIO =
    Poker (lIO >=> rIO)

instance Monoid Poker where
  mempty =
    Poker return

{-# INLINE byteString #-}
byteString :: ByteString -> Poker
byteString bs =
  Poker $ \ ptr -> ByteStringIO.pokeByteString ptr bs
