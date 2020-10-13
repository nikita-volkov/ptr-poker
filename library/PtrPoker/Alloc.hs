module PtrPoker.Alloc
where

import PtrPoker.Prelude
import qualified PtrPoker.IO.ByteString as ByteStringIO
import qualified PtrPoker.IO.Prim as PrimIO
import qualified PtrPoker.IO.Ascii as AsciiIO
import qualified PtrPoker.Poke as Poke
import qualified PtrPoker.Size as Size
import qualified PtrPoker.Ffi.IntEncoding as IntEncodingFfi


{-|
Specification of how much bytes to allocate and how to populate them.

Useful for creating strict bytestrings and tasks like that.
-}
data Alloc =
  Alloc {
    allocSize :: Int,
    allocPoke :: Poke.Poke
    }

instance Semigroup Alloc where
  {-# INLINE (<>) #-}
  Alloc lSize lPoke <> Alloc rSize rPoke =
    Alloc (lSize + rSize) (lPoke <> rPoke)

instance Monoid Alloc where
  {-# INLINE mempty #-}
  mempty =
    Alloc 0 mempty

{-# INLINE intAsciiDec #-}
intAsciiDec :: Int -> Alloc
intAsciiDec =
  int64AsciiDec . fromIntegral

{-# INLINE int64AsciiDec #-}
int64AsciiDec :: Int64 -> Alloc
int64AsciiDec a =
  Alloc size poke
  where
    size =
      Size.int64Dec a
    poke =
      Poke.sizedReverse size (IntEncodingFfi.revPokeInt64InReverse (fromIntegral a))
