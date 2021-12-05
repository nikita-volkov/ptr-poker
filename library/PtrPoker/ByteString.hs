{-# LANGUAGE CPP #-}
module PtrPoker.ByteString
where

import PtrPoker.Prelude hiding (empty)
import Data.ByteString
import Data.ByteString.Internal
import Data.ByteString.Builder.Prim
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Builder.Scientific as ScientificBuilder
import qualified PtrPoker.Ffi as Ffi


builderWithStrategy strategy builder =
  builder
    & Builder.toLazyByteStringWith strategy Lazy.empty
    & Lazy.toStrict

scientific :: Scientific -> ByteString
scientific sci =
  sci
    & ScientificBuilder.scientificBuilder
    & builderWithStrategy (Builder.untrimmedStrategy 128 128)

double :: Double -> ByteString
double dbl =
  unsafeCreateUptoN 25 (\ ptr ->
    Ffi.pokeDouble dbl ptr
      & fmap fromIntegral)

unsafeCreateDownToN :: Int -> (Ptr Word8 -> IO Int) -> ByteString
unsafeCreateDownToN allocSize populate =
  unsafeDupablePerformIO $ do
    fp <- mallocByteString allocSize
    actualSize <- withForeignPtr fp (\ p -> populate (plusPtr p (pred allocSize)))
    return $! PS fp (allocSize - actualSize) actualSize

{-# INLINABLE textUtf8 #-}
textUtf8 :: Text -> ByteString
#if MIN_VERSION_text(2,0,0)
textUtf8 (TextInternal.Text (TextArray.ByteArray arr) off len) =
#else
textUtf8 (TextInternal.Text (TextArray.aBA -> arr) off len) =
#endif
  if len == 0
    then
      empty
    else
      unsafeCreateUptoN (len * 3) $ \ ptr -> do
        postPtr <- inline Ffi.encodeText ptr arr (fromIntegral off) (fromIntegral len)
        return (minusPtr postPtr ptr)
