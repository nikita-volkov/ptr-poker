{-# LANGUAGE CPP #-}

module PtrPoker.IO.ByteString where

import Data.ByteString.Internal
import PtrPoker.Prelude

#if MIN_VERSION_bytestring(0,11,0)

{-# INLINE pokeByteString #-}
pokeByteString :: Ptr Word8 -> ByteString -> IO (Ptr Word8)
pokeByteString ptr (BS fptr length) =
  {-# SCC "pokeByteString" #-}
  withForeignPtr fptr $ \ bytesPtr ->
    memcpy ptr bytesPtr length $>
    plusPtr ptr length

#else

{-# INLINE pokeByteString #-}
pokeByteString :: Ptr Word8 -> ByteString -> IO (Ptr Word8)
pokeByteString ptr (PS fptr offset length) =
  {-# SCC "pokeByteString" #-}
  withForeignPtr fptr $ \ bytesPtr ->
    memcpy ptr (plusPtr bytesPtr offset) length $>
    plusPtr ptr length

#endif
