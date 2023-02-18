{-# LANGUAGE CPP #-}

module PtrPoker.Compat.ByteString where

#if MIN_VERSION_bytestring(0,11,0)
import Data.ByteString.Internal
import PtrPoker.Prelude

{-# INLINE poke #-}
poke :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
poke (BS fptr length) ptr =
  {-# SCC "poke" #-}
  withForeignPtr fptr $ \ bytesPtr ->
    memcpy ptr bytesPtr length $>
    plusPtr ptr length
#else
import Data.ByteString.Internal
import PtrPoker.Prelude

{-# INLINE poke #-}
poke :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
poke (PS fptr offset length) ptr =
  {-# SCC "poke" #-}
  withForeignPtr fptr $ \ bytesPtr ->
    memcpy ptr (plusPtr bytesPtr offset) length $>
    plusPtr ptr length
#endif
