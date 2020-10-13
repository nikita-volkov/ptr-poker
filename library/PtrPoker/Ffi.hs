module PtrPoker.Ffi
where

import PtrPoker.Prelude
import Foreign.C


foreign import ccall unsafe "static rev_poke_int64_in_reverse"
  revPokeInt64InReverse :: CLLong -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static dtoa_grisu3"
  pokeDouble :: Double -> Ptr Word8 -> IO CInt
