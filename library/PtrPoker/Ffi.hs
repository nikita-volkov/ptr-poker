module PtrPoker.Ffi
where

import PtrPoker.Prelude
import Foreign.C


foreign import ccall unsafe "static rev_poke_int64"
  revPokeInt64 :: CLLong -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static dtoa_grisu3"
  pokeDouble :: Double -> Ptr Word8 -> IO CInt
