module PtrPoker.Ffi.IntEncoding
where

import PtrPoker.Prelude
import Foreign.C


foreign import ccall unsafe "static rev_poke_int64_in_reverse"
  revPokeInt64InReverse :: CLLong -> Ptr Word8 -> IO ()
