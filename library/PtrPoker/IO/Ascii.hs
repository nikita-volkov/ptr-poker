module PtrPoker.IO.Ascii
where

import PtrPoker.Prelude
import Foreign.C.Types


foreign import ccall unsafe "static _hs_bytestring_int_dec" pokeIntInDec
    :: CInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_long_long_int_dec" pokeLongLongIntInDec
    :: CLLong -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_uint_dec" pokeUIntInDec
    :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_long_long_uint_dec" pokeLongLongUIntInDec
    :: CULLong -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_uint_hex" pokeUIntInHex
    :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_long_long_uint_hex" pokeLongLongUIntInHex
    :: CULLong -> Ptr Word8 -> IO (Ptr Word8)
