{-# LANGUAGE CPP #-}
module PtrPoker.IO.Prim
where

import PtrPoker.Prelude
import qualified PtrPoker.UncheckedShifting as UncheckedShifting


{-# INLINE pokeStorable #-}
pokeStorable :: Storable a => Ptr Word8 -> a -> IO ()
pokeStorable ptr value =
  {-# SCC "pokeStorable" #-} 
  poke (castPtr ptr) value

{-# INLINE pokeWord8 #-}
pokeWord8 :: Ptr Word8 -> Word8 -> IO ()
pokeWord8 ptr value =
  {-# SCC "pokeWord8" #-} 
  poke ptr value

{-# INLINE pokeWord8Off #-}
pokeWord8Off :: Ptr Word8 -> Int -> Word8 -> IO ()
pokeWord8Off ptr off value =
  {-# SCC "pokeWord8Off" #-} 
  pokeByteOff ptr off value

{-# INLINE pokeBEWord16 #-}
pokeBEWord16 :: Ptr Word8 -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord16 =
  {-# SCC "pokeBEWord16" #-} 
  pokeStorable
#else
pokeBEWord16 ptr value =
  {-# SCC "pokeBEWord16" #-} 
  do
    pokeStorable ptr (fromIntegral (UncheckedShifting.shiftr_w16 value 8) :: Word8)
    pokeByteOff ptr 1 (fromIntegral value :: Word8)
#endif

{-# INLINE pokeBEWord32 #-}
pokeBEWord32 :: Ptr Word8 -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord32 =
  {-# SCC "pokeBEWord32" #-} 
  pokeStorable
#else
pokeBEWord32 ptr value =
  {-# SCC "pokeBEWord32" #-} 
  do
    pokeStorable ptr (fromIntegral (UncheckedShifting.shiftr_w32 value 24) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (UncheckedShifting.shiftr_w32 value 16) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (UncheckedShifting.shiftr_w32 value 8) :: Word8)
    pokeByteOff ptr 3 (fromIntegral value :: Word8)
#endif

{-# INLINE pokeBEWord64 #-}
pokeBEWord64 :: Ptr Word8 -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord64 =
  {-# SCC "pokeBEWord64" #-} 
  pokeStorable
#else
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
pokeBEWord64 ptr value =
  {-# SCC "pokeBEWord64" #-} 
  do
    pokeBEWord32 ptr (fromIntegral (UncheckedShifting.shiftr_w64 value 32))
    pokeBEWord32 (plusPtr ptr 4) (fromIntegral value)
#else
pokeBEWord64 ptr value =
  {-# SCC "pokeBEWord64" #-} 
  do
    pokeStorable ptr (fromIntegral (UncheckedShifting.shiftr_w64 value 56) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (UncheckedShifting.shiftr_w64 value 48) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (UncheckedShifting.shiftr_w64 value 40) :: Word8)
    pokeByteOff ptr 3 (fromIntegral (UncheckedShifting.shiftr_w64 value 32) :: Word8)
    pokeByteOff ptr 4 (fromIntegral (UncheckedShifting.shiftr_w64 value 24) :: Word8)
    pokeByteOff ptr 5 (fromIntegral (UncheckedShifting.shiftr_w64 value 16) :: Word8)
    pokeByteOff ptr 6 (fromIntegral (UncheckedShifting.shiftr_w64 value  8) :: Word8)
    pokeByteOff ptr 7 (fromIntegral value :: Word8)
#endif
#endif

{-# INLINE pokeLEWord16 #-}
pokeLEWord16 :: Ptr Word8 -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeLEWord16 p w =
  {-# SCC "pokeLEWord16" #-} 
  do
    pokeWord8 p (fromIntegral w)
    pokeWord8Off p 1 (fromIntegral (UncheckedShifting.shiftr_w16 w 8))
#else
pokeLEWord16 =
  {-# SCC "pokeLEWord16" #-} 
  pokeStorable
#endif

{-# INLINE pokeLEWord32 #-}
pokeLEWord32 :: Ptr Word8 -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeLEWord32 p w =
  {-# SCC "pokeLEWord32" #-}
  do
    pokeWord8 p (fromIntegral w)
    pokeWord8Off p 1 (fromIntegral (UncheckedShifting.shiftr_w32 w 8))
    pokeWord8Off p 2 (fromIntegral (UncheckedShifting.shiftr_w32 w 16))
    pokeWord8Off p 3 (fromIntegral (UncheckedShifting.shiftr_w32 w 24))
#else
pokeLEWord32 =
  {-# SCC "pokeLEWord32" #-} 
  pokeStorable
#endif

{-# INLINE pokeLEWord64 #-}
pokeLEWord64 :: Ptr Word8 -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
pokeLEWord64 p w =
  {-# SCC "pokeLEWord64" #-} 
  do
    let b = fromIntegral (UncheckedShifting.shiftr_w64 w 32) :: Word32
        a = fromIntegral w :: Word32
    pokeWord8 p (fromIntegral a)
    pokeWord8Off p 1 (fromIntegral (UncheckedShifting.shiftr_w32 a 8))
    pokeWord8Off p 2 (fromIntegral (UncheckedShifting.shiftr_w32 a 16))
    pokeWord8Off p 3 (fromIntegral (UncheckedShifting.shiftr_w32 a 24))
    pokeWord8Off p 4 (fromIntegral b)
    pokeWord8Off p 5 (fromIntegral (UncheckedShifting.shiftr_w32 b 8))
    pokeWord8Off p 6 (fromIntegral (UncheckedShifting.shiftr_w32 b 16))
    pokeWord8Off p 7 (fromIntegral (UncheckedShifting.shiftr_w32 b 24))
#else
pokeLEWord64 p w =
  {-# SCC "pokeLEWord64" #-} 
  do
    pokeWord8 p (fromIntegral w)
    pokeWord8Off p 1 (fromIntegral (UncheckedShifting.shiftr_w64 w 8))
    pokeWord8Off p 2 (fromIntegral (UncheckedShifting.shiftr_w64 w 16))
    pokeWord8Off p 3 (fromIntegral (UncheckedShifting.shiftr_w64 w 24))
    pokeWord8Off p 4 (fromIntegral (UncheckedShifting.shiftr_w64 w 32))
    pokeWord8Off p 5 (fromIntegral (UncheckedShifting.shiftr_w64 w 40))
    pokeWord8Off p 6 (fromIntegral (UncheckedShifting.shiftr_w64 w 48))
    pokeWord8Off p 7 (fromIntegral (UncheckedShifting.shiftr_w64 w 56))
#endif
#else
pokeLEWord64 =
  {-# SCC "pokeLEWord64" #-} 
  pokeStorable
#endif
