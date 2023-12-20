{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
module Data.MonoMutableIndexable
  ( MutableIndexable(..)
  ) where

import Data.Vector.Unboxed.Base (Unbox)
--import Data.Vector.Unboxed.Mutable (STVector, unsafeNew, {- unsafeWrite, -} write)
import Data.Vector.Unboxed.Mutable qualified as UMV
import Foreign.C.String (CString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable (pokeByteOff)
import Data.Array.Byte (MutableByteArray(MutableByteArray))
import GHC.Int (Int(I#))
import GHC.Word (Word16(W16#))
import GHC.ST qualified
import GHC.Exts (newByteArray#, writeWord16Array#)

class MutableIndexable a m where
  type Index a :: Type
  type Element a :: Type
  allocate :: Index a -> m a
  writeIndex :: a -> Index a -> Element a -> m ()
  --readIndex :: a -> Index a -> m (Element a)
  --modifyIndex :: Monad m => a -> Index a -> (Element a -> Element a) -> m ()
  --modifyIndex xs i f = writeIndex xs i . f =<< readIndex xs i

instance Unbox a => MutableIndexable (UMV.STVector s a) (ST s) where
  type Index (UMV.STVector s a) = Int
  type Element (UMV.STVector s a) = a
  allocate = UMV.unsafeNew
#ifdef SAFE_INDEX
  writeIndex = UMV.write
#else
  writeIndex = UMV.unsafeWrite
#endif

instance MutableIndexable CString IO where
  type Index CString = Int
  type Element CString = CChar
  allocate = mallocBytes
  writeIndex = pokeByteOff

instance MutableIndexable (MutableByteArray s) (ST s) where
  type Index (MutableByteArray _) = Int
  type Element (MutableByteArray _) = Word16
  allocate ((* 2) ->(I# n#)) = GHC.ST.ST \s1# -> case newByteArray# n# s1# of
    (# s2#, mba# #) -> (# s2#, MutableByteArray mba# #)
  writeIndex (MutableByteArray marr) (I# i#) (W16# e#) =
    GHC.ST.ST \s1# -> case writeWord16Array# marr i# e# s1# of
      s2# -> (# s2#, () #)

