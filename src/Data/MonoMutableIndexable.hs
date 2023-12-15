module Data.MonoMutableIndexable
  ( MutableIndexable(..)
  ) where

import Data.Vector.Unboxed.Base (Unbox)
import Data.Vector.Unboxed.Mutable (STVector, unsafeNew, {- unsafeWrite, -} write)

class MutableIndexable a m where
  type Index a :: Type
  type Element a :: Type
  allocate :: Index a -> m a
  writeIndex :: a -> Index a -> Element a -> m ()
  --readIndex :: a -> Index a -> m (Element a)
  --modifyIndex :: Monad m => a -> Index a -> (Element a -> Element a) -> m ()
  --modifyIndex xs i f = writeIndex xs i . f =<< readIndex xs i

instance Unbox a => MutableIndexable (STVector s a) (ST s) where
  type Index (STVector s a) = Int
  type Element (STVector s a) = a
  allocate = unsafeNew
  writeIndex = write

