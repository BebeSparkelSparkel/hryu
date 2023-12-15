{-# LANGUAGE MagicHash #-}
module Numeric.Printers.Ryu.MutableConstructor
  ( MutableConstructor(..)
  ) where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Internal qualified as T
import Data.Text.Lazy qualified as TL
import Data.ByteString.Unsafe qualified as B
import Data.ByteString.Lazy qualified as BL
import Foreign.C.String (CString)
import Data.Array.Byte (MutableByteArray(MutableByteArray))
import Data.Vector.Unboxed.Mutable qualified as MV
import Text.Show (ShowS)
import Data.MonoMutableIndexable (Index)
import Data.Text.Array (MArray(MArray), unsafeFreeze)

class MutableConstructor a m where
  type MutableCollection a m :: Type
  fromMutable :: Index (MutableCollection a m) -> MutableCollection a m -> m a

instance MutableConstructor String (ST s) where
  type MutableCollection String (ST s) = MV.STVector s Char
  fromMutable i = fromMutable @ShowS i >$> ($ [])
instance MutableConstructor ShowS (ST s) where
  type MutableCollection ShowS (ST s) = MV.STVector s Char
  fromMutable i = MV.foldr ((.) . (:)) id . MV.take i

instance MutableConstructor B.ByteString IO where
  type MutableCollection B.ByteString _ = CString
  fromMutable i cstr = B.unsafePackMallocCStringLen (cstr, i)
instance MutableConstructor BL.ByteString IO where
  type MutableCollection BL.ByteString _ = CString
  fromMutable i cstr = BL.fromStrict <$> fromMutable i cstr

instance MutableConstructor T.Text (ST s) where
  type MutableCollection T.Text (ST s) = MutableByteArray s
  fromMutable i (MutableByteArray xs#) = (\xs' -> T.Text xs' 0 i) <$> unsafeFreeze (MArray xs# )
instance MutableConstructor TL.Text (ST s) where
  type MutableCollection TL.Text (ST s) = MutableByteArray s
  fromMutable i mba = TL.fromStrict <$> fromMutable i mba

