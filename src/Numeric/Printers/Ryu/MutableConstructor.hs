module Numeric.Printers.Ryu.MutableConstructor
  ( MutableConstructor(..)
  ) where

import Data.Vector.Unboxed.Mutable qualified as MV
import Text.Show (ShowS)
import Data.MonoMutableIndexable (Index)

class MutableConstructor a where
  type MutableCollection a s :: Type
  fromMutable :: Index (MutableCollection a s) -> MutableCollection a s -> ST s a

instance MutableConstructor String where
  type MutableCollection String s = MV.STVector s Char
  fromMutable i = MV.foldr (:) mempty . MV.take i

instance MutableConstructor ShowS where
  type MutableCollection ShowS s = MV.STVector s Char
  fromMutable i = MV.foldr ((.) . (:)) id . MV.take i

-- Need to implement
--instance MutableConstructor Data.ByteString.ByteString where
--instance MutableConstructor Data.ByteString.Lazy.ByteString where
--instance MutableConstructor Data.Text.Text where
--instance MutableConstructor Data.Text.Lazy.Text where
--instance MutableConstructor Foreign.C.CString where
