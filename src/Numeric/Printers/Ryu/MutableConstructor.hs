{-# LANGUAGE MagicHash #-}
module Numeric.Printers.Ryu.MutableConstructor
  ( MutableConstructor(..)
  ) where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as B
import Data.MonoMutableIndexable (Index)
import Data.Vector.Unboxed.Mutable qualified as MV
import Foreign.C.String (CString)
import Text.Show (ShowS)

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

