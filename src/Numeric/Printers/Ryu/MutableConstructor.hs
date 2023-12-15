module Numeric.Printers.Ryu.MutableConstructor
  ( MutableConstructor(..)
  ) where

import Data.Vector.Unboxed.Mutable qualified as MV
import Text.Show (ShowS)
import Control.Monad.ST (ST)
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

