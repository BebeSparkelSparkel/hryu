module Prelude
  ( module Control.Applicative
  , module Control.Monad
  , module Data.Bifunctor
  , module Data.Bits
  , module Data.Bool
  , module Data.Char
  , module Data.Coerce
  , module Data.Default
  , module Data.Either
  , module Data.Eq
  , module Data.Function
  , module Data.Functor
  , module Data.Int
  , module Data.Kind
  , module Data.Monoid
  , module Data.Ord
  , module Data.String
  , module Data.Tuple
  , module Data.Vector
  , module Data.Word
  , module GHC.Enum
  , module GHC.Float
  , module GHC.Num
  , module GHC.Real
  , module GHC.TypeNats
  , module Text.Show

  , (>$>)
  ) where

import Control.Applicative ((*>), pure)
import Control.Monad (Monad, (=<<), (>>=), (>=>), MonadFail, fail, when)
import Data.Bifunctor (first)
import Data.Bits (Bits, shiftL, shiftR, rotateR, (.|.), (.&.))
import Data.Bool (Bool(True,False), (&&), (||), not, otherwise, bool)
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Default (Default, def)
import Data.Either (Either(Left,Right), either)
import Data.Eq (Eq, (/=), (==))
import Data.Function ((.), ($), (&), id, flip, const)
import Data.Functor ((<$>), ($>))
import Data.Int (Int32, Int64, Int)
import Data.Kind (Type, Constraint)
import Data.Monoid (mempty, (<>))
import Data.Ord (Ord, (<=), (>=), (>), (<))
import Data.String (String, IsString(fromString))
import Data.Tuple (fst, snd, uncurry)
import Data.Vector (Vector)
import Data.Word (Word64, Word32, Word)
import GHC.Enum (Enum, toEnum, fromEnum, pred)
import GHC.Float (Double, Float)
import GHC.Num (Num, (+), (-))
import GHC.Real (Real, Integral)
import GHC.TypeNats (Nat)
import Text.Show (Show(show))

(>$>) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(>$>) f g = f >=> pure . g

