{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
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
  , module Data.Maybe
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
  , module Control.Monad.ST
  , module Text.Printf
  , module System.IO
  , module System.IO.Unsafe

  , (>$>)
  ) where

import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)
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
import Data.Maybe (Maybe(Nothing,Just), fromMaybe)
import Data.Monoid (mempty, (<>))
import Data.Ord (Ord, (<=), (>=), (>), (<))
import Data.String (String, IsString(fromString))
import Data.Tuple (fst, snd, uncurry)
import Data.Vector (Vector)
import Data.Word (Word64, Word32, Word16, Word8, Word)
import GHC.Enum (Enum, toEnum, fromEnum, pred, succ)
import GHC.Float (Double, Float)
import GHC.Num (Num, (+), (-), (*), negate, subtract)
import GHC.Real (Real, Integral, mod, div, fromIntegral)
import GHC.TypeNats (Nat)
import Text.Show (Show(show))
import Control.Monad.ST (runST, ST)
import Text.Printf (IsChar, fromChar, toChar)

import Foreign.C.String (castCharToCChar)
import Foreign.C.Types (CChar)
import Data.ByteString.Internal (c2w)

(>$>) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(>$>) f g = f >=> pure . g

instance IsChar CChar where
  --toChar = castCCharToChar
  fromChar = castCharToCChar

instance IsChar Word8 where
  fromChar = c2w

instance IsChar Word16 where
  fromChar = fromIntegral . c2w

