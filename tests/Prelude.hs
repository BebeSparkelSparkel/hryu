module Prelude
  ( module Control.Monad
  , module Data.Bool
  , module Data.Char
  , module Data.Eq
  , module Data.Function
  , module Data.List
  , module Data.Semigroup
  , module Data.String
  , module GHC.Float
  , module GHC.Num
  , module Numeric.Printers.Ryu
  , module Numeric.Printers.Ryu.Notations
  , module Numeric.Printers.Ryu.Types
  , module System.IO
  , module Test.Hspec
  , module Text.Show
  , module Text.Read
  , module Test.Hspec.QuickCheck
  , module Data.Int
  , module Data.Bits
  , module Data.Ord
  , module Data.Word
  , module GHC.Real
  , module Numeric.IEEE
  , module Unsafe.Coerce
  , module Data.MonoTraversable.Unprefixed
  , module Data.MonoTraversable
  ) where

import Control.Monad ((>>=), return, MonadFail, fail, unless)
import Data.Bits (rotateR, shiftL, (.|.))
import Data.Bool (Bool(False), otherwise)
import Data.Char (Char)
import Data.Eq (Eq, (==))
import Data.Function (($), (.), id)
import Data.Int (Int)
import Data.List ((++))
import Data.Ord ((<=))
import Data.Semigroup ((<>))
import Data.String (IsString, fromString, String)
import Data.Word (Word64, Word)
import GHC.Float (Double)
import GHC.Num ((+), (-), negate)
import GHC.Real (fromIntegral, (/))
import Numeric.IEEE (infinity, nan)
import Numeric.Printers.Ryu (ryu)
import Numeric.Printers.Ryu.Notations (Notation, ScientificNotation, DecimalNotation, ShortestOfDecimalAndScientificNotation, E(Capital))
import Numeric.Printers.Ryu.Types (Sign, ExponentWord, MantissaWord)
import System.IO (IO, FilePath)
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read (read)
import Text.Show (Show(show))
import Unsafe.Coerce (unsafeCoerce)
import Data.MonoTraversable.Unprefixed (length)
import Data.MonoTraversable (MonoFoldable)

import Foreign.C.String (CString, newCString)
import System.IO.Unsafe (unsafePerformIO)

instance IsString CString where fromString = unsafePerformIO . newCString

