module Numeric.Printers.Ryu.Notations
  ( Notation(..)
  , ScientificNotation
  , DecimalNotation
  , ShortestOfDecimalAndScientificNotation
  ) where

import Numeric.Printers.Ryu.Types (Sign, MantissaWord)

class Notation notation a text where notation :: Sign -> MantissaWord a -> Int -> text

data ScientificNotation

data DecimalNotation

data ShortestOfDecimalAndScientificNotation
