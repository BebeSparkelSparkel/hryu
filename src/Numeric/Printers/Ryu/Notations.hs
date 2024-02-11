module Numeric.Printers.Ryu.Notations
  ( Notation(..)
  , ScientificNotation(..)
  , E(..)
  , EChar(..)
  , DecimalNotation(..)
  , ShortestOfDecimalAndScientificNotation
  ) where

import Numeric.Printers.Ryu.Types (Sign, MantissaWord)

class Notation a text notation where notation :: notation -> Sign -> MantissaWord a -> Int -> text

type ScientificNotation :: E -> Type
data ScientificNotation e = ScientificNotation
instance Default (ScientificNotation e) where def = ScientificNotation

data E = Capital | Lower

type EChar :: E -> Constraint
class EChar e where e :: IsChar char => char
instance EChar 'Capital where e = fromChar 'E'
instance EChar 'Lower where e = fromChar 'e'

data DecimalNotation = DecimalNotation
instance Default DecimalNotation where def = DecimalNotation

type ShortestOfDecimalAndScientificNotation :: E -> Type
data ShortestOfDecimalAndScientificNotation e = ShortestOfDecimalAndScientificNotation
instance Default (ShortestOfDecimalAndScientificNotation e) where def = ShortestOfDecimalAndScientificNotation

