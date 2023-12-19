module Numeric.Printers.Ryu.NonNormal
  ( NonNormalReturns(..)
  ) where

import Numeric.Printers.Ryu.Notations (ScientificNotation, DecimalNotation, ShortestOfDecimalAndScientificNotation)

data NonNormalReturns notation text = NonNormalReturns
  { negativeInfinity :: text
  , positiveInfinity :: text
  , notANumber       :: text
  , negativeZero     :: text
  , positiveZero     :: text
  } deriving Show

instance IsString text => Default (NonNormalReturns ScientificNotation text) where
  def = NonNormalReturns
    { negativeInfinity = "-Infinity"
    , positiveInfinity = "Infinity"
    , notANumber       = "NaN"
    , negativeZero     = "-0E0"
    , positiveZero     = "0E0"
    }

instance IsString text => Default (NonNormalReturns DecimalNotation text) where
  def = NonNormalReturns
    { negativeInfinity = "-Infinity"
    , positiveInfinity = "Infinity"
    , notANumber       = "NaN"
    , negativeZero     = "-0"
    , positiveZero     = "0"
    }

instance IsString text => Default (NonNormalReturns ShortestOfDecimalAndScientificNotation text) where
  def = NonNormalReturns
    { negativeInfinity = "-Inf"
    , positiveInfinity = "Inf"
    , notANumber       = "NaN"
    , negativeZero     = "-0"
    , positiveZero     = "0"
    }

