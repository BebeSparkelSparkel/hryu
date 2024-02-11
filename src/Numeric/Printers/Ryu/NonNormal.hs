module Numeric.Printers.Ryu.NonNormal
  ( NonNormalReturns(..)
  , NonNormals(..)
  ) where

import Numeric.Printers.Ryu.Notations (ScientificNotation, DecimalNotation, ShortestOfDecimalAndScientificNotation, EChar, e)

data NonNormalReturns text = NonNormalReturns
  { negativeInfinity :: text
  , positiveInfinity :: text
  , notANumber       :: text
  , negativeZero     :: text
  , positiveZero     :: text
  } deriving Show

class NonNormals notation text where nonNormals :: notation -> NonNormalReturns text

instance (IsString text, EChar e) => NonNormals (ScientificNotation e) text where
  nonNormals = const NonNormalReturns
    { negativeInfinity = "-Infinity"
    , positiveInfinity = "Infinity"
    , notANumber       = "NaN"
    , negativeZero     = fromString $ "-0" <> (e @e : "0")
    , positiveZero     = fromString $ '0' : e @e : "0"
    }

instance IsString text => NonNormals DecimalNotation text where
  nonNormals = const NonNormalReturns
    { negativeInfinity = "-Infinity"
    , positiveInfinity = "Infinity"
    , notANumber       = "NaN"
    , negativeZero     = "-0"
    , positiveZero     = "0"
    }

instance IsString text => NonNormals (ShortestOfDecimalAndScientificNotation e) text where
  nonNormals = const NonNormalReturns
    { negativeInfinity = "-Inf"
    , positiveInfinity = "Inf"
    , notANumber       = "NaN"
    , negativeZero     = "-0"
    , positiveZero     = "0"
    }

