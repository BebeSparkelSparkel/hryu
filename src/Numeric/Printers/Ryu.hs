module Numeric.Printers.Ryu
  ( ryu
  , ryuTotal
  , RyuNormals
  , ClassifyType
  , module Numeric.Printers.Ryu.Notations
  ) where

import Numeric.Printers.Ryu.Double ()
import Numeric.Printers.Ryu.NonNormal (NonNormalReturns(NonNormalReturns), positiveInfinity, negativeInfinity, positiveZero, negativeZero, notANumber, NonNormals, nonNormals)
import Numeric.Printers.Ryu.Types (RyuNormals, ryuSEM, ClassifyType, classifyType, SpecialValue(NegativeZero,PositiveZero,PositiveInfinity,NegativeInfinity,NotANumber))
import Numeric.Printers.Ryu.Notations (Notation, ScientificNotation, DecimalNotation, ShortestOfDecimalAndScientificNotation, EChar, E(Capital,Lower))


ryu :: forall notation a text.
  ( RyuNormals a text notation
  , ClassifyType a
  , Default notation
  , NonNormals notation text
  ) => a -> text
ryu = ryuTotal @notation def (nonNormals @notation def)

ryuTotal :: forall notation a text.
  ( RyuNormals a text notation
  , ClassifyType a
  ) => notation -> NonNormalReturns text -> a -> text
ryuTotal n (NonNormalReturns {..})
  = either
    (\case
      PositiveInfinity -> positiveInfinity
      NegativeInfinity -> negativeInfinity
      PositiveZero -> positiveZero
      NegativeZero -> negativeZero
      NotANumber -> notANumber
      )
    (\(s,e,m) -> ryuSEM @a n s e m)
  . classifyType

