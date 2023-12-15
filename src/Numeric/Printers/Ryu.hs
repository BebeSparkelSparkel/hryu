module Numeric.Printers.Ryu
  ( ryu
  , ryuTotal
  ) where

import Numeric.Printers.Ryu.Double ()
import Numeric.Printers.Ryu.NonNormal (NonNormalReturns(NonNormalReturns), positiveInfinity, negativeInfinity, positiveZero, negativeZero, notANumber)
import Numeric.Printers.Ryu.Types (RyuNormals, ryuNormalSubnormal, ClassifyType, classifyType, SpecialValue(NegativeZero,PositiveZero,PositiveInfinity,NegativeInfinity,NotANumber))

ryu :: forall notation a text.
  ( RyuNormals notation a text
  , ClassifyType a
  , Default (NonNormalReturns notation text)
  ) => a -> text
ryu = ryuTotal @notation def

ryuTotal :: forall notation a text.
  ( RyuNormals notation a text
  , ClassifyType a
  ) => NonNormalReturns notation text -> a -> text
ryuTotal (NonNormalReturns {..})
  = either
    (\case
      PositiveInfinity -> positiveInfinity
      NegativeInfinity -> negativeInfinity
      PositiveZero -> positiveZero
      NegativeZero -> negativeZero
      NotANumber -> notANumber
      )
    (\(s,e,m) -> ryuNormalSubnormal @notation @a s e m)
  . classifyType

