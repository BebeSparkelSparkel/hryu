{-# LANGUAGE UndecidableInstances #-}
module Numeric.Printers.Ryu.Types
  ( Sign
  , RyuNormals(..)
  , ClassifyType(..)
  , SpecialValue(..)
  ) where

type Sign = Bool

class RyuNormals a text notation where
  type ExponentWord a :: Type
  type MantissaWord a :: Type
  ryuNormals :: MonadFail m => notation -> a -> m text
  ryuNormalSubnormal :: notation -> Sign -> ExponentWord a -> MantissaWord a -> text

class ClassifyType a where classifyType :: a -> Either SpecialValue (Sign, ExponentWord a, MantissaWord a)

data SpecialValue
  = PositiveInfinity
  | NegativeInfinity
  | PositiveZero
  | NegativeZero
  | NotANumber
  deriving Show

