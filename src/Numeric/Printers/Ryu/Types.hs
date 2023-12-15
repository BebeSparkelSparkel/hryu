{-# LANGUAGE UndecidableInstances #-}
module Numeric.Printers.Ryu.Types
  ( Sign
  , RyuNormals(..)
  , ClassifyType(..)
  , SpecialValue(..)
  ) where

type Sign = Bool

class RyuNormals notation a text where
  type ExponentWord a :: Type
  type MantissaWord a :: Type
  ryuNormals :: MonadFail m => a -> m text
  ryuNormalSubnormal :: Sign -> ExponentWord a -> MantissaWord a -> text

class ClassifyType a where classifyType :: a -> Either SpecialValue (Sign, ExponentWord a, MantissaWord a)

data SpecialValue
  = PositiveInfinity
  | NegativeInfinity
  | PositiveZero
  | NegativeZero
  | NotANumber
  deriving Show

