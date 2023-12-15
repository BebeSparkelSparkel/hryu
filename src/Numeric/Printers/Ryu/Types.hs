{-# LANGUAGE UndecidableInstances #-}
module Numeric.Printers.Ryu.Types
  --( CompatableWord
  --, CompatableInt
  --, NumberOfBits
  ( Sign
  --, Exponent(..)
  --, Mantissa(..)
  --, maskRight
  --, max
  --, NumBits(..)
  --, BoolResolve(..)
  --, Bias(..)
  , RyuNormals(..)
  , ClassifyType(..)
  , SpecialValue(..)
  ) where

--import Numeric (showHex)

-- * Compatable types that can hold the bits of the floating point number

--type CompatableWord :: Type -> Type
--type CompatableWord a = BitsToWord (NumberOfBits a)
--type BitsToWord :: Nat -> Type
--type family BitsToWord a where
--  BitsToWord 32 = Word32
--  BitsToWord 64 = Word64
--
--type CompatableInt :: Type -> Type
--type CompatableInt a = BitsToInt (NumberOfBits a)
--type BitsToInt :: Nat -> Type
--type family BitsToInt a where
--  BitsToInt 32 = Int32
--  BitsToInt 64 = Int64
--
--type NumberOfBits :: Type -> Nat
--type family NumberOfBits a
--type instance NumberOfBits Float = 32
--type instance NumberOfBits Double = 64

-- * Newtypes for differentiateing the floating point bit ranges

type Sign = Bool

--newtype Exponent a = Exponent {unExponent :: CompatableWord a}
--instance Integral (CompatableWord a) => Show (Exponent a) where show = ("Exponent " <>) . ($ mempty) . showHex
--deriving instance Bits     (CompatableWord a) => Bits     (Exponent a)
--deriving instance Num      (CompatableWord a) => Num      (Exponent a)
--deriving instance Eq       (CompatableWord a) => Eq       (Exponent a)
--deriving instance Ord      (CompatableWord a) => Ord      (Exponent a)
--deriving instance Real     (CompatableWord a) => Real     (Exponent a)
--deriving instance Integral (CompatableWord a) => Integral (Exponent a)
--deriving instance Enum     (CompatableWord a) => Enum     (Exponent a)
--
--newtype Mantissa a = Mantissa {unMantissa :: CompatableWord a}
--instance Integral (CompatableWord a) => Show (Mantissa a) where show = ("Mantissa " <>) . ($ mempty) . showHex
--deriving instance Bits     (CompatableWord a) => Bits     (Mantissa a)
--deriving instance Num      (CompatableWord a) => Num      (Mantissa a)
--deriving instance Eq       (CompatableWord a) => Eq       (Mantissa a)
--deriving instance Ord      (CompatableWord a) => Ord      (Mantissa a)
--deriving instance Real     (CompatableWord a) => Real     (Mantissa a)
--deriving instance Integral (CompatableWord a) => Integral (Mantissa a)
--deriving instance Enum     (CompatableWord a) => Enum     (Mantissa a)

-- * Bits for interacting the the different floating point ranges

---- | Mask for the range with the ones on the right
--maskRight :: forall a. (NumBits a, Num a, Bits a) => a
--maskRight = 1 `shiftL` numBits @a - 1
--
---- | Maximum value
--max :: forall a. (NumBits a, Num a, Bits a) => a
--max = maskRight

---- | Number of bits in the range
--class NumBits a where numBits :: Int
--instance NumBits (Exponent Double) where numBits = 11
--instance NumBits (Mantissa Double) where numBits = 52

-- * Type Bool

--type BoolResolve :: Bool -> Constraint
--class BoolResolve a where boolResolve :: Bool
--instance BoolResolve 'True  where boolResolve = True
--instance BoolResolve 'False where boolResolve = False

-- * Exponent Bias

--class Bias a where bias :: Int
--instance Bias Double where bias = 1023

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
