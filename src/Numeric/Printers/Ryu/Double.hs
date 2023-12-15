{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Numeric.Printers.Ryu.Double () where

import Data.MonoMutableIndexable (MutableIndexable, Element, Index, allocate, writeIndex)
import Data.Tuple.Extra (first3, second3, third3)
import Data.Vector qualified as V
import Data.WideWord.Word128 (Word128(Word128), word128Lo64)
import GHC.Err (undefined)
import GHC.Float (isNegativeZero, isInfinite, isNaN)
import GHC.Real (fromIntegral)
import Numeric.Printers.Ryu.Double2StringFullTable (doublePow5InvSplit, doublePow5Split)
import Numeric.Printers.Ryu.MutableConstructor (MutableCollection, fromMutable)
import Numeric.Printers.Ryu.NonNormal ()
import Numeric.Printers.Ryu.Notations (Notation, notation, ScientificNotation)
import Numeric.Printers.Ryu.Types (RyuNormals, ExponentWord, MantissaWord, ryuNormals, ryuNormalSubnormal, ClassifyType, classifyType, Sign, SpecialValue(NegativeZero,PositiveZero,PositiveInfinity,NegativeInfinity,NotANumber))
import Text.Printf (IsChar(fromChar))
import Unsafe.Coerce (unsafeCoerce)

instance Notation notation Double text => RyuNormals notation Double text where
  type ExponentWord Double = Word32
  type MantissaWord Double = Word64
  ryuNormals f = case classifyType f of
    Right (s,e,m) -> pure $ ryuNormalSubnormal @notation @Double s e m
    _ -> fail $ "Expected a normal or subnormal number but received: " <> show f
  ryuNormalSubnormal sign ieeeExponent ieeeMantissa = uncurry (notation @notation @Double sign) $ fromMaybe
    (d2d ieeeExponent ieeeMantissa)
    (d2d_small_int ieeeExponent ieeeMantissa)

instance ClassifyType Double where
  classifyType f = if ieeeExponent == maskExponentRight || (ieeeExponent == 0 && ieeeMantissa == 0)
  then Left if
    | isNegativeZero f -> NegativeZero
    | f == 0 -> PositiveZero
    | isInfinite f -> if f > 0 then PositiveInfinity else NegativeInfinity
    | isNaN f -> NotANumber
    | True -> undefined
  else Right (ieeeSign, ieeeExponent, ieeeMantissa)
    where
    ieeeSign = extractSign f
    ieeeExponent = extractExponent f
    ieeeMantissa = extractMantissa f

extractSign :: Double -> Sign
extractSign = (/= 0) . (negZero .&.) . toWord

extractExponent :: Double -> Word32
extractExponent = fromIntegral . (.&. maskExponentRight) . (`shiftR` numMantissaBits) . toWord @Word64

extractMantissa :: Double -> Word64
extractMantissa = (.&. maskMantissaRight) . toWord

d2d :: Exponent -> Mantissa -> (Word64, Int)
d2d ieeeExponent ieeeMantissa = (output, exp)
  where
  exp = e10 + removed
  (removed, output) = if vmIsTrailingZeros || vrIsTrailingZeros
    then generalLoopsUncommon vr vp vm vmIsTrailingZeros vrIsTrailingZeros 0 0 acceptBounds
    else specializedLoopsCommon vr vp vm False 0
  (e10, vr, vp, vm, vmIsTrailingZeros, vrIsTrailingZeros) = if e2 >= 0
    then let
      q = bool id pred (e2 > 3) . fromIntegral $ log10Pow2 e2 :: Int
      e10 = q :: Int
      k = pow5InvBitCount + pow5bits q - 1 :: Int
      i = negate e2 + q + k :: Int
      (vp', vm, vr) = mulShiftAll64 m2 (doublePow5InvSplit V.! q) i mmShift
      mvMod5 = mv - 5 * (mv `div` 5)
      (vp, vmIsTrailingZeros, vrIsTrailingZeros) = (vp', False, False) &
        if q <= 21
        then if mvMod5 == 0
          then third3 $ const $ multipleOfPowerOf5 mv q
          else if acceptBounds
          then second3 $ const $ multipleOfPowerOf5 (mv - 1 - mmShift) q
          else bool id (first3 pred) $ multipleOfPowerOf5 (mv + 2) q
        else id
      in (e10, vr, vp, vm, vmIsTrailingZeros, vrIsTrailingZeros)
    else let
      ne2 = negate e2
      q = bool id pred (ne2 > 0) . fromIntegral $ log10Pow5 ne2 :: Int
      e10 = q + e2 :: Int
      i = ne2 - q :: Int
      k = pow5bits i - pow5BitCount :: Int
      j = q - k :: Int
      (vp', vm, vr) = mulShiftAll64 m2 (doublePow5Split V.! i) j mmShift
      (vp, vmIsTrailingZeros, vrIsTrailingZeros) = (vp', False, False) &
        if q <= 1
        then third3 (const True) . bool (first3 pred) (second3 $ const $ mmShift == 1) acceptBounds
        else if q < 63
        then third3 $ const $ multipleOfPowerOf2 mv q
        else id
      in (e10, vr, vp, vm, vmIsTrailingZeros, vrIsTrailingZeros)
  mv = 4 * m2
  mmShift = bool 0 1 $ ieeeMantissa /= 0 || ieeeExponent <= 1
  acceptBounds = even
  even = m2 .&. 1 == 0
  (e2 :: Int, m2) = first (subtract $ bias + numMantissaBits + 2) case ieeeExponent of
    0 -> (1, ieeeMantissa)
    _ -> ( fromIntegral ieeeExponent
         , 1 `shiftL` numMantissaBits .|. ieeeMantissa )
  generalLoopsUncommon :: VR -> VP -> VM -> VmIsTrailingZeros -> VrIsTrailingZeros -> Removed -> LastRemovedDigit -> AcceptBounds -> (Removed, Output)
  generalLoopsUncommon vr vp vm vmIsTrailingZeros vrIsTrailingZeros removed lastRemovedDigit
    | vpDiv10 <= vmDiv10 = if vmIsTrailingZeros
      then generalLoopsUncommon' vr vp vm vrIsTrailingZeros removed lastRemovedDigit vmIsTrailingZeros
      else generalLoopsUncommonOutput vr vm lastRemovedDigit vrIsTrailingZeros removed vmIsTrailingZeros
    | otherwise = generalLoopsUncommon 
      vrDiv10 -- vr
      vpDiv10 -- vp
      vmDiv10 -- vm
      (vmIsTrailingZeros && vmMod10 == 0) -- vmIsTrailingZeros
      (vrIsTrailingZeros && lastRemovedDigit == 0) -- vrIsTrailingZeros
      (succ removed)-- removed
      (fromIntegral vrMod10) -- lastRemovedDigit
    where
    vmDiv10 = vm `div` 10
    vmMod10 = vm `mod` 10
    vpDiv10 = vp `div` 10
    vrDiv10 = vr `div` 10
    vrMod10 = vr - 10 * vrDiv10
  generalLoopsUncommon' :: VR -> VP -> VM -> VrIsTrailingZeros -> Removed -> LastRemovedDigit -> VmIsTrailingZeros -> AcceptBounds -> (Removed, Output)
  generalLoopsUncommon' vr vp vm vrIsTrailingZeros removed lastRemovedDigit
    | vmMod10 /= 0 = generalLoopsUncommonOutput vr vm lastRemovedDigit vrIsTrailingZeros removed
    | otherwise = generalLoopsUncommon'
      vrDiv10 -- vr
      vpDiv10 -- vp
      vmDiv10 -- vm
      (vrIsTrailingZeros && lastRemovedDigit == 0) -- vrIsTrailingZeros
      (succ removed) -- removed
      (fromIntegral vrMod10) -- lastRemovedDigit
    where
    vmDiv10 = vm `div` 10
    vmMod10 = vm - 10 * vmDiv10
    vpDiv10 = vp `div` 10
    vrDiv10 = vr `div` 10
    vrMod10 = vr - 10 * vrDiv10
  generalLoopsUncommonOutput :: VR -> VM -> LastRemovedDigit -> VrIsTrailingZeros -> Removed -> VmIsTrailingZeros -> AcceptBounds -> (Removed, Output)
  generalLoopsUncommonOutput vr vm lastRemovedDigit vrIsTrailingZeros removed vmIsTrailingZeros acceptBounds =
    (removed, vr + if (vr == vm && (not acceptBounds || not vmIsTrailingZeros)) || lrd >= 5 then 1 else 0)
    where
    lrd = if vrIsTrailingZeros && lastRemovedDigit == 5 && vr `mod` 2 == 0
      then 4
      else lastRemovedDigit
  specializedLoopsCommon :: VR -> VP -> VM -> RoundUp -> Removed -> (Removed, Output)
  specializedLoopsCommon vr vp vm roundUp removed
    | vpDiv100 > vmDiv100 = specializedLoopsCommon'
      vrDiv100 -- vr
      vpDiv100-- vp
      vmDiv100 -- vm
      (vrMod100 >= 50) -- roundUp
      (removed + 2) -- removed
    | otherwise = specializedLoopsCommon' vr vp vm roundUp removed
    where
    vpDiv100 = vp `div` 100
    vmDiv100 = vm `div` 100
    vrDiv100 = vr `div` 100
    vrMod100 = vr `mod` 100
  specializedLoopsCommon' :: VR -> VP -> VM -> RoundUp -> Removed -> (Removed, Output)
  specializedLoopsCommon' vr vp vm roundUp removed
    | vpDiv10 <= vmDiv10 = (removed, vr & bool id succ (vr == vm || roundUp))
    | otherwise = specializedLoopsCommon'
      vrDiv10 -- vr
      vpDiv10 -- vp
      vmDiv10 -- vm
      (vrMod10 >= 5) -- roundUp
      (succ removed) -- removed
    where
    vrDiv10 = vr `div` 10
    vrMod10 = vr `mod` 10
    vpDiv10 = vp `div` 10
    vmDiv10 = vm `div` 10
type Exponent = Word32
type Mantissa = Word64
type VR = Word64
type VP = Word64
type VM = Word64
type VmIsTrailingZeros = Bool
type VrIsTrailingZeros = Bool
type Removed = Int
type RoundUp = Bool
type LastRemovedDigit = Word
type AcceptBounds = Bool
type Output = Word64

d2d_small_int :: Exponent -> Mantissa -> Maybe (Word64, Int)
d2d_small_int ieeeExponent ieeeMantissa =
  if e2 > 0 || e2 < (-52) || fraction /= 0
  then Nothing
  else Just $ loop (m2 `shiftR` negate e2) 0
  where
  m2 = (1 `shiftL` numMantissaBits) .|. ieeeMantissa
  e2 = fromIntegral ieeeExponent - bias - numMantissaBits
  mask = 1 `shiftL` negate e2 - 1
  fraction = m2 .&. mask
  loop :: Mantissa -> Int -> (Word64, Int)
  loop m e = if r /= 0
    then (m, e)
    else loop q (succ e)
    where
    q = m `div` 10
    r = fromIntegral m - 10 * fromIntegral q :: Word32

-- * Helpers

negZero :: Word64
negZero = 1 `rotateR` 1

toWord :: Double -> w
toWord = unsafeCoerce

numExponentBits :: Int
numExponentBits = 11

maskExponentRight :: (Bits w, Num w) => w
maskExponentRight = makeMaskRight numExponentBits

numMantissaBits :: Int
numMantissaBits = 52

maskMantissaRight :: Word64
maskMantissaRight = makeMaskRight numMantissaBits

makeMaskRight :: (Bits w, Num w) => Int -> w
makeMaskRight = subtract 1 . shiftL 1

-- * Notation Printers

-- String could be optimized to not use the mutable container but rather function composition
instance Notation ScientificNotation Double String where
  notation s m e = runST $ uncurry fromMutable =<< notationScientificMutable @String s m e

-- Need to implement
--instance Notation ScientificNotation Double Data.ByteString.ByteString where
--instance Notation ScientificNotation Double Data.ByteString.Lazy.ByteString where
--instance Notation ScientificNotation Double Data.Text.Text where
--instance Notation ScientificNotation Double Data.Text.Lazy.Text where
--instance Notation ScientificNotation Double Foreign.C.CString where

notationScientificMutable :: forall f i char c s.
  ( c ~ MutableCollection f s
  , i ~ Index c
  , Num i
  , Enum i
  , MutableIndexable c (ST s)
  , IsChar char
  , Enum char
  , char ~ Element c
  )
  => Sign
  -> Word64
  -> Int
  -> ST s (i, MutableCollection f s)
notationScientificMutable sign output exponent =
  (allocate 25 :: ST s c) >>= \(result :: c)  -> let
  writeSign :: ST s i
  writeSign = if sign
    then wi 0 (fromChar '-') $> 1
    else pure 0
  -- We prefer 32-bit operations, even on 64-bit platforms.
  -- We have at most 17 digits, and uint32_t can store 9 digits.
  -- If output doesn't fit into uint32_t, we cut off 8 digits,
  -- so the rest will fit into uint32_t.
  writeDecimalDigits :: i -> ST s ()
  writeDecimalDigits index = cutOff8Digits >>= whileGT10000 >>= whenLT10000
    where
    cutOff8Digits :: ST s (i, Word32)
    cutOff8Digits = if (output `shiftR` 32 /= 0)
      then do
        copy2Chars (i' - 1) c0
        copy2Chars (i' - 3) c1
        copy2Chars (i' - 5) d0
        copy2Chars (i' - 7) d1
        let output2 = fromIntegral q
        pure (8, output2)
      else let output2 = fromIntegral output in pure (0, output2)
      where
      q = output `div` 100000000 :: Word64
      output2 = fromIntegral output - 100000000 * fromIntegral q :: Word32
      c = output2 `mod` 10000
      d = (output2 `div` 10000) `mod` 10000
      c0 = (c `mod` 100) `shiftL` 1
      c1 = (c `div` 100) `shiftL` 1
      d0 = (d `mod` 100) `shiftL` 1
      d1 = (d `div` 100) `shiftL` 1
    whileGT10000 :: (i, Word32) -> ST s (i, Word32)
    whileGT10000 (i, output2) = if output2 >= 10000
      then do
        copy2Chars (i' - i - 1) c0
        copy2Chars (i' - i - 3) c1
        whileGT10000 (i + 4, output2 `div` 10000)
      else pure (i, output2)
      where
      c = output2 `mod` 10000
      c0 = (c `mod` 100) `shiftL` 1
      c1 = (c `div` 100) `shiftL` 1
    whenLT10000 :: (i, Word32) -> ST s ()
    whenLT10000
      =   (\t@(i,output2) -> if output2 >= 100
            then copy2Chars (i' - i - 1) ((output2 `mod` 100) `shiftL` 1)
                  $> (i + 2, output2 `div` 100)
            else pure t )
      >=> \(i,output2) -> if output2 >= 10
            then do
              let c = output2 `shiftL` 1
              wi (i' - i) $ digitTable V.! fromIntegral (c + 1)
              wi index $ digitTable V.! fromIntegral c
            else wi index $ addToZeroChar output2
    i' = index + fromIntegral olength
  writeDecimalPoint :: i -> ST s i
  writeDecimalPoint index =
    if (olength > 1)
    then do
      wi (succ index) (fromChar '.')
      pure $ index + fromIntegral olength + 1
    else pure $ succ index
  writeExponent :: i -> ST s i
  writeExponent
    =   (\index -> wi index (fromChar 'E') $> succ index)
    >=> (\index -> let exp = exponent + olength - 1 in if exp < 0
          then wi index (fromChar '-') $> (index + 1, negate exp)
          else pure (index, exp) )
    >=> \(index, exp) -> 
      if exp >= 100
      then do
        let c = exp `mod` 10
        copy2Chars index (2 * (exp `div` 10))
        wi (index + 2) $ addToZeroChar c
        pure $ index + 3
      else if exp >= 10
      then do
        copy2Chars index (2 * exp)
        pure $ index + 2
      else do
        wi index $ addToZeroChar exp
        pure $ succ index
  wi :: i -> char -> ST s ()
  wi = writeIndex result
  -- used inplace of memcpy
  copy2Chars :: Integral n => i -> n -> ST s ()
  copy2Chars i (fromIntegral -> i') = do
    wi i $ digitTable V.! i'
    wi (succ i) $ digitTable V.! succ i'
  in writeSign >>= \index -> writeDecimalDigits index *> writeDecimalPoint index >>= writeExponent >$> (, result)
  where
  addToZeroChar :: Integral n => n -> char
  addToZeroChar = toEnum . (fromEnum (fromChar '0' :: char) +) . fromIntegral
  olength = decimalLength17 output

-- The average output length is 16.38 digits, so we check high-to-low.
-- Function precondition: v is not an 18, 19, or 20-digit number.
-- 17 digits are sufficient for round-tripping.
decimalLength17 :: (Ord a, Num a) => a -> Int
decimalLength17 v
  | v >= 10000000000000000 = 17
  | v >= 1000000000000000 = 16
  | v >= 100000000000000 = 15
  | v >= 10000000000000 = 14
  | v >= 1000000000000 = 13
  | v >= 100000000000 = 12
  | v >= 10000000000 = 11
  | v >= 1000000000 = 10
  | v >= 100000000 = 9
  | v >= 10000000 = 8
  | v >= 1000000 = 7
  | v >= 100000 = 6
  | v >= 10000 = 5
  | v >= 1000 = 4
  | v >= 100 = 3
  | v >= 10 = 2
  | otherwise = 1

-- * Helper functions

class To128 a where to128 :: a -> Word128
instance To128 Word64 where to128 = Word128 0

-- Using the non space optimized verstion with Word128
mulShiftAll64 :: Word64 -> (Word64, Word64) -> Int -> Word64 -> (VP,VM,VR)
mulShiftAll64 m mul j mmShift = (vp, vm, vr)
  where
  vp = mulShift64 $ m4 + 2
  vm = mulShift64 $ m4 - 1 - mmShift
  vr = mulShift64 $ m4
  m4 = 4 * coerce m
  --mulShift64 :: Word64 -> Word64
  mulShift64 m' = word128Lo64 $ ((b0 `shiftR` 64) + b2) `shiftR` (j - 64)
    where
    b0 = m128 * (to128 . fst) mul
    b2 = m128 * (to128 . snd) mul
    m128 = to128 m'

-- Returns true if value is divisible by 5^p.
-- I tried a case distinction on p, but there was no performance difference.
multipleOfPowerOf5 :: Word64 -> Int -> Bool
multipleOfPowerOf5 value p = pow5Factor 0 value >= p
  where
  pow5Factor count value
    | v > n_div_5 = count
    | otherwise = pow5Factor (succ count) v
    where
    v = value * m_inv_5
  m_inv_5 = 14757395258967641293 :: Word64 -- 5 * m_inv_5 = 1 (mod 2^64)
  n_div_5 = 3689348814741910323 :: Word64  -- #{ n | n = 0 (mod 2^64) } = 2^64 / 5

-- Returns true if value is divisible by 2^p.
multipleOfPowerOf2 :: Word64 -> Int -> Bool
multipleOfPowerOf2 value p = (value .&. ((1 `shiftL` p) - 1)) == 0

pow5BitCount :: Int
pow5BitCount = 125

pow5InvBitCount :: Int
pow5InvBitCount = 125

-- Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 32768.
pow5bits :: Int -> Int
pow5bits = (+ 1) . (`shiftR` 46) . (* 163391164108059)

-- Returns floor(log_10(2^e)); requires 0 <= e <= 1650.
-- The first value this approximation fails for is 2^1651 which is just greater than 10^297.
log10Pow2 :: Int -> Word
log10Pow2 = (`shiftR` 18) . (* 78913) . fromIntegral

-- Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
-- The first value this approximation fails for is 5^2621 which is just greater than 10^1832.
log10Pow5 :: Int -> Word
log10Pow5 = (`shiftR` 20) . (* 732923) . fromIntegral

-- A table of all two-digit numbers. This is used to speed up decimal digit
-- generation by copying pairs of digits into the final output.
digitTable :: IsChar char => Vector char
digitTable = V.fromList $ fromChar <$>
  ['0','0','0','1','0','2','0','3','0','4','0','5','0','6','0','7','0','8','0','9'
  ,'1','0','1','1','1','2','1','3','1','4','1','5','1','6','1','7','1','8','1','9'
  ,'2','0','2','1','2','2','2','3','2','4','2','5','2','6','2','7','2','8','2','9'
  ,'3','0','3','1','3','2','3','3','3','4','3','5','3','6','3','7','3','8','3','9'
  ,'4','0','4','1','4','2','4','3','4','4','4','5','4','6','4','7','4','8','4','9'
  ,'5','0','5','1','5','2','5','3','5','4','5','5','5','6','5','7','5','8','5','9'
  ,'6','0','6','1','6','2','6','3','6','4','6','5','6','6','6','7','6','8','6','9'
  ,'7','0','7','1','7','2','7','3','7','4','7','5','7','6','7','7','7','8','7','9'
  ,'8','0','8','1','8','2','8','3','8','4','8','5','8','6','8','7','8','8','8','9'
  ,'9','0','9','1','9','2','9','3','9','4','9','5','9','6','9','7','9','8','9','9'
  ]

bias :: Int
bias = 1023

