-- Copyright 2018 Ulf Adams
--
-- The contents of this file may be used under the terms of the Apache License,
-- Version 2.0.
--
--    (See accompanying file LICENSE-Apache or copy at
--     http:--www.apache.org/licenses/LICENSE-2.0)
--
-- Alternatively, the contents of this file may be used under the terms of
-- the Boost Software License, Version 1.0.
--    (See accompanying file LICENSE-Boost or copy at
--     https:--www.boost.org/LICENSE_1_0.txt)
--
-- Unless required by applicable law or agreed to in writing, this software
-- is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied.

module Numeric.Printers.Ryu.DoubleSpec where

import Numeric.Printers.Ryu.Double ()

import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL

spec :: Spec
spec = do
  describe "ScientificNotation" do
    describe "String"          $ scientificNotationTests $ ryu @(ScientificNotation 'Capital) @Double @String
    describe "ByteString"      $ scientificNotationTests $ ryu @(ScientificNotation 'Capital) @Double @B.ByteString
    describe "Lazy-ByteString" $ scientificNotationTests $ ryu @(ScientificNotation 'Capital) @Double @BL.ByteString

  describe "DecimalNotation" do
    describe "String"          $ decimalNotationTests $ ryu @DecimalNotation @Double @String
    describe "ByteString"      $ decimalNotationTests $ ryu @DecimalNotation @Double @B.ByteString
    describe "Lazy-ByteString" $ decimalNotationTests $ ryu @DecimalNotation @Double @BL.ByteString

  describe "ShortestOfDecimalAndScientificNotation" do
    describe "String"          $ shortestOfDecimalAndScientificNotationTests $ ryu @(ShortestOfDecimalAndScientificNotation 'Capital) @Double @String
    describe "ByteString"      $ shortestOfDecimalAndScientificNotationTests $ ryu @(ShortestOfDecimalAndScientificNotation 'Capital) @Double @B.ByteString
    describe "Lazy-ByteString" $ shortestOfDecimalAndScientificNotationTests $ ryu @(ShortestOfDecimalAndScientificNotation 'Capital) @Double @BL.ByteString

scientificNotationTests :: (IsString text, Show text, Eq text) => (Double -> text) -> Spec
scientificNotationTests ryu' = do
  describe "Basic" do
    it "positive zero" $ ryu' 0.0 `shouldBe` "0E0"
    it "negative zero" $ ryu' (-0.0) `shouldBe` "-0E0"
    it "positive one" $ ryu' 1.0 `shouldBe` "1E0"
    it "negative one" $ ryu' (-1.0) `shouldBe` "-1E0"
    it "NaN" $ ryu' nan `shouldBe` "NaN"
    it "infinity" $ ryu' infinity `shouldBe` "Infinity"
    it "negative infinity" $ ryu' (negate infinity) `shouldBe` "-Infinity"

  it "SwitchToSubnormal" do
    ryu' 2.2250738585072014E-308 `shouldBe` "2.2250738585072014E-308"

  it "MinAndMax" do
    ryu' (word64Bits2Double 0x7fefffffffffffff) `shouldBe` "1.7976931348623157E308"
    ryu' (word64Bits2Double 1) `shouldBe` "5E-324"

  it "LotsOfTrailingZeros" do
    ryu' 2.98023223876953125E-8 `shouldBe` "2.9802322387695312E-8"

  it "Regression" do
    ryu' (-2.109808898695963E16) `shouldBe` "-2.109808898695963E16"
    ryu' 4.940656E-318 `shouldBe` "4.940656E-318"
    ryu' 1.18575755E-316 `shouldBe` "1.18575755E-316"
    ryu' 2.989102097996E-312 `shouldBe` "2.989102097996E-312"
    ryu' 9.0608011534336E15 `shouldBe` "9.0608011534336E15"
    ryu' 4.708356024711512E18 `shouldBe` "4.708356024711512E18"
    ryu' 9.409340012568248E18 `shouldBe` "9.409340012568248E18"
    ryu' 1.2345678 `shouldBe` "1.2345678E0"

  it "LooksLikePow5" do
    -- These numbers have a mantissa that is a multiple of the largest power of 5 that fits,
    -- and an exponent that causes the computation for q to result in 22, which is a corner
    -- case for Ryu.
    ryu' (word64Bits2Double 0x4830F0CF064DD592) `shouldBe` "5.764607523034235E39"
    ryu' (word64Bits2Double 0x4840F0CF064DD592) `shouldBe` "1.152921504606847E40"
    ryu' (word64Bits2Double 0x4850F0CF064DD592) `shouldBe` "2.305843009213694E40"

  describe "OutputLength" do
    it "no chunked" do
      ryu' 1 `shouldBe` "1E0" -- already tested in Basic
      ryu' 1.2 `shouldBe` "1.2E0"
      ryu' 1.23 `shouldBe` "1.23E0"
      ryu' 1.234 `shouldBe` "1.234E0"
      ryu' 1.2345 `shouldBe` "1.2345E0"
      ryu' 1.23456 `shouldBe` "1.23456E0"
      ryu' 1.234567 `shouldBe` "1.234567E0"
      ryu' 1.2345678 `shouldBe` "1.2345678E0" -- already tested in Regression
      ryu' 1.23456789 `shouldBe` "1.23456789E0"
      ryu' 1.234567895 `shouldBe` "1.234567895E0" -- 1.234567890 would be trimmed
      ryu' 1.2345678901 `shouldBe` "1.2345678901E0"
      ryu' 1.23456789012 `shouldBe` "1.23456789012E0"
      ryu' 1.234567890123 `shouldBe` "1.234567890123E0"
      ryu' 1.2345678901234 `shouldBe` "1.2345678901234E0"
      ryu' 1.23456789012345 `shouldBe` "1.23456789012345E0"
      ryu' 1.234567890123456 `shouldBe` "1.234567890123456E0"
      ryu' 1.2345678901234567 `shouldBe` "1.2345678901234567E0"

    it "32-bit chunking" do
      ryu' 4.294967294 `shouldBe` "4.294967294E0" -- 2^32 - 2
      ryu' 4.294967295 `shouldBe` "4.294967295E0" -- 2^32 - 1
      ryu' 4.294967296 `shouldBe` "4.294967296E0" -- 2^32
      ryu' 4.294967297 `shouldBe` "4.294967297E0" -- 2^32 + 1
      ryu' 4.294967298 `shouldBe` "4.294967298E0" -- 2^32 + 2

    ---- Test min, max shift values in shiftright128
  it "MinMaxShift" do
    let maxMantissa = (1 `shiftL` 53) - 1 :: Word64
    -- 32-bit opt-size=0:  49 <= dist <= 50
    -- 32-bit opt-size=1:  30 <= dist <= 50
    -- 64-bit opt-size=0:  50 <= dist <= 50
    -- 64-bit opt-size=1:  30 <= dist <= 50
    ieeeParts2Double False 4 0 >>= \x ->
       ryu' x `shouldBe` "1.7800590868057611E-307"
    -- 32-bit opt-size=0:  49 <= dist <= 49
    -- 32-bit opt-size=1:  28 <= dist <= 49
    -- 64-bit opt-size=0:  50 <= dist <= 50
    -- 64-bit opt-size=1:  28 <= dist <= 50
    ieeeParts2Double False 6 maxMantissa >>= \x ->
       ryu' x `shouldBe` "2.8480945388892175E-306"
    -- 32-bit opt-size=0:  52 <= dist <= 53
    -- 32-bit opt-size=1:   2 <= dist <= 53
    -- 64-bit opt-size=0:  53 <= dist <= 53
    -- 64-bit opt-size=1:   2 <= dist <= 53
    ieeeParts2Double False 41 0 >>= \x ->
       ryu' x `shouldBe` "2.446494580089078E-296"
    -- 32-bit opt-size=0:  52 <= dist <= 52
    -- 32-bit opt-size=1:   2 <= dist <= 52
    -- 64-bit opt-size=0:  53 <= dist <= 53
    -- 64-bit opt-size=1:   2 <= dist <= 53
    ieeeParts2Double False 40 maxMantissa >>= \x ->
       ryu' x `shouldBe` "4.8929891601781557E-296"
    -- 32-bit opt-size=0:  57 <= dist <= 58
    -- 32-bit opt-size=1:  57 <= dist <= 58
    -- 64-bit opt-size=0:  58 <= dist <= 58
    -- 64-bit opt-size=1:  58 <= dist <= 58
    ieeeParts2Double False 1077 0 >>= \x ->
       ryu' x `shouldBe` "1.8014398509481984E16"
    -- 32-bit opt-size=0:  57 <= dist <= 57
    -- 32-bit opt-size=1:  57 <= dist <= 57
    -- 64-bit opt-size=0:  58 <= dist <= 58
    -- 64-bit opt-size=1:  58 <= dist <= 58
    ieeeParts2Double False 1076 maxMantissa >>= \x ->
       ryu' x `shouldBe` "3.6028797018963964E16"
    -- 32-bit opt-size=0:  51 <= dist <= 52
    -- 32-bit opt-size=1:  51 <= dist <= 59
    -- 64-bit opt-size=0:  52 <= dist <= 52
    -- 64-bit opt-size=1:  52 <= dist <= 59
    ieeeParts2Double False 307 0 >>= \x ->
       ryu' x `shouldBe` "2.900835519859558E-216"
    -- 32-bit opt-size=0:  51 <= dist <= 51
    -- 32-bit opt-size=1:  51 <= dist <= 59
    -- 64-bit opt-size=0:  52 <= dist <= 52
    -- 64-bit opt-size=1:  52 <= dist <= 59
    ieeeParts2Double False 306 maxMantissa >>= \x ->
       ryu' x `shouldBe` "5.801671039719115E-216"
    -- https://github.com/ulfjack/ryu/commit/19e44d16d80236f5de25800f56d82606d1be00b9#commitcomment-30146483
    -- 32-bit opt-size=0:  49 <= dist <= 49
    -- 32-bit opt-size=1:  44 <= dist <= 49
    -- 64-bit opt-size=0:  50 <= dist <= 50
    -- 64-bit opt-size=1:  44 <= dist <= 50
    ieeeParts2Double False 934 0x000FA7161A4D6E0C >>= \x ->
       ryu' x `shouldBe` "3.196104012172126E-27"

  it "SmallIntegers" do
    ryu' 9007199254740991.0 `shouldBe` "9.007199254740991E15" -- 2^53-1
    ryu' 9007199254740992.0 `shouldBe` "9.007199254740992E15" -- 2^53

    ryu' 1.0e+0 `shouldBe` "1E0"
    ryu' 1.2e+1 `shouldBe` "1.2E1"
    ryu' 1.23e+2 `shouldBe` "1.23E2"
    ryu' 1.234e+3 `shouldBe` "1.234E3"
    ryu' 1.2345e+4 `shouldBe` "1.2345E4"
    ryu' 1.23456e+5 `shouldBe` "1.23456E5"
    ryu' 1.234567e+6 `shouldBe` "1.234567E6"
    ryu' 1.2345678e+7 `shouldBe` "1.2345678E7"
    ryu' 1.23456789e+8 `shouldBe` "1.23456789E8"
    ryu' 1.23456789e+9 `shouldBe` "1.23456789E9"
    ryu' 1.234567895e+9 `shouldBe` "1.234567895E9"
    ryu' 1.2345678901e+10 `shouldBe` "1.2345678901E10"
    ryu' 1.23456789012e+11 `shouldBe` "1.23456789012E11"
    ryu' 1.234567890123e+12 `shouldBe` "1.234567890123E12"
    ryu' 1.2345678901234e+13 `shouldBe` "1.2345678901234E13"
    ryu' 1.23456789012345e+14 `shouldBe` "1.23456789012345E14"
    ryu' 1.234567890123456e+15 `shouldBe` "1.234567890123456E15"

    -- 10^i
    ryu' 1.0e+0 `shouldBe` "1E0"
    ryu' 1.0e+1 `shouldBe` "1E1"
    ryu' 1.0e+2 `shouldBe` "1E2"
    ryu' 1.0e+3 `shouldBe` "1E3"
    ryu' 1.0e+4 `shouldBe` "1E4"
    ryu' 1.0e+5 `shouldBe` "1E5"
    ryu' 1.0e+6 `shouldBe` "1E6"
    ryu' 1.0e+7 `shouldBe` "1E7"
    ryu' 1.0e+8 `shouldBe` "1E8"
    ryu' 1.0e+9 `shouldBe` "1E9"
    ryu' 1.0e+10 `shouldBe` "1E10"
    ryu' 1.0e+11 `shouldBe` "1E11"
    ryu' 1.0e+12 `shouldBe` "1E12"
    ryu' 1.0e+13 `shouldBe` "1E13"
    ryu' 1.0e+14 `shouldBe` "1E14"
    ryu' 1.0e+15 `shouldBe` "1E15"

    -- 10^15 + 10^i
    ryu' (1.0e+15 + 1.0e+0) `shouldBe` "1.000000000000001E15"
    ryu' (1.0e+15 + 1.0e+1) `shouldBe` "1.00000000000001E15"
    ryu' (1.0e+15 + 1.0e+2) `shouldBe` "1.0000000000001E15"
    ryu' (1.0e+15 + 1.0e+3) `shouldBe` "1.000000000001E15"
    ryu' (1.0e+15 + 1.0e+4) `shouldBe` "1.00000000001E15"
    ryu' (1.0e+15 + 1.0e+5) `shouldBe` "1.0000000001E15"
    ryu' (1.0e+15 + 1.0e+6) `shouldBe` "1.000000001E15"
    ryu' (1.0e+15 + 1.0e+7) `shouldBe` "1.00000001E15"
    ryu' (1.0e+15 + 1.0e+8) `shouldBe` "1.0000001E15"
    ryu' (1.0e+15 + 1.0e+9) `shouldBe` "1.000001E15"
    ryu' (1.0e+15 + 1.0e+10) `shouldBe` "1.00001E15"
    ryu' (1.0e+15 + 1.0e+11) `shouldBe` "1.0001E15"
    ryu' (1.0e+15 + 1.0e+12) `shouldBe` "1.001E15"
    ryu' (1.0e+15 + 1.0e+13) `shouldBe` "1.01E15"
    ryu' (1.0e+15 + 1.0e+14) `shouldBe` "1.1E15"
    
    -- Largest power of 2 <= 10^(i+1)
    ryu' 8.0 `shouldBe` "8E0"
    ryu' 64.0 `shouldBe` "6.4E1"
    ryu' 512.0 `shouldBe` "5.12E2"
    ryu' 8192.0 `shouldBe` "8.192E3"
    ryu' 65536.0 `shouldBe` "6.5536E4"
    ryu' 524288.0 `shouldBe` "5.24288E5"
    ryu' 8388608.0 `shouldBe` "8.388608E6"
    ryu' 67108864.0 `shouldBe` "6.7108864E7"
    ryu' 536870912.0 `shouldBe` "5.36870912E8"
    ryu' 8589934592.0 `shouldBe` "8.589934592E9"
    ryu' 68719476736.0 `shouldBe` "6.8719476736E10"
    ryu' 549755813888.0 `shouldBe` "5.49755813888E11"
    ryu' 8796093022208.0 `shouldBe` "8.796093022208E12"
    ryu' 70368744177664.0 `shouldBe` "7.0368744177664E13"
    ryu' 562949953421312.0 `shouldBe` "5.62949953421312E14"
    ryu' 9007199254740992.0 `shouldBe` "9.007199254740992E15"

    -- 1000 * (Largest power of 2 <= 10^(i+1))
    ryu' 8.0e+3 `shouldBe` "8E3"
    ryu' 64.0e+3 `shouldBe` "6.4E4"
    ryu' 512.0e+3 `shouldBe` "5.12E5"
    ryu' 8192.0e+3 `shouldBe` "8.192E6"
    ryu' 65536.0e+3 `shouldBe` "6.5536E7"
    ryu' 524288.0e+3 `shouldBe` "5.24288E8"
    ryu' 8388608.0e+3 `shouldBe` "8.388608E9"
    ryu' 67108864.0e+3 `shouldBe` "6.7108864E10"
    ryu' 536870912.0e+3 `shouldBe` "5.36870912E11"
    ryu' 8589934592.0e+3 `shouldBe` "8.589934592E12"
    ryu' 68719476736.0e+3 `shouldBe` "6.8719476736E13"
    ryu' 549755813888.0e+3 `shouldBe` "5.49755813888E14"
    ryu' 8796093022208.0e+3 `shouldBe` "8.796093022208E15"

ieeeParts2Double :: MonadFail m => Sign -> ExponentWord Double -> MantissaWord Double -> m Double
ieeeParts2Double sign ieeeExponent ieeeMantissa = do
  unless (ieeeExponent <= 2047) $ fail $ "Exponent value " <> show ieeeExponent <> " is to large."
  unless (ieeeMantissa <= 1 `shiftL` 53 - 1) $ fail $ "Mantissa value " <> show ieeeMantissa <> " is to large."
  return $ word64Bits2Double
    $   (if sign then 1 `rotateR` 1 else 0)
    .|. (fromIntegral ieeeExponent `shiftL` 52)
    .|. ieeeMantissa

word64Bits2Double :: Word64 -> Double
word64Bits2Double = unsafeCoerce

decimalNotationTests :: (IsString text, Show text, Eq text, ToString text) => (Double -> text) -> Spec
decimalNotationTests ryu' = do
  it "non-nomral" do
    ryu' (negate infinity) `shouldBe` "-Infinity"
    ryu' infinity `shouldBe` "Infinity"
    ryu' nan `shouldBe` "NaN"
    ryu' (-0) `shouldBe` "-0"
    ryu' 0 `shouldBe` "0"
 
  describe "exponent >= 0" do
    it "basic" do
      ryu' 1      `shouldBe` "1"
      ryu' 10     `shouldBe` "10"
      ryu' 12     `shouldBe` "12"
      ryu' 900    `shouldBe` "900"
      ryu' (-1)   `shouldBe` "-1"
      ryu' (-10)  `shouldBe` "-10"
      ryu' (-12)  `shouldBe` "-12"
      ryu' (-900) `shouldBe` "-900"

    prop "quickcheck" \(x :: Int) -> ryu' (fromIntegral x) `shouldBe` fromString (show x)

  describe "inter decimal point" do
    it "basic" do
      ryu' 1.2      `shouldBe` "1.2"
      ryu' 1.23     `shouldBe` "1.23"
      ryu' 12.3     `shouldBe` "12.3"
      ryu' 12.34    `shouldBe` "12.34"
      ryu' (-1.2)   `shouldBe` "-1.2"
      ryu' (-1.23)  `shouldBe` "-1.23"
      ryu' (-12.3)  `shouldBe` "-12.3"
      ryu' (-12.34) `shouldBe` "-12.34"

  describe "0 < abs x < 1" do
    describe "basic" do
      it "no leading zeros" do
        ryu' 0.1 `shouldBe` "0.1"
        ryu' 0.12 `shouldBe` "0.12"
        ryu' (-0.1) `shouldBe` "-0.1"
        ryu' (-0.12) `shouldBe` "-0.12"

      it "has leading zeros" do
        ryu' 0.01 `shouldBe` "0.01"
        ryu' 0.001 `shouldBe` "0.001"
        ryu' 0.012 `shouldBe` "0.012"
        ryu' 0.0012 `shouldBe` "0.0012"
        ryu' (-0.01) `shouldBe` "-0.01"
        ryu' (-0.012) `shouldBe` "-0.012"
        ryu' (-0.001) `shouldBe` "-0.001"
        ryu' (-0.0012) `shouldBe` "-0.0012"

    prop "quickcheck" \(x :: Int) -> let
      d = (1 / fromIntegral x)
      in (read $ toString $ ryu' d) `shouldBe` d

  prop "any normal double" \d -> (read $ toString $ ryu' d) `shouldBe` d

shortestOfDecimalAndScientificNotationTests :: forall text.
  ( IsString text
  , Show text
  , Eq text
  , ToString text
  , MonoFoldable text
  ) => (Double -> text) -> Spec
shortestOfDecimalAndScientificNotationTests ryu' = do
  it "non-nomral" do
    ryu' (negate infinity) `shouldBe` "-Inf"
    ryu' infinity `shouldBe` "Inf"
    ryu' nan `shouldBe` "NaN"
    ryu' (-0) `shouldBe` "-0"
    ryu' 0 `shouldBe` "0"

  it "found errors" do
    
    ryu' 2.0678 `shouldBe` "2.0678"
    ryu' 2.06785137318 `shouldBe` "2.06785137318"
    ryu' 1.33226762955E-15 `shouldBe` "1.33226762955E-15"
    ryu' 1.33226762955 `shouldBe` "1.33226762955"

  prop "any normal double" \d -> (read $ toString $ ryu' d) `shouldBe` d

  prop "length always <= ScientificNotation" \d ->
    ryu' d `shouldSatisfy` (<= length (ryu @(ScientificNotation 'Capital) @_ @String d)) . length

class ToString a where toString :: a -> String
instance ToString String where toString = id 
instance ToString B.ByteString where toString = B.unpack
instance ToString BL.ByteString where toString = BL.unpack

