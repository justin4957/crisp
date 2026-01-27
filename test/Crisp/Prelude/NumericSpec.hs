{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Prelude.NumericSpec
-- Description : Test suite for numeric conversion functions
--
-- Tests for numeric type conversions, formatting, parsing,
-- and arithmetic functions in the Crisp prelude.

module Crisp.Prelude.NumericSpec (spec) where

import Test.Hspec

import Crisp.Prelude.Numeric
import Crisp.Prelude.Functions (PreludeFunction(..), FunctionBody(..))
import Crisp.Core.Term (Type(..))

import Data.Maybe (isJust)
import qualified Data.Text as T

spec :: Spec
spec = do
  conversionFunctionTests
  formattingFunctionTests
  parsingFunctionTests
  arithmeticFunctionTests
  lookupTests

-- =============================================================================
-- Conversion Function Tests
-- =============================================================================

conversionFunctionTests :: Spec
conversionFunctionTests = describe "conversion functions" $ do
  describe "int_to_float" $ do
    it "has correct name" $ do
      fnName intToFloatFn `shouldBe` "int_to_float"

    it "takes Int parameter" $ do
      case fnParams intToFloatFn of
        [(name, TyCon "Int" [])] -> name `shouldBe` "n"
        _ -> expectationFailure "Expected Int parameter"

    it "returns Float type" $ do
      fnReturnType intToFloatFn `shouldBe` TyCon "Float" []

    it "is a builtin" $ do
      case fnBody intToFloatFn of
        BodyBuiltin name -> name `shouldBe` "int_to_float"
        _ -> expectationFailure "Expected builtin body"

  describe "float_to_int" $ do
    it "has correct name" $ do
      fnName floatToIntFn `shouldBe` "float_to_int"

    it "takes Float parameter" $ do
      case fnParams floatToIntFn of
        [(name, TyCon "Float" [])] -> name `shouldBe` "f"
        _ -> expectationFailure "Expected Float parameter"

    it "returns Int type" $ do
      fnReturnType floatToIntFn `shouldBe` TyCon "Int" []

  describe "float_to_int_round" $ do
    it "has correct name" $ do
      fnName floatToIntRoundFn `shouldBe` "float_to_int_round"

    it "is a builtin" $ do
      case fnBody floatToIntRoundFn of
        BodyBuiltin name -> name `shouldBe` "float_to_int_round"
        _ -> expectationFailure "Expected builtin body"

  describe "float_to_int_ceil" $ do
    it "has correct name" $ do
      fnName floatToIntCeilFn `shouldBe` "float_to_int_ceil"

    it "returns Int type" $ do
      fnReturnType floatToIntCeilFn `shouldBe` TyCon "Int" []

  describe "float_to_int_floor" $ do
    it "has correct name" $ do
      fnName floatToIntFloorFn `shouldBe` "float_to_int_floor"

    it "returns Int type" $ do
      fnReturnType floatToIntFloorFn `shouldBe` TyCon "Int" []

-- =============================================================================
-- Formatting Function Tests
-- =============================================================================

formattingFunctionTests :: Spec
formattingFunctionTests = describe "formatting functions" $ do
  describe "int_to_string" $ do
    it "has correct name" $ do
      fnName intToStringFn `shouldBe` "int_to_string"

    it "takes Int parameter" $ do
      case fnParams intToStringFn of
        [(name, TyCon "Int" [])] -> name `shouldBe` "n"
        _ -> expectationFailure "Expected Int parameter"

    it "returns String type" $ do
      fnReturnType intToStringFn `shouldBe` TyCon "String" []

    it "is a builtin" $ do
      case fnBody intToStringFn of
        BodyBuiltin name -> name `shouldBe` "int_to_string"
        _ -> expectationFailure "Expected builtin body"

  describe "int_to_string_radix" $ do
    it "has correct name" $ do
      fnName intToStringRadixFn `shouldBe` "int_to_string_radix"

    it "takes two Int parameters" $ do
      length (fnParams intToStringRadixFn) `shouldBe` 2

    it "returns String type" $ do
      fnReturnType intToStringRadixFn `shouldBe` TyCon "String" []

  describe "float_to_string" $ do
    it "has correct name" $ do
      fnName floatToStringFn `shouldBe` "float_to_string"

    it "takes Float parameter" $ do
      case fnParams floatToStringFn of
        [(name, TyCon "Float" [])] -> name `shouldBe` "f"
        _ -> expectationFailure "Expected Float parameter"

    it "returns String type" $ do
      fnReturnType floatToStringFn `shouldBe` TyCon "String" []

  describe "float_to_string_precision" $ do
    it "has correct name" $ do
      fnName floatToStringPrecisionFn `shouldBe` "float_to_string_precision"

    it "takes Float and Int parameters" $ do
      case fnParams floatToStringPrecisionFn of
        [(_, TyCon "Float" []), (_, TyCon "Int" [])] -> return ()
        _ -> expectationFailure "Expected Float and Int parameters"

    it "returns String type" $ do
      fnReturnType floatToStringPrecisionFn `shouldBe` TyCon "String" []

-- =============================================================================
-- Parsing Function Tests
-- =============================================================================

parsingFunctionTests :: Spec
parsingFunctionTests = describe "parsing functions" $ do
  describe "parse_int" $ do
    it "has correct name" $ do
      fnName parseIntFn `shouldBe` "parse_int"

    it "takes String parameter" $ do
      case fnParams parseIntFn of
        [(name, TyCon "String" [])] -> name `shouldBe` "s"
        _ -> expectationFailure "Expected String parameter"

    it "returns Option Int type" $ do
      case fnReturnType parseIntFn of
        TyCon "Option" [TyCon "Int" []] -> return ()
        other -> expectationFailure $ "Expected Option Int, got " ++ show other

    it "is a builtin" $ do
      case fnBody parseIntFn of
        BodyBuiltin name -> name `shouldBe` "parse_int"
        _ -> expectationFailure "Expected builtin body"

  describe "parse_float" $ do
    it "has correct name" $ do
      fnName parseFloatFn `shouldBe` "parse_float"

    it "takes String parameter" $ do
      case fnParams parseFloatFn of
        [(name, TyCon "String" [])] -> name `shouldBe` "s"
        _ -> expectationFailure "Expected String parameter"

    it "returns Option Float type" $ do
      case fnReturnType parseFloatFn of
        TyCon "Option" [TyCon "Float" []] -> return ()
        other -> expectationFailure $ "Expected Option Float, got " ++ show other

-- =============================================================================
-- Arithmetic Function Tests
-- =============================================================================

arithmeticFunctionTests :: Spec
arithmeticFunctionTests = describe "arithmetic functions" $ do
  describe "abs" $ do
    it "has correct name" $ do
      fnName absFn `shouldBe` "abs"

    it "takes Int parameter" $ do
      case fnParams absFn of
        [(name, TyCon "Int" [])] -> name `shouldBe` "n"
        _ -> expectationFailure "Expected Int parameter"

    it "returns Int type" $ do
      fnReturnType absFn `shouldBe` TyCon "Int" []

  describe "negate" $ do
    it "has correct name" $ do
      fnName negateFn `shouldBe` "negate"

    it "returns Int type" $ do
      fnReturnType negateFn `shouldBe` TyCon "Int" []

  describe "signum" $ do
    it "has correct name" $ do
      fnName signumFn `shouldBe` "signum"

    it "returns Int type" $ do
      fnReturnType signumFn `shouldBe` TyCon "Int" []

  describe "min" $ do
    it "has correct name" $ do
      fnName minFn `shouldBe` "min"

    it "takes two Int parameters" $ do
      length (fnParams minFn) `shouldBe` 2
      case fnParams minFn of
        [(_, TyCon "Int" []), (_, TyCon "Int" [])] -> return ()
        _ -> expectationFailure "Expected two Int parameters"

    it "returns Int type" $ do
      fnReturnType minFn `shouldBe` TyCon "Int" []

  describe "max" $ do
    it "has correct name" $ do
      fnName maxFn `shouldBe` "max"

    it "takes two Int parameters" $ do
      length (fnParams maxFn) `shouldBe` 2

    it "returns Int type" $ do
      fnReturnType maxFn `shouldBe` TyCon "Int" []

  describe "clamp" $ do
    it "has correct name" $ do
      fnName clampFn `shouldBe` "clamp"

    it "takes three Int parameters" $ do
      length (fnParams clampFn) `shouldBe` 3
      case fnParams clampFn of
        [(_, TyCon "Int" []), (_, TyCon "Int" []), (_, TyCon "Int" [])] -> return ()
        _ -> expectationFailure "Expected three Int parameters"

    it "returns Int type" $ do
      fnReturnType clampFn `shouldBe` TyCon "Int" []

-- =============================================================================
-- Lookup Tests
-- =============================================================================

lookupTests :: Spec
lookupTests = describe "lookup functions" $ do
  describe "lookupNumericFunction" $ do
    it "finds int_to_float" $ do
      lookupNumericFunction "int_to_float" `shouldSatisfy` isJust

    it "finds float_to_int" $ do
      lookupNumericFunction "float_to_int" `shouldSatisfy` isJust

    it "finds int_to_string" $ do
      lookupNumericFunction "int_to_string" `shouldSatisfy` isJust

    it "finds parse_int" $ do
      lookupNumericFunction "parse_int" `shouldSatisfy` isJust

    it "finds abs" $ do
      lookupNumericFunction "abs" `shouldSatisfy` isJust

    it "finds clamp" $ do
      lookupNumericFunction "clamp" `shouldSatisfy` isJust

    it "returns Nothing for unknown" $ do
      lookupNumericFunction "unknown" `shouldBe` Nothing

  describe "isNumericFunction" $ do
    it "returns True for numeric functions" $ do
      isNumericFunction "int_to_float" `shouldBe` True
      isNumericFunction "float_to_int" `shouldBe` True
      isNumericFunction "int_to_string" `shouldBe` True
      isNumericFunction "parse_int" `shouldBe` True
      isNumericFunction "abs" `shouldBe` True
      isNumericFunction "min" `shouldBe` True
      isNumericFunction "max" `shouldBe` True

    it "returns False for non-numeric functions" $ do
      isNumericFunction "map" `shouldBe` False
      isNumericFunction "filter" `shouldBe` False
      isNumericFunction "id" `shouldBe` False

  describe "numericFunctions" $ do
    it "contains 17 functions" $ do
      length numericFunctions `shouldBe` 17

    it "all have unique names" $ do
      let names = map fnName numericFunctions
      length names `shouldBe` length (filter (uncurry (/=)) $ zip names (tail names ++ [""]))
