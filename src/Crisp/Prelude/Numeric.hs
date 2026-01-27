{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Prelude.Numeric
-- Description : Numeric conversion and formatting functions for the Crisp prelude
--
-- Provides standard library functions for numeric type conversions and formatting:
--
-- * Type conversions between Int and Float
-- * Rounding modes for Float to Int conversion
-- * Formatting numbers as strings
-- * Parsing strings to numbers
--
-- These are fundamental operations needed in virtually any application.
--
-- == Design Philosophy
--
-- Numeric operations are implemented as builtins that map to WebAssembly
-- instructions or runtime functions:
--
-- * int_to_float -> f64.convert_i64_s
-- * float_to_int -> i64.trunc_f64_s
-- * Formatting/parsing -> runtime string functions

module Crisp.Prelude.Numeric
  ( -- * Conversion Functions
    intToFloatFn
  , floatToIntFn
  , floatToIntRoundFn
  , floatToIntCeilFn
  , floatToIntFloorFn
    -- * Formatting Functions
  , intToStringFn
  , intToStringRadixFn
  , floatToStringFn
  , floatToStringPrecisionFn
    -- * Parsing Functions
  , parseIntFn
  , parseFloatFn
    -- * Arithmetic Functions
  , absFn
  , negateFn
  , signumFn
  , minFn
  , maxFn
  , clampFn
    -- * All Numeric Functions
  , numericFunctions
    -- * Lookup
  , lookupNumericFunction
  , isNumericFunction
  ) where

import Crisp.Prelude.Functions (PreludeFunction(..), FunctionBody(..))
import Crisp.Core.Term (Type(..), Kind(..), EffectRow(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create a pure function type
pureFn :: Type -> Type -> Type
pureFn from to = TyPi "_" from EffEmpty to

-- | Type constructor shorthand
tyCon :: Text -> [Type] -> Type
tyCon = TyCon

-- | Int type
intTy :: Type
intTy = tyCon "Int" []

-- | Float type
floatTy :: Type
floatTy = tyCon "Float" []

-- | String type
stringTy :: Type
stringTy = tyCon "String" []

-- | Bool type
boolTy :: Type
boolTy = tyCon "Bool" []

-- | Option type
optionTy :: Type -> Type
optionTy a = tyCon "Option" [a]

--------------------------------------------------------------------------------
-- Type Conversion Functions
--------------------------------------------------------------------------------

-- | int_to_float : Int -> Float
--
-- Converts an integer to a floating-point number.
-- This is a lossless conversion for integers within Float's precision range.
intToFloatFn :: PreludeFunction
intToFloatFn = PreludeFunction
  { fnName = "int_to_float"
  , fnTypeParams = []
  , fnParams = [("n", intTy)]
  , fnReturnType = floatTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "int_to_float"
  }

-- | float_to_int : Float -> Int
--
-- Converts a floating-point number to an integer by truncation (towards zero).
-- For example: 3.7 -> 3, -3.7 -> -3
floatToIntFn :: PreludeFunction
floatToIntFn = PreludeFunction
  { fnName = "float_to_int"
  , fnTypeParams = []
  , fnParams = [("f", floatTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "float_to_int"
  }

-- | float_to_int_round : Float -> Int
--
-- Converts a floating-point number to an integer by rounding to nearest.
-- Ties round to even (banker's rounding).
-- For example: 3.5 -> 4, 2.5 -> 2, 3.7 -> 4, 3.2 -> 3
floatToIntRoundFn :: PreludeFunction
floatToIntRoundFn = PreludeFunction
  { fnName = "float_to_int_round"
  , fnTypeParams = []
  , fnParams = [("f", floatTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "float_to_int_round"
  }

-- | float_to_int_ceil : Float -> Int
--
-- Converts a floating-point number to an integer by rounding up (towards positive infinity).
-- For example: 3.1 -> 4, -3.1 -> -3
floatToIntCeilFn :: PreludeFunction
floatToIntCeilFn = PreludeFunction
  { fnName = "float_to_int_ceil"
  , fnTypeParams = []
  , fnParams = [("f", floatTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "float_to_int_ceil"
  }

-- | float_to_int_floor : Float -> Int
--
-- Converts a floating-point number to an integer by rounding down (towards negative infinity).
-- For example: 3.9 -> 3, -3.1 -> -4
floatToIntFloorFn :: PreludeFunction
floatToIntFloorFn = PreludeFunction
  { fnName = "float_to_int_floor"
  , fnTypeParams = []
  , fnParams = [("f", floatTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "float_to_int_floor"
  }

--------------------------------------------------------------------------------
-- Formatting Functions
--------------------------------------------------------------------------------

-- | int_to_string : Int -> String
--
-- Formats an integer as a decimal string.
-- For example: 42 -> "42", -17 -> "-17"
intToStringFn :: PreludeFunction
intToStringFn = PreludeFunction
  { fnName = "int_to_string"
  , fnTypeParams = []
  , fnParams = [("n", intTy)]
  , fnReturnType = stringTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "int_to_string"
  }

-- | int_to_string_radix : Int -> Int -> String
--
-- Formats an integer in a given radix (base 2-36).
-- For example: int_to_string_radix(255, 16) -> "ff"
intToStringRadixFn :: PreludeFunction
intToStringRadixFn = PreludeFunction
  { fnName = "int_to_string_radix"
  , fnTypeParams = []
  , fnParams = [("n", intTy), ("radix", intTy)]
  , fnReturnType = stringTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "int_to_string_radix"
  }

-- | float_to_string : Float -> String
--
-- Formats a floating-point number as a string.
-- Uses default precision (full precision needed to represent the value).
floatToStringFn :: PreludeFunction
floatToStringFn = PreludeFunction
  { fnName = "float_to_string"
  , fnTypeParams = []
  , fnParams = [("f", floatTy)]
  , fnReturnType = stringTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "float_to_string"
  }

-- | float_to_string_precision : Float -> Int -> String
--
-- Formats a floating-point number with a specified number of decimal places.
-- For example: float_to_string_precision(3.14159, 2) -> "3.14"
floatToStringPrecisionFn :: PreludeFunction
floatToStringPrecisionFn = PreludeFunction
  { fnName = "float_to_string_precision"
  , fnTypeParams = []
  , fnParams = [("f", floatTy), ("decimals", intTy)]
  , fnReturnType = stringTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "float_to_string_precision"
  }

--------------------------------------------------------------------------------
-- Parsing Functions
--------------------------------------------------------------------------------

-- | parse_int : String -> Option Int
--
-- Parses a string as a decimal integer.
-- Returns None if the string is not a valid integer.
-- For example: parse_int("42") -> Some(42), parse_int("abc") -> None
parseIntFn :: PreludeFunction
parseIntFn = PreludeFunction
  { fnName = "parse_int"
  , fnTypeParams = []
  , fnParams = [("s", stringTy)]
  , fnReturnType = optionTy intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "parse_int"
  }

-- | parse_float : String -> Option Float
--
-- Parses a string as a floating-point number.
-- Returns None if the string is not a valid float.
-- For example: parse_float("3.14") -> Some(3.14), parse_float("abc") -> None
parseFloatFn :: PreludeFunction
parseFloatFn = PreludeFunction
  { fnName = "parse_float"
  , fnTypeParams = []
  , fnParams = [("s", stringTy)]
  , fnReturnType = optionTy floatTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "parse_float"
  }

--------------------------------------------------------------------------------
-- Arithmetic Functions
--------------------------------------------------------------------------------

-- | abs : Int -> Int
--
-- Returns the absolute value of an integer.
-- For example: abs(-5) -> 5, abs(5) -> 5
absFn :: PreludeFunction
absFn = PreludeFunction
  { fnName = "abs"
  , fnTypeParams = []
  , fnParams = [("n", intTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "abs"
  }

-- | negate : Int -> Int
--
-- Negates an integer.
-- For example: negate(5) -> -5, negate(-5) -> 5
negateFn :: PreludeFunction
negateFn = PreludeFunction
  { fnName = "negate"
  , fnTypeParams = []
  , fnParams = [("n", intTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "negate"
  }

-- | signum : Int -> Int
--
-- Returns the sign of an integer: -1, 0, or 1.
-- For example: signum(-5) -> -1, signum(0) -> 0, signum(5) -> 1
signumFn :: PreludeFunction
signumFn = PreludeFunction
  { fnName = "signum"
  , fnTypeParams = []
  , fnParams = [("n", intTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "signum"
  }

-- | min : Int -> Int -> Int
--
-- Returns the smaller of two integers.
minFn :: PreludeFunction
minFn = PreludeFunction
  { fnName = "min"
  , fnTypeParams = []
  , fnParams = [("a", intTy), ("b", intTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "min"
  }

-- | max : Int -> Int -> Int
--
-- Returns the larger of two integers.
maxFn :: PreludeFunction
maxFn = PreludeFunction
  { fnName = "max"
  , fnTypeParams = []
  , fnParams = [("a", intTy), ("b", intTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "max"
  }

-- | clamp : Int -> Int -> Int -> Int
--
-- Clamps a value to be within a range [min, max].
-- For example: clamp(5, 0, 10) -> 5, clamp(-5, 0, 10) -> 0, clamp(15, 0, 10) -> 10
clampFn :: PreludeFunction
clampFn = PreludeFunction
  { fnName = "clamp"
  , fnTypeParams = []
  , fnParams = [("value", intTy), ("minVal", intTy), ("maxVal", intTy)]
  , fnReturnType = intTy
  , fnEffects = EffEmpty
  , fnBody = BodyBuiltin "clamp"
  }

--------------------------------------------------------------------------------
-- All Numeric Functions
--------------------------------------------------------------------------------

-- | All numeric functions
numericFunctions :: [PreludeFunction]
numericFunctions =
  [ -- Conversions
    intToFloatFn
  , floatToIntFn
  , floatToIntRoundFn
  , floatToIntCeilFn
  , floatToIntFloorFn
    -- Formatting
  , intToStringFn
  , intToStringRadixFn
  , floatToStringFn
  , floatToStringPrecisionFn
    -- Parsing
  , parseIntFn
  , parseFloatFn
    -- Arithmetic
  , absFn
  , negateFn
  , signumFn
  , minFn
  , maxFn
  , clampFn
  ]

--------------------------------------------------------------------------------
-- Lookup Functions
--------------------------------------------------------------------------------

-- | Map of numeric function names to definitions
numericFunctionMap :: Map Text PreludeFunction
numericFunctionMap = Map.fromList
  [(fnName f, f) | f <- numericFunctions]

-- | Look up a numeric function by name
lookupNumericFunction :: Text -> Maybe PreludeFunction
lookupNumericFunction name = Map.lookup name numericFunctionMap

-- | Check if a name is a numeric function
isNumericFunction :: Text -> Bool
isNumericFunction name = Map.member name numericFunctionMap
