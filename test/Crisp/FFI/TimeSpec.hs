{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.FFI.TimeSpec
-- Description : Test suite for time FFI bindings
--
-- Tests for time-related external function definitions, parsing,
-- context registration, and type checking.

module Crisp.FFI.TimeSpec (spec) where

import Test.Hspec

import Crisp.FFI.Time
import Crisp.Parser.Parser
import Crisp.Types.Context
import Crisp.Core.Term (Type(..), simpleType, pureFnType)

import Data.Either (isRight)
import Data.Maybe (isJust)
import qualified Data.Text as T

-- | Helper to check that parsing succeeds
shouldParse :: (Show a, Show b) => Either a b -> Expectation
shouldParse result = result `shouldSatisfy` isRight

spec :: Spec
spec = do
  timeExternalDefinitionTests
  timeExternalParsingTests
  timeExternalContextTests
  timeTypeTests
  durationTests

-- =============================================================================
-- Time External Definition Tests
-- =============================================================================

timeExternalDefinitionTests :: Spec
timeExternalDefinitionTests = describe "time external definitions" $ do
  describe "time_ms" $ do
    it "has correct name" $ do
      externalInfoName timeMsExternal `shouldBe` "time_ms"

    it "has correct module" $ do
      externalInfoModule timeMsExternal `shouldBe` "system"

    it "has correct external function name" $ do
      externalInfoFunction timeMsExternal `shouldBe` "time_ms"

    it "returns Int type" $ do
      let fnType = externalInfoType timeMsExternal
      case fnType of
        TyPi _ _ _ retTy -> retTy `shouldBe` simpleType "Int"
        _ -> expectationFailure "Expected function type"

  describe "timezone" $ do
    it "has correct name" $ do
      externalInfoName timezoneExternal `shouldBe` "timezone"

    it "has correct module" $ do
      externalInfoModule timezoneExternal `shouldBe` "system"

    it "returns String type" $ do
      let fnType = externalInfoType timezoneExternal
      case fnType of
        TyPi _ _ _ retTy -> retTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type"

  describe "format_iso8601" $ do
    it "has correct name" $ do
      externalInfoName formatIso8601External `shouldBe` "format_iso8601"

    it "has correct module" $ do
      externalInfoModule formatIso8601External `shouldBe` "system"

    it "takes Int parameter" $ do
      let fnType = externalInfoType formatIso8601External
      case fnType of
        TyPi _ paramTy _ _ -> paramTy `shouldBe` simpleType "Int"
        _ -> expectationFailure "Expected function type"

    it "returns String type" $ do
      let fnType = externalInfoType formatIso8601External
      case fnType of
        TyPi _ _ _ retTy -> retTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type"

  describe "monotonic_ms" $ do
    it "has correct name" $ do
      externalInfoName monotonicMsExternal `shouldBe` "monotonic_ms"

    it "has correct module" $ do
      externalInfoModule monotonicMsExternal `shouldBe` "system"

    it "returns Int type" $ do
      let fnType = externalInfoType monotonicMsExternal
      case fnType of
        TyPi _ _ _ retTy -> retTy `shouldBe` simpleType "Int"
        _ -> expectationFailure "Expected function type"

-- =============================================================================
-- Time External Parsing Tests
-- =============================================================================

timeExternalParsingTests :: Spec
timeExternalParsingTests = describe "time external parsing" $ do
  it "parses time_ms external definition" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn time_ms() -> Int = (\"system\", \"time_ms\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses timezone external definition" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn timezone() -> String = (\"system\", \"timezone\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses format_iso8601 external definition" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn format_iso8601(unix_ms: Int) -> String = (\"system\", \"format_iso8601\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses monotonic_ms external definition" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn monotonic_ms() -> Int = (\"system\", \"monotonic_ms\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses time_ms call in expression" $ do
    let src = T.unlines
          [ "module Main"
          , "fn get_current_time() -> Int:"
          , "  external(\"system\", \"time_ms\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses format_iso8601 call with argument" $ do
    let src = T.unlines
          [ "module Main"
          , "fn format_time(ms: Int) -> String:"
          , "  external(\"system\", \"format_iso8601\") ms"
          ]
    shouldParse $ parseModule "test" src

  it "parses multiple time functions in sequence" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn time_ms() -> Int = (\"system\", \"time_ms\")"
          , "external fn format_iso8601(ms: Int) -> String = (\"system\", \"format_iso8601\")"
          , "fn create_timestamp(ms: Int) -> String:"
          , "  format_iso8601(ms)"
          ]
    shouldParse $ parseModule "test" src

-- =============================================================================
-- Time External Context Tests
-- =============================================================================

timeExternalContextTests :: Spec
timeExternalContextTests = describe "time external context" $ do
  it "registers time_ms in context" $ do
    let ctx = registerExternal timeMsExternal emptyContext
    lookupExternal "time_ms" ctx `shouldSatisfy` isJust

  it "registers all time externals" $ do
    let ctx = foldr registerExternal emptyContext timeExternals
    lookupExternal "time_ms" ctx `shouldSatisfy` isJust
    lookupExternal "timezone" ctx `shouldSatisfy` isJust
    lookupExternal "format_iso8601" ctx `shouldSatisfy` isJust
    lookupExternal "monotonic_ms" ctx `shouldSatisfy` isJust

  it "lookupTimeExternal finds time_ms" $ do
    lookupTimeExternal "time_ms" `shouldSatisfy` isJust

  it "lookupTimeExternal finds timezone" $ do
    lookupTimeExternal "timezone" `shouldSatisfy` isJust

  it "lookupTimeExternal finds format_iso8601" $ do
    lookupTimeExternal "format_iso8601" `shouldSatisfy` isJust

  it "lookupTimeExternal returns Nothing for unknown" $ do
    lookupTimeExternal "unknown" `shouldBe` Nothing

  it "isTimeExternal returns True for time functions" $ do
    isTimeExternal "time_ms" `shouldBe` True
    isTimeExternal "timezone" `shouldBe` True
    isTimeExternal "format_iso8601" `shouldBe` True
    isTimeExternal "monotonic_ms" `shouldBe` True

  it "isTimeExternal returns False for non-time functions" $ do
    isTimeExternal "log" `shouldBe` False
    isTimeExternal "print" `shouldBe` False

  it "allTimeExternals returns all definitions" $ do
    length allTimeExternals `shouldBe` 4

-- =============================================================================
-- Time Type Tests
-- =============================================================================

timeTypeTests :: Spec
timeTypeTests = describe "time types" $ do
  it "timestampType is Int" $ do
    timestampType `shouldBe` simpleType "Int"

  it "durationMsType is Int" $ do
    durationMsType `shouldBe` simpleType "Int"

  it "TimeInfo stores unix milliseconds" $ do
    let info = TimeInfo 1706400000000 "UTC" "2024-01-28T00:00:00.000Z"
    timeUnixMs info `shouldBe` 1706400000000

  it "TimeInfo stores timezone" $ do
    let info = TimeInfo 1706400000000 "America/New_York" "2024-01-27T19:00:00.000-05:00"
    timeTimezone info `shouldBe` "America/New_York"

  it "TimeInfo stores ISO 8601 string" $ do
    let info = TimeInfo 1706400000000 "UTC" "2024-01-28T00:00:00.000Z"
    timeIso8601 info `shouldBe` "2024-01-28T00:00:00.000Z"

-- =============================================================================
-- Duration Tests
-- =============================================================================

durationTests :: Spec
durationTests = describe "Duration" $ do
  it "stores milliseconds" $ do
    let d = Duration 5000
    durationMs d `shouldBe` 5000

  it "supports equality" $ do
    Duration 1000 `shouldBe` Duration 1000
    Duration 1000 `shouldNotBe` Duration 2000

  it "supports ordering" $ do
    Duration 1000 < Duration 2000 `shouldBe` True
    Duration 2000 > Duration 1000 `shouldBe` True

  it "supports numeric operations" $ do
    Duration 1000 + Duration 500 `shouldBe` Duration 1500
    Duration 2000 - Duration 500 `shouldBe` Duration 1500
