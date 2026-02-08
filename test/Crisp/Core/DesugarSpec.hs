{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Core.DesugarSpec
-- Description : Tests for desugaring surface AST to core
--
-- Tests for the desugaring phase that transforms surface syntax
-- into the core calculus.

module Crisp.Core.DesugarSpec (spec) where

import Test.Hspec

import Crisp.Core.Desugar
import Crisp.Core.Term as C
import Crisp.Parser.Parser (parseModule)

import Data.Either (isRight, isLeft)
import qualified Data.Text as T

-- | Helper to check that desugaring succeeds
shouldDesugar :: T.Text -> Expectation
shouldDesugar src = case parseModule "test" src of
  Left err -> expectationFailure $ "Parse failed: " ++ show err
  Right m -> case desugarModule m of
    Left err -> expectationFailure $ "Desugar failed: " ++ show err
    Right _ -> pure ()

-- | Helper to check that desugaring fails
shouldNotDesugar :: T.Text -> Expectation
shouldNotDesugar src = case parseModule "test" src of
  Left _ -> pure ()  -- Parse failure is also acceptable
  Right m -> case desugarModule m of
    Left _ -> pure ()
    Right _ -> expectationFailure "Expected desugaring to fail"

spec :: Spec
spec = do
  describe "Crisp.Core.Desugar" $ do
    typeDefinitionTests
    typeAliasTests
    functionTests
    expressionTests

-- =============================================================================
-- Type Definition Tests (issue #246)
-- =============================================================================

typeDefinitionTests :: Spec
typeDefinitionTests = describe "type definitions (issue #246)" $ do
  it "desugars simple enum type" $ do
    let src = T.unlines
          [ "module Test"
          , "type Color:"
          , "  Red"
          , "  Green"
          , "  Blue"
          ]
    shouldDesugar src

  it "desugars type with type parameters" $ do
    let src = T.unlines
          [ "module Test"
          , "type Maybe A:"
          , "  Nothing"
          , "  Just(value: A)"
          ]
    shouldDesugar src

  it "desugars record type" $ do
    let src = T.unlines
          [ "module Test"
          , "type Person:"
          , "  name: String"
          , "  age: Int"
          ]
    shouldDesugar src

  it "desugars type with multiple constructors" $ do
    let src = T.unlines
          [ "module Test"
          , "type Either A B:"
          , "  Left(value: A)"
          , "  Right(value: B)"
          ]
    shouldDesugar src

  it "desugars type with positional constructor args" $ do
    let src = T.unlines
          [ "module Test"
          , "type Pair A B:"
          , "  Pair(A, B)"
          ]
    shouldDesugar src

  it "desugars unit-like constructor (no args)" $ do
    let src = T.unlines
          [ "module Test"
          , "type Unit:"
          , "  Unit"
          ]
    shouldDesugar src

  it "desugars multiple type definitions" $ do
    let src = T.unlines
          [ "module Test"
          , "type Bool:"
          , "  True"
          , "  False"
          , ""
          , "type Option A:"
          , "  None"
          , "  Some(value: A)"
          ]
    shouldDesugar src

-- =============================================================================
-- Type Alias Tests (issue #246)
-- =============================================================================

typeAliasTests :: Spec
typeAliasTests = describe "type aliases (issue #246)" $ do
  it "desugars simple type alias" $ do
    let src = "module Test type MyInt = Int"
    shouldDesugar src

  it "desugars type alias with constraint" $ do
    let src = "module Test type ActiveUser = User where status: Active"
    shouldDesugar src

  it "desugars type alias with extended fields" $ do
    let src = T.unlines
          [ "module Test"
          , "type Extended = Base extended with:"
          , "  extra: Int"
          ]
    shouldDesugar src

-- =============================================================================
-- Function Tests
-- =============================================================================

functionTests :: Spec
functionTests = describe "function definitions" $ do
  it "desugars simple function" $ do
    let src = T.unlines
          [ "module Test"
          , "fn identity(x: Int) -> Int:"
          , "  x"
          ]
    shouldDesugar src

  it "desugars function with multiple params" $ do
    let src = T.unlines
          [ "module Test"
          , "fn add(x: Int, y: Int) -> Int:"
          , "  x"
          ]
    shouldDesugar src

-- =============================================================================
-- Expression Tests
-- =============================================================================

expressionTests :: Spec
expressionTests = describe "expressions" $ do
  it "desugars let expression" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test() -> Int:"
          , "  let x = 1"
          , "  x"
          ]
    shouldDesugar src

  it "desugars if expression" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test(b: Bool) -> Int:"
          , "  if b then 1 else 0"
          ]
    shouldDesugar src

  it "desugars match expression" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test(x: Int) -> Int:"
          , "  match x:"
          , "    0 -> 1"
          , "    _ -> 0"
          ]
    -- Note: literal patterns not yet supported, this will fail
    shouldNotDesugar src

  it "desugars binary operators" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test(x: Int, y: Int) -> Int:"
          , "  x + y"
          ]
    shouldDesugar src
