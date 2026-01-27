{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.BoundedPolySpec
-- Description : Test suite for higher-kinded types with bounded polymorphism
--
-- Tests for parsing bounded type parameters, where clauses,
-- and constraint validation for higher-kinded types.

module Crisp.Types.BoundedPolySpec (spec) where

import Test.Hspec

import Crisp.Parser.Parser
import Crisp.Syntax.Surface
import Crisp.Types.Context
import Crisp.Core.Term (Type(..), simpleType, pureFnType)

import Data.Either (isRight, isLeft)
import qualified Data.Text as T

-- | Helper to check that parsing succeeds
shouldParse :: (Show a, Show b) => Either a b -> Expectation
shouldParse result = result `shouldSatisfy` isRight

-- | Helper to check that parsing fails
shouldNotParse :: (Show a, Show b) => Either a b -> Expectation
shouldNotParse result = result `shouldSatisfy` isLeft

spec :: Spec
spec = do
  boundedTypeParamParsingTests
  whereClauseParsingTests
  typeDefWithConstraintsTests
  constraintValidationTests

-- =============================================================================
-- Bounded Type Parameter Parsing Tests
-- =============================================================================

boundedTypeParamParsingTests :: Spec
boundedTypeParamParsingTests = describe "bounded type parameter parsing" $ do
  it "parses simple type parameter" $ do
    let src = T.unlines
          [ "module Main"
          , "type Container A:"
          , "  Empty"
          ]
    shouldParse $ parseModule "test" src

  it "parses type parameter with kind" $ do
    let src = T.unlines
          [ "module Main"
          , "type HKT (F: Type -> Type):"
          , "  Wrap"
          ]
    shouldParse $ parseModule "test" src

  it "parses bounded type parameter with single trait" $ do
    let src = T.unlines
          [ "module Main"
          , "type Sorted (A: Ord):"
          , "  SortedList"
          ]
    shouldParse $ parseModule "test" src

  it "parses bounded type parameter with multiple traits" $ do
    let src = T.unlines
          [ "module Main"
          , "type Printable (A: Eq + Show):"
          , "  Value"
          ]
    shouldParse $ parseModule "test" src

  it "parses bounded type parameter with kind and traits" $ do
    let src = T.unlines
          [ "module Main"
          , "type Mappable (F: Type -> Type: Functor):"
          , "  Wrapped"
          ]
    shouldParse $ parseModule "test" src

  it "extracts BoundedTypeVar from parsed type definition" $ do
    let src = T.unlines
          [ "module Main"
          , "type Container (A: Ord):"
          , "  Box"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType typeDef] -> case typeDefParams typeDef of
          [BoundedTypeVar name _ traits _] -> do
            name `shouldBe` "A"
            traits `shouldBe` ["Ord"]
          other -> expectationFailure $ "Expected BoundedTypeVar, got " ++ show other
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts multiple trait bounds" $ do
    let src = T.unlines
          [ "module Main"
          , "type Container (A: Eq + Ord + Show):"
          , "  Box"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType typeDef] -> case typeDefParams typeDef of
          [BoundedTypeVar _ _ traits _] ->
            traits `shouldBe` ["Eq", "Ord", "Show"]
          other -> expectationFailure $ "Expected BoundedTypeVar, got " ++ show other
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Where Clause Parsing Tests
-- =============================================================================

whereClauseParsingTests :: Spec
whereClauseParsingTests = describe "where clause parsing" $ do
  it "parses type definition with single where constraint" $ do
    let src = T.unlines
          [ "module Main"
          , "type Authority A where A: Action:"
          , "  Grant"
          ]
    shouldParse $ parseModule "test" src

  it "parses type definition with multiple where constraints" $ do
    let src = T.unlines
          [ "module Main"
          , "type Validator A B where A: Eq, B: Show:"
          , "  Check"
          ]
    shouldParse $ parseModule "test" src

  it "extracts where clause constraints" $ do
    let src = T.unlines
          [ "module Main"
          , "type Authority A where A: Action:"
          , "  Grant"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType typeDef] -> do
          length (typeDefConstraints typeDef) `shouldBe` 1
          case typeDefConstraints typeDef of
            [TraitConstraint trait _ _] ->
              trait `shouldBe` "Action"
            _ -> expectationFailure "Expected single constraint"
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts multiple where clause constraints" $ do
    let src = T.unlines
          [ "module Main"
          , "type Pair A B where A: Eq, B: Ord:"
          , "  MkPair"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType typeDef] ->
          length (typeDefConstraints typeDef) `shouldBe` 2
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Type Definition with Constraints Tests
-- =============================================================================

typeDefWithConstraintsTests :: Spec
typeDefWithConstraintsTests = describe "type definitions with constraints" $ do
  it "parses parameterized authority type" $ do
    let src = T.unlines
          [ "module Main"
          , "type Authority (A: Action):"
          , "  Grant String A"
          ]
    shouldParse $ parseModule "test" src

  it "parses multiple bounded parameters" $ do
    let src = T.unlines
          [ "module Main"
          , "type Map (K: Ord) V:"
          , "  Empty"
          , "  Entry K V"
          ]
    shouldParse $ parseModule "test" src

  it "parses mixed bounded and unbounded parameters" $ do
    let src = T.unlines
          [ "module Main"
          , "type PriorityQueue (P: Ord) V:"
          , "  EmptyQueue"
          ]
    shouldParse $ parseModule "test" src

  it "parses type with deriving and constraints" $ do
    let src = T.unlines
          [ "module Main"
          , "type Container (A: Eq) deriving Eq:"
          , "  Box A"
          ]
    shouldParse $ parseModule "test" src

  it "parses higher-kinded type parameter" $ do
    let src = T.unlines
          [ "module Main"
          , "type Wrapper (F: Type -> Type) A:"
          , "  Wrap"
          ]
    shouldParse $ parseModule "test" src

-- =============================================================================
-- Constraint Validation Tests
-- =============================================================================

constraintValidationTests :: Spec
constraintValidationTests = describe "constraint validation" $ do
  it "satisfiesConstraint returns True for implemented trait" $ do
    -- Int has Eq and Ord in prelude
    satisfiesConstraint (simpleType "Int") "Eq" withPrelude `shouldBe` True

  it "satisfiesConstraint returns True for Ord on Int" $ do
    satisfiesConstraint (simpleType "Int") "Ord" withPrelude `shouldBe` True

  it "satisfiesConstraint returns False for non-implemented trait" $ do
    satisfiesConstraint (simpleType "Int") "NonExistent" withPrelude `shouldBe` False

  it "satisfiesAllConstraints returns empty for all satisfied" $ do
    satisfiesAllConstraints (simpleType "Int") ["Eq", "Ord"] withPrelude
      `shouldBe` []

  it "satisfiesAllConstraints returns unsatisfied traits" $ do
    satisfiesAllConstraints (simpleType "Int") ["Eq", "NonExistent"] withPrelude
      `shouldBe` ["NonExistent"]

  it "checkTypeParamConstraints returns empty for satisfied constraints" $ do
    let constraints = [(simpleType "Int", ["Eq", "Ord"])]
    checkTypeParamConstraints constraints withPrelude `shouldBe` []

  it "checkTypeParamConstraints returns violations" $ do
    let constraints = [(simpleType "Int", ["Eq", "NonExistent"])]
    checkTypeParamConstraints constraints withPrelude
      `shouldBe` [(simpleType "Int", "NonExistent")]

  it "checkTypeParamConstraints handles multiple type arguments" $ do
    let constraints =
          [ (simpleType "Int", ["Eq"])
          , (simpleType "String", ["Ord"])
          ]
    checkTypeParamConstraints constraints withPrelude `shouldBe` []

  it "checkTypeParamConstraints reports multiple violations" $ do
    let constraints =
          [ (simpleType "Int", ["Missing1"])
          , (simpleType "String", ["Missing2"])
          ]
    let violations = checkTypeParamConstraints constraints withPrelude
    length violations `shouldBe` 2
