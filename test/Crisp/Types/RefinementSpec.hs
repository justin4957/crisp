{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.RefinementSpec
-- Description : Test suite for refinement types
--
-- Tests for parsing refinement type syntax and tracking refinement
-- predicates in the type system.

module Crisp.Types.RefinementSpec (spec) where

import Test.Hspec

import Crisp.Parser.Parser
import Crisp.Syntax.Surface
import Crisp.Types.Context
import Crisp.Core.Term (Type(..), simpleType, Term(..))

import Data.Either (isRight, isLeft)
import Data.Maybe (isJust)
import qualified Data.Text as T

-- | Helper to check that parsing succeeds
shouldParse :: (Show a, Show b) => Either a b -> Expectation
shouldParse result = result `shouldSatisfy` isRight

-- | Helper to check that parsing fails
shouldNotParse :: (Show a, Show b) => Either a b -> Expectation
shouldNotParse result = result `shouldSatisfy` isLeft

spec :: Spec
spec = do
  refinementTypeParsingTests
  refinementPredicateTests
  refinementContextTests

-- =============================================================================
-- Refinement Type Parsing Tests
-- =============================================================================

refinementTypeParsingTests :: Spec
refinementTypeParsingTests = describe "refinement type parsing" $ do
  it "parses simple refinement type" $ do
    let src = "Int { self > 0 }"
    shouldParse $ parseType "test" src

  it "parses refinement with self variable" $ do
    let src = "Int { self >= 1 }"
    shouldParse $ parseType "test" src

  it "parses refinement with less-than-or-equal" $ do
    let src = "Int { self <= 100 }"
    shouldParse $ parseType "test" src

  it "parses refinement with equality" $ do
    let src = "Int { self == 42 }"
    shouldParse $ parseType "test" src

  it "parses refinement with inequality" $ do
    let src = "Int { self /= 0 }"
    shouldParse $ parseType "test" src

  it "parses chained comparison (range)" $ do
    let src = "Int { 1 <= self <= 12 }"
    shouldParse $ parseType "test" src

  it "parses multiple predicates with comma" $ do
    let src = "Int { self > 0, self < 100 }"
    shouldParse $ parseType "test" src

  it "parses refinement with && conjunction" $ do
    let src = "Int { self > 0 && self < 100 }"
    shouldParse $ parseType "test" src

  it "parses refinement with || disjunction" $ do
    let src = "Int { self < 0 || self > 100 }"
    shouldParse $ parseType "test" src

  it "parses refinement with negation" $ do
    let src = "Int { !(self == 0) }"
    shouldParse $ parseType "test" src

  it "parses refinement on type constructor" $ do
    let src = "Nat { self > 0 }"
    shouldParse $ parseType "test" src

  it "extracts TyRefinement from parsed type" $ do
    let src = "Int { self > 0 }"
    case parseType "test" src of
      Right (TyRefinement baseType _ _) ->
        case baseType of
          TyName "Int" _ -> pure ()
          _ -> expectationFailure $ "Expected TyName Int, got " ++ show baseType
      Right other -> expectationFailure $ "Expected TyRefinement, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Refinement Predicate Tests
-- =============================================================================

refinementPredicateTests :: Spec
refinementPredicateTests = describe "refinement predicates" $ do
  it "parses comparison predicate" $ do
    let src = "Int { self > 0 }"
    case parseType "test" src of
      Right (TyRefinement _ preds _) ->
        case preds of
          [RefinementComparison _ _ _ _] -> pure ()
          _ -> expectationFailure $ "Expected single comparison predicate, got " ++ show preds
      Right other -> expectationFailure $ "Expected TyRefinement, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses self as EVar" $ do
    let src = "Int { self > 0 }"
    case parseType "test" src of
      Right (TyRefinement _ [RefinementComparison leftExpr _ _ _] _) ->
        case leftExpr of
          EVar "self" _ -> pure ()
          _ -> expectationFailure $ "Expected EVar self, got " ++ show leftExpr
      Right other -> expectationFailure $ "Unexpected type structure: " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses integer literal in predicate" $ do
    let src = "Int { self > 42 }"
    case parseType "test" src of
      Right (TyRefinement _ [RefinementComparison _ _ rightExpr _] _) ->
        case rightExpr of
          EIntLit 42 _ -> pure ()
          _ -> expectationFailure $ "Expected EIntLit 42, got " ++ show rightExpr
      Right other -> expectationFailure $ "Unexpected type structure: " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses OpGt operator" $ do
    let src = "Int { self > 0 }"
    case parseType "test" src of
      Right (TyRefinement _ [RefinementComparison _ op _ _] _) ->
        op `shouldBe` OpGt
      Right other -> expectationFailure $ "Unexpected type structure: " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses OpLt operator" $ do
    let src = "Int { self < 100 }"
    case parseType "test" src of
      Right (TyRefinement _ [RefinementComparison _ op _ _] _) ->
        op `shouldBe` OpLt
      Right other -> expectationFailure $ "Unexpected type structure: " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses OpLe operator" $ do
    let src = "Int { self <= 100 }"
    case parseType "test" src of
      Right (TyRefinement _ [RefinementComparison _ op _ _] _) ->
        op `shouldBe` OpLe
      Right other -> expectationFailure $ "Unexpected type structure: " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses OpGe operator" $ do
    let src = "Int { self >= 1 }"
    case parseType "test" src of
      Right (TyRefinement _ [RefinementComparison _ op _ _] _) ->
        op `shouldBe` OpGe
      Right other -> expectationFailure $ "Unexpected type structure: " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses OpEq operator" $ do
    let src = "Int { self == 0 }"
    case parseType "test" src of
      Right (TyRefinement _ [RefinementComparison _ op _ _] _) ->
        op `shouldBe` OpEq
      Right other -> expectationFailure $ "Unexpected type structure: " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses OpNe operator" $ do
    let src = "Int { self /= 0 }"
    case parseType "test" src of
      Right (TyRefinement _ [RefinementComparison _ op _ _] _) ->
        op `shouldBe` OpNe
      Right other -> expectationFailure $ "Unexpected type structure: " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses chained comparison as And" $ do
    let src = "Int { 1 <= self <= 12 }"
    case parseType "test" src of
      Right (TyRefinement _ [RefinementAnd _ _ _] _) -> pure ()
      Right (TyRefinement _ preds _) ->
        expectationFailure $ "Expected RefinementAnd, got " ++ show preds
      Right other -> expectationFailure $ "Expected TyRefinement, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Refinement Context Tests
-- =============================================================================

refinementContextTests :: Spec
refinementContextTests = describe "refinement context" $ do
  it "registers a refinement type" $ do
    let refinementInfo = RefinementInfo
          "Month"
          (simpleType "Int")
          [TmVar "predicate" 0]
    let ctx = registerRefinement refinementInfo emptyContext
    lookupRefinement "Month" ctx `shouldSatisfy` isJust

  it "registers multiple refinement types" $ do
    let month = RefinementInfo "Month" (simpleType "Int") []
    let posInt = RefinementInfo "PosInt" (simpleType "Int") []
    let ctx = registerRefinement posInt $ registerRefinement month emptyContext
    lookupRefinement "Month" ctx `shouldSatisfy` isJust
    lookupRefinement "PosInt" ctx `shouldSatisfy` isJust

  it "looks up nonexistent refinement returns Nothing" $ do
    lookupRefinement "NonExistent" emptyContext `shouldBe` Nothing

  it "refinement info preserves base type" $ do
    let refinementInfo = RefinementInfo "Month" (simpleType "Int") []
    let ctx = registerRefinement refinementInfo emptyContext
    case lookupRefinement "Month" ctx of
      Just info -> refinementInfoBaseType info `shouldBe` simpleType "Int"
      Nothing -> expectationFailure "Expected to find Month refinement"

  it "refinement info preserves name" $ do
    let refinementInfo = RefinementInfo "Month" (simpleType "Int") []
    let ctx = registerRefinement refinementInfo emptyContext
    case lookupRefinement "Month" ctx of
      Just info -> refinementInfoName info `shouldBe` "Month"
      Nothing -> expectationFailure "Expected to find Month refinement"

  it "finds refinements for base type" $ do
    let month = RefinementInfo "Month" (simpleType "Int") []
    let posInt = RefinementInfo "PosInt" (simpleType "Int") []
    let posFloat = RefinementInfo "PosFloat" (simpleType "Float") []
    let ctx = registerRefinement posFloat $ registerRefinement posInt $ registerRefinement month emptyContext
    let intRefinements = lookupRefinementsForType (simpleType "Int") ctx
    length intRefinements `shouldBe` 2
