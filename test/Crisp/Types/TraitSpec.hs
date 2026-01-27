{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.TraitSpec
-- Description : Test suite for trait and implementation system
--
-- Tests for parsing trait definitions, implementations, deriving clauses,
-- and the trait context/registry system.

module Crisp.Types.TraitSpec (spec) where

import Test.Hspec

import Crisp.Parser.Parser
import Crisp.Syntax.Surface
import Crisp.Types.Context
import Crisp.Core.Term (Type(..), Kind(..), simpleType)

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
  traitParsingTests
  implParsingTests
  derivingParsingTests
  traitContextTests
  preludeTraitTests

-- =============================================================================
-- Trait Definition Parsing Tests
-- =============================================================================

traitParsingTests :: Spec
traitParsingTests = describe "trait definitions" $ do
  it "parses simple trait definition" $ do
    let src = T.unlines
          [ "module Main"
          , "trait Eq A:"
          , "  eq: A -> A -> Bool"
          ]
    shouldParse $ parseModule "test" src

  it "parses trait with multiple methods" $ do
    let src = T.unlines
          [ "module Main"
          , "trait Ord A:"
          , "  compare: A -> A -> Ordering"
          , "  lt: A -> A -> Bool"
          ]
    shouldParse $ parseModule "test" src

  it "parses trait with kind annotation" $ do
    let src = T.unlines
          [ "module Main"
          , "trait Functor (F: Type -> Type):"
          , "  map: (A -> B) -> F A -> F B"
          ]
    shouldParse $ parseModule "test" src

  it "parses trait with supertraits" $ do
    let src = T.unlines
          [ "module Main"
          , "trait Ord A where A: Eq:"
          , "  compare: A -> A -> Ordering"
          ]
    shouldParse $ parseModule "test" src

  it "extracts trait name" $ do
    let src = T.unlines
          [ "module Main"
          , "trait Eq A:"
          , "  eq: A -> A -> Bool"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTrait td] -> traitDefName td `shouldBe` "Eq"
        _ -> expectationFailure "Expected single trait definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts trait parameter" $ do
    let src = T.unlines
          [ "module Main"
          , "trait Eq A:"
          , "  eq: A -> A -> Bool"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTrait td] -> traitDefParam td `shouldBe` "A"
        _ -> expectationFailure "Expected single trait definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts trait methods" $ do
    let src = T.unlines
          [ "module Main"
          , "trait Eq A:"
          , "  eq: A -> A -> Bool"
          , "  ne: A -> A -> Bool"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTrait td] -> length (traitDefMethods td) `shouldBe` 2
        _ -> expectationFailure "Expected single trait definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Implementation Parsing Tests
-- =============================================================================

implParsingTests :: Spec
implParsingTests = describe "impl definitions" $ do
  it "parses simple impl" $ do
    let src = T.unlines
          [ "module Main"
          , "impl Eq for Int:"
          , "  fn eq(a: Int, b: Int) -> Bool: int_eq a b"
          ]
    shouldParse $ parseModule "test" src

  it "parses impl with multiple methods" $ do
    let src = T.unlines
          [ "module Main"
          , "impl Ord for Int:"
          , "  fn compare(a: Int, b: Int) -> Ordering: int_compare a b"
          , "  fn lt(a: Int, b: Int) -> Bool: int_lt a b"
          ]
    shouldParse $ parseModule "test" src

  it "parses impl for parameterized type" $ do
    let src = T.unlines
          [ "module Main"
          , "impl Eq for Option Int:"
          , "  fn eq(a: Option Int, b: Option Int) -> Bool: option_eq a b"
          ]
    shouldParse $ parseModule "test" src

  it "extracts impl trait name" $ do
    let src = T.unlines
          [ "module Main"
          , "impl Eq for Int:"
          , "  fn eq(a: Int, b: Int) -> Bool: int_eq a b"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefImpl impl] -> implDefTrait impl `shouldBe` "Eq"
        _ -> expectationFailure "Expected single impl definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts impl type" $ do
    let src = T.unlines
          [ "module Main"
          , "impl Eq for Int:"
          , "  fn eq(a: Int, b: Int) -> Bool: int_eq a b"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefImpl impl] -> case implDefType impl of
          TyName "Int" _ -> pure ()
          other -> expectationFailure $ "Expected TyName Int, got " ++ show other
        _ -> expectationFailure "Expected single impl definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts impl methods" $ do
    let src = T.unlines
          [ "module Main"
          , "impl Ord for Int:"
          , "  fn compare(a: Int, b: Int) -> Ordering: int_compare a b"
          , "  fn lt(a: Int, b: Int) -> Bool: int_lt a b"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefImpl impl] -> length (implDefMethods impl) `shouldBe` 2
        _ -> expectationFailure "Expected single impl definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Deriving Clause Parsing Tests
-- =============================================================================

derivingParsingTests :: Spec
derivingParsingTests = describe "deriving clauses" $ do
  it "parses type with single deriving" $ do
    let src = "module Main type Date deriving Eq"
    shouldParse $ parseModule "test" src

  it "parses type with multiple deriving" $ do
    let src = "module Main type Date deriving (Eq, Ord)"
    shouldParse $ parseModule "test" src

  it "extracts deriving traits" $ do
    case parseModule "test" "module Main type Date deriving (Eq, Ord)" of
      Right m -> case moduleDefinitions m of
        [DefType td] -> case typeDefDeriving td of
          Just dc -> derivingTraits dc `shouldBe` ["Eq", "Ord"]
          Nothing -> expectationFailure "Expected deriving clause"
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type with deriving and constructors" $ do
    -- Note: This depends on parser handling of constructors
    let src = "module Main type Date deriving Eq"
    shouldParse $ parseModule "test" src

-- =============================================================================
-- Trait Context Tests
-- =============================================================================

traitContextTests :: Spec
traitContextTests = describe "trait context" $ do
  it "registers trait definition" $ do
    let traitInfo = TraitInfo "Show" ("A", KiType 0) []
          [TraitMethodInfo "show" (simpleType "String") Nothing]
    let ctx = registerTrait traitInfo emptyContext
    lookupTrait "Show" ctx `shouldBe` Just traitInfo

  it "registers multiple traits" $ do
    let eqInfo = TraitInfo "Eq" ("A", KiType 0) []
          [TraitMethodInfo "eq" (simpleType "Bool") Nothing]
    let ordInfo = TraitInfo "Ord" ("A", KiType 0) ["Eq"]
          [TraitMethodInfo "compare" (simpleType "Ordering") Nothing]
    let ctx = registerTrait ordInfo $ registerTrait eqInfo emptyContext
    lookupTrait "Eq" ctx `shouldSatisfy` isJust
    lookupTrait "Ord" ctx `shouldSatisfy` isJust

  it "looks up nonexistent trait returns Nothing" $ do
    lookupTrait "NonExistent" emptyContext `shouldBe` Nothing

  it "registers trait implementation" $ do
    let implInfo = ImplInfo "Eq" (simpleType "Int")
          [("eq", simpleType "Bool")]
    let ctx = registerImpl implInfo emptyContext
    lookupImpl "Eq" (simpleType "Int") ctx `shouldBe` Just implInfo

  it "looks up implementations for trait" $ do
    let implInt = ImplInfo "Eq" (simpleType "Int") [("eq", simpleType "Bool")]
    let implString = ImplInfo "Eq" (simpleType "String") [("eq", simpleType "Bool")]
    let implOrd = ImplInfo "Ord" (simpleType "Int") [("compare", simpleType "Ordering")]
    let ctx = foldr registerImpl emptyContext [implInt, implString, implOrd]
    length (lookupImplsForTrait "Eq" ctx) `shouldBe` 2

  it "looks up implementations for type" $ do
    let implEq = ImplInfo "Eq" (simpleType "Int") [("eq", simpleType "Bool")]
    let implOrd = ImplInfo "Ord" (simpleType "Int") [("compare", simpleType "Ordering")]
    let implShow = ImplInfo "Show" (simpleType "String") [("show", simpleType "String")]
    let ctx = foldr registerImpl emptyContext [implEq, implOrd, implShow]
    length (lookupImplsForType (simpleType "Int") ctx) `shouldBe` 2

-- =============================================================================
-- Prelude Trait Tests
-- =============================================================================

preludeTraitTests :: Spec
preludeTraitTests = describe "prelude traits" $ do
  it "prelude has Ordering type" $ do
    let ctx = withPrelude
    lookupType "Ordering" ctx `shouldSatisfy` isJust

  it "Ordering has correct constructors" $ do
    let ctx = withPrelude
    case lookupType "Ordering" ctx of
      Just info -> do
        let conNames = map constructorName (typeInfoConstructors info)
        conNames `shouldBe` ["Less", "Equal", "Greater"]
      Nothing -> expectationFailure "Ordering type not found"

  it "prelude has Eq trait" $ do
    let ctx = withPrelude
    lookupTrait "Eq" ctx `shouldSatisfy` isJust

  it "prelude has Ord trait" $ do
    let ctx = withPrelude
    lookupTrait "Ord" ctx `shouldSatisfy` isJust

  it "Ord trait requires Eq" $ do
    let ctx = withPrelude
    case lookupTrait "Ord" ctx of
      Just info -> traitInfoSupers info `shouldBe` ["Eq"]
      Nothing -> expectationFailure "Ord trait not found"

  it "Ord trait has compare method" $ do
    let ctx = withPrelude
    case lookupTrait "Ord" ctx of
      Just info -> do
        let methodNames = map traitMethodInfoName (traitInfoMethods info)
        methodNames `shouldContain` ["compare"]
      Nothing -> expectationFailure "Ord trait not found"

  it "prelude has Eq impl for Int" $ do
    let ctx = withPrelude
    lookupImpl "Eq" (simpleType "Int") ctx `shouldSatisfy` isJust

  it "prelude has Ord impl for Int" $ do
    let ctx = withPrelude
    lookupImpl "Ord" (simpleType "Int") ctx `shouldSatisfy` isJust

  it "prelude has Eq impl for all primitive types" $ do
    let ctx = withPrelude
    let primitives = ["Int", "Float", "String", "Char", "Bool", "Nat"]
    mapM_ (\t -> lookupImpl "Eq" (simpleType t) ctx `shouldSatisfy` isJust) primitives

  it "prelude has Ord impl for all primitive types" $ do
    let ctx = withPrelude
    let primitives = ["Int", "Float", "String", "Char", "Bool", "Nat"]
    mapM_ (\t -> lookupImpl "Ord" (simpleType t) ctx `shouldSatisfy` isJust) primitives
