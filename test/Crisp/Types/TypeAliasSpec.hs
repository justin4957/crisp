{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.TypeAliasSpec
-- Description : Test suite for type aliases with field constraints
--
-- Tests for parsing type alias definitions and field constraints,
-- as well as context registration and lookup.

module Crisp.Types.TypeAliasSpec (spec) where

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
  simpleTypeAliasTests
  fieldConstraintTests
  parameterizedAliasTests
  contextTests

-- =============================================================================
-- Simple Type Alias Parsing Tests
-- =============================================================================

simpleTypeAliasTests :: Spec
simpleTypeAliasTests = describe "simple type alias parsing" $ do
  it "parses simple type alias" $ do
    let src = T.unlines
          [ "module Main"
          , "type MyInt = Int"
          ]
    shouldParse $ parseModule "test" src

  it "parses type alias with type application" $ do
    let src = T.unlines
          [ "module Main"
          , "type IntList = List Int"
          ]
    shouldParse $ parseModule "test" src

  it "parses type alias with multiple type arguments" $ do
    let src = T.unlines
          [ "module Main"
          , "type StringIntMap = Map String Int"
          ]
    shouldParse $ parseModule "test" src

  it "extracts type alias name" $ do
    let src = T.unlines
          [ "module Main"
          , "type MyInt = Int"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] -> typeAliasName alias `shouldBe` "MyInt"
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts base type" $ do
    let src = T.unlines
          [ "module Main"
          , "type MyInt = Int"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] ->
          case typeAliasBase alias of
            TyName "Int" _ -> pure ()
            other -> expectationFailure $ "Expected TyName Int, got " ++ show other
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Field Constraint Parsing Tests
-- =============================================================================

fieldConstraintTests :: Spec
fieldConstraintTests = describe "field constraint parsing" $ do
  it "parses type alias with single field constraint" $ do
    let src = T.unlines
          [ "module Main"
          , "type JudicialAuthority = Authority { action: Judicial }"
          ]
    shouldParse $ parseModule "test" src

  it "parses type alias with pattern constraint" $ do
    let src = T.unlines
          [ "module Main"
          , "type ActiveUser = User { status: Active }"
          ]
    shouldParse $ parseModule "test" src

  it "parses type alias with constructor pattern constraint" $ do
    let src = T.unlines
          [ "module Main"
          , "type JudicialAuthority = Authority { action: Judicial x }"
          ]
    shouldParse $ parseModule "test" src

  it "parses type alias with wildcard pattern constraint" $ do
    let src = T.unlines
          [ "module Main"
          , "type AnyJudicial = Authority { action: Judicial _ }"
          ]
    shouldParse $ parseModule "test" src

  it "parses type alias with multiple field constraints" $ do
    let src = T.unlines
          [ "module Main"
          , "type ActiveAdmin = User { status: Active, role: Admin }"
          ]
    shouldParse $ parseModule "test" src

  it "extracts field constraint name" $ do
    let src = T.unlines
          [ "module Main"
          , "type JudicialAuthority = Authority { action: Judicial }"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] -> case typeAliasConstraints alias of
          [fc] -> fieldConstraintName fc `shouldBe` "action"
          _ -> expectationFailure "Expected single field constraint"
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts field constraint pattern" $ do
    let src = T.unlines
          [ "module Main"
          , "type ActiveUser = User { status: Active }"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] -> case typeAliasConstraints alias of
          [fc] -> case fieldConstraintPatterns fc of
            [PatCon "Active" [] _] -> pure ()
            other -> expectationFailure $ "Expected [PatCon Active], got " ++ show other
          _ -> expectationFailure "Expected single field constraint"
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts multiple field constraints" $ do
    let src = T.unlines
          [ "module Main"
          , "type ActiveAdmin = User { status: Active, role: Admin }"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] ->
          length (typeAliasConstraints alias) `shouldBe` 2
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses OR pattern in field constraint (issue #218)" $ do
    let src = T.unlines
          [ "module Main"
          , "type DissentOpinion = Opinion where opinion_type: Dissent | DissentInPart"
          ]
    shouldParse $ parseModule "test" src

  it "extracts OR pattern alternatives (issue #218)" $ do
    let src = T.unlines
          [ "module Main"
          , "type DissentOpinion = Opinion where opinion_type: Dissent | DissentInPart"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] -> case typeAliasConstraints alias of
          [fc] -> do
            fieldConstraintName fc `shouldBe` "opinion_type"
            length (fieldConstraintPatterns fc) `shouldBe` 2
            case fieldConstraintPatterns fc of
              [PatCon "Dissent" [] _, PatCon "DissentInPart" [] _] -> pure ()
              other -> expectationFailure $ "Expected [Dissent, DissentInPart], got " ++ show other
          _ -> expectationFailure "Expected single field constraint"
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses triple OR pattern in field constraint (issue #218)" $ do
    let src = T.unlines
          [ "module Main"
          , "type StrongInterpretation = Interpretation where strength: Strong | Definitive | Binding"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] -> case typeAliasConstraints alias of
          [fc] -> length (fieldConstraintPatterns fc) `shouldBe` 3
          _ -> expectationFailure "Expected single field constraint"
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses OR pattern with constructor arguments (issue #218)" $ do
    let src = T.unlines
          [ "module Main"
          , "type MandatoryBinding = BindingForce where force: Mandatory(_) | Required(_)"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] -> case typeAliasConstraints alias of
          [fc] -> do
            length (fieldConstraintPatterns fc) `shouldBe` 2
            case fieldConstraintPatterns fc of
              [PatCon "Mandatory" [PatWildcard _] _, PatCon "Required" [PatWildcard _] _] -> pure ()
              other -> expectationFailure $ "Expected [Mandatory(_), Required(_)], got " ++ show other
          _ -> expectationFailure "Expected single field constraint"
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "single pattern field constraint still works (issue #218 regression)" $ do
    let src = T.unlines
          [ "module Main"
          , "type MandatoryBinding = BindingForce where force: Mandatory"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] -> case typeAliasConstraints alias of
          [fc] -> case fieldConstraintPatterns fc of
            [PatCon "Mandatory" [] _] -> pure ()
            other -> expectationFailure $ "Expected [Mandatory], got " ++ show other
          _ -> expectationFailure "Expected single field constraint"
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses OR pattern with brace-style field constraints (issue #218)" $ do
    let src = T.unlines
          [ "module Main"
          , "type DissentOpinion = Opinion { opinion_type: Dissent | DissentInPart }"
          ]
    shouldParse $ parseModule "test" src

-- =============================================================================
-- Parameterized Type Alias Tests
-- =============================================================================

parameterizedAliasTests :: Spec
parameterizedAliasTests = describe "parameterized type alias parsing" $ do
  it "parses parameterized type alias" $ do
    let src = T.unlines
          [ "module Main"
          , "type StringMap V = Map String V"
          ]
    shouldParse $ parseModule "test" src

  it "parses type alias with multiple type parameters" $ do
    let src = T.unlines
          [ "module Main"
          , "type Pair A B = Tuple A B"
          ]
    shouldParse $ parseModule "test" src

  it "parses parameterized alias with constraints" $ do
    let src = T.unlines
          [ "module Main"
          , "type ActiveContainer A = Container A { status: Active }"
          ]
    shouldParse $ parseModule "test" src

  it "extracts type parameters" $ do
    let src = T.unlines
          [ "module Main"
          , "type StringMap V = Map String V"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefTypeAlias alias] ->
          length (typeAliasParams alias) `shouldBe` 1
        _ -> expectationFailure "Expected single type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Context Registration Tests
-- =============================================================================

contextTests :: Spec
contextTests = describe "type alias context" $ do
  it "registers a type alias" $ do
    let aliasInfo = TypeAliasInfo
          "JudicialAuthority"
          []
          (simpleType "Authority")
          [FieldConstraintInfo "action" "Judicial(_)"]
    let ctx = registerTypeAlias aliasInfo emptyContext
    lookupTypeAlias "JudicialAuthority" ctx `shouldSatisfy` isJust

  it "registers multiple type aliases" $ do
    let alias1 = TypeAliasInfo "MyInt" [] (simpleType "Int") []
    let alias2 = TypeAliasInfo "MyString" [] (simpleType "String") []
    let ctx = registerTypeAlias alias2 $ registerTypeAlias alias1 emptyContext
    lookupTypeAlias "MyInt" ctx `shouldSatisfy` isJust
    lookupTypeAlias "MyString" ctx `shouldSatisfy` isJust

  it "looks up nonexistent alias returns Nothing" $ do
    lookupTypeAlias "NonExistent" emptyContext `shouldBe` Nothing

  it "expands type alias to base type" $ do
    let aliasInfo = TypeAliasInfo "MyInt" [] (simpleType "Int") []
    let ctx = registerTypeAlias aliasInfo emptyContext
    expandTypeAlias "MyInt" ctx `shouldBe` Just (simpleType "Int")

  it "expand nonexistent alias returns Nothing" $ do
    expandTypeAlias "NonExistent" emptyContext `shouldBe` Nothing

  it "preserves field constraints in alias info" $ do
    let constraints = [FieldConstraintInfo "action" "Judicial(_)"]
    let aliasInfo = TypeAliasInfo "JudicialAuthority" [] (simpleType "Authority") constraints
    let ctx = registerTypeAlias aliasInfo emptyContext
    case lookupTypeAlias "JudicialAuthority" ctx of
      Just info -> length (typeAliasInfoConstraints info) `shouldBe` 1
      Nothing -> expectationFailure "Expected to find alias"

  it "preserves type parameters in alias info" $ do
    let aliasInfo = TypeAliasInfo "StringMap" [("V", KiType 0)] (simpleType "Map") []
    let ctx = registerTypeAlias aliasInfo emptyContext
    case lookupTypeAlias "StringMap" ctx of
      Just info -> length (typeAliasInfoParams info) `shouldBe` 1
      Nothing -> expectationFailure "Expected to find alias"
