{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.ExhaustiveSpec
-- Description : Tests for pattern exhaustiveness checking
--
-- TDD tests for pattern exhaustiveness checking, ensuring all pattern
-- matches cover all possible cases.

module Crisp.Types.ExhaustiveSpec (spec) where

import Test.Hspec

import Crisp.Types.Exhaustive
import Crisp.Core.Term

import qualified Data.Text as T

-- | Helper to create a pattern variable
patVar :: T.Text -> Pattern
patVar = PatVar

-- | Helper to create a wildcard pattern
patWild :: Pattern
patWild = PatWild

-- | Helper to create a constructor pattern
patCon :: T.Text -> [Pattern] -> Pattern
patCon = PatCon

-- | Helper type info for Bool
boolTypeInfo :: TypeInfo
boolTypeInfo = TypeInfo
  { typeInfoName = "Bool"
  , typeInfoConstructors =
      [ ConstructorInfo "True" []
      , ConstructorInfo "False" []
      ]
  }

-- | Helper type info for Option a
optionTypeInfo :: TypeInfo
optionTypeInfo = TypeInfo
  { typeInfoName = "Option"
  , typeInfoConstructors =
      [ ConstructorInfo "Some" [ArgInfo "value" (simpleType "a")]
      , ConstructorInfo "None" []
      ]
  }

-- | Helper type info for List a
listTypeInfo :: TypeInfo
listTypeInfo = TypeInfo
  { typeInfoName = "List"
  , typeInfoConstructors =
      [ ConstructorInfo "Cons" [ArgInfo "head" (simpleType "a"), ArgInfo "tail" (simpleType "List")]
      , ConstructorInfo "Nil" []
      ]
  }

-- | Helper type info for Result a b
resultTypeInfo :: TypeInfo
resultTypeInfo = TypeInfo
  { typeInfoName = "Result"
  , typeInfoConstructors =
      [ ConstructorInfo "Ok" [ArgInfo "value" (simpleType "a")]
      , ConstructorInfo "Err" [ArgInfo "error" (simpleType "b")]
      ]
  }

-- | Helper type info for Unit
unitTypeInfo :: TypeInfo
unitTypeInfo = TypeInfo
  { typeInfoName = "Unit"
  , typeInfoConstructors =
      [ ConstructorInfo "Unit" []
      ]
  }

-- | Helper type info for triple Option (Some, None, Unknown)
tripleOptionTypeInfo :: TypeInfo
tripleOptionTypeInfo = TypeInfo
  { typeInfoName = "TripleOption"
  , typeInfoConstructors =
      [ ConstructorInfo "Present" [ArgInfo "value" (simpleType "a")]
      , ConstructorInfo "Absent" []
      , ConstructorInfo "Unknown" []
      ]
  }

-- | Default type environment for tests
defaultTypeEnv :: TypeEnv
defaultTypeEnv = TypeEnv
  { typeEnvTypes =
      [ ("Bool", boolTypeInfo)
      , ("Option", optionTypeInfo)
      , ("List", listTypeInfo)
      , ("Result", resultTypeInfo)
      , ("Unit", unitTypeInfo)
      , ("TripleOption", tripleOptionTypeInfo)
      ]
  }

spec :: Spec
spec = do
  describe "Exhaustive Boolean Matches" $ do
    booleanExhaustiveTests

  describe "Exhaustive Option Matches" $ do
    optionExhaustiveTests

  describe "Exhaustive List Matches" $ do
    listExhaustiveTests

  describe "Exhaustive Result Matches" $ do
    resultExhaustiveTests

  describe "Non-Exhaustive Matches" $ do
    nonExhaustiveTests

  describe "Wildcard Patterns" $ do
    wildcardTests

  describe "Variable Patterns" $ do
    variablePatternTests

  describe "Nested Pattern Exhaustiveness" $ do
    nestedPatternTests

  describe "Redundant Pattern Detection" $ do
    redundantPatternTests

  describe "Missing Pattern Reporting" $ do
    missingPatternTests

  describe "Edge Cases" $ do
    edgeCaseTests

-- | Tests for exhaustive boolean matches
booleanExhaustiveTests :: Spec
booleanExhaustiveTests = describe "boolean patterns" $ do
  it "accepts exhaustive boolean match (True, False)" $ do
    let patterns = [patCon "True" [], patCon "False" []]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "accepts exhaustive boolean match (False, True)" $ do
    let patterns = [patCon "False" [], patCon "True" []]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "accepts wildcard for boolean" $ do
    let patterns = [patWild]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "accepts variable for boolean" $ do
    let patterns = [patVar "x"]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "rejects single True pattern" $ do
    let patterns = [patCon "True" []]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` False

  it "rejects single False pattern" $ do
    let patterns = [patCon "False" []]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` False

-- | Tests for exhaustive option matches
optionExhaustiveTests :: Spec
optionExhaustiveTests = describe "option patterns" $ do
  it "accepts exhaustive option match (Some, None)" $ do
    let patterns = [patCon "Some" [patVar "x"], patCon "None" []]
    isExhaustive defaultTypeEnv optionTypeInfo patterns `shouldBe` True

  it "accepts exhaustive option match (None, Some)" $ do
    let patterns = [patCon "None" [], patCon "Some" [patVar "x"]]
    isExhaustive defaultTypeEnv optionTypeInfo patterns `shouldBe` True

  it "accepts Some with wildcard subpattern" $ do
    let patterns = [patCon "Some" [patWild], patCon "None" []]
    isExhaustive defaultTypeEnv optionTypeInfo patterns `shouldBe` True

  it "accepts wildcard for option" $ do
    let patterns = [patWild]
    isExhaustive defaultTypeEnv optionTypeInfo patterns `shouldBe` True

  it "rejects single Some pattern" $ do
    let patterns = [patCon "Some" [patVar "x"]]
    isExhaustive defaultTypeEnv optionTypeInfo patterns `shouldBe` False

  it "rejects single None pattern" $ do
    let patterns = [patCon "None" []]
    isExhaustive defaultTypeEnv optionTypeInfo patterns `shouldBe` False

-- | Tests for exhaustive list matches
listExhaustiveTests :: Spec
listExhaustiveTests = describe "list patterns" $ do
  it "accepts exhaustive list match (Cons, Nil)" $ do
    let patterns = [patCon "Cons" [patVar "h", patVar "t"], patCon "Nil" []]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` True

  it "accepts exhaustive list match (Nil, Cons)" $ do
    let patterns = [patCon "Nil" [], patCon "Cons" [patVar "h", patVar "t"]]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` True

  it "accepts wildcard for list" $ do
    let patterns = [patWild]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` True

  it "rejects single Cons pattern" $ do
    let patterns = [patCon "Cons" [patVar "h", patVar "t"]]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` False

  it "rejects single Nil pattern" $ do
    let patterns = [patCon "Nil" []]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` False

-- | Tests for exhaustive result matches
resultExhaustiveTests :: Spec
resultExhaustiveTests = describe "result patterns" $ do
  it "accepts exhaustive result match (Ok, Err)" $ do
    let patterns = [patCon "Ok" [patVar "v"], patCon "Err" [patVar "e"]]
    isExhaustive defaultTypeEnv resultTypeInfo patterns `shouldBe` True

  it "accepts exhaustive result match (Err, Ok)" $ do
    let patterns = [patCon "Err" [patVar "e"], patCon "Ok" [patVar "v"]]
    isExhaustive defaultTypeEnv resultTypeInfo patterns `shouldBe` True

  it "rejects single Ok pattern" $ do
    let patterns = [patCon "Ok" [patVar "v"]]
    isExhaustive defaultTypeEnv resultTypeInfo patterns `shouldBe` False

  it "rejects single Err pattern" $ do
    let patterns = [patCon "Err" [patVar "e"]]
    isExhaustive defaultTypeEnv resultTypeInfo patterns `shouldBe` False

-- | Tests for non-exhaustive matches
nonExhaustiveTests :: Spec
nonExhaustiveTests = describe "non-exhaustive patterns" $ do
  it "rejects empty pattern list" $ do
    let patterns = []
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` False

  it "rejects missing constructor in triple option" $ do
    let patterns = [patCon "Present" [patVar "x"], patCon "Absent" []]
    isExhaustive defaultTypeEnv tripleOptionTypeInfo patterns `shouldBe` False

  it "rejects two missing constructors in triple option" $ do
    let patterns = [patCon "Present" [patVar "x"]]
    isExhaustive defaultTypeEnv tripleOptionTypeInfo patterns `shouldBe` False

-- | Tests for wildcard patterns
wildcardTests :: Spec
wildcardTests = describe "wildcard patterns" $ do
  it "wildcard covers all constructors" $ do
    let patterns = [patWild]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "wildcard covers complex types" $ do
    let patterns = [patWild]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` True

  it "wildcard after specific patterns makes it exhaustive" $ do
    let patterns = [patCon "True" [], patWild]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "wildcard in middle makes remaining patterns redundant" $ do
    let patterns = [patWild, patCon "True" []]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` True

-- | Tests for variable patterns
variablePatternTests :: Spec
variablePatternTests = describe "variable patterns" $ do
  it "variable covers all constructors" $ do
    let patterns = [patVar "x"]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "variable in subpattern covers that position" $ do
    let patterns = [patCon "Some" [patVar "x"], patCon "None" []]
    isExhaustive defaultTypeEnv optionTypeInfo patterns `shouldBe` True

  it "multiple variables in list pattern" $ do
    let patterns = [patCon "Cons" [patVar "h", patVar "t"], patCon "Nil" []]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` True

-- | Tests for nested pattern exhaustiveness
nestedPatternTests :: Spec
nestedPatternTests = describe "nested patterns" $ do
  it "Some with any subpattern and None is exhaustive" $ do
    -- When subpatterns are wildcards/variables, covers all nested cases
    let patterns =
          [ patCon "Some" [patVar "x"]
          , patCon "None" []
          ]
    isExhaustive defaultTypeEnv optionTypeInfo patterns `shouldBe` True

  it "Cons with nested wildcard" $ do
    let patterns = [patCon "Cons" [patWild, patWild], patCon "Nil" []]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` True

  it "list with variable subpatterns is exhaustive" $ do
    let patterns =
          [ patCon "Cons" [patVar "h", patVar "t"]
          , patCon "Nil" []
          ]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` True

  it "wildcards in subpatterns ensure full coverage" $ do
    -- Wildcard subpatterns cover all possible inner values
    let patterns =
          [ patCon "Some" [patWild]
          , patCon "None" []
          ]
    isExhaustive defaultTypeEnv optionTypeInfo patterns `shouldBe` True

-- | Tests for redundant pattern detection
redundantPatternTests :: Spec
redundantPatternTests = describe "redundant patterns" $ do
  it "detects redundant pattern after wildcard" $ do
    let patterns = [patWild, patCon "True" []]
    hasRedundantPatterns defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "detects redundant pattern after variable" $ do
    let patterns = [patVar "x", patCon "False" []]
    hasRedundantPatterns defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "no redundancy in complete match" $ do
    let patterns = [patCon "True" [], patCon "False" []]
    hasRedundantPatterns defaultTypeEnv boolTypeInfo patterns `shouldBe` False

  it "no redundancy with specific then wildcard" $ do
    let patterns = [patCon "True" [], patWild]
    hasRedundantPatterns defaultTypeEnv boolTypeInfo patterns `shouldBe` False

  it "detects duplicate constructor patterns" $ do
    let patterns = [patCon "True" [], patCon "True" [], patCon "False" []]
    hasRedundantPatterns defaultTypeEnv boolTypeInfo patterns `shouldBe` True

-- | Tests for missing pattern reporting
missingPatternTests :: Spec
missingPatternTests = describe "missing patterns" $ do
  it "reports missing True" $ do
    let patterns = [patCon "False" []]
        missing = missingPatterns defaultTypeEnv boolTypeInfo patterns
    missing `shouldContain` ["True"]

  it "reports missing False" $ do
    let patterns = [patCon "True" []]
        missing = missingPatterns defaultTypeEnv boolTypeInfo patterns
    missing `shouldContain` ["False"]

  it "reports missing None" $ do
    let patterns = [patCon "Some" [patVar "x"]]
        missing = missingPatterns defaultTypeEnv optionTypeInfo patterns
    missing `shouldContain` ["None"]

  it "reports missing Some" $ do
    let patterns = [patCon "None" []]
        missing = missingPatterns defaultTypeEnv optionTypeInfo patterns
    missing `shouldContain` ["Some(_)"]

  it "reports multiple missing constructors" $ do
    let patterns = [patCon "Present" [patVar "x"]]
        missing = missingPatterns defaultTypeEnv tripleOptionTypeInfo patterns
    length missing `shouldBe` 2

  it "reports all constructors missing for empty patterns" $ do
    let patterns = []
        missing = missingPatterns defaultTypeEnv boolTypeInfo patterns
    length missing `shouldBe` 2

  it "no missing patterns when exhaustive" $ do
    let patterns = [patCon "True" [], patCon "False" []]
        missing = missingPatterns defaultTypeEnv boolTypeInfo patterns
    missing `shouldBe` []

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles single-constructor types" $ do
    let patterns = [patCon "Unit" []]
    isExhaustive defaultTypeEnv unitTypeInfo patterns `shouldBe` True

  it "empty patterns for single-constructor type is not exhaustive" $ do
    let patterns = []
    isExhaustive defaultTypeEnv unitTypeInfo patterns `shouldBe` False

  it "multiple wildcards are redundant but exhaustive" $ do
    let patterns = [patWild, patWild]
    isExhaustive defaultTypeEnv boolTypeInfo patterns `shouldBe` True
    hasRedundantPatterns defaultTypeEnv boolTypeInfo patterns `shouldBe` True

  it "complex patterns with wildcards in subpatterns" $ do
    let patterns =
          [ patCon "Cons" [patWild, patWild]
          , patCon "Nil" []
          ]
    isExhaustive defaultTypeEnv listTypeInfo patterns `shouldBe` True

  it "order of patterns doesn't affect exhaustiveness" $ do
    let patterns1 = [patCon "True" [], patCon "False" []]
        patterns2 = [patCon "False" [], patCon "True" []]
    isExhaustive defaultTypeEnv boolTypeInfo patterns1 `shouldBe`
      isExhaustive defaultTypeEnv boolTypeInfo patterns2
