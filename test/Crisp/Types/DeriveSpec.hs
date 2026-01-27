{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.DeriveSpec
-- Description : Test suite for automatic trait derivation
--
-- Tests for deriving Eq and Ord implementations for enum/sum types.

module Crisp.Types.DeriveSpec (spec) where

import Test.Hspec

import Crisp.Types.Derive
import Crisp.Types.Context
import Crisp.Core.Term (Type(..), Kind(..), simpleType)

import Data.Either (isRight, isLeft)
import Data.Text (Text)

-- | Helper to create a simple enum type
mkEnumType :: Text -> [Text] -> TypeInfo
mkEnumType name constructors = TypeInfo
  { typeInfoName = name
  , typeInfoParams = []
  , typeInfoKind = KiType 0
  , typeInfoConstructors = map mkSimpleCon constructors
  , typeInfoIsProp = False
  , typeInfoIsLinear = False
  }
  where
    mkSimpleCon conName = ConstructorInfo conName [] (simpleType name)

-- | Helper to create a type with parameterized constructors
mkSumType :: Text -> [(Text, [Type])] -> TypeInfo
mkSumType name constructors = TypeInfo
  { typeInfoName = name
  , typeInfoParams = []
  , typeInfoKind = KiType 0
  , typeInfoConstructors = map mkCon constructors
  , typeInfoIsProp = False
  , typeInfoIsLinear = False
  }
  where
    mkCon (conName, params) = ConstructorInfo conName params (simpleType name)

spec :: Spec
spec = do
  deriveEqTests
  deriveOrdTests
  processDerivingTests
  errorCaseTests

-- =============================================================================
-- Eq Derivation Tests
-- =============================================================================

deriveEqTests :: Spec
deriveEqTests = describe "deriveEq" $ do
  it "derives Eq for simple enum type" $ do
    let typeInfo = mkEnumType "Color" ["Red", "Green", "Blue"]
    deriveEq typeInfo `shouldSatisfy` isRight

  it "generates correct trait name" $ do
    let typeInfo = mkEnumType "Color" ["Red", "Green", "Blue"]
    case deriveEq typeInfo of
      Right impl -> implInfoTrait impl `shouldBe` "Eq"
      Left err -> expectationFailure $ "Derivation failed: " ++ show err

  it "generates correct type" $ do
    let typeInfo = mkEnumType "Color" ["Red", "Green", "Blue"]
    case deriveEq typeInfo of
      Right impl -> implInfoType impl `shouldBe` simpleType "Color"
      Left err -> expectationFailure $ "Derivation failed: " ++ show err

  it "generates eq and ne methods" $ do
    let typeInfo = mkEnumType "JudicialAction"
          ["HearCase", "IssueInjunction", "HoldInContempt", "IssueWarrant"]
    case deriveEq typeInfo of
      Right impl -> do
        let methodNames = map fst (implInfoMethods impl)
        methodNames `shouldContain` ["eq"]
        methodNames `shouldContain` ["ne"]
      Left err -> expectationFailure $ "Derivation failed: " ++ show err

  it "derives Eq for single constructor enum" $ do
    let typeInfo = mkEnumType "Unit" ["Unit"]
    deriveEq typeInfo `shouldSatisfy` isRight

  it "fails for type with no constructors" $ do
    let typeInfo = mkEnumType "Empty" []
    deriveEq typeInfo `shouldSatisfy` isLeft

  it "fails for type with parameterized constructors" $ do
    let typeInfo = mkSumType "Option" [("Some", [simpleType "Int"]), ("None", [])]
    deriveEq typeInfo `shouldSatisfy` isLeft

-- =============================================================================
-- Ord Derivation Tests
-- =============================================================================

deriveOrdTests :: Spec
deriveOrdTests = describe "deriveOrd" $ do
  it "derives Ord when Eq is implemented" $ do
    let typeInfo = mkEnumType "Ordering" ["Less", "Equal", "Greater"]
    -- First derive Eq, then try Ord
    case deriveEq typeInfo of
      Right eqImpl -> do
        let ctx = registerImpl eqImpl withPrelude
        deriveOrd typeInfo ctx `shouldSatisfy` isRight
      Left err -> expectationFailure $ "Eq derivation failed: " ++ show err

  it "generates Ord methods" $ do
    let typeInfo = mkEnumType "Priority" ["Low", "Medium", "High"]
    case deriveEq typeInfo of
      Right eqImpl -> do
        let ctx = registerImpl eqImpl withPrelude
        case deriveOrd typeInfo ctx of
          Right impl -> do
            let methodNames = map fst (implInfoMethods impl)
            methodNames `shouldContain` ["compare"]
            methodNames `shouldContain` ["lt"]
            methodNames `shouldContain` ["le"]
            methodNames `shouldContain` ["gt"]
            methodNames `shouldContain` ["ge"]
          Left err -> expectationFailure $ "Ord derivation failed: " ++ show err
      Left err -> expectationFailure $ "Eq derivation failed: " ++ show err

  it "fails when Eq is not implemented" $ do
    let typeInfo = mkEnumType "Priority" ["Low", "Medium", "High"]
    -- Don't derive Eq first
    deriveOrd typeInfo withPrelude `shouldSatisfy` isLeft

  it "fails for type with no constructors" $ do
    let typeInfo = mkEnumType "Empty" []
    deriveOrd typeInfo withPrelude `shouldSatisfy` isLeft

-- =============================================================================
-- Process Deriving Clauses Tests
-- =============================================================================

processDerivingTests :: Spec
processDerivingTests = describe "processDerivingClauses" $ do
  it "processes single Eq derivation" $ do
    let typeInfo = mkEnumType "Status" ["Active", "Inactive", "Pending"]
    case processDerivingClauses ["Eq"] typeInfo withPrelude of
      Right ctx -> do
        lookupImpl "Eq" (simpleType "Status") ctx `shouldSatisfy` isJust
      Left errs -> expectationFailure $ "Derivation failed: " ++ show errs

  it "processes Eq and Ord together" $ do
    let typeInfo = mkEnumType "Priority" ["Low", "Medium", "High"]
    case processDerivingClauses ["Eq", "Ord"] typeInfo withPrelude of
      Right ctx -> do
        lookupImpl "Eq" (simpleType "Priority") ctx `shouldSatisfy` isJust
        lookupImpl "Ord" (simpleType "Priority") ctx `shouldSatisfy` isJust
      Left errs -> expectationFailure $ "Derivation failed: " ++ show errs

  it "processes Ord before Eq fails (dependency order)" $ do
    let typeInfo = mkEnumType "Priority" ["Low", "Medium", "High"]
    -- Ord needs Eq, so if we process Ord first without Eq, it should fail
    case processDerivingClauses ["Ord"] typeInfo withPrelude of
      Right _ -> expectationFailure "Expected failure when deriving Ord without Eq"
      Left errs -> do
        length errs `shouldBe` 1
        case head errs of
          MissingSuperTrait _ _ "Eq" -> pure ()
          other -> expectationFailure $ "Expected MissingSuperTrait, got: " ++ show other

  it "registers type before deriving" $ do
    let typeInfo = mkEnumType "Action" ["Read", "Write", "Delete"]
    let ctx = registerType typeInfo withPrelude
    case processDerivingClauses ["Eq"] typeInfo ctx of
      Right ctx' -> do
        lookupType "Action" ctx' `shouldSatisfy` isJust
        lookupImpl "Eq" (simpleType "Action") ctx' `shouldSatisfy` isJust
      Left errs -> expectationFailure $ "Derivation failed: " ++ show errs

-- =============================================================================
-- Error Case Tests
-- =============================================================================

errorCaseTests :: Spec
errorCaseTests = describe "error cases" $ do
  it "reports UnsupportedTrait for unknown traits" $ do
    let typeInfo = mkEnumType "Color" ["Red", "Green", "Blue"]
    case deriveImpl "Show" typeInfo withPrelude of
      Left (UnsupportedTrait "Show" "Color") -> pure ()
      Left other -> expectationFailure $ "Expected UnsupportedTrait, got: " ++ show other
      Right _ -> expectationFailure "Expected failure for unsupported trait"

  it "reports NoConstructors for empty types" $ do
    let typeInfo = mkEnumType "Empty" []
    case deriveEq typeInfo of
      Left (NoConstructors "Eq" "Empty") -> pure ()
      Left other -> expectationFailure $ "Expected NoConstructors, got: " ++ show other
      Right _ -> expectationFailure "Expected failure for empty type"

  it "reports ParameterizedConstructor for complex types" $ do
    let typeInfo = mkSumType "Maybe" [("Just", [simpleType "Int"]), ("Nothing", [])]
    case deriveEq typeInfo of
      Left (ParameterizedConstructor "Eq" "Maybe" "Just") -> pure ()
      Left other -> expectationFailure $ "Expected ParameterizedConstructor, got: " ++ show other
      Right _ -> expectationFailure "Expected failure for parameterized constructor"

  it "formats error messages correctly" $ do
    let err1 = UnsupportedTrait "Show" "Color"
    formatDeriveError err1 `shouldBe`
      "Cannot derive Show for Color: trait is not derivable"

    let err2 = NoConstructors "Eq" "Empty"
    formatDeriveError err2 `shouldBe`
      "Cannot derive Eq for Empty: type has no constructors"

    let err3 = MissingSuperTrait "Ord" "Priority" "Eq"
    formatDeriveError err3 `shouldBe`
      "Cannot derive Ord for Priority: missing required supertrait Eq"

    let err4 = ParameterizedConstructor "Eq" "Maybe" "Just"
    formatDeriveError err4 `shouldBe`
      "Cannot derive Eq for Maybe: constructor Just has parameters (only simple enums are supported)"

-- =============================================================================
-- Helpers
-- =============================================================================

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False
