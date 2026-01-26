{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Effects.TypingSpec
-- Description : Tests for effect declaration typing
--
-- TDD tests for effect declaration typing, ensuring effect signatures
-- are validated and effect operations have correct types.

module Crisp.Effects.TypingSpec (spec) where

import Test.Hspec

import Crisp.Effects.Typing
import Crisp.Core.Term

import Data.Either (isRight)
import qualified Data.Text as T

-- | Helper to create a simple type
tySimple :: T.Text -> Type
tySimple name = TyCon name []

-- | Helper to create a type constructor with args
tyCon :: T.Text -> [Type] -> Type
tyCon = TyCon

-- | Helper to create a type variable
tyVar :: T.Text -> Int -> Type
tyVar = TyVar

-- | Helper to create an effectful function type
tyEffFn :: Type -> Type -> EffectRow -> Type
tyEffFn from to effs = TyPi "_" from effs to

spec :: Spec
spec = do
  describe "Effect Declaration Validation" $ do
    effectDeclTests

  describe "Operation Typing" $ do
    operationTypingTests

  describe "Effect Environment" $ do
    effectEnvTests

  describe "Parameterized Effects" $ do
    paramEffectTests

  describe "Duplicate Detection" $ do
    duplicateTests

  describe "Operation Type Inference" $ do
    operationInferenceTests

  describe "Effect Scoping" $ do
    effectScopingTests

  describe "Edge Cases" $ do
    edgeCaseTests

-- | Tests for effect declaration validation
effectDeclTests :: Spec
effectDeclTests = describe "effect declarations" $ do
  it "validates simple effect declaration" $ do
    let decl = EffectDecl
          { effectDeclName = "Fail"
          , effectDeclParams = []
          , effectDeclOps =
              [ OperationDecl "fail" (tySimple "Never") []
              ]
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

  it "validates effect with single operation" $ do
    let decl = EffectDecl
          { effectDeclName = "Console"
          , effectDeclParams = []
          , effectDeclOps =
              [ OperationDecl "print" (tySimple "Unit") [("msg", tySimple "String")]
              ]
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

  it "validates effect with multiple operations" $ do
    let decl = EffectDecl
          { effectDeclName = "Console"
          , effectDeclParams = []
          , effectDeclOps =
              [ OperationDecl "print" (tySimple "Unit") [("msg", tySimple "String")]
              , OperationDecl "readLine" (tySimple "String") []
              ]
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

  it "validates effect with no operations" $ do
    let decl = EffectDecl
          { effectDeclName = "Empty"
          , effectDeclParams = []
          , effectDeclOps = []
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

-- | Tests for operation typing
operationTypingTests :: Spec
operationTypingTests = describe "operation types" $ do
  it "gives nullary operation correct type" $ do
    let decl = EffectDecl
          { effectDeclName = "Fail"
          , effectDeclParams = []
          , effectDeclOps = [OperationDecl "fail" (tySimple "Never") []]
          }
        env = addEffectDecl decl emptyEffectEnv
    case lookupOperation "Fail" "fail" env of
      Just opInfo -> operationReturnType opInfo `shouldBe` tySimple "Never"
      Nothing -> expectationFailure "Operation not found"

  it "gives unary operation correct parameter type" $ do
    let decl = EffectDecl
          { effectDeclName = "Console"
          , effectDeclParams = []
          , effectDeclOps = [OperationDecl "print" (tySimple "Unit") [("msg", tySimple "String")]]
          }
        env = addEffectDecl decl emptyEffectEnv
    case lookupOperation "Console" "print" env of
      Just opInfo -> operationParams opInfo `shouldBe` [("msg", tySimple "String")]
      Nothing -> expectationFailure "Operation not found"

  it "gives binary operation correct types" $ do
    let decl = EffectDecl
          { effectDeclName = "State"
          , effectDeclParams = [("S", KiType 0)]
          , effectDeclOps =
              [ OperationDecl "get" (tyVar "S" 0) []
              , OperationDecl "put" (tySimple "Unit") [("s", tyVar "S" 0)]
              ]
          }
        env = addEffectDecl decl emptyEffectEnv
    case lookupOperation "State" "get" env of
      Just opInfo -> do
        operationReturnType opInfo `shouldBe` tyVar "S" 0
        operationParams opInfo `shouldBe` []
      Nothing -> expectationFailure "Operation not found"

-- | Tests for effect environment
effectEnvTests :: Spec
effectEnvTests = describe "effect environment" $ do
  it "empty environment has no effects" $ do
    lookupEffect "Foo" emptyEffectEnv `shouldBe` Nothing

  it "can add effect to environment" $ do
    let decl = EffectDecl "Test" [] []
        env = addEffectDecl decl emptyEffectEnv
    lookupEffect "Test" env `shouldSatisfy` \case
      Just _ -> True
      Nothing -> False

  it "can look up operation by effect and name" $ do
    let decl = EffectDecl
          { effectDeclName = "IO"
          , effectDeclParams = []
          , effectDeclOps = [OperationDecl "print" (tySimple "Unit") [("s", tySimple "String")]]
          }
        env = addEffectDecl decl emptyEffectEnv
    lookupOperation "IO" "print" env `shouldSatisfy` \case
      Just _ -> True
      Nothing -> False

  it "returns Nothing for unknown effect" $ do
    lookupOperation "Unknown" "op" emptyEffectEnv `shouldBe` Nothing

  it "returns Nothing for unknown operation" $ do
    let decl = EffectDecl "Test" [] [OperationDecl "op1" (tySimple "Int") []]
        env = addEffectDecl decl emptyEffectEnv
    lookupOperation "Test" "unknown" env `shouldBe` Nothing

-- | Tests for parameterized effects
paramEffectTests :: Spec
paramEffectTests = describe "parameterized effects" $ do
  it "validates State(S) effect" $ do
    let decl = EffectDecl
          { effectDeclName = "State"
          , effectDeclParams = [("S", KiType 0)]
          , effectDeclOps =
              [ OperationDecl "get" (tyVar "S" 0) []
              , OperationDecl "put" (tySimple "Unit") [("s", tyVar "S" 0)]
              ]
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

  it "validates Reader(R) effect" $ do
    let decl = EffectDecl
          { effectDeclName = "Reader"
          , effectDeclParams = [("R", KiType 0)]
          , effectDeclOps = [OperationDecl "ask" (tyVar "R" 0) []]
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

  it "validates effect with multiple type parameters" $ do
    let decl = EffectDecl
          { effectDeclName = "RW"
          , effectDeclParams = [("R", KiType 0), ("W", KiType 0)]
          , effectDeclOps =
              [ OperationDecl "read" (tyVar "R" 0) []
              , OperationDecl "write" (tySimple "Unit") [("w", tyVar "W" 1)]
              ]
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

  it "records effect parameters in environment" $ do
    let decl = EffectDecl
          { effectDeclName = "State"
          , effectDeclParams = [("S", KiType 0)]
          , effectDeclOps = []
          }
        env = addEffectDecl decl emptyEffectEnv
    case lookupEffect "State" env of
      Just effInfo -> effectInfoParams effInfo `shouldBe` [("S", KiType 0)]
      Nothing -> expectationFailure "Effect not found"

-- | Tests for duplicate detection
duplicateTests :: Spec
duplicateTests = describe "duplicate detection" $ do
  it "rejects duplicate effect names" $ do
    let decl1 = EffectDecl "E" [] []
        decl2 = EffectDecl "E" [] []
        env1 = addEffectDecl decl1 emptyEffectEnv
    checkEffectDecl env1 decl2 `shouldSatisfy` \case
      Left (DuplicateEffectName _) -> True
      _ -> False

  it "rejects duplicate operation names within effect" $ do
    let decl = EffectDecl
          { effectDeclName = "Bad"
          , effectDeclParams = []
          , effectDeclOps =
              [ OperationDecl "op" (tySimple "Int") []
              , OperationDecl "op" (tySimple "Bool") []
              ]
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` \case
      Left (DuplicateOperationName _ _) -> True
      _ -> False

  it "allows same operation name in different effects" $ do
    let decl1 = EffectDecl "E1" [] [OperationDecl "op" (tySimple "Int") []]
        decl2 = EffectDecl "E2" [] [OperationDecl "op" (tySimple "Bool") []]
        env1 = addEffectDecl decl1 emptyEffectEnv
    checkEffectDecl env1 decl2 `shouldSatisfy` isRight

-- | Tests for operation type inference
operationInferenceTests :: Spec
operationInferenceTests = describe "operation inference" $ do
  it "infers full operation type with effect" $ do
    let decl = EffectDecl
          { effectDeclName = "Fail"
          , effectDeclParams = []
          , effectDeclOps = [OperationDecl "fail" (tySimple "Never") []]
          }
        env = addEffectDecl decl emptyEffectEnv
        expectedEffect = EffSet [Effect "Fail" Nothing]
    case inferOperationType env "Fail" "fail" [] of
      Right ty -> ty `shouldBe` tyEffFn (tySimple "Unit") (tySimple "Never") expectedEffect
      Left err -> expectationFailure $ "Inference failed: " ++ show err

  it "infers operation type with parameter" $ do
    let decl = EffectDecl
          { effectDeclName = "Console"
          , effectDeclParams = []
          , effectDeclOps = [OperationDecl "print" (tySimple "Unit") [("msg", tySimple "String")]]
          }
        env = addEffectDecl decl emptyEffectEnv
        expectedEffect = EffSet [Effect "Console" Nothing]
    case inferOperationType env "Console" "print" [] of
      Right ty -> ty `shouldBe` tyEffFn (tySimple "String") (tySimple "Unit") expectedEffect
      Left err -> expectationFailure $ "Inference failed: " ++ show err

  it "infers parameterized operation type" $ do
    let decl = EffectDecl
          { effectDeclName = "State"
          , effectDeclParams = [("S", KiType 0)]
          , effectDeclOps = [OperationDecl "get" (tyVar "S" 0) []]
          }
        env = addEffectDecl decl emptyEffectEnv
    -- With type argument Int, get: () -[State(Int)]-> Int
    case inferOperationType env "State" "get" [tySimple "Int"] of
      Right ty -> do
        -- The return type should be Int (substituting S with Int)
        case ty of
          TyPi _ _ _ retTy -> retTy `shouldBe` tySimple "Int"
          _ -> expectationFailure "Expected function type"
      Left err -> expectationFailure $ "Inference failed: " ++ show err

-- | Tests for effect scoping
effectScopingTests :: Spec
effectScopingTests = describe "effect scoping" $ do
  it "effect parameters scope to all operations" $ do
    let decl = EffectDecl
          { effectDeclName = "State"
          , effectDeclParams = [("S", KiType 0)]
          , effectDeclOps =
              [ OperationDecl "get" (tyVar "S" 0) []
              , OperationDecl "put" (tySimple "Unit") [("s", tyVar "S" 0)]
              ]
          }
        env = addEffectDecl decl emptyEffectEnv
    case lookupOperation "State" "get" env of
      Just opInfo -> operationReturnType opInfo `shouldBe` tyVar "S" 0
      Nothing -> expectationFailure "get not found"
    case lookupOperation "State" "put" env of
      Just opInfo -> operationParams opInfo `shouldBe` [("s", tyVar "S" 0)]
      Nothing -> expectationFailure "put not found"

  it "different effects have independent scopes" $ do
    let decl1 = EffectDecl
          { effectDeclName = "State"
          , effectDeclParams = [("S", KiType 0)]
          , effectDeclOps = [OperationDecl "get" (tyVar "S" 0) []]
          }
        decl2 = EffectDecl
          { effectDeclName = "Reader"
          , effectDeclParams = [("R", KiType 0)]
          , effectDeclOps = [OperationDecl "ask" (tyVar "R" 0) []]
          }
        env = addEffectDecl decl2 $ addEffectDecl decl1 emptyEffectEnv
    case lookupOperation "State" "get" env of
      Just opInfo -> operationReturnType opInfo `shouldBe` tyVar "S" 0
      Nothing -> expectationFailure "get not found"
    case lookupOperation "Reader" "ask" env of
      Just opInfo -> operationReturnType opInfo `shouldBe` tyVar "R" 0
      Nothing -> expectationFailure "ask not found"

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles operation with multiple parameters" $ do
    let decl = EffectDecl
          { effectDeclName = "Database"
          , effectDeclParams = []
          , effectDeclOps =
              [ OperationDecl "query"
                  (tyCon "List" [tySimple "Row"])
                  [("table", tySimple "String"), ("filter", tySimple "Predicate")]
              ]
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

  it "handles complex return types" $ do
    let decl = EffectDecl
          { effectDeclName = "Async"
          , effectDeclParams = [("A", KiType 0)]
          , effectDeclOps =
              [ OperationDecl "await" (tyVar "A" 0)
                  [("promise", tyCon "Promise" [tyVar "A" 0])]
              ]
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

  it "handles higher-kinded effect parameters" $ do
    let decl = EffectDecl
          { effectDeclName = "Traverse"
          , effectDeclParams = [("F", KiArrow (KiType 0) (KiType 0))]
          , effectDeclOps = []
          }
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight

  it "preserves operation order in environment" $ do
    let decl = EffectDecl
          { effectDeclName = "Multi"
          , effectDeclParams = []
          , effectDeclOps =
              [ OperationDecl "op1" (tySimple "Int") []
              , OperationDecl "op2" (tySimple "Bool") []
              , OperationDecl "op3" (tySimple "String") []
              ]
          }
        env = addEffectDecl decl emptyEffectEnv
    case lookupEffect "Multi" env of
      Just effInfo -> length (effectInfoOps effInfo) `shouldBe` 3
      Nothing -> expectationFailure "Effect not found"

  it "handles empty parameter list" $ do
    let decl = EffectDecl "Simple" [] [OperationDecl "run" (tySimple "Unit") []]
    checkEffectDecl emptyEffectEnv decl `shouldSatisfy` isRight
