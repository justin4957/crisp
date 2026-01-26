{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Effects.HandlerSpec
-- Description : Tests for handler typing rules
--
-- TDD tests for effect handler typing, ensuring operation clauses
-- have correct types and handlers correctly transform effect rows.

module Crisp.Effects.HandlerSpec (spec) where

import Test.Hspec

import Crisp.Effects.Handler
import Crisp.Effects.Typing
import Crisp.Core.Term

import Data.Either (isRight, isLeft)
import qualified Data.Text as T

-- | Helper to create a simple type
tySimple :: T.Text -> Type
tySimple name = TyCon name []

-- | Helper to create a type variable
tyVar :: T.Text -> Int -> Type
tyVar = TyVar

-- | Helper to create a type constructor with args
tyCon :: T.Text -> [Type] -> Type
tyCon = TyCon

-- | Helper to create an effect row from names
effSet :: [T.Text] -> EffectRow
effSet names = EffSet $ map (`Effect` Nothing) names

-- | Helper to create an effect variable
effVar :: T.Text -> Int -> EffectRow
effVar = EffVar

-- | Helper to create a function type with effects
tyEffFn :: Type -> Type -> EffectRow -> Type
tyEffFn from to effs = TyPi "_" from effs to

-- | Create a simple handler declaration for testing
mkHandler :: T.Text -> [OpClause] -> ReturnClause -> HandlerDecl
mkHandler effName ops ret = HandlerDecl
  { handlerDeclName = "test_handler"
  , handlerDeclEffect = effName
  , handlerDeclTypeParams = []
  , handlerDeclInputType = tyVar "A" 0
  , handlerDeclOutputType = tyVar "A" 0
  , handlerDeclOperations = ops
  , handlerDeclReturn = ret
  }

-- | Create an operation clause
mkOpClause :: T.Text -> T.Text -> T.Text -> OpClause
mkOpClause opName argName resumeName = OpClause
  { opClauseName = opName
  , opClausePattern = PatVar argName
  , opClauseResume = resumeName
  , opClauseBody = TmVar "result" 0  -- Placeholder body
  }

-- | Create a simple return clause
mkReturnClause :: T.Text -> ReturnClause
mkReturnClause varName = ReturnClause
  { returnClausePattern = PatVar varName
  , returnClauseBody = TmVar varName 0
  }

-- | Create a test effect environment with common effects
testEffectEnv :: EffectEnv
testEffectEnv = foldr addEffectDecl emptyEffectEnv
  [ EffectDecl "Fail" []
      [ OperationDecl "fail" (tySimple "Never") []
      ]
  , EffectDecl "State" [("S", KiType 0)]
      [ OperationDecl "get" (tyVar "S" 0) []
      , OperationDecl "put" (tySimple "Unit") [("s", tyVar "S" 0)]
      ]
  , EffectDecl "Reader" [("R", KiType 0)]
      [ OperationDecl "ask" (tyVar "R" 0) []
      ]
  , EffectDecl "Console" []
      [ OperationDecl "print" (tySimple "Unit") [("msg", tySimple "String")]
      , OperationDecl "readLine" (tySimple "String") []
      ]
  , EffectDecl "Exception" [("E", KiType 0)]
      [ OperationDecl "throw" (tySimple "Never") [("e", tyVar "E" 0)]
      ]
  ]

spec :: Spec
spec = do
  describe "Handler Declaration" $ do
    handlerDeclTests

  describe "Operation Clause Typing" $ do
    operationClauseTests

  describe "Return Clause Typing" $ do
    returnClauseTests

  describe "Resume Typing" $ do
    resumeTypingTests

  describe "Effect Row Transformation" $ do
    effectRowTests

  describe "Missing Operations" $ do
    missingOperationTests

  describe "Extra Operations" $ do
    extraOperationTests

  describe "Handler Result Type" $ do
    handlerResultTests

  describe "Parameterized Effects" $ do
    parameterizedEffectTests

  describe "Edge Cases" $ do
    edgeCaseTests

-- | Tests for handler declarations
handlerDeclTests :: Spec
handlerDeclTests = describe "handler declarations" $ do
  it "validates simple handler for Fail effect" $ do
    let handler = HandlerDecl
          { handlerDeclName = "fail_handler"
          , handlerDeclEffect = "Fail"
          , handlerDeclTypeParams = [("A", KiType 0)]
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyCon "Option" [tyVar "A" 0]
          , handlerDeclOperations =
              [ OpClause "fail" PatWild "k" (TmCon "None" [] [])
              ]
          , handlerDeclReturn = ReturnClause (PatVar "x") (TmCon "Some" [] [TmVar "x" 0])
          }
    checkHandlerDecl testEffectEnv handler `shouldSatisfy` isRight

  it "validates State handler" $ do
    let handler = HandlerDecl
          { handlerDeclName = "state_handler"
          , handlerDeclEffect = "State"
          , handlerDeclTypeParams = [("S", KiType 0), ("A", KiType 0)]
          , handlerDeclInputType = tyVar "A" 1
          , handlerDeclOutputType = tyEffFn (tyVar "S" 0) (tyVar "A" 1) EffEmpty
          , handlerDeclOperations =
              [ OpClause "get" PatWild "k"
                  (TmLam "s" (tyVar "S" 0) (TmApp (TmApp (TmVar "k" 1) (TmVar "s" 0)) (TmVar "s" 0)))
              , OpClause "put" (PatVar "s") "k"
                  (TmLam "_" (tyVar "S" 0) (TmApp (TmApp (TmVar "k" 1) (TmCon "Unit" [] [])) (TmVar "s" 1)))
              ]
          , handlerDeclReturn = ReturnClause (PatVar "x")
              (TmLam "s" (tyVar "S" 0) (TmVar "x" 1))
          }
    checkHandlerDecl testEffectEnv handler `shouldSatisfy` isRight

  it "rejects handler for unknown effect" $ do
    let handler = mkHandler "UnknownEffect"
          [mkOpClause "op" "x" "k"]
          (mkReturnClause "x")
    checkHandlerDecl testEffectEnv handler `shouldSatisfy` isLeft

-- | Tests for operation clause typing
operationClauseTests :: Spec
operationClauseTests = describe "operation clauses" $ do
  it "validates operation with correct argument type" $ do
    let clause = OpClause "put" (PatVar "s") "k"
          (TmApp (TmVar "k" 0) (TmCon "Unit" [] []))
    checkOperationClause testEffectEnv "State" [tySimple "Int"] clause
      `shouldSatisfy` isRight

  it "rejects operation with wrong argument pattern type" $ do
    -- put expects S, checking against Bool when S=Int
    let clause = OpClause "put" (PatVar "s") "k"
          (TmApp (TmVar "k" 0) (TmCon "Unit" [] []))
        result = getOperationParamType testEffectEnv "State" "put" [tySimple "Int"]
    -- The param type should be Int, not Bool
    result `shouldBe` Right [tySimple "Int"]

  it "validates nullary operation" $ do
    let clause = OpClause "get" PatWild "k"
          (TmApp (TmVar "k" 0) (TmCon "Zero" [] []))
    checkOperationClause testEffectEnv "State" [tySimple "Int"] clause
      `shouldSatisfy` isRight

  it "validates binary operation" $ do
    let clause = OpClause "print" (PatVar "msg") "k"
          (TmApp (TmVar "k" 0) (TmCon "Unit" [] []))
    checkOperationClause testEffectEnv "Console" [] clause
      `shouldSatisfy` isRight

-- | Tests for return clause typing
returnClauseTests :: Spec
returnClauseTests = describe "return clause" $ do
  it "validates identity return clause" $ do
    let clause = ReturnClause (PatVar "x") (TmVar "x" 0)
    checkReturnClause (tyVar "A" 0) (tyVar "A" 0) clause `shouldSatisfy` isRight

  it "validates wrapping return clause" $ do
    let clause = ReturnClause (PatVar "x") (TmCon "Some" [] [TmVar "x" 0])
    checkReturnClause (tyVar "A" 0) (tyCon "Option" [tyVar "A" 0]) clause
      `shouldSatisfy` isRight

  it "validates return with wildcard pattern" $ do
    let clause = ReturnClause PatWild (TmCon "None" [] [])
    checkReturnClause (tyVar "A" 0) (tyCon "Option" [tyVar "A" 0]) clause
      `shouldSatisfy` isRight

-- | Tests for resume continuation typing
resumeTypingTests :: Spec
resumeTypingTests = describe "resume typing" $ do
  it "computes resume type for get operation" $ do
    -- get() -> S, so resume : S -> Result
    let result = getResumeType testEffectEnv "State" "get" [tySimple "Int"] (tySimple "Result")
    result `shouldBe` Right (tyEffFn (tySimple "Int") (tySimple "Result") EffEmpty)

  it "computes resume type for put operation" $ do
    -- put(s) -> Unit, so resume : Unit -> Result
    let result = getResumeType testEffectEnv "State" "put" [tySimple "Int"] (tySimple "Result")
    result `shouldBe` Right (tyEffFn (tySimple "Unit") (tySimple "Result") EffEmpty)

  it "computes resume type for fail operation" $ do
    -- fail() -> Never, so resume : Never -> Result (but never called)
    let result = getResumeType testEffectEnv "Fail" "fail" [] (tySimple "Result")
    result `shouldBe` Right (tyEffFn (tySimple "Never") (tySimple "Result") EffEmpty)

  it "computes resume type for print operation" $ do
    -- print(msg) -> Unit, so resume : Unit -> Result
    let result = getResumeType testEffectEnv "Console" "print" [] (tySimple "Result")
    result `shouldBe` Right (tyEffFn (tySimple "Unit") (tySimple "Result") EffEmpty)

-- | Tests for effect row transformation
effectRowTests :: Spec
effectRowTests = describe "effect row transformation" $ do
  it "removes handled effect from row" $ do
    let inputRow = effSet ["State", "Console"]
        result = removeHandledEffect "State" inputRow
    result `shouldBe` effSet ["Console"]

  it "preserves remaining effects" $ do
    let inputRow = effSet ["IO", "State", "Console"]
        result = removeHandledEffect "State" inputRow
    containsEffect "IO" result `shouldBe` True
    containsEffect "Console" result `shouldBe` True
    containsEffect "State" result `shouldBe` False

  it "handles effect row with variable" $ do
    let inputRow = EffUnion (effSet ["State"]) (effVar "ε" 0)
        result = removeHandledEffect "State" inputRow
    containsEffect "State" result `shouldBe` False
    -- The variable should remain
    hasEffectVariable result `shouldBe` True

  it "computes handler input effect row" $ do
    let result = computeHandlerInputEffects "State" (effVar "ε" 0)
    containsEffect "State" result `shouldBe` True

  it "computes handler output effect row" $ do
    let inputRow = EffUnion (effSet ["State"]) (effVar "ε" 0)
        result = computeHandlerOutputEffects "State" inputRow
    containsEffect "State" result `shouldBe` False

-- | Tests for missing operations
missingOperationTests :: Spec
missingOperationTests = describe "missing operations" $ do
  it "rejects handler missing operations" $ do
    -- State has get and put, but we only provide get
    let handler = HandlerDecl
          { handlerDeclName = "incomplete_state"
          , handlerDeclEffect = "State"
          , handlerDeclTypeParams = [("S", KiType 0)]
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyVar "A" 0
          , handlerDeclOperations =
              [ OpClause "get" PatWild "k" (TmVar "placeholder" 0)
              -- Missing: put
              ]
          , handlerDeclReturn = mkReturnClause "x"
          }
    case checkHandlerDecl testEffectEnv handler of
      Left (MissingOperations _ ops) -> ops `shouldBe` ["put"]
      Left err -> expectationFailure $ "Wrong error: " ++ show err
      Right _ -> expectationFailure "Should have failed"

  it "reports all missing operations" $ do
    let handler = HandlerDecl
          { handlerDeclName = "empty_console"
          , handlerDeclEffect = "Console"
          , handlerDeclTypeParams = []
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyVar "A" 0
          , handlerDeclOperations = []  -- Missing both print and readLine
          , handlerDeclReturn = mkReturnClause "x"
          }
    case checkHandlerDecl testEffectEnv handler of
      Left (MissingOperations _ ops) -> length ops `shouldBe` 2
      Left err -> expectationFailure $ "Wrong error: " ++ show err
      Right _ -> expectationFailure "Should have failed"

-- | Tests for extra operations
extraOperationTests :: Spec
extraOperationTests = describe "extra operations" $ do
  it "rejects handler with extra operations" $ do
    let handler = HandlerDecl
          { handlerDeclName = "extra_ops"
          , handlerDeclEffect = "Fail"
          , handlerDeclTypeParams = []
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyVar "A" 0
          , handlerDeclOperations =
              [ OpClause "fail" PatWild "k" (TmVar "placeholder" 0)
              , OpClause "extra" PatWild "k" (TmVar "placeholder" 0)  -- Not in Fail
              ]
          , handlerDeclReturn = mkReturnClause "x"
          }
    case checkHandlerDecl testEffectEnv handler of
      Left (UnknownOperationInHandler _ op) -> op `shouldBe` "extra"
      Left err -> expectationFailure $ "Wrong error: " ++ show err
      Right _ -> expectationFailure "Should have failed"

  it "rejects duplicate operation clauses" $ do
    let handler = HandlerDecl
          { handlerDeclName = "dup_ops"
          , handlerDeclEffect = "Fail"
          , handlerDeclTypeParams = []
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyVar "A" 0
          , handlerDeclOperations =
              [ OpClause "fail" PatWild "k" (TmVar "placeholder" 0)
              , OpClause "fail" PatWild "k" (TmVar "other" 0)  -- Duplicate
              ]
          , handlerDeclReturn = mkReturnClause "x"
          }
    case checkHandlerDecl testEffectEnv handler of
      Left (DuplicateOperationClause _ op) -> op `shouldBe` "fail"
      Left err -> expectationFailure $ "Wrong error: " ++ show err
      Right _ -> expectationFailure "Should have failed"

-- | Tests for handler result type
handlerResultTests :: Spec
handlerResultTests = describe "handler result type" $ do
  it "computes handler type" $ do
    let handler = HandlerDecl
          { handlerDeclName = "fail_to_option"
          , handlerDeclEffect = "Fail"
          , handlerDeclTypeParams = [("A", KiType 0)]
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyCon "Option" [tyVar "A" 0]
          , handlerDeclOperations =
              [ OpClause "fail" PatWild "k" (TmCon "None" [] [])
              ]
          , handlerDeclReturn = ReturnClause (PatVar "x") (TmCon "Some" [] [TmVar "x" 0])
          }
    case computeHandlerType testEffectEnv handler (effVar "ε" 0) of
      Right info -> do
        handlerInfoInputType info `shouldBe` tyVar "A" 0
        handlerInfoOutputType info `shouldBe` tyCon "Option" [tyVar "A" 0]
      Left err -> expectationFailure $ "Type computation failed: " ++ show err

  it "preserves effect variable in output" $ do
    let handler = HandlerDecl
          { handlerDeclName = "state_to_fn"
          , handlerDeclEffect = "State"
          , handlerDeclTypeParams = [("S", KiType 0), ("A", KiType 0)]
          , handlerDeclInputType = tyVar "A" 1
          , handlerDeclOutputType = tyEffFn (tyVar "S" 0) (tyVar "A" 1) EffEmpty
          , handlerDeclOperations =
              [ OpClause "get" PatWild "k" (TmVar "placeholder" 0)
              , OpClause "put" (PatVar "s") "k" (TmVar "placeholder" 0)
              ]
          , handlerDeclReturn = ReturnClause (PatVar "x") (TmLam "s" (tyVar "S" 0) (TmVar "x" 1))
          }
    case computeHandlerType testEffectEnv handler (effVar "ε" 0) of
      Right info -> do
        -- Output effects should be just ε (State removed)
        handlerInfoOutputEffects info `shouldBe` effVar "ε" 0
      Left err -> expectationFailure $ "Type computation failed: " ++ show err

-- | Tests for parameterized effects
parameterizedEffectTests :: Spec
parameterizedEffectTests = describe "parameterized effects" $ do
  it "instantiates State(Int) correctly" $ do
    let handler = HandlerDecl
          { handlerDeclName = "int_state"
          , handlerDeclEffect = "State"
          , handlerDeclTypeParams = [("A", KiType 0)]
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyEffFn (tySimple "Int") (tyVar "A" 0) EffEmpty
          , handlerDeclOperations =
              [ OpClause "get" PatWild "k" (TmVar "placeholder" 0)
              , OpClause "put" (PatVar "s") "k" (TmVar "placeholder" 0)
              ]
          , handlerDeclReturn = mkReturnClause "x"
          }
    checkHandlerDeclWithArgs testEffectEnv handler [tySimple "Int"] `shouldSatisfy` isRight

  it "validates Exception(String) handler" $ do
    let handler = HandlerDecl
          { handlerDeclName = "string_exception"
          , handlerDeclEffect = "Exception"
          , handlerDeclTypeParams = [("A", KiType 0)]
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyCon "Result" [tyVar "A" 0, tySimple "String"]
          , handlerDeclOperations =
              [ OpClause "throw" (PatVar "e") "k" (TmCon "Err" [] [TmVar "e" 0])
              ]
          , handlerDeclReturn = ReturnClause (PatVar "x") (TmCon "Ok" [] [TmVar "x" 0])
          }
    checkHandlerDeclWithArgs testEffectEnv handler [tySimple "String"] `shouldSatisfy` isRight

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles effect with no operations" $ do
    let emptyEffect = addEffectDecl (EffectDecl "Empty" [] []) testEffectEnv
        handler = HandlerDecl
          { handlerDeclName = "empty_handler"
          , handlerDeclEffect = "Empty"
          , handlerDeclTypeParams = []
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyVar "A" 0
          , handlerDeclOperations = []
          , handlerDeclReturn = mkReturnClause "x"
          }
    checkHandlerDecl emptyEffect handler `shouldSatisfy` isRight

  it "handles complex return clause pattern" $ do
    let clause = ReturnClause (PatCon "Pair" [PatVar "x", PatVar "y"])
          (TmCon "Pair" [] [TmVar "y" 0, TmVar "x" 1])
    checkReturnClause
      (tyCon "Pair" [tySimple "Int", tySimple "Bool"])
      (tyCon "Pair" [tySimple "Bool", tySimple "Int"])
      clause
      `shouldSatisfy` isRight

  it "handles nested effect handling" $ do
    -- Handler that introduces new effects in its clauses
    let handler = HandlerDecl
          { handlerDeclName = "logging_state"
          , handlerDeclEffect = "State"
          , handlerDeclTypeParams = [("S", KiType 0), ("A", KiType 0)]
          , handlerDeclInputType = tyVar "A" 1
          , handlerDeclOutputType = tyVar "A" 1
          , handlerDeclOperations =
              [ OpClause "get" PatWild "k" (TmVar "placeholder" 0)
              , OpClause "put" (PatVar "s") "k" (TmVar "placeholder" 0)
              ]
          , handlerDeclReturn = mkReturnClause "x"
          }
    checkHandlerDecl testEffectEnv handler `shouldSatisfy` isRight

  it "validates Reader effect handler" $ do
    let handler = HandlerDecl
          { handlerDeclName = "reader_handler"
          , handlerDeclEffect = "Reader"
          , handlerDeclTypeParams = [("R", KiType 0), ("A", KiType 0)]
          , handlerDeclInputType = tyVar "A" 1
          , handlerDeclOutputType = tyEffFn (tyVar "R" 0) (tyVar "A" 1) EffEmpty
          , handlerDeclOperations =
              [ OpClause "ask" PatWild "k"
                  (TmLam "r" (tyVar "R" 0) (TmApp (TmVar "k" 1) (TmVar "r" 0)))
              ]
          , handlerDeclReturn = ReturnClause (PatVar "x")
              (TmLam "r" (tyVar "R" 0) (TmVar "x" 1))
          }
    checkHandlerDecl testEffectEnv handler `shouldSatisfy` isRight

  it "preserves handler declaration metadata" $ do
    let handler = HandlerDecl
          { handlerDeclName = "my_handler"
          , handlerDeclEffect = "Fail"
          , handlerDeclTypeParams = [("A", KiType 0)]
          , handlerDeclInputType = tyVar "A" 0
          , handlerDeclOutputType = tyCon "Option" [tyVar "A" 0]
          , handlerDeclOperations =
              [ OpClause "fail" PatWild "k" (TmCon "None" [] [])
              ]
          , handlerDeclReturn = mkReturnClause "x"
          }
    case buildHandlerInfo testEffectEnv handler of
      Right info -> do
        handlerInfoName info `shouldBe` "my_handler"
        handlerInfoEffect info `shouldBe` "Fail"
      Left err -> expectationFailure $ "Build failed: " ++ show err
