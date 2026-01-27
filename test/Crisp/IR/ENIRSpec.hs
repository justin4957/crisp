{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.IR.ENIRSpec
-- Description : Tests for Effect-Normalized IR transformation
--
-- Tests for transforming Core terms to ENIR, including handler
-- transformation with CPS-style resumptions.

module Crisp.IR.ENIRSpec (spec) where

import Test.Hspec

import Crisp.IR.ENIR
import Crisp.Core.Term

import Data.Text (Text)

spec :: Spec
spec = do
  basicTransformTests
  handlerTransformTests
  operationHandlerTests
  returnHandlerTests
  integrationTests

-- =============================================================================
-- Basic Transformation Tests
-- =============================================================================

basicTransformTests :: Spec
basicTransformTests = describe "basic toENIR transformation" $ do
  it "transforms variable to return" $ do
    let term = TmVar "x" 0
    case toENIR term of
      ENIRReturn (ENIRVVar "x" 0) -> pure ()
      other -> expectationFailure $ "Expected ENIRReturn, got: " ++ show other

  it "transforms lambda to return of lambda value" $ do
    let term = TmLam "x" (simpleType "Int") (TmVar "x" 0)
    case toENIR term of
      ENIRReturn (ENIRVLam "x" _ _) -> pure ()
      other -> expectationFailure $ "Expected lambda value, got: " ++ show other

  it "transforms constructor to return of constructor value" $ do
    let term = TmCon "Just" [simpleType "Int"] [TmVar "x" 0]
    case toENIR term of
      ENIRReturn (ENIRVCon "Just" [ENIRVVar "x" 0]) -> pure ()
      other -> expectationFailure $ "Expected constructor value, got: " ++ show other

  it "transforms let binding" $ do
    let term = TmLet "x" (simpleType "Int") (TmVar "y" 0) (TmVar "x" 0)
    case toENIR term of
      ENIRLet "x" _ _ _ -> pure ()
      other -> expectationFailure $ "Expected let, got: " ++ show other

  it "transforms perform to effect call with continuation" $ do
    let term = TmPerform "State" "get" (TmCon "Unit" [] [])
    case toENIR term of
      ENIRCall "State" "get" _ (Continuation "result" _) -> pure ()
      other -> expectationFailure $ "Expected effect call, got: " ++ show other

  it "erases type abstraction" $ do
    let term = TmTyAbs "A" (KiType 0) (TmVar "x" 0)
    case toENIR term of
      ENIRReturn (ENIRVVar "x" 0) -> pure ()
      other -> expectationFailure $ "Expected erased type abs, got: " ++ show other

  it "erases type application" $ do
    let term = TmTyApp (TmVar "f" 0) (simpleType "Int")
    case toENIR term of
      ENIRReturn (ENIRVVar "f" 0) -> pure ()
      other -> expectationFailure $ "Expected erased type app, got: " ++ show other

-- =============================================================================
-- Handler Transformation Tests
-- =============================================================================

handlerTransformTests :: Spec
handlerTransformTests = describe "handler transformation" $ do
  it "transforms simple handler" $ do
    let handler = mkSimpleHandler "State" [("get", "x", TmVar "x" 0)]
    let term = TmHandle handler (TmVar "body" 0)
    case toENIR term of
      ENIRHandle h _ -> do
        enirHandlerEffect h `shouldBe` "State"
        length (enirHandlerOps h) `shouldBe` 1
      other -> expectationFailure $ "Expected handle, got: " ++ show other

  it "transforms handler with multiple operations" $ do
    let handler = mkSimpleHandler "State"
          [ ("get", "x", TmVar "state" 0)
          , ("put", "newState", TmVar "newState" 0)
          ]
    let term = TmHandle handler (TmVar "body" 0)
    case toENIR term of
      ENIRHandle h _ -> do
        enirHandlerEffect h `shouldBe` "State"
        length (enirHandlerOps h) `shouldBe` 2
        map enirOpName (enirHandlerOps h) `shouldBe` ["get", "put"]
      other -> expectationFailure $ "Expected handle, got: " ++ show other

  it "preserves handler body" $ do
    let handler = mkSimpleHandler "IO" [("print", "msg", TmCon "Unit" [] [])]
    let term = TmHandle handler (TmPerform "IO" "print" (TmVar "hello" 0))
    case toENIR term of
      ENIRHandle _ (ENIRCall "IO" "print" _ _) -> pure ()
      other -> expectationFailure $ "Expected handle with call body, got: " ++ show other

  it "transforms nested handlers" $ do
    let innerHandler = mkSimpleHandler "State" [("get", "x", TmVar "x" 0)]
    let outerHandler = mkSimpleHandler "IO" [("print", "msg", TmCon "Unit" [] [])]
    let term = TmHandle outerHandler (TmHandle innerHandler (TmVar "body" 0))
    case toENIR term of
      ENIRHandle outer (ENIRHandle inner _) -> do
        enirHandlerEffect outer `shouldBe` "IO"
        enirHandlerEffect inner `shouldBe` "State"
      other -> expectationFailure $ "Expected nested handles, got: " ++ show other

-- =============================================================================
-- Operation Handler Tests
-- =============================================================================

operationHandlerTests :: Spec
operationHandlerTests = describe "operation handler transformation" $ do
  it "extracts operation name" $ do
    let opHandler = mkOpHandler "get" "arg" "resume" (TmVar "state" 0)
    let enirOp = transformOpHandler opHandler
    enirOpName enirOp `shouldBe` "get"

  it "extracts parameter name from variable pattern" $ do
    let opHandler = mkOpHandler "put" "newState" "resume" (TmVar "newState" 0)
    let enirOp = transformOpHandler opHandler
    enirOpParam enirOp `shouldBe` "newState"

  it "extracts resumption name" $ do
    let opHandler = mkOpHandler "ask" "x" "k" (TmVar "k" 0)
    let enirOp = transformOpHandler opHandler
    enirOpResume enirOp `shouldBe` "k"

  it "transforms operation body" $ do
    let opHandler = mkOpHandler "get" "x" "resume"
          (TmApp (TmVar "resume" 0) (TmVar "state" 1))
    let enirOp = transformOpHandler opHandler
    case enirOpBody enirOp of
      ENIRApp _ _ -> pure ()
      other -> expectationFailure $ "Expected app in body, got: " ++ show other

  it "handles wildcard pattern" $ do
    let opHandler = OpHandler
          { opHandlerOperation = "tick"
          , opHandlerPattern = PatWild
          , opHandlerResume = "k"
          , opHandlerBody = TmVar "k" 0
          }
    let enirOp = transformOpHandler opHandler
    enirOpParam enirOp `shouldBe` "_"

  it "handles constructor pattern" $ do
    let opHandler = OpHandler
          { opHandlerOperation = "handle"
          , opHandlerPattern = PatCon "Some" [PatVar "x"]
          , opHandlerResume = "k"
          , opHandlerBody = TmVar "x" 0
          }
    let enirOp = transformOpHandler opHandler
    enirOpParam enirOp `shouldBe` "_arg"

-- =============================================================================
-- Return Handler Tests
-- =============================================================================

returnHandlerTests :: Spec
returnHandlerTests = describe "return handler transformation" $ do
  it "extracts return parameter name" $ do
    let handler = mkHandlerWithReturn "IO" [] "result" (TmVar "result" 0)
    let enirHandler = transformHandler handler
    enirReturnParam (enirHandlerReturn enirHandler) `shouldBe` "result"

  it "transforms return body" $ do
    let handler = mkHandlerWithReturn "State" [] "x"
          (TmCon "Pair" [] [TmVar "x" 0, TmVar "state" 1])
    let enirHandler = transformHandler handler
    case enirReturnBody (enirHandlerReturn enirHandler) of
      ENIRReturn (ENIRVCon "Pair" _) -> pure ()
      other -> expectationFailure $ "Expected constructor return, got: " ++ show other

  it "handles identity return" $ do
    let handler = mkHandlerWithReturn "Reader" [] "x" (TmVar "x" 0)
    let enirHandler = transformHandler handler
    case enirReturnBody (enirHandlerReturn enirHandler) of
      ENIRReturn (ENIRVVar "x" 0) -> pure ()
      other -> expectationFailure $ "Expected identity return, got: " ++ show other

-- =============================================================================
-- Integration Tests
-- =============================================================================

integrationTests :: Spec
integrationTests = describe "integration" $ do
  it "transforms state handler idiom" $ do
    -- handle State { get(_) -> resume(state), put(s) -> { state = s; resume(()) } }
    --   in body
    let handler = mkSimpleHandler "State"
          [ ("get", "_", TmApp (TmVar "resume" 0) (TmVar "state" 1))
          , ("put", "s", TmLet "state" (simpleType "Int") (TmVar "s" 0)
                           (TmApp (TmVar "resume" 1) (TmCon "Unit" [] [])))
          ]
    let term = TmHandle handler (TmPerform "State" "get" (TmCon "Unit" [] []))
    case toENIR term of
      ENIRHandle h (ENIRCall "State" "get" _ _) -> do
        enirHandlerEffect h `shouldBe` "State"
        length (enirHandlerOps h) `shouldBe` 2
      other -> expectationFailure $ "Expected state handler, got: " ++ show other

  it "transforms reader handler idiom" $ do
    -- handle Reader { ask(_) -> resume(env) } in body
    let handler = mkSimpleHandler "Reader"
          [ ("ask", "_", TmApp (TmVar "resume" 0) (TmVar "env" 1))
          ]
    let term = TmHandle handler (TmPerform "Reader" "ask" (TmCon "Unit" [] []))
    case toENIR term of
      ENIRHandle h _ -> do
        enirHandlerEffect h `shouldBe` "Reader"
        let askOp = head (enirHandlerOps h)
        enirOpName askOp `shouldBe` "ask"
        enirOpResume askOp `shouldBe` "resume"
      other -> expectationFailure $ "Expected reader handler, got: " ++ show other

  it "transforms exception handler idiom" $ do
    -- handle Exn { raise(e) -> None } with return(x) -> Some(x) in body
    let handler = Handler
          { handlerEffect = "Exn"
          , handlerIntroducedEffects = EffEmpty
          , handlerOperations =
              [ OpHandler "raise" (PatVar "e") "resume"
                  (TmCon "None" [] [])
              ]
          , handlerReturn = ReturnHandler (PatVar "x")
              (TmCon "Some" [] [TmVar "x" 0])
          }
    let term = TmHandle handler (TmVar "computation" 0)
    case toENIR term of
      ENIRHandle h _ -> do
        enirHandlerEffect h `shouldBe` "Exn"
        -- Check operation handler
        let raiseOp = head (enirHandlerOps h)
        enirOpName raiseOp `shouldBe` "raise"
        case enirOpBody raiseOp of
          ENIRReturn (ENIRVCon "None" []) -> pure ()
          other -> expectationFailure $ "Expected None, got: " ++ show other
        -- Check return handler
        case enirReturnBody (enirHandlerReturn h) of
          ENIRReturn (ENIRVCon "Some" _) -> pure ()
          other -> expectationFailure $ "Expected Some, got: " ++ show other
      other -> expectationFailure $ "Expected exception handler, got: " ++ show other

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- | Create a simple handler with operations and identity return
mkSimpleHandler :: Text -> [(Text, Text, Term)] -> Handler
mkSimpleHandler effect ops = Handler
  { handlerEffect = effect
  , handlerIntroducedEffects = EffEmpty
  , handlerOperations = map mkOp ops
  , handlerReturn = ReturnHandler (PatVar "x") (TmVar "x" 0)
  }
  where
    mkOp (opName, param, body) = OpHandler opName (PatVar param) "resume" body

-- | Create a handler with custom return clause
mkHandlerWithReturn :: Text -> [(Text, Text, Term)] -> Text -> Term -> Handler
mkHandlerWithReturn effect ops returnParam returnBody = Handler
  { handlerEffect = effect
  , handlerIntroducedEffects = EffEmpty
  , handlerOperations = map mkOp ops
  , handlerReturn = ReturnHandler (PatVar returnParam) returnBody
  }
  where
    mkOp (opName, param, body) = OpHandler opName (PatVar param) "resume" body

-- | Create an operation handler
mkOpHandler :: Text -> Text -> Text -> Term -> OpHandler
mkOpHandler opName param resume body = OpHandler
  { opHandlerOperation = opName
  , opHandlerPattern = PatVar param
  , opHandlerResume = resume
  , opHandlerBody = body
  }
