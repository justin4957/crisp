{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.IR.CPSSpec
-- Description : Tests for CPS transformation of effects
--
-- TDD tests for the CPS (Continuation-Passing Style) transformation
-- that converts effect operations and handlers into explicit
-- continuation manipulation.

module Crisp.IR.CPSSpec (spec) where

import Test.Hspec

import Crisp.IR.CPS
import Crisp.Core.Term
import qualified Data.Text as T

-- | Helper to create a simple type
tySimple :: T.Text -> Type
tySimple name = TyCon name []

-- | Helper to create a variable term
var :: T.Text -> Int -> Term
var = TmVar

-- | Helper to create a lambda
lam :: T.Text -> Type -> Term -> Term
lam = TmLam

-- | Helper to create an application
app :: Term -> Term -> Term
app = TmApp

-- | Helper to create a let binding
letIn :: T.Text -> Type -> Term -> Term -> Term
letIn = TmLet

-- | Helper to create a constructor
con :: T.Text -> [Term] -> Term
con name args = TmCon name [] args

-- | Helper to create a perform (effect operation)
perform :: T.Text -> T.Text -> Term -> Term
perform = TmPerform

-- | Helper to create a pattern variable
patVar :: T.Text -> Pattern
patVar = PatVar

-- | Helper to create a wildcard pattern
patWild :: Pattern
patWild = PatWild

-- | Helper to create a constructor pattern
patCon :: T.Text -> [Pattern] -> Pattern
patCon = PatCon

-- | Helper to create a match case
matchCase :: Pattern -> Term -> Case
matchCase = Case

-- | Helper to create a simple handler
simpleHandler :: T.Text -> [(T.Text, Pattern, T.Text, Term)] -> Pattern -> Term -> Handler
simpleHandler effName ops retPat retBody = Handler
  { handlerEffect = effName
  , handlerIntroducedEffects = EffEmpty
  , handlerOperations = map toOpHandler ops
  , handlerReturn = ReturnHandler retPat retBody
  }
  where
    toOpHandler (opName, opPat, resumeName, opBody) = OpHandler
      { opHandlerOperation = opName
      , opHandlerPattern = opPat
      , opHandlerResume = resumeName
      , opHandlerBody = opBody
      }

spec :: Spec
spec = do
  describe "CPS Transformation" $ do

    describe "simple value transformation" $ do
      it "transforms variable to CPS" $ do
        let term = var "x" 0
            result = cpsTransform term
        -- Variable should wrap in CPS structure
        cpsIsValue result `shouldBe` True

      it "transforms constructor to CPS" $ do
        let term = con "True" []
            result = cpsTransform term
        cpsIsValue result `shouldBe` True

      it "transforms unit to CPS" $ do
        let term = con "Unit" []
            result = cpsTransform term
        cpsIsValue result `shouldBe` True

      it "preserves variable name in CPS value" $ do
        let term = var "myVar" 3
            result = cpsTransform term
        cpsGetVarName result `shouldBe` Just "myVar"

    describe "lambda transformation" $ do
      it "transforms identity lambda" $ do
        let term = lam "x" (tySimple "Int") (var "x" 0)
            result = cpsTransform term
        cpsIsLambda result `shouldBe` True

      it "transforms lambda with body in CPS" $ do
        let term = lam "x" (tySimple "Int") (var "x" 0)
            result = cpsTransform term
        cpsLambdaHasCPSBody result `shouldBe` True

      it "adds continuation parameter to lambda" $ do
        let term = lam "x" (tySimple "Int") (var "x" 0)
            result = cpsTransform term
        cpsLambdaHasCont result `shouldBe` True

    describe "application transformation" $ do
      it "transforms simple application" $ do
        let term = app (var "f" 0) (var "x" 1)
            result = cpsTransform term
        cpsIsApp result `shouldBe` True

      it "evaluates function first in application" $ do
        let term = app (var "f" 0) (var "x" 1)
            result = cpsTransform term
        cpsAppEvalsFuncFirst result `shouldBe` True

      it "evaluates argument after function" $ do
        let term = app (var "f" 0) (var "x" 1)
            result = cpsTransform term
        cpsAppEvalsArgSecond result `shouldBe` True

      it "passes continuation to function call" $ do
        let term = app (var "f" 0) (var "x" 1)
            result = cpsTransform term
        cpsAppPassesCont result `shouldBe` True

    describe "let binding transformation" $ do
      it "transforms let binding" $ do
        let term = letIn "x" (tySimple "Int") (con "Zero" []) (var "x" 0)
            result = cpsTransform term
        cpsIsLet result `shouldBe` True

      it "evaluates let value in CPS" $ do
        let term = letIn "x" (tySimple "Int") (var "y" 0) (var "x" 0)
            result = cpsTransform term
        cpsLetValueIsCPS result `shouldBe` True

      it "body receives bound variable" $ do
        let term = letIn "x" (tySimple "Int") (con "Zero" []) (var "x" 0)
            result = cpsTransform term
        cpsLetBodyUsesVar result `shouldBe` True

    describe "effect operation transformation" $ do
      it "transforms perform to continuation capture" $ do
        let term = perform "State" "get" (con "Unit" [])
            result = cpsTransform term
        cpsIsPerform result `shouldBe` True

      it "captures continuation at perform site" $ do
        let term = perform "State" "get" (con "Unit" [])
            result = cpsTransform term
        cpsPerformHasCont result `shouldBe` True

      it "records effect name in perform" $ do
        let term = perform "State" "get" (con "Unit" [])
            result = cpsTransform term
        cpsPerformEffectName result `shouldBe` Just "State"

      it "records operation name in perform" $ do
        let term = perform "State" "get" (con "Unit" [])
            result = cpsTransform term
        cpsPerformOpName result `shouldBe` Just "get"

      it "transforms perform argument" $ do
        let term = perform "State" "put" (var "newState" 0)
            result = cpsTransform term
        cpsPerformHasArg result `shouldBe` True

    describe "effect in expression context" $ do
      it "sequences effect before use" $ do
        -- get() then use result
        let getTerm = perform "State" "get" (con "Unit" [])
            term = letIn "s" (tySimple "Int") getTerm (var "s" 0)
            result = cpsTransform term
        cpsSequencesEffect result `shouldBe` True

      it "nests continuations correctly" $ do
        -- let x = get(); let y = get(); ...
        let get1 = perform "State" "get" (con "Unit" [])
            get2 = perform "State" "get" (con "Unit" [])
            term = letIn "x" (tySimple "Int") get1
                     (letIn "y" (tySimple "Int") get2
                       (var "x" 1))
            result = cpsTransform term
        cpsHasNestedConts result `shouldBe` True

    describe "handler transformation" $ do
      it "transforms handler to CPS handler" $ do
        let handler = simpleHandler "State"
              [("get", patWild, "k", app (var "k" 0) (var "init" 2))]
              (patVar "x") (var "x" 0)
            body = var "computation" 0
            term = TmHandle handler body
            result = cpsTransform term
        cpsIsHandler result `shouldBe` True

      it "handler captures body continuation" $ do
        let handler = simpleHandler "Exc"
              [("throw", patVar "e", "k", con "None" [])]
              (patVar "x") (con "Some" [var "x" 0])
            body = con "Unit" []
            term = TmHandle handler body
            result = cpsTransform term
        cpsHandlerHasBodyCont result `shouldBe` True

      it "handler operation clauses have continuation access" $ do
        let handler = simpleHandler "State"
              [("get", patWild, "resume", app (var "resume" 0) (var "init" 2))]
              (patVar "x") (var "x" 0)
            body = perform "State" "get" (con "Unit" [])
            term = TmHandle handler body
            result = cpsTransform term
        cpsHandlerOpsHaveCont result `shouldBe` True

      it "return clause is CPS-transformed" $ do
        let handler = simpleHandler "State"
              [("get", patWild, "k", app (var "k" 0) (con "Zero" []))]
              (patVar "result") (var "result" 0)
            body = con "Unit" []
            term = TmHandle handler body
            result = cpsTransform term
        cpsHandlerReturnIsCPS result `shouldBe` True

    describe "resume continuation" $ do
      it "resume is available in operation clause" $ do
        let handler = simpleHandler "Choice"
              [("choose", patWild, "resume", app (var "resume" 0) (con "True" []))]
              (patVar "x") (var "x" 0)
            body = perform "Choice" "choose" (con "Unit" [])
            term = TmHandle handler body
            result = cpsTransform term
        cpsResumeAvailable result `shouldBe` True

      it "resume invokes captured continuation" $ do
        let handler = simpleHandler "State"
              [("get", patWild, "k", app (var "k" 0) (var "state" 1))]
              (patVar "x") (var "x" 0)
            body = perform "State" "get" (con "Unit" [])
            term = TmHandle handler body
            result = cpsTransform term
        cpsResumeInvokesCont result `shouldBe` True

      it "non-resuming handler can discard continuation" $ do
        let handler = simpleHandler "Exc"
              [("throw", patVar "e", "k", con "Error" [var "e" 0])]  -- k not used
              (patVar "x") (con "Ok" [var "x" 0])
            body = perform "Exc" "throw" (con "SomeError" [])
            term = TmHandle handler body
            result = cpsTransform term
        cpsCanDiscardCont result `shouldBe` True

    describe "multi-shot continuations" $ do
      it "continuation can be called multiple times" $ do
        -- choose() -> resume(true) + resume(false)
        let resumeTrue = app (var "k" 0) (con "True" [])
            resumeFalse = app (var "k" 0) (con "False" [])
            multiResume = app (app (var "add" 2) resumeTrue) resumeFalse
            handler = simpleHandler "Choice"
              [("choose", patWild, "k", multiResume)]
              (patVar "x") (var "x" 0)
            body = perform "Choice" "choose" (con "Unit" [])
            term = TmHandle handler body
            result = cpsTransform term
        cpsSupportsMultiShot result `shouldBe` True

      it "multi-shot generates independent results" $ do
        let handler = simpleHandler "Choice"
              [("choose", patWild, "k",
                -- Both branches independently
                letIn "r1" (tySimple "Int") (app (var "k" 0) (con "True" []))
                  (letIn "r2" (tySimple "Int") (app (var "k" 1) (con "False" []))
                    (app (app (var "add" 4) (var "r1" 1)) (var "r2" 0))))]
              (patVar "x") (var "x" 0)
            body = perform "Choice" "choose" (con "Unit" [])
            term = TmHandle handler body
            result = cpsTransform term
        cpsMultiShotIndependent result `shouldBe` True

    describe "nested handlers" $ do
      it "handles nested handlers correctly" $ do
        let innerHandler = simpleHandler "State"
              [("get", patWild, "k", app (var "k" 0) (con "Zero" []))]
              (patVar "x") (var "x" 0)
            outerHandler = simpleHandler "Exc"
              [("throw", patVar "e", "k", con "None" [])]
              (patVar "x") (con "Some" [var "x" 0])
            innerBody = perform "State" "get" (con "Unit" [])
            nested = TmHandle innerHandler innerBody
            term = TmHandle outerHandler nested
            result = cpsTransform term
        cpsNestedHandlersCorrect result `shouldBe` True

      it "inner handler sees outer handler" $ do
        let innerHandler = simpleHandler "State"
              [("get", patWild, "k",
                -- Inner handler can throw!
                perform "Exc" "throw" (con "NoState" []))]
              (patVar "x") (var "x" 0)
            outerHandler = simpleHandler "Exc"
              [("throw", patVar "e", "k", con "Error" [var "e" 0])]
              (patVar "x") (con "Ok" [var "x" 0])
            body = perform "State" "get" (con "Unit" [])
            innerTerm = TmHandle innerHandler body
            term = TmHandle outerHandler innerTerm
            result = cpsTransform term
        cpsInnerSeesOuter result `shouldBe` True

      it "handler order matters" $ do
        let h1 = simpleHandler "E1"
              [("op1", patWild, "k", var "r1" 0)]
              (patVar "x") (var "x" 0)
            h2 = simpleHandler "E2"
              [("op2", patWild, "k", var "r2" 0)]
              (patVar "x") (var "x" 0)
            body = con "Unit" []
            t1 = TmHandle h1 (TmHandle h2 body)
            t2 = TmHandle h2 (TmHandle h1 body)
            r1 = cpsTransform t1
            r2 = cpsTransform t2
        cpsHandlerOrderMatters r1 r2 `shouldBe` True

    describe "evaluation order preservation" $ do
      it "preserves left-to-right evaluation" $ do
        -- f(get(), put(x)) should evaluate get before put
        let getOp = perform "State" "get" (con "Unit" [])
            putOp = perform "State" "put" (var "x" 0)
            term = app (app (var "f" 0) getOp) putOp
            result = cpsTransform term
        cpsPreservesLR result `shouldBe` True

      it "preserves evaluation order in let" $ do
        let get1 = perform "State" "get" (con "Unit" [])
            get2 = perform "State" "get" (con "Unit" [])
            term = letIn "a" (tySimple "Int") get1
                     (letIn "b" (tySimple "Int") get2
                       (var "a" 1))
            result = cpsTransform term
        cpsPreservesLetOrder result `shouldBe` True

    describe "type annotation handling" $ do
      it "erases type annotations in CPS" $ do
        let term = TmAnnot (var "x" 0) (tySimple "Int")
            result = cpsTransform term
        cpsErasesAnnot result `shouldBe` True

      it "preserves underlying term" $ do
        let inner = var "x" 0
            term = TmAnnot inner (tySimple "Int")
            result = cpsTransform term
        cpsPreservesAnnotatedTerm result inner `shouldBe` True

    describe "lazy and force transformation" $ do
      it "transforms lazy to lambda" $ do
        let term = TmLazy (var "x" 0)
            result = cpsTransform term
        cpsLazyIsLambda result `shouldBe` True

      it "transforms force to application" $ do
        let term = TmForce (var "thunk" 0)
            result = cpsTransform term
        cpsForceIsApp result `shouldBe` True

    describe "match transformation" $ do
      it "transforms match scrutinee first" $ do
        let term = TmMatch (var "x" 0) (tySimple "Bool")
                     [matchCase (patCon "True" []) (con "One" []),
                      matchCase (patCon "False" []) (con "Zero" [])]
            result = cpsTransform term
        cpsMatchScrutineeFirst result `shouldBe` True

      it "CPS-transforms each branch" $ do
        let term = TmMatch (var "x" 0) (tySimple "Bool")
                     [matchCase (patCon "True" []) (var "a" 0),
                      matchCase (patCon "False" []) (var "b" 1)]
            result = cpsTransform term
        cpsBranchesAreCPS result `shouldBe` True

      it "continuation flows to each branch" $ do
        let term = TmMatch (var "x" 0) (tySimple "Bool")
                     [matchCase (patCon "True" []) (var "t" 0),
                      matchCase (patCon "False" []) (var "f" 1)]
            result = cpsTransform term
        cpsContFlowsToBranches result `shouldBe` True

    describe "CPS well-formedness" $ do
      it "all effect calls have continuations" $ do
        let term = letIn "s" (tySimple "Int")
                     (perform "State" "get" (con "Unit" []))
                     (perform "State" "put" (var "s" 0))
            result = cpsTransform term
        cpsAllPerformsHaveCont result `shouldBe` True

      it "no dangling continuations" $ do
        let term = perform "Exc" "throw" (con "Error" [])
            result = cpsTransform term
        cpsNoDanglingConts result `shouldBe` True

      it "continuation names are unique" $ do
        let get1 = perform "State" "get" (con "Unit" [])
            get2 = perform "State" "get" (con "Unit" [])
            term = letIn "a" (tySimple "Int") get1
                     (letIn "b" (tySimple "Int") get2
                       (var "a" 1))
            result = cpsTransform term
        cpsContNamesUnique result `shouldBe` True

    describe "edge cases" $ do
      it "handles empty handler (no operations)" $ do
        let handler = simpleHandler "Empty" [] (patVar "x") (var "x" 0)
            body = con "Unit" []
            term = TmHandle handler body
            result = cpsTransform term
        cpsHandlesEmptyHandler result `shouldBe` True

      it "handles deeply nested effects" $ do
        let get = perform "State" "get" (con "Unit" [])
            nested = letIn "a" (tySimple "Int") get
                       (letIn "b" (tySimple "Int") get
                         (letIn "c" (tySimple "Int") get
                           (var "a" 2)))
            result = cpsTransform nested
        cpsHandlesDeepNesting result `shouldBe` True

      it "handles type abstraction (erasure)" $ do
        let term = TmTyAbs "a" (KiType 0) (var "x" 0)
            result = cpsTransform term
        cpsTyAbsErased result `shouldBe` True

      it "handles type application (erasure)" $ do
        let term = TmTyApp (var "id" 0) (tySimple "Int")
            result = cpsTransform term
        cpsTyAppErased result `shouldBe` True
