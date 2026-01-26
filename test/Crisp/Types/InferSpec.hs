{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.InferSpec
-- Description : Tests for type inference
--
-- TDD tests for type inference of let-bindings, including generalization
-- and instantiation for let-polymorphism.

module Crisp.Types.InferSpec (spec) where

import Test.Hspec

import Crisp.Types.Infer
import Crisp.Types.Substitution
import Crisp.Core.Term

import Data.Either (isRight, isLeft)
import qualified Data.Text as T
import qualified Data.Set as Set

-- | Helper to create a type variable
tyVar :: T.Text -> Int -> Type
tyVar = TyVar

-- | Helper to create a simple type (no args)
tySimple :: T.Text -> Type
tySimple name = TyCon name []

-- | Helper to create a type constructor with args
tyCon :: T.Text -> [Type] -> Type
tyCon = TyCon

-- | Helper to create a function type (pure)
tyFn :: Type -> Type -> Type
tyFn a b = TyPi "_" a EffEmpty b

-- | Helper to create a forall type
_tyForall :: T.Text -> Kind -> Type -> Type
_tyForall = TyForall

-- | Helper for term variable
tmVar :: T.Text -> Int -> Term
tmVar = TmVar

-- | Helper for term lambda
tmLam :: T.Text -> Type -> Term -> Term
tmLam = TmLam

-- | Helper for term let
tmLet :: T.Text -> Type -> Term -> Term -> Term
tmLet = TmLet

-- | Helper for term application
tmApp :: Term -> Term -> Term
tmApp = TmApp

-- | Helper for term constructor
tmCon :: T.Text -> [Type] -> [Term] -> Term
tmCon = TmCon

-- | Helper for checking inference success
shouldInferType :: Either InferError (Type, Substitution) -> Type -> Expectation
shouldInferType result expectedTy = case result of
  Right (ty, _) ->
    -- Check if types are equivalent (up to alpha-renaming of variables)
    typesEquivalent ty expectedTy `shouldBe` True
  Left err -> expectationFailure $ "Inference failed: " ++ show err

-- | Check if two types are equivalent (same structure, possibly different var names)
typesEquivalent :: Type -> Type -> Bool
typesEquivalent t1 t2 = case (t1, t2) of
  (TyCon n1 args1, TyCon n2 args2) ->
    n1 == n2 && length args1 == length args2 &&
    all (uncurry typesEquivalent) (zip args1 args2)
  (TyPi _ p1 _ r1, TyPi _ p2 _ r2) ->
    typesEquivalent p1 p2 && typesEquivalent r1 r2
  (TyVar _ _, TyVar _ _) -> True  -- Variables are equivalent
  (TyLazy i1, TyLazy i2) -> typesEquivalent i1 i2
  (TyRef i1, TyRef i2) -> typesEquivalent i1 i2
  (TyRefMut i1, TyRefMut i2) -> typesEquivalent i1 i2
  (TyLinear i1, TyLinear i2) -> typesEquivalent i1 i2
  (TyForall _ k1 b1, TyForall _ k2 b2) -> k1 == k2 && typesEquivalent b1 b2
  (TyUniverse l1, TyUniverse l2) -> l1 == l2
  (TyProp, TyProp) -> True
  _ -> t1 == t2

-- | Helper for checking inference succeeds (kept for future tests)
_shouldInferSuccessfully :: Either InferError (Type, Substitution) -> Expectation
_shouldInferSuccessfully result = result `shouldSatisfy` isRight

-- | Helper for checking inference fails (kept for future tests)
_shouldFailInference :: Either InferError (Type, Substitution) -> Expectation
_shouldFailInference result = result `shouldSatisfy` isLeft

spec :: Spec
spec = do
  describe "Simple Inference" $ do
    simpleInferenceTests
    literalInferenceTests
    variableInferenceTests

  describe "Function Inference" $ do
    lambdaInferenceTests
    applicationInferenceTests

  describe "Let-Binding Inference" $ do
    simpleLetTests
    polymorphicLetTests
    annotatedLetTests

  describe "Generalization" $ do
    generalizationTests

  describe "Instantiation" $ do
    instantiationTests

  describe "Type Scheme Operations" $ do
    schemeTests

  describe "Free Type Variables" $ do
    freeVarsTests

  describe "Edge Cases" $ do
    edgeCaseTests

-- | Tests for simple inference
simpleInferenceTests :: Spec
simpleInferenceTests = describe "simple terms" $ do
  it "infers type of unit constructor" $ do
    let term = tmCon "Unit" [] []
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Unit"

  it "infers type of True constructor" $ do
    let term = tmCon "True" [] []
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Bool"

  it "infers type of False constructor" $ do
    let term = tmCon "False" [] []
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Bool"

-- | Tests for literal inference (represented as constructors)
literalInferenceTests :: Spec
literalInferenceTests = describe "literals" $ do
  it "infers Int for integer literals" $ do
    -- Integer literals are represented as TmCon "Int" ...
    let term = tmCon "Int" [] []
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Int"

-- | Tests for variable inference
variableInferenceTests :: Spec
variableInferenceTests = describe "variables" $ do
  it "infers type of bound variable" $ do
    let env = extendInferEnv "x" (monoScheme (tySimple "Int")) emptyInferEnv
        term = tmVar "x" 0
    infer env term `shouldInferType` tySimple "Int"

  it "fails on unbound variable" $ do
    let term = tmVar "unknown" 0
        env = emptyInferEnv
    infer env term `shouldSatisfy` isLeft

  it "infers polymorphic variable after instantiation" $ do
    -- x : forall a. a -> a
    let scheme = Scheme (Set.singleton 0) (tyFn (tyVar "a" 0) (tyVar "a" 0))
        env = extendInferEnv "id" scheme emptyInferEnv
        term = tmVar "id" 0
    -- Should instantiate to a fresh type variable
    infer env term `shouldSatisfy` isRight

-- | Tests for lambda inference
lambdaInferenceTests :: Spec
lambdaInferenceTests = describe "lambda expressions" $ do
  it "infers identity function type" $ do
    -- \x: a. x  should have type a -> a
    let term = tmLam "x" (tyVar "a" 0) (tmVar "x" 0)
        env = emptyInferEnv
    let result = infer env term
    result `shouldSatisfy` isRight
    case result of
      Right (TyPi _ paramTy _ retTy, _) ->
        typesEquivalent paramTy retTy `shouldBe` True
      Right (other, _) -> expectationFailure $ "Expected function type, got: " ++ show other
      Left err -> expectationFailure $ "Inference failed: " ++ show err

  it "infers constant function type" $ do
    -- \x: Int. \y: Bool. x  should have type Int -> Bool -> Int
    let inner = tmLam "y" (tySimple "Bool") (tmVar "x" 1)
        term = tmLam "x" (tySimple "Int") inner
        env = emptyInferEnv
    let result = infer env term
    result `shouldSatisfy` isRight

  it "infers lambda with application in body" $ do
    -- \f: Int -> Bool. \x: Int. f x
    let fTy = tyFn (tySimple "Int") (tySimple "Bool")
        body = tmApp (tmVar "f" 1) (tmVar "x" 0)
        inner = tmLam "x" (tySimple "Int") body
        term = tmLam "f" fTy inner
        env = emptyInferEnv
    infer env term `shouldSatisfy` isRight

-- | Tests for application inference
applicationInferenceTests :: Spec
applicationInferenceTests = describe "application" $ do
  it "infers application result type" $ do
    -- (f : Int -> Bool) applied to (x : Int) gives Bool
    let fScheme = monoScheme (tyFn (tySimple "Int") (tySimple "Bool"))
        xScheme = monoScheme (tySimple "Int")
        env = extendInferEnv "x" xScheme $
              extendInferEnv "f" fScheme emptyInferEnv
        term = tmApp (tmVar "f" 1) (tmVar "x" 0)
    infer env term `shouldInferType` tySimple "Bool"

  it "fails on type mismatch in application" $ do
    -- (f : Int -> Bool) applied to (x : String) should fail
    let fScheme = monoScheme (tyFn (tySimple "Int") (tySimple "Bool"))
        xScheme = monoScheme (tySimple "String")
        env = extendInferEnv "x" xScheme $
              extendInferEnv "f" fScheme emptyInferEnv
        term = tmApp (tmVar "f" 1) (tmVar "x" 0)
    infer env term `shouldSatisfy` isLeft

  it "fails when applying non-function" $ do
    -- 42 applied to something should fail
    let xScheme = monoScheme (tySimple "Int")
        yScheme = monoScheme (tySimple "Bool")
        env = extendInferEnv "y" yScheme $
              extendInferEnv "x" xScheme emptyInferEnv
        term = tmApp (tmVar "x" 1) (tmVar "y" 0)
    infer env term `shouldSatisfy` isLeft

-- | Tests for simple let-bindings
simpleLetTests :: Spec
simpleLetTests = describe "simple let-bindings" $ do
  it "infers let-bound variable in body" $ do
    -- let x: Int = 42 in x  should have type Int
    let term = tmLet "x" (tySimple "Int") (tmCon "Int" [] []) (tmVar "x" 0)
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Int"

  it "infers nested let-bindings" $ do
    -- let x: Int = 42 in let y: Bool = True in x
    let inner = tmLet "y" (tySimple "Bool") (tmCon "True" [] []) (tmVar "x" 1)
        term = tmLet "x" (tySimple "Int") (tmCon "Int" [] []) inner
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Int"

  it "shadows outer bindings" $ do
    -- let x: Int = 42 in let x: Bool = True in x
    let inner = tmLet "x" (tySimple "Bool") (tmCon "True" [] []) (tmVar "x" 0)
        term = tmLet "x" (tySimple "Int") (tmCon "Int" [] []) inner
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Bool"

  it "uses let-bound function" $ do
    -- let f: Int -> Bool = ... in f 42
    let fTy = tyFn (tySimple "Int") (tySimple "Bool")
        fBody = tmLam "x" (tySimple "Int") (tmCon "True" [] [])
        body = tmApp (tmVar "f" 0) (tmCon "Int" [] [])
        term = tmLet "f" fTy fBody body
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Bool"

-- | Tests for polymorphic let-bindings
polymorphicLetTests :: Spec
polymorphicLetTests = describe "polymorphic let-bindings" $ do
  it "generalizes identity function" $ do
    -- let id = \x. x in id  should have polymorphic type
    let idBody = tmLam "x" (tyVar "a" 0) (tmVar "x" 0)
        idTy = tyFn (tyVar "a" 0) (tyVar "a" 0)
        term = tmLet "id" idTy idBody (tmVar "id" 0)
        env = emptyInferEnv
    let result = infer env term
    result `shouldSatisfy` isRight

  it "instantiates polymorphic binding at different types" $ do
    -- let id = \x. x in (id True, id 42)
    -- Each use of id should be instantiated separately
    let idBody = tmLam "x" (tyVar "a" 0) (tmVar "x" 0)
        idTy = tyFn (tyVar "a" 0) (tyVar "a" 0)
        -- id True
        use1 = tmApp (tmVar "id" 0) (tmCon "True" [] [])
        -- For this test, we just check that each application type-checks
        term1 = tmLet "id" idTy idBody use1
        env = emptyInferEnv
    infer env term1 `shouldInferType` tySimple "Bool"

  it "const function is polymorphic" $ do
    -- let const = \x. \y. x in const True 42
    let constTy = tyFn (tyVar "a" 0) (tyFn (tyVar "b" 1) (tyVar "a" 0))
        innerLam = tmLam "y" (tyVar "b" 1) (tmVar "x" 1)
        constBody = tmLam "x" (tyVar "a" 0) innerLam
        body = tmApp (tmApp (tmVar "const" 0) (tmCon "True" [] [])) (tmCon "Int" [] [])
        term = tmLet "const" constTy constBody body
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Bool"

-- | Tests for annotated let-bindings
annotatedLetTests :: Spec
annotatedLetTests = describe "annotated let-bindings" $ do
  it "respects explicit type annotation" $ do
    -- let x: Int = 42 in x
    let term = tmLet "x" (tySimple "Int") (tmCon "Int" [] []) (tmVar "x" 0)
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Int"

  it "annotation constrains inferred type" $ do
    -- let f: Int -> Int = \x. x in f
    let fTy = tyFn (tySimple "Int") (tySimple "Int")
        fBody = tmLam "x" (tySimple "Int") (tmVar "x" 0)
        term = tmLet "f" fTy fBody (tmVar "f" 0)
        env = emptyInferEnv
    infer env term `shouldInferType` fTy

-- | Tests for generalization
generalizationTests :: Spec
generalizationTests = describe "generalization" $ do
  it "generalizes free type variables" $ do
    let ty = tyFn (tyVar "a" 0) (tyVar "a" 0)
        env = emptyInferEnv
        scheme = generalize env ty
    -- Should have quantified over variable 0
    schemeVars scheme `shouldSatisfy` (not . Set.null)

  it "does not generalize variables in environment" $ do
    -- If 'a' is already in scope, it should not be generalized
    let ty = tyFn (tyVar "a" 0) (tySimple "Int")
        aScheme = monoScheme (tyVar "a" 0)
        env = extendInferEnv "x" aScheme emptyInferEnv
        scheme = generalize env ty
    -- Variable 0 appears in env, so should not be generalized
    schemeVars scheme `shouldSatisfy` (Set.notMember 0)

  it "generalizes multiple free variables" $ do
    let ty = tyFn (tyVar "a" 0) (tyFn (tyVar "b" 1) (tyVar "a" 0))
        env = emptyInferEnv
        scheme = generalize env ty
    -- Should have quantified over both variables
    Set.size (schemeVars scheme) `shouldBe` 2

-- | Tests for instantiation
instantiationTests :: Spec
instantiationTests = describe "instantiation" $ do
  it "instantiates monomorphic scheme unchanged" $ do
    let scheme = monoScheme (tySimple "Int")
    runInferWith emptyInferEnv $ do
      ty <- instantiate scheme
      pure ty
    -- The type should be Int
    `shouldSatisfy` \case
      Right ty -> ty == tySimple "Int"
      Left _ -> False

  it "instantiates polymorphic scheme with fresh variables" $ do
    let scheme = Scheme (Set.singleton 0) (tyFn (tyVar "a" 0) (tyVar "a" 0))
    let result = runInferWith emptyInferEnv $ instantiate scheme
    result `shouldSatisfy` isRight

  it "different instantiations get different fresh variables" $ do
    let scheme = Scheme (Set.singleton 0) (tyVar "a" 0)
    let result = runInferWith emptyInferEnv $ do
          t1 <- instantiate scheme
          t2 <- instantiate scheme
          pure (t1, t2)
    case result of
      Right (TyVar _ i1, TyVar _ i2) -> i1 `shouldNotBe` i2
      Right _ -> expectationFailure "Expected type variables"
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

-- | Tests for type scheme operations
schemeTests :: Spec
schemeTests = describe "type schemes" $ do
  it "monoScheme creates scheme with no quantified vars" $ do
    let scheme = monoScheme (tySimple "Int")
    schemeVars scheme `shouldBe` Set.empty

  it "schemeType extracts the type from scheme" $ do
    let ty = tyFn (tySimple "Int") (tySimple "Bool")
        scheme = monoScheme ty
    schemeType scheme `shouldBe` ty

  it "scheme with quantified variables" $ do
    let scheme = Scheme (Set.fromList [0, 1]) (tyFn (tyVar "a" 0) (tyVar "b" 1))
    Set.size (schemeVars scheme) `shouldBe` 2

-- | Tests for free type variable computation
freeVarsTests :: Spec
freeVarsTests = describe "free type variables" $ do
  it "simple type has no free vars" $ do
    freeTypeVars (tySimple "Int") `shouldBe` Set.empty

  it "type variable is free" $ do
    freeTypeVars (tyVar "a" 0) `shouldBe` Set.singleton 0

  it "function type collects vars from both sides" $ do
    let ty = tyFn (tyVar "a" 0) (tyVar "b" 1)
    freeTypeVars ty `shouldBe` Set.fromList [0, 1]

  it "type constructor collects vars from args" $ do
    let ty = tyCon "List" [tyVar "a" 0]
    freeTypeVars ty `shouldBe` Set.singleton 0

  it "nested types collect all vars" $ do
    let ty = tyFn (tyCon "List" [tyVar "a" 0]) (tyFn (tyVar "b" 1) (tyVar "a" 0))
    freeTypeVars ty `shouldBe` Set.fromList [0, 1]

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "deeply nested let-bindings" $ do
    -- let x = 1 in let y = x in let z = y in z
    let z = tmVar "z" 0
        letZ = tmLet "z" (tySimple "Int") (tmVar "y" 0) z
        letY = tmLet "y" (tySimple "Int") (tmVar "x" 0) letZ
        term = tmLet "x" (tySimple "Int") (tmCon "Int" [] []) letY
        env = emptyInferEnv
    infer env term `shouldInferType` tySimple "Int"

  it "let-binding with complex type" $ do
    let listIntTy = tyCon "List" [tySimple "Int"]
        term = tmLet "xs" listIntTy (tmCon "Nil" [] []) (tmVar "xs" 0)
        env = emptyInferEnv
    infer env term `shouldInferType` listIntTy

  it "function returning function" $ do
    let ty = tyFn (tySimple "Int") (tyFn (tySimple "Bool") (tySimple "String"))
        innerLam = tmLam "y" (tySimple "Bool") (tmCon "String" [] [])
        body = tmLam "x" (tySimple "Int") innerLam
        term = tmLet "f" ty body (tmVar "f" 0)
        env = emptyInferEnv
    infer env term `shouldInferType` ty

  it "higher-order function" $ do
    -- let apply = \f. \x. f x in apply
    let fTy = tyFn (tyVar "a" 0) (tyVar "b" 1)
        applyTy = tyFn fTy (tyFn (tyVar "a" 0) (tyVar "b" 1))
        appBody = tmApp (tmVar "f" 1) (tmVar "x" 0)
        innerLam = tmLam "x" (tyVar "a" 0) appBody
        body = tmLam "f" fTy innerLam
        term = tmLet "apply" applyTy body (tmVar "apply" 0)
        env = emptyInferEnv
    infer env term `shouldSatisfy` isRight

  it "composition function" $ do
    -- let compose = \f. \g. \x. f (g x) in compose
    let aTy = tyVar "a" 0
        bTy = tyVar "b" 1
        cTy = tyVar "c" 2
        gTy = tyFn aTy bTy
        fTy = tyFn bTy cTy
        composeTy = tyFn fTy (tyFn gTy (tyFn aTy cTy))
        gx = tmApp (tmVar "g" 1) (tmVar "x" 0)
        fgx = tmApp (tmVar "f" 2) gx
        innerLam = tmLam "x" aTy fgx
        midLam = tmLam "g" gTy innerLam
        body = tmLam "f" fTy midLam
        term = tmLet "compose" composeTy body (tmVar "compose" 0)
        env = emptyInferEnv
    infer env term `shouldSatisfy` isRight
