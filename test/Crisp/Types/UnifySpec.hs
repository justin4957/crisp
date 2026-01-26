{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.UnifySpec
-- Description : Tests for type unification
--
-- TDD tests for the type unification algorithm that enables type inference
-- through constraint solving.

module Crisp.Types.UnifySpec (spec) where

import Test.Hspec

import Crisp.Types.Unify
import Crisp.Types.Substitution
import Crisp.Core.Term

import Data.Either (isRight, isLeft)
import qualified Data.Text as T

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
tyForall :: T.Text -> Kind -> Type -> Type
tyForall = TyForall

-- | Helper for checking unification success
shouldUnifyTo :: Either UnifyError Substitution -> Substitution -> Expectation
shouldUnifyTo result expected = case result of
  Right subst -> subst `shouldBe` expected
  Left err -> expectationFailure $ "Unification failed: " ++ show err

-- | Helper for checking unification succeeds (any substitution)
shouldUnifySuccessfully :: Either UnifyError Substitution -> Expectation
shouldUnifySuccessfully result = result `shouldSatisfy` isRight

-- | Helper for checking unification fails
shouldFailUnification :: Either UnifyError Substitution -> Expectation
shouldFailUnification result = result `shouldSatisfy` isLeft

-- | Check if error is an occurs check violation
isOccursCheckError :: Either UnifyError a -> Bool
isOccursCheckError (Left (OccursCheck _ _)) = True
isOccursCheckError _ = False

-- | Check if error is a type mismatch
isTypeMismatchError :: Either UnifyError a -> Bool
isTypeMismatchError (Left (TypeMismatch _ _)) = True
isTypeMismatchError _ = False

spec :: Spec
spec = do
  describe "Basic Unification" $ do
    identicalTypesTests
    typeVariableTests

  describe "Function Type Unification" $ do
    functionTypeTests

  describe "Type Constructor Unification" $ do
    typeConstructorTests

  describe "Occurs Check" $ do
    occursCheckTests

  describe "Substitution Operations" $ do
    substitutionTests

  describe "Effect Row Unification" $ do
    effectRowTests

  describe "Symmetry" $ do
    symmetryTests

  describe "Complex Types" $ do
    complexTypeTests

  describe "Edge Cases" $ do
    edgeCaseTests

-- | Tests for unifying identical types
identicalTypesTests :: Spec
identicalTypesTests = describe "identical types" $ do
  it "unifies identical simple types" $ do
    unify (tySimple "Int") (tySimple "Int") `shouldUnifyTo` emptySubst

  it "unifies identical Bool types" $ do
    unify (tySimple "Bool") (tySimple "Bool") `shouldUnifyTo` emptySubst

  it "unifies identical type variables (same index)" $ do
    unify (tyVar "a" 0) (tyVar "a" 0) `shouldUnifyTo` emptySubst

  it "unifies TyProp with TyProp" $ do
    unify TyProp TyProp `shouldUnifyTo` emptySubst

  it "unifies TyUniverse at same level" $ do
    unify (TyUniverse 0) (TyUniverse 0) `shouldUnifyTo` emptySubst

  it "unifies identical Lazy types" $ do
    unify (TyLazy (tySimple "Int")) (TyLazy (tySimple "Int")) `shouldUnifyTo` emptySubst

  it "unifies identical Ref types" $ do
    unify (TyRef (tySimple "Int")) (TyRef (tySimple "Int")) `shouldUnifyTo` emptySubst

  it "unifies identical RefMut types" $ do
    unify (TyRefMut (tySimple "Int")) (TyRefMut (tySimple "Int")) `shouldUnifyTo` emptySubst

  it "unifies identical Linear types" $ do
    unify (TyLinear (tySimple "Int")) (TyLinear (tySimple "Int")) `shouldUnifyTo` emptySubst

-- | Tests for type variable unification
typeVariableTests :: Spec
typeVariableTests = describe "type variables" $ do
  it "unifies type variable with concrete type" $ do
    let result = unify (tyVar "a" 0) (tySimple "Int")
    result `shouldSatisfy` isRight
    case result of
      Right subst -> applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"
      Left _ -> expectationFailure "Should have unified"

  it "unifies concrete type with type variable" $ do
    let result = unify (tySimple "Bool") (tyVar "b" 1)
    result `shouldSatisfy` isRight
    case result of
      Right subst -> applySubst subst (tyVar "b" 1) `shouldBe` tySimple "Bool"
      Left _ -> expectationFailure "Should have unified"

  it "unifies two different type variables" $ do
    let result = unify (tyVar "a" 0) (tyVar "b" 1)
    result `shouldSatisfy` isRight

  it "binds variable to itself produces empty substitution" $ do
    unify (tyVar "a" 0) (tyVar "a" 0) `shouldUnifyTo` emptySubst

  it "creates correct binding for variable to type" $ do
    let result = unify (tyVar "x" 0) (tySimple "String")
    case result of
      Right subst -> lookupSubst 0 subst `shouldBe` Just (tySimple "String")
      Left err -> expectationFailure $ "Unification failed: " ++ show err

-- | Tests for function type unification
functionTypeTests :: Spec
functionTypeTests = describe "function types" $ do
  it "unifies identical function types" $ do
    let t = tyFn (tySimple "Int") (tySimple "Bool")
    unify t t `shouldUnifyTo` emptySubst

  it "unifies function types with type variable in domain" $ do
    let t1 = tyFn (tyVar "a" 0) (tySimple "Bool")
        t2 = tyFn (tySimple "Int") (tySimple "Bool")
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"
      Left _ -> expectationFailure "Should have unified"

  it "unifies function types with type variable in codomain" $ do
    let t1 = tyFn (tySimple "Int") (tyVar "a" 0)
        t2 = tyFn (tySimple "Int") (tySimple "Bool")
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Bool"
      Left _ -> expectationFailure "Should have unified"

  it "unifies function types with variables in both positions" $ do
    let t1 = tyFn (tyVar "a" 0) (tyVar "b" 1)
        t2 = tyFn (tySimple "Int") (tySimple "Bool")
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> do
        applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"
        applySubst subst (tyVar "b" 1) `shouldBe` tySimple "Bool"
      Left _ -> expectationFailure "Should have unified"

  it "fails on incompatible function types (domain mismatch)" $ do
    let t1 = tyFn (tySimple "Int") (tySimple "Bool")
        t2 = tyFn (tySimple "String") (tySimple "Bool")
    unify t1 t2 `shouldSatisfy` isTypeMismatchError

  it "fails on incompatible function types (codomain mismatch)" $ do
    let t1 = tyFn (tySimple "Int") (tySimple "Bool")
        t2 = tyFn (tySimple "Int") (tySimple "String")
    unify t1 t2 `shouldSatisfy` isTypeMismatchError

  it "unifies nested function types" $ do
    let t1 = tyFn (tySimple "Int") (tyFn (tySimple "Bool") (tyVar "a" 0))
        t2 = tyFn (tySimple "Int") (tyFn (tySimple "Bool") (tySimple "String"))
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> applySubst subst (tyVar "a" 0) `shouldBe` tySimple "String"
      Left _ -> expectationFailure "Should have unified"

-- | Tests for type constructor unification
typeConstructorTests :: Spec
typeConstructorTests = describe "type constructors" $ do
  it "unifies constructors with same name and no args" $ do
    unify (tyCon "Unit" []) (tyCon "Unit" []) `shouldUnifyTo` emptySubst

  it "unifies constructors with same name and matching args" $ do
    unify (tyCon "List" [tySimple "Int"])
          (tyCon "List" [tySimple "Int"])
      `shouldUnifyTo` emptySubst

  it "fails on constructors with different names" $ do
    unify (tyCon "List" [tySimple "Int"])
          (tyCon "Vector" [tySimple "Int"])
      `shouldSatisfy` isTypeMismatchError

  it "unifies constructor args with type variables" $ do
    let t1 = tyCon "Maybe" [tyVar "a" 0]
        t2 = tyCon "Maybe" [tySimple "Int"]
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"
      Left _ -> expectationFailure "Should have unified"

  it "unifies constructor with multiple args" $ do
    let t1 = tyCon "Pair" [tyVar "a" 0, tyVar "b" 1]
        t2 = tyCon "Pair" [tySimple "Int", tySimple "Bool"]
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> do
        applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"
        applySubst subst (tyVar "b" 1) `shouldBe` tySimple "Bool"
      Left _ -> expectationFailure "Should have unified"

  it "fails on constructors with different arity" $ do
    unify (tyCon "Pair" [tySimple "Int", tySimple "Bool"])
          (tyCon "Pair" [tySimple "Int"])
      `shouldSatisfy` isLeft

  it "propagates substitution through constructor args" $ do
    -- Unify Maybe (a -> a) with Maybe (Int -> b)
    let t1 = tyCon "Maybe" [tyFn (tyVar "a" 0) (tyVar "a" 0)]
        t2 = tyCon "Maybe" [tyFn (tySimple "Int") (tyVar "b" 1)]
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> do
        applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"
        applySubst subst (tyVar "b" 1) `shouldBe` tySimple "Int"
      Left _ -> expectationFailure "Should have unified"

-- | Tests for occurs check
occursCheckTests :: Spec
occursCheckTests = describe "occurs check" $ do
  it "fails on direct circular reference" $ do
    -- a ~ a -> Int should fail
    let t1 = tyVar "a" 0
        t2 = tyFn (tyVar "a" 0) (tySimple "Int")
    unify t1 t2 `shouldSatisfy` isOccursCheckError

  it "fails on nested circular reference" $ do
    -- a ~ List a should fail
    let t1 = tyVar "a" 0
        t2 = tyCon "List" [tyVar "a" 0]
    unify t1 t2 `shouldSatisfy` isOccursCheckError

  it "fails on deeply nested circular reference" $ do
    -- a ~ Maybe (List (Maybe a)) should fail
    let t1 = tyVar "a" 0
        t2 = tyCon "Maybe" [tyCon "List" [tyCon "Maybe" [tyVar "a" 0]]]
    unify t1 t2 `shouldSatisfy` isOccursCheckError

  it "succeeds when variable appears only on one side" $ do
    -- a ~ Int should succeed
    let result = unify (tyVar "a" 0) (tySimple "Int")
    result `shouldSatisfy` isRight

  it "succeeds with different variables" $ do
    -- a ~ b -> Int should succeed (a and b are different)
    let t1 = tyVar "a" 0
        t2 = tyFn (tyVar "b" 1) (tySimple "Int")
    let result = unify t1 t2
    result `shouldSatisfy` isRight

-- | Tests for substitution operations
substitutionTests :: Spec
substitutionTests = describe "substitution operations" $ do
  it "empty substitution doesn't change types" $ do
    let t = tyFn (tySimple "Int") (tySimple "Bool")
    applySubst emptySubst t `shouldBe` t

  it "single substitution replaces variable" $ do
    let subst = singleSubst 0 (tySimple "Int")
    applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"

  it "substitution doesn't affect unbound variables" $ do
    let subst = singleSubst 0 (tySimple "Int")
    applySubst subst (tyVar "b" 1) `shouldBe` tyVar "b" 1

  it "substitution applies through type constructors" $ do
    let subst = singleSubst 0 (tySimple "Int")
        t = tyCon "List" [tyVar "a" 0]
    applySubst subst t `shouldBe` tyCon "List" [tySimple "Int"]

  it "substitution applies through function types" $ do
    let subst = singleSubst 0 (tySimple "Int")
        t = tyFn (tyVar "a" 0) (tyVar "a" 0)
    applySubst subst t `shouldBe` tyFn (tySimple "Int") (tySimple "Int")

  it "composition applies substitutions in order" $ do
    let s1 = singleSubst 0 (tyVar "b" 1)
        s2 = singleSubst 1 (tySimple "Int")
        composed = composeSubst s2 s1
    applySubst composed (tyVar "a" 0) `shouldBe` tySimple "Int"

  it "composition is associative with types" $ do
    let s1 = singleSubst 0 (tyVar "b" 1)
        s2 = singleSubst 1 (tyVar "c" 2)
        s3 = singleSubst 2 (tySimple "Int")
        composed1 = composeSubst s3 (composeSubst s2 s1)
        composed2 = composeSubst (composeSubst s3 s2) s1
        t = tyVar "a" 0
    applySubst composed1 t `shouldBe` applySubst composed2 t

-- | Tests for effect row unification
effectRowTests :: Spec
effectRowTests = describe "effect row unification" $ do
  it "unifies empty effect rows" $ do
    unifyEffects EffEmpty EffEmpty `shouldUnifyTo` emptySubst

  it "unifies identical effect sets" $ do
    let eff = EffSet [Effect "IO" Nothing]
    unifyEffects eff eff `shouldUnifyTo` emptySubst

  it "unifies effect variable with concrete effects" $ do
    let effVar = EffVar "rho" 0
        effConcrete = EffSet [Effect "IO" Nothing]
    let result = unifyEffects effVar effConcrete
    result `shouldSatisfy` isRight

  it "unifies concrete effects with effect variable" $ do
    let effVar = EffVar "rho" 0
        effConcrete = EffSet [Effect "State" Nothing]
    let result = unifyEffects effConcrete effVar
    result `shouldSatisfy` isRight

  it "unifies two effect variables" $ do
    let eff1 = EffVar "rho1" 0
        eff2 = EffVar "rho2" 1
    unifyEffects eff1 eff2 `shouldSatisfy` isRight

  it "unifies effect variable with empty row" $ do
    let effVar = EffVar "rho" 0
    unifyEffects effVar EffEmpty `shouldSatisfy` isRight

  it "unifies effect unions" $ do
    let eff1 = EffUnion (EffSet [Effect "IO" Nothing]) EffEmpty
        eff2 = EffSet [Effect "IO" Nothing]
    unifyEffects eff1 eff2 `shouldSatisfy` isRight

-- | Tests for symmetry
symmetryTests :: Spec
symmetryTests = describe "symmetry" $ do
  it "unify a b produces same result type as unify b a for simple types" $ do
    let t1 = tyVar "a" 0
        t2 = tySimple "Int"
    case (unify t1 t2, unify t2 t1) of
      (Right s1, Right s2) ->
        applySubst s1 t1 `shouldBe` applySubst s2 t1
      _ -> expectationFailure "Both should succeed"

  it "unify is symmetric for function types" $ do
    let t1 = tyFn (tyVar "a" 0) (tySimple "Bool")
        t2 = tyFn (tySimple "Int") (tySimple "Bool")
    case (unify t1 t2, unify t2 t1) of
      (Right s1, Right s2) ->
        applySubst s1 t1 `shouldBe` applySubst s2 t1
      _ -> expectationFailure "Both should succeed"

  it "unify failure is symmetric" $ do
    let t1 = tySimple "Int"
        t2 = tySimple "Bool"
    (isLeft (unify t1 t2)) `shouldBe` (isLeft (unify t2 t1))

  it "occurs check failure is symmetric" $ do
    let t1 = tyVar "a" 0
        t2 = tyFn (tyVar "a" 0) (tySimple "Int")
    (isOccursCheckError (unify t1 t2)) `shouldBe` (isOccursCheckError (unify t2 t1))

-- | Tests for complex type unification
complexTypeTests :: Spec
complexTypeTests = describe "complex types" $ do
  it "unifies forall types with matching structure" $ do
    let t1 = tyForall "a" (KiType 0) (tyFn (tyVar "a" 0) (tyVar "a" 0))
        t2 = tyForall "b" (KiType 0) (tyFn (tyVar "b" 0) (tyVar "b" 0))
    unify t1 t2 `shouldUnifyTo` emptySubst

  it "unifies Lazy types with variable content" $ do
    let t1 = TyLazy (tyVar "a" 0)
        t2 = TyLazy (tySimple "Int")
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"
      Left _ -> expectationFailure "Should have unified"

  it "unifies Ref types with variable content" $ do
    let t1 = TyRef (tyVar "a" 0)
        t2 = TyRef (tySimple "String")
    let result = unify t1 t2
    result `shouldSatisfy` isRight

  it "unifies RefMut types with variable content" $ do
    let t1 = TyRefMut (tyVar "a" 0)
        t2 = TyRefMut (tySimple "Int")
    let result = unify t1 t2
    result `shouldSatisfy` isRight

  it "unifies Linear types with variable content" $ do
    let t1 = TyLinear (tyVar "a" 0)
        t2 = TyLinear (tySimple "Resource")
    let result = unify t1 t2
    result `shouldSatisfy` isRight

  it "fails on mismatched wrapper types" $ do
    unify (TyLazy (tySimple "Int")) (TyRef (tySimple "Int"))
      `shouldSatisfy` isLeft

  it "unifies complex nested types" $ do
    -- Maybe (a -> List b) ~ Maybe (Int -> List Bool)
    let t1 = tyCon "Maybe" [tyFn (tyVar "a" 0) (tyCon "List" [tyVar "b" 1])]
        t2 = tyCon "Maybe" [tyFn (tySimple "Int") (tyCon "List" [tySimple "Bool"])]
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> do
        applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"
        applySubst subst (tyVar "b" 1) `shouldBe` tySimple "Bool"
      Left _ -> expectationFailure "Should have unified"

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles multiple occurrences of same variable" $ do
    -- (a, a) ~ (Int, Int) should succeed
    let t1 = tyCon "Pair" [tyVar "a" 0, tyVar "a" 0]
        t2 = tyCon "Pair" [tySimple "Int", tySimple "Int"]
    let result = unify t1 t2
    result `shouldSatisfy` isRight

  it "fails on inconsistent variable binding" $ do
    -- (a, a) ~ (Int, Bool) should fail
    let t1 = tyCon "Pair" [tyVar "a" 0, tyVar "a" 0]
        t2 = tyCon "Pair" [tySimple "Int", tySimple "Bool"]
    unify t1 t2 `shouldSatisfy` isTypeMismatchError

  it "handles chain of variable bindings" $ do
    -- (a, b, c) ~ (b, c, Int)
    let t1 = tyCon "Triple" [tyVar "a" 0, tyVar "b" 1, tyVar "c" 2]
        t2 = tyCon "Triple" [tyVar "b" 1, tyVar "c" 2, tySimple "Int"]
    let result = unify t1 t2
    result `shouldSatisfy` isRight
    case result of
      Right subst -> do
        applySubst subst (tyVar "a" 0) `shouldBe` tySimple "Int"
        applySubst subst (tyVar "b" 1) `shouldBe` tySimple "Int"
        applySubst subst (tyVar "c" 2) `shouldBe` tySimple "Int"
      Left _ -> expectationFailure "Should have unified"

  it "handles deeply nested equal types" $ do
    let mkDeep n t = if n <= 0 then t else tyCon "Box" [mkDeep (n-1) t]
        t = mkDeep (10 :: Int) (tySimple "Int")
    unify t t `shouldUnifyTo` emptySubst

  it "handles TyUniverse level mismatch" $ do
    unify (TyUniverse 0) (TyUniverse 1) `shouldSatisfy` isTypeMismatchError

  it "handles TyForallDep types" $ do
    let t1 = TyForallDep "x" (tySimple "Nat") (tyVar "a" 0)
        t2 = TyForallDep "y" (tySimple "Nat") (tySimple "Bool")
    let result = unify t1 t2
    result `shouldSatisfy` isRight
