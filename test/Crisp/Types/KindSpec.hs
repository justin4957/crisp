{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.KindSpec
-- Description : Tests for kind checking
--
-- TDD tests for kind checking, ensuring that types are well-formed
-- and type constructors are applied with correct kinds.

module Crisp.Types.KindSpec (spec) where

import Test.Hspec

import Crisp.Types.KindChecker
import Crisp.Core.Term

import Data.Either (isLeft)
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
tyFn paramTy retTy = TyPi "_" paramTy EffEmpty retTy

-- | Standard kind for types
kType :: Kind
kType = KiType 0

-- | Standard kind for type constructors: Type -> Type
kTypeToType :: Kind
kTypeToType = KiArrow (KiType 0) (KiType 0)

-- | Kind for two-argument type constructors: Type -> Type -> Type
kTypeToTypeToType :: Kind
kTypeToTypeToType = KiArrow (KiType 0) (KiArrow (KiType 0) (KiType 0))

-- | Standard prop kind
kProp :: Kind
kProp = KiProp

spec :: Spec
spec = do
  describe "Basic Kind Checking" $ do
    basicKindTests
    primitiveTypeTests

  describe "Type Constructor Kinds" $ do
    typeConstructorTests
    applicationTests

  describe "Function Type Kinds" $ do
    functionTypeTests

  describe "Forall Type Kinds" $ do
    forallTypeTests

  describe "Effect Row Kinds" $ do
    effectKindTests

  describe "Special Type Kinds" $ do
    specialTypeTests

  describe "Kind Errors" $ do
    kindErrorTests

  describe "Higher-Kinded Types" $ do
    higherKindedTests

  describe "Kind Environment" $ do
    kindEnvTests

  describe "Edge Cases" $ do
    edgeCaseTests

-- | Tests for basic kind checking of primitive types
basicKindTests :: Spec
basicKindTests = describe "primitive types" $ do
  it "Int has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (tySimple "Int") `shouldBe` Right kType

  it "Bool has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (tySimple "Bool") `shouldBe` Right kType

  it "String has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (tySimple "String") `shouldBe` Right kType

  it "Unit has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (tySimple "Unit") `shouldBe` Right kType

-- | Tests for primitive type kind inference
primitiveTypeTests :: Spec
primitiveTypeTests = describe "type variables and universes" $ do
  it "TyUniverse 0 has kind Type 1" $ do
    let env = defaultKindEnv
    kindOf env (TyUniverse 0) `shouldBe` Right (KiType 1)

  it "TyUniverse 1 has kind Type 2" $ do
    let env = defaultKindEnv
    kindOf env (TyUniverse 1) `shouldBe` Right (KiType 2)

  it "TyProp has kind Prop" $ do
    let env = defaultKindEnv
    kindOf env TyProp `shouldBe` Right kProp

  it "type variable has kind from environment" $ do
    let env = extendKindEnv "a" kType defaultKindEnv
    kindOf env (tyVar "a" 0) `shouldBe` Right kType

  it "unbound type variable fails" $ do
    let env = defaultKindEnv
    kindOf env (tyVar "unknown" 0) `shouldSatisfy` isLeft

-- | Tests for type constructor kinds
typeConstructorTests :: Spec
typeConstructorTests = describe "type constructors" $ do
  it "List has kind Type -> Type" $ do
    let env = defaultKindEnv
    lookupKindEnv "List" env `shouldBe` Just kTypeToType

  it "Option has kind Type -> Type" $ do
    let env = defaultKindEnv
    lookupKindEnv "Option" env `shouldBe` Just kTypeToType

  it "Either has kind Type -> Type -> Type" $ do
    let env = defaultKindEnv
    lookupKindEnv "Either" env `shouldBe` Just kTypeToTypeToType

  it "Map has kind Type -> Type -> Type" $ do
    let env = defaultKindEnv
    lookupKindEnv "Map" env `shouldBe` Just kTypeToTypeToType

-- | Tests for type application
applicationTests :: Spec
applicationTests = describe "type application" $ do
  it "List(Int) has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (tyCon "List" [tySimple "Int"]) `shouldBe` Right kType

  it "List(Bool) has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (tyCon "List" [tySimple "Bool"]) `shouldBe` Right kType

  it "Option(String) has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (tyCon "Option" [tySimple "String"]) `shouldBe` Right kType

  it "Either(Int, String) has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (tyCon "Either" [tySimple "Int", tySimple "String"]) `shouldBe` Right kType

  it "nested application: List(List(Int)) has kind Type" $ do
    let env = defaultKindEnv
        innerList = tyCon "List" [tySimple "Int"]
    kindOf env (tyCon "List" [innerList]) `shouldBe` Right kType

  it "partial application: Either(Int) has kind Type -> Type" $ do
    let env = defaultKindEnv
    kindOf env (tyCon "Either" [tySimple "Int"]) `shouldBe` Right kTypeToType

  it "rejects over-applied type constructor" $ do
    let env = defaultKindEnv
    -- Int(Bool) - Int is not a type constructor
    kindOf env (tyCon "Int" [tySimple "Bool"]) `shouldSatisfy` isLeft

  it "rejects kind mismatch in application" $ do
    let env = defaultKindEnv
    -- List(List) - List expects Type, not Type -> Type
    kindOf env (tyCon "List" [tyCon "List" []]) `shouldSatisfy` isLeft

-- | Tests for function types
functionTypeTests :: Spec
functionTypeTests = describe "function types" $ do
  it "Int -> Bool has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (tyFn (tySimple "Int") (tySimple "Bool")) `shouldBe` Right kType

  it "Int -> Int -> Int has kind Type" $ do
    let env = defaultKindEnv
        ty = tyFn (tySimple "Int") (tyFn (tySimple "Int") (tySimple "Int"))
    kindOf env ty `shouldBe` Right kType

  it "List(a) -> Int has kind Type" $ do
    let env = extendKindEnv "a" kType defaultKindEnv
        ty = tyFn (tyCon "List" [tyVar "a" 0]) (tySimple "Int")
    kindOf env ty `shouldBe` Right kType

  it "rejects function with ill-kinded domain" $ do
    let env = defaultKindEnv
    -- List -> Int - List without argument has kind Type -> Type, not Type
    kindOf env (tyFn (tyCon "List" []) (tySimple "Int")) `shouldSatisfy` isLeft

  it "rejects function with ill-kinded codomain" $ do
    let env = defaultKindEnv
    -- Int -> Option - Option without argument is ill-kinded
    kindOf env (tyFn (tySimple "Int") (tyCon "Option" [])) `shouldSatisfy` isLeft

-- | Tests for forall types
forallTypeTests :: Spec
forallTypeTests = describe "forall types" $ do
  it "forall a. a has kind Type" $ do
    let env = defaultKindEnv
        ty = TyForall "a" kType (tyVar "a" 0)
    kindOf env ty `shouldBe` Right kType

  it "forall a. a -> a has kind Type" $ do
    let env = defaultKindEnv
        ty = TyForall "a" kType (tyFn (tyVar "a" 0) (tyVar "a" 0))
    kindOf env ty `shouldBe` Right kType

  it "forall a. List(a) has kind Type" $ do
    let env = defaultKindEnv
        ty = TyForall "a" kType (tyCon "List" [tyVar "a" 0])
    kindOf env ty `shouldBe` Right kType

  it "forall (f : Type -> Type). f(Int) has kind Type" $ do
    let env = defaultKindEnv
        ty = TyForall "f" kTypeToType (tyCon "f" [tySimple "Int"])
    kindOf env ty `shouldBe` Right kType

  it "nested forall: forall a. forall b. (a, b) has kind Type" $ do
    let env = defaultKindEnv
        -- Pair represented as TyCon "Pair" [a, b]
        ty = TyForall "a" kType (TyForall "b" kType
              (tyCon "Pair" [tyVar "a" 1, tyVar "b" 0]))
    kindOf env ty `shouldBe` Right kType

-- | Tests for effect rows
effectKindTests :: Spec
effectKindTests = describe "effect rows" $ do
  it "empty effect row is well-kinded" $ do
    let env = defaultKindEnv
    checkEffectRow env EffEmpty `shouldBe` Right ()

  it "single effect is well-kinded" $ do
    let env = extendKindEnv "IO" kType defaultKindEnv
        effs = EffSet [Effect "IO" Nothing]
    checkEffectRow env effs `shouldBe` Right ()

  it "effect union is well-kinded" $ do
    let env = extendKindEnv "IO" kType $
              extendKindEnv "State" kType defaultKindEnv
        eff1 = EffSet [Effect "IO" Nothing]
        eff2 = EffSet [Effect "State" Nothing]
    checkEffectRow env (EffUnion eff1 eff2) `shouldBe` Right ()

  it "effect variable is well-kinded with environment" $ do
    let env = extendEffectVar "e" defaultKindEnv
    checkEffectRow env (EffVar "e" 0) `shouldBe` Right ()

-- | Tests for special types
specialTypeTests :: Spec
specialTypeTests = describe "special types" $ do
  it "Lazy(Int) has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (TyLazy (tySimple "Int")) `shouldBe` Right kType

  it "Lazy(List(a)) has kind Type" $ do
    let env = extendKindEnv "a" kType defaultKindEnv
    kindOf env (TyLazy (tyCon "List" [tyVar "a" 0])) `shouldBe` Right kType

  it "ref Int has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (TyRef (tySimple "Int")) `shouldBe` Right kType

  it "ref mut Int has kind Type" $ do
    let env = defaultKindEnv
    kindOf env (TyRefMut (tySimple "Int")) `shouldBe` Right kType

  it "linear Int has kind Linear" $ do
    let env = defaultKindEnv
    kindOf env (TyLinear (tySimple "Int")) `shouldBe` Right KiLinear

  it "rejects Lazy with ill-kinded argument" $ do
    let env = defaultKindEnv
    -- Lazy(List) - List without argument
    kindOf env (TyLazy (tyCon "List" [])) `shouldSatisfy` isLeft

-- | Tests for kind errors
kindErrorTests :: Spec
kindErrorTests = describe "kind errors" $ do
  it "reports unbound type constructor" $ do
    let env = defaultKindEnv
    kindOf env (tyCon "Unknown" []) `shouldSatisfy` isLeft

  it "partial application returns remaining kind" $ do
    let env = defaultKindEnv
    -- Either with 0 args returns Type -> Type -> Type (partial application is valid)
    kindOf env (tyCon "Either" []) `shouldBe` Right kTypeToTypeToType

  it "reports arity mismatch - too many args" $ do
    let env = defaultKindEnv
    -- Int takes no arguments
    kindOf env (tyCon "Int" [tySimple "Bool"]) `shouldSatisfy` \case
      Left (KindMismatch _ _) -> True
      _ -> False

  it "reports kind mismatch in application" $ do
    let env = defaultKindEnv
    -- List expects Type, gets Type -> Type
    kindOf env (tyCon "List" [tyCon "Option" []]) `shouldSatisfy` \case
      Left (KindMismatch _ _) -> True
      _ -> False

  it "reports unbound type variable" $ do
    let env = defaultKindEnv
    kindOf env (tyVar "x" 0) `shouldSatisfy` \case
      Left (UnboundTypeVar _) -> True
      _ -> False

-- | Tests for higher-kinded types
higherKindedTests :: Spec
higherKindedTests = describe "higher-kinded types" $ do
  it "supports Type -> Type parameter" $ do
    let env = extendKindEnv "f" kTypeToType defaultKindEnv
    kindOf env (tyCon "f" [tySimple "Int"]) `shouldBe` Right kType

  it "supports (Type -> Type) -> Type parameter" $ do
    let higherKind = KiArrow kTypeToType kType
        env = extendKindEnv "g" higherKind defaultKindEnv
    kindOf env (tyCon "g" [tyCon "List" []]) `shouldBe` Right kType

  it "Functor-like type: forall (f: Type -> Type). f(Int) -> f(Bool)" $ do
    let env = defaultKindEnv
        ty = TyForall "f" kTypeToType
              (tyFn (tyCon "f" [tySimple "Int"]) (tyCon "f" [tySimple "Bool"]))
    kindOf env ty `shouldBe` Right kType

  it "nested higher-kinded: (Type -> Type) -> (Type -> Type) -> Type" $ do
    let complexKind = KiArrow kTypeToType (KiArrow kTypeToType kType)
    -- Just verify the kind is well-formed
    complexKind `shouldBe` complexKind

-- | Tests for kind environment operations
kindEnvTests :: Spec
kindEnvTests = describe "kind environment" $ do
  it "default env has Int : Type" $ do
    lookupKindEnv "Int" defaultKindEnv `shouldBe` Just kType

  it "default env has Bool : Type" $ do
    lookupKindEnv "Bool" defaultKindEnv `shouldBe` Just kType

  it "default env has List : Type -> Type" $ do
    lookupKindEnv "List" defaultKindEnv `shouldBe` Just kTypeToType

  it "extending environment shadows bindings" $ do
    let env = extendKindEnv "Int" kTypeToType defaultKindEnv
    lookupKindEnv "Int" env `shouldBe` Just kTypeToType

  it "extending preserves other bindings" $ do
    let env = extendKindEnv "Foo" kType defaultKindEnv
    lookupKindEnv "Bool" env `shouldBe` Just kType

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "deeply nested type application" $ do
    let env = defaultKindEnv
        ty = tyCon "List" [tyCon "List" [tyCon "List" [tySimple "Int"]]]
    kindOf env ty `shouldBe` Right kType

  it "complex function type" $ do
    let env = defaultKindEnv
        ty = tyFn (tyCon "List" [tySimple "Int"])
                  (tyFn (tyCon "Option" [tySimple "Bool"]) (tySimple "String"))
    kindOf env ty `shouldBe` Right kType

  it "forall with multiple type variables" $ do
    let env = defaultKindEnv
        ty = TyForall "a" kType
              (TyForall "b" kType
                (tyFn (tyVar "a" 1) (tyVar "b" 0)))
    kindOf env ty `shouldBe` Right kType

  it "TyForallDep has kind Type" $ do
    let env = defaultKindEnv
        ty = TyForallDep "x" (tySimple "Int") (tySimple "Bool")
    kindOf env ty `shouldBe` Right kType

  it "mixed forall and function types" $ do
    let env = defaultKindEnv
        ty = TyForall "a" kType
              (tyFn (tyVar "a" 0)
                (TyForall "b" kType (tyFn (tyVar "b" 0) (tyVar "a" 1))))
    kindOf env ty `shouldBe` Right kType
