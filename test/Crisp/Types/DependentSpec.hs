{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.DependentSpec
-- Description : Tests for dependent types (Pi and Sigma types)
--
-- TDD tests for Pi types (dependent function types) and Sigma types
-- (dependent pairs), including type-level evaluation and purity checking.

module Crisp.Types.DependentSpec (spec) where

import Test.Hspec

import Crisp.Types.Dependent
import Crisp.Types.Context
import Crisp.Core.Term (Term(..), Type(..), Pattern(..), Kind(..), EffectRow(..))

import qualified Data.Text as T

-- | Helper to create a simple type
tySimple :: T.Text -> Type
tySimple name = TyCon name []

-- | Helper to create a type variable
tyVar :: T.Text -> Int -> Type
tyVar = TyVar

-- | Helper to create a function type
tyFn :: Type -> Type -> Type
tyFn from to = TyPi "_" from EffEmpty to

-- | Helper to create a dependent function type
tyPi :: T.Text -> Type -> Type -> Type
tyPi name from to = TyPi name from EffEmpty to

-- | Helper to create a forall type
tyForall :: T.Text -> Kind -> Type -> Type
tyForall = TyForall

-- | Helper to create a dependent forall type
tyForallDep :: T.Text -> Type -> Type -> Type
tyForallDep = TyForallDep

-- | Helper to create a sigma type
tySigma :: T.Text -> Type -> Type -> Type
tySigma = TySigma

-- | Helper kind
kType :: Kind
kType = KiType 0

-- | Nat type
tyNat :: Type
tyNat = tySimple "Nat"

-- | Int type
tyInt :: Type
tyInt = tySimple "Int"

-- | Bool type
tyBool :: Type
tyBool = tySimple "Bool"

-- | Unit type
tyUnit :: Type
tyUnit = tySimple "Unit"

-- | Vec type constructor
tyVec :: Type -> Type -> Type
tyVec elemTy len = TyCon "Vec" [elemTy, len]

-- | Helper to create a Nat literal type
tyNatLit :: Int -> Type
tyNatLit = TyNatLit

-- | Helper for type-level addition
tyAdd :: Type -> Type -> Type
tyAdd = TyAdd

-- | Term helpers
var :: T.Text -> Int -> Term
var = TmVar

lam :: T.Text -> Type -> Term -> Term
lam = TmLam

app :: Term -> Term -> Term
app = TmApp

con :: T.Text -> [Term] -> Term
con name args = TmCon name [] args

spec :: Spec
spec = do
  describe "Dependent Types" $ do

    describe "Pi type formation" $ do
      it "creates simple Pi type" $ do
        let piType = tyPi "n" tyNat (tyVec tyInt (tyVar "n" 0))
        isPiType piType `shouldBe` True

      it "extracts Pi type components" $ do
        let piType = tyPi "x" tyNat tyBool
        piParamName piType `shouldBe` Just "x"
        piParamType piType `shouldBe` Just tyNat
        piReturnType piType `shouldBe` Just tyBool

      it "non-dependent function is special case of Pi" $ do
        let fnType = tyFn tyInt tyBool
        isPiType fnType `shouldBe` True
        piParamName fnType `shouldBe` Just "_"

      it "nested Pi types" $ do
        let piType = tyPi "n" tyNat (tyPi "m" tyNat (tyVec tyInt (tyAdd (tyVar "n" 1) (tyVar "m" 0))))
        isPiType piType `shouldBe` True
        case piReturnType piType of
          Just inner -> isPiType inner `shouldBe` True
          Nothing -> expectationFailure "Expected return type"

    describe "Pi type checking" $ do
      it "checks Pi type is well-formed" $ do
        let piType = tyPi "n" tyNat (tyVec tyInt (tyVar "n" 0))
            ctx = withPrelude
        checkPiTypeWellFormed ctx piType `shouldBe` Right ()

      it "rejects Pi with unbound variable in return type" $ do
        let piType = tyPi "n" tyNat (tyVec tyInt (tyVar "m" 0))  -- m not bound
            ctx = withPrelude
        checkPiTypeWellFormed ctx piType `shouldSatisfy` isLeft

      it "checks domain type is well-kinded" $ do
        let piType = tyPi "n" tyNat (tyVec tyInt (tyVar "n" 0))
            ctx = withPrelude
        getPiDomainKind ctx piType `shouldBe` Right (KiType 0)

    describe "dependent application" $ do
      it "substitutes in return type on application" $ do
        -- (n: Nat) -> Vec(Int, n) applied to 5 gives Vec(Int, 5)
        let piType = tyPi "n" tyNat (tyVec tyInt (tyVar "n" 0))
            argType = tyNatLit 5
            result = applyPiType piType argType
        result `shouldBe` Right (tyVec tyInt (tyNatLit 5))

      it "substitutes complex expressions" $ do
        -- (n: Nat) -> Vec(Int, n + 1) applied to 3 gives Vec(Int, 4)
        let piType = tyPi "n" tyNat (tyVec tyInt (tyAdd (tyVar "n" 0) (tyNatLit 1)))
            argType = tyNatLit 3
            result = applyPiType piType argType
        -- After evaluation: Vec(Int, 3 + 1) = Vec(Int, 4)
        normalizeResult result `shouldBe` Right (tyVec tyInt (tyNatLit 4))

      it "handles nested substitutions" $ do
        -- (n: Nat) -> (m: Nat) -> Vec(Int, n + m)
        let innerPi = tyPi "m" tyNat (tyVec tyInt (tyAdd (tyVar "n" 1) (tyVar "m" 0)))
            outerPi = tyPi "n" tyNat innerPi
            result1 = applyPiType outerPi (tyNatLit 2)
        -- After first application: (m: Nat) -> Vec(Int, 2 + m)
        case result1 of
          Right inner -> do
            let result2 = applyPiType inner (tyNatLit 3)
            normalizeResult result2 `shouldBe` Right (tyVec tyInt (tyNatLit 5))
          Left _ -> expectationFailure "First application failed"

    describe "type-level evaluation" $ do
      it "evaluates Nat addition" $ do
        let expr = tyAdd (tyNatLit 2) (tyNatLit 3)
        evalType expr `shouldBe` tyNatLit 5

      it "evaluates nested additions" $ do
        let expr = tyAdd (tyAdd (tyNatLit 1) (tyNatLit 2)) (tyNatLit 3)
        evalType expr `shouldBe` tyNatLit 6

      it "preserves variables in partial evaluation" $ do
        let expr = tyAdd (tyNatLit 2) (tyVar "n" 0)
        -- Can't fully evaluate, but 2 + n should simplify if possible
        evalType expr `shouldBe` tyAdd (tyNatLit 2) (tyVar "n" 0)

      it "evaluates type constructor applications" $ do
        let expr = tyVec tyInt (tyAdd (tyNatLit 1) (tyNatLit 2))
        evalType expr `shouldBe` tyVec tyInt (tyNatLit 3)

      it "handles zero in addition" $ do
        evalType (tyAdd (tyNatLit 0) (tyNatLit 5)) `shouldBe` tyNatLit 5
        evalType (tyAdd (tyNatLit 5) (tyNatLit 0)) `shouldBe` tyNatLit 5

    describe "purity enforcement" $ do
      it "rejects effectful expressions in types" $ do
        -- A type like Vec(Int, get()) should be rejected
        let effExpr = TyEffect "State" (tySimple "get")
        isPure effExpr `shouldBe` False

      it "accepts pure expressions in types" $ do
        let pureExpr = tyAdd (tyNatLit 2) (tyNatLit 3)
        isPure pureExpr `shouldBe` True

      it "accepts type variables as pure" $ do
        isPure (tyVar "n" 0) `shouldBe` True

      it "accepts type constructors as pure" $ do
        isPure (tyVec tyInt (tyNatLit 5)) `shouldBe` True

      it "rejects nested effectful expressions" $ do
        let effExpr = TyEffect "IO" (tySimple "read")
            nested = tyVec tyInt effExpr
        isPure nested `shouldBe` False

    describe "Sigma types (dependent pairs)" $ do
      it "creates Sigma type" $ do
        -- Exists n: Nat. Vec(Int, n)
        let sigma = tySigma "n" tyNat (tyVec tyInt (tyVar "n" 0))
        isSigmaType sigma `shouldBe` True

      it "extracts Sigma type components" $ do
        let sigma = tySigma "x" tyBool tyInt
        sigmaFstType sigma `shouldBe` Just tyBool
        sigmaSndTypeVar sigma `shouldBe` Just "x"

      it "checks Sigma type well-formedness" $ do
        let sigma = tySigma "n" tyNat (tyVec tyInt (tyVar "n" 0))
            ctx = withPrelude
        checkSigmaTypeWellFormed ctx sigma `shouldBe` Right ()

      it "rejects Sigma with unbound variable" $ do
        let sigma = tySigma "n" tyNat (tyVec tyInt (tyVar "m" 0))  -- m not n
            ctx = withPrelude
        checkSigmaTypeWellFormed ctx sigma `shouldSatisfy` isLeft

    describe "Sigma type operations" $ do
      it "projects first component type" $ do
        let sigma = tySigma "n" tyNat (tyVec tyInt (tyVar "n" 0))
        sigmaFst sigma `shouldBe` Right tyNat

      it "projects second component type with substitution" $ do
        -- If pair is (5, vec) : Sigma n:Nat. Vec(Int, n)
        -- then snd has type Vec(Int, 5)
        let sigma = tySigma "n" tyNat (tyVec tyInt (tyVar "n" 0))
            fstValue = tyNatLit 5
        sigmaSnd sigma fstValue `shouldBe` Right (tyVec tyInt (tyNatLit 5))

    describe "Pi type unification" $ do
      it "unifies identical Pi types" $ do
        let pi1 = tyPi "n" tyNat (tyVec tyInt (tyVar "n" 0))
            pi2 = tyPi "m" tyNat (tyVec tyInt (tyVar "m" 0))
        unifyPiTypes pi1 pi2 `shouldBe` Right ()

      it "unifies Pi types with alpha-equivalent bodies" $ do
        let pi1 = tyPi "x" tyNat (tyFn (tyVar "x" 0) tyBool)
            pi2 = tyPi "y" tyNat (tyFn (tyVar "y" 0) tyBool)
        unifyPiTypes pi1 pi2 `shouldBe` Right ()

      it "fails on mismatched domain types" $ do
        let pi1 = tyPi "n" tyNat (tyVar "n" 0)
            pi2 = tyPi "n" tyBool (tyVar "n" 0)
        unifyPiTypes pi1 pi2 `shouldSatisfy` isLeft

      it "fails on mismatched return types" $ do
        let pi1 = tyPi "n" tyNat tyInt
            pi2 = tyPi "n" tyNat tyBool
        unifyPiTypes pi1 pi2 `shouldSatisfy` isLeft

    describe "Sigma type unification" $ do
      it "unifies identical Sigma types" $ do
        let sig1 = tySigma "n" tyNat (tyVec tyInt (tyVar "n" 0))
            sig2 = tySigma "m" tyNat (tyVec tyInt (tyVar "m" 0))
        unifySigmaTypes sig1 sig2 `shouldBe` Right ()

      it "fails on mismatched first types" $ do
        let sig1 = tySigma "n" tyNat tyInt
            sig2 = tySigma "n" tyBool tyInt
        unifySigmaTypes sig1 sig2 `shouldSatisfy` isLeft

    describe "implicit Pi (forall) desugaring" $ do
      it "forall desugars to Pi with erased domain" $ do
        -- forall a. a -> a desugars to (a: Type) -> a -> a
        let forallType = tyForall "a" kType (tyFn (tyVar "a" 0) (tyVar "a" 0))
            piType = desugarForall forallType
        isPiType piType `shouldBe` True

      it "preserves forall kind annotation" $ do
        let forallType = tyForall "f" (KiArrow kType kType) (tyVar "f" 0)
            piType = desugarForall forallType
        -- Arrow kind (k1 -> k2) desugars to Pi type (_ : Type) -> Type
        case piParamType piType of
          Just (TyPi _ _ _ _) -> pure ()
          Just (TyUniverse _) -> pure ()  -- Simple kind case
          _ -> expectationFailure "Expected Pi or universe type for kind"

    describe "dependent pattern matching" $ do
      it "refines type in pattern branch" $ do
        -- match xs with Nil -> ... | Cons(h, t) -> ...
        -- In Cons branch, if xs: Vec(A, S(n)), then t: Vec(A, n)
        let vecType = tyVec tyInt (tyNatLit 3)
            ctx = withPrelude
            nilPattern = PatCon "Nil" []
            consPattern = PatCon "Cons" [PatVar "h", PatVar "t"]
        refinedCtx <- refineTypeInPattern ctx vecType consPattern
        -- The tail should have length 2
        lookupRefinedType "t" refinedCtx `shouldBe` Just (tyVec tyInt (tyNatLit 2))

      it "handles wildcard pattern (no refinement)" $ do
        let vecType = tyVec tyInt (tyNatLit 3)
            ctx = withPrelude
        refinedCtx <- refineTypeInPattern ctx vecType PatWild
        -- No new bindings
        refinedCtx `shouldBe` ctx

    describe "type normalization" $ do
      it "normalizes to weak head normal form" $ do
        let ty = tyAdd (tyAdd (tyNatLit 1) (tyNatLit 2)) (tyNatLit 3)
        whnf ty `shouldBe` tyNatLit 6

      it "normalizes type applications" $ do
        let ty = tyVec tyInt (tyAdd (tyNatLit 1) (tyNatLit 1))
        whnf ty `shouldBe` tyVec tyInt (tyNatLit 2)

      it "full normalization reduces all subterms" $ do
        let ty = tyPi "n" tyNat (tyVec tyInt (tyAdd (tyNatLit 0) (tyVar "n" 0)))
        normalize ty `shouldBe` tyPi "n" tyNat (tyVec tyInt (tyVar "n" 0))

    describe "type equality with evaluation" $ do
      it "equates types that normalize to same form" $ do
        let ty1 = tyVec tyInt (tyAdd (tyNatLit 2) (tyNatLit 3))
            ty2 = tyVec tyInt (tyNatLit 5)
        typeEquiv ty1 ty2 `shouldBe` True

      it "equates nested equal types" $ do
        let ty1 = tyPi "n" tyNat (tyVec tyInt (tyAdd (tyVar "n" 0) (tyNatLit 0)))
            ty2 = tyPi "n" tyNat (tyVec tyInt (tyVar "n" 0))
        typeEquiv ty1 ty2 `shouldBe` True

      it "distinguishes different types" $ do
        let ty1 = tyVec tyInt (tyNatLit 3)
            ty2 = tyVec tyInt (tyNatLit 4)
        typeEquiv ty1 ty2 `shouldBe` False

    describe "edge cases" $ do
      it "handles empty Sigma type" $ do
        let sigma = tySigma "x" tyUnit tyUnit
        checkSigmaTypeWellFormed withPrelude sigma `shouldBe` Right ()

      it "handles Pi type with universe domain" $ do
        let piType = TyPi "A" (TyUniverse 0) EffEmpty (tyFn (tyVar "A" 0) (tyVar "A" 0))
        checkPiTypeWellFormed withPrelude piType `shouldBe` Right ()

      it "handles deeply nested dependent types" $ do
        let innerSigma = tySigma "m" tyNat tyInt
            outerPi = tyPi "n" tyNat innerSigma
        isPiType outerPi `shouldBe` True
        case piReturnType outerPi of
          Just ret -> isSigmaType ret `shouldBe` True
          Nothing -> expectationFailure "Expected return type"

      it "type-level computation terminates" $ do
        -- Test that evaluation doesn't loop
        let expr = tyAdd (tyNatLit 100) (tyNatLit 100)
        evalType expr `shouldBe` tyNatLit 200

-- | Helper to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Helper to normalize a result
normalizeResult :: Either DependentTypeError Type -> Either DependentTypeError Type
normalizeResult (Right ty) = Right (evalType ty)
normalizeResult err = err

-- | Helper to lookup refined type
lookupRefinedType :: T.Text -> Context -> Maybe Type
lookupRefinedType name ctx = fst <$> lookupTerm name ctx
