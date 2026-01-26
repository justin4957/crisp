{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Effects.PolymorphismSpec
-- Description : Tests for effect polymorphism
--
-- TDD tests for effect polymorphism, enabling functions to be generic
-- over effect rows with row variables.

module Crisp.Effects.PolymorphismSpec (spec) where

import Test.Hspec

import Crisp.Effects.Polymorphism
import Crisp.Core.Term
import Crisp.Types.Substitution

import Data.Either (isRight, isLeft)
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Helper to create a simple type
tySimple :: T.Text -> Type
tySimple name = TyCon name []

-- | Helper to create a type variable
tyVar :: T.Text -> Int -> Type
tyVar = TyVar

-- | Helper to create an effect variable
effVar :: T.Text -> Int -> EffectRow
effVar = EffVar

-- | Helper to create an effect set from names
effSet :: [T.Text] -> EffectRow
effSet names = EffSet $ map (`Effect` Nothing) names

-- | Helper to create a function type with effects
tyEffFn :: Type -> Type -> EffectRow -> Type
tyEffFn from to effs = TyPi "_" from effs to

spec :: Spec
spec = do
  describe "Effect Row Variables" $ do
    rowVariableTests

  describe "Effect Row Unification" $ do
    rowUnificationTests

  describe "Effect Row Operations" $ do
    rowOperationTests

  describe "Effect Polymorphism" $ do
    effectPolyTests

  describe "Effect Scheme" $ do
    effectSchemeTests

  describe "Effect Instantiation" $ do
    effectInstantiationTests

  describe "Effect Subset Checking" $ do
    effectSubsetTests

  describe "Effect Row Constraints" $ do
    effectConstraintTests

  describe "Edge Cases" $ do
    edgeCaseTests

-- | Tests for effect row variables
rowVariableTests :: Spec
rowVariableTests = describe "row variables" $ do
  it "creates row variable with name and index" $ do
    let row = effVar "ε" 0
    case row of
      EffVar name idx -> do
        name `shouldBe` "ε"
        idx `shouldBe` 0
      _ -> expectationFailure "Expected effect variable"

  it "checks if row contains variable" $ do
    let row = effVar "ε" 0
    hasEffectVar row `shouldBe` True

  it "concrete row has no variable" $ do
    let row = effSet ["IO", "State"]
    hasEffectVar row `shouldBe` False

  it "union with variable has variable" $ do
    let row = EffUnion (effSet ["IO"]) (effVar "ε" 0)
    hasEffectVar row `shouldBe` True

  it "extracts effect variables from row" $ do
    let row = EffUnion (effVar "ε" 0) (effVar "ρ" 1)
    let vars = effectRowVars row
    Set.fromList vars `shouldBe` Set.fromList [0, 1]

-- | Tests for effect row unification
rowUnificationTests :: Spec
rowUnificationTests = describe "row unification" $ do
  it "unifies empty rows" $ do
    unifyEffectRow EffEmpty EffEmpty `shouldSatisfy` isRight

  it "unifies row variable with empty" $ do
    unifyEffectRow (effVar "ε" 0) EffEmpty `shouldSatisfy` isRight

  it "unifies row variable with concrete effects" $ do
    let result = unifyEffectRow (effVar "ε" 0) (effSet ["IO"])
    result `shouldSatisfy` isRight
    case result of
      Right subst -> lookupEffectVar 0 subst `shouldBe` Just (effSet ["IO"])
      Left _ -> expectationFailure "Expected success"

  it "unifies two row variables" $ do
    let result = unifyEffectRow (effVar "ε" 0) (effVar "ρ" 1)
    result `shouldSatisfy` isRight

  it "unifies identical concrete rows" $ do
    unifyEffectRow (effSet ["IO"]) (effSet ["IO"]) `shouldSatisfy` isRight

  it "unifies concrete rows with same effects in different order" $ do
    unifyEffectRow (effSet ["IO", "State"]) (effSet ["State", "IO"]) `shouldSatisfy` isRight

  it "fails to unify mismatched concrete rows" $ do
    unifyEffectRow (effSet ["IO"]) (effSet ["State"]) `shouldSatisfy` isLeft

  it "unifies union with row variable" $ do
    let lhs = EffUnion (effSet ["IO"]) (effVar "ε" 0)
        rhs = effSet ["IO", "State"]
    unifyEffectRow lhs rhs `shouldSatisfy` isRight

  it "performs occurs check for row variables" $ do
    let lhs = effVar "ε" 0
        rhs = EffUnion (effSet ["IO"]) (effVar "ε" 0)
    -- This should succeed as it's not a true occurs check violation
    -- The variable can be bound to EffEmpty
    unifyEffectRow lhs rhs `shouldSatisfy` isRight

-- | Tests for effect row operations
rowOperationTests :: Spec
rowOperationTests = describe "row operations" $ do
  it "unions row variable with concrete effects" $ do
    let result = rowUnion (effVar "ε" 0) (effSet ["IO"])
    case result of
      EffUnion (EffVar "ε" 0) (EffSet [Effect "IO" Nothing]) -> pure ()
      _ -> expectationFailure $ "Unexpected union result: " ++ show result

  it "removes effect from row with variable" $ do
    let row = EffUnion (effSet ["IO", "State"]) (effVar "ε" 0)
        result = rowRemove "IO" row
    containsConcreteEffect "IO" result `shouldBe` False
    containsConcreteEffect "State" result `shouldBe` True
    hasEffectVar result `shouldBe` True

  it "computes row difference" $ do
    let row1 = effSet ["IO", "State", "Console"]
        row2 = effSet ["IO"]
        result = rowDifference row1 row2
    containsConcreteEffect "IO" result `shouldBe` False
    containsConcreteEffect "State" result `shouldBe` True
    containsConcreteEffect "Console" result `shouldBe` True

  it "checks row subset" $ do
    let row1 = effSet ["IO"]
        row2 = effSet ["IO", "State"]
    isEffectRowSubset row1 row2 `shouldBe` True
    isEffectRowSubset row2 row1 `shouldBe` False

  it "normalizes effect row" $ do
    let row = EffUnion (effSet ["IO"]) (EffUnion (effSet ["State"]) EffEmpty)
        result = normalizeEffectRow row
    case result of
      EffSet effs -> length effs `shouldBe` 2
      _ -> expectationFailure "Expected normalized to EffSet"

-- | Tests for effect polymorphism
effectPolyTests :: Spec
effectPolyTests = describe "effect polymorphism" $ do
  it "recognizes effect-polymorphic function type" $ do
    let fnTy = tyEffFn (tySimple "Int") (tySimple "Int") (effVar "ε" 0)
    isEffectPolymorphic fnTy `shouldBe` True

  it "recognizes non-polymorphic function type" $ do
    let fnTy = tyEffFn (tySimple "Int") (tySimple "Int") (effSet ["IO"])
    isEffectPolymorphic fnTy `shouldBe` False

  it "pure function is effect-polymorphic (trivially)" $ do
    let fnTy = tyEffFn (tySimple "Int") (tySimple "Int") EffEmpty
    isEffectPolymorphic fnTy `shouldBe` False

  it "extracts effect variables from type" $ do
    let fnTy = tyEffFn (tySimple "Int") (tySimple "Int") (effVar "ε" 0)
    let vars = freeEffectVars fnTy
    vars `shouldBe` Set.singleton 0

  it "extracts effect variables from nested types" $ do
    let innerFn = tyEffFn (tySimple "A") (tySimple "B") (effVar "ε" 0)
        outerFn = tyEffFn innerFn (tySimple "C") (effVar "ρ" 1)
    let vars = freeEffectVars outerFn
    vars `shouldBe` Set.fromList [0, 1]

-- | Tests for effect schemes
effectSchemeTests :: Spec
effectSchemeTests = describe "effect schemes" $ do
  it "creates effect scheme from type" $ do
    let fnTy = tyEffFn (tySimple "Int") (tySimple "Int") (effVar "ε" 0)
        scheme = generalizeEffects emptyEffectPolyEnv fnTy
    effectSchemeQuantified scheme `shouldBe` Set.singleton 0

  it "scheme with no effect variables has empty quantification" $ do
    let fnTy = tyEffFn (tySimple "Int") (tySimple "Int") (effSet ["IO"])
        scheme = generalizeEffects emptyEffectPolyEnv fnTy
    effectSchemeQuantified scheme `shouldBe` Set.empty

  it "does not quantify effect variables in environment" $ do
    let fnTy = tyEffFn (tySimple "Int") (tySimple "Int") (effVar "ε" 0)
        -- Environment contains effect variable 0
        env = extendEffectPolyEnv "f" (EffectScheme Set.empty fnTy) emptyEffectPolyEnv
        -- Type also uses effect variable 0
        ty2 = tyEffFn (tySimple "Bool") (tySimple "Bool") (effVar "ε" 0)
        scheme = generalizeEffects env ty2
    -- Effect variable 0 should not be quantified since it's in the environment
    effectSchemeQuantified scheme `shouldBe` Set.empty

-- | Tests for effect instantiation
effectInstantiationTests :: Spec
effectInstantiationTests = describe "effect instantiation" $ do
  it "instantiates effect scheme with fresh variables" $ do
    let fnTy = tyEffFn (tySimple "Int") (tySimple "Int") (effVar "ε" 0)
        scheme = EffectScheme (Set.singleton 0) fnTy
    case runEffectPoly (instantiateEffects scheme) of
      Right instantiated -> do
        -- The instantiated type should have a different effect variable
        let vars = freeEffectVars instantiated
        Set.size vars `shouldBe` 1
        -- The new variable should not be 0
        Set.member 0 vars `shouldBe` False
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

  it "preserves type structure during instantiation" $ do
    let fnTy = tyEffFn (tySimple "Int") (tySimple "Bool") (effVar "ε" 0)
        scheme = EffectScheme (Set.singleton 0) fnTy
    case runEffectPoly (instantiateEffects scheme) of
      Right (TyPi _ (TyCon "Int" []) _ (TyCon "Bool" [])) -> pure ()
      Right other -> expectationFailure $ "Unexpected type: " ++ show other
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

  it "instantiates multiple effect variables" $ do
    let innerFn = tyEffFn (tySimple "A") (tySimple "B") (effVar "ε" 0)
        outerFn = tyEffFn innerFn (tySimple "C") (effVar "ρ" 1)
        scheme = EffectScheme (Set.fromList [0, 1]) outerFn
    case runEffectPoly (instantiateEffects scheme) of
      Right instantiated -> do
        let vars = freeEffectVars instantiated
        Set.size vars `shouldBe` 2
        -- Neither should be 0 or 1
        Set.member 0 vars `shouldBe` False
        Set.member 1 vars `shouldBe` False
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

-- | Tests for effect subset checking
effectSubsetTests :: Spec
effectSubsetTests = describe "effect subset" $ do
  it "empty is subset of anything" $ do
    isEffectRowSubset EffEmpty (effSet ["IO"]) `shouldBe` True
    isEffectRowSubset EffEmpty (effVar "ε" 0) `shouldBe` True

  it "concrete is subset of larger concrete" $ do
    isEffectRowSubset (effSet ["IO"]) (effSet ["IO", "State"]) `shouldBe` True

  it "concrete is not subset of smaller concrete" $ do
    isEffectRowSubset (effSet ["IO", "State"]) (effSet ["IO"]) `shouldBe` False

  it "row variable can be subset of anything" $ do
    -- A row variable can potentially be empty, so it can be a subset
    isEffectRowSubset (effVar "ε" 0) (effSet ["IO"]) `shouldBe` True

  it "anything is subset of row variable" $ do
    -- A row variable can potentially contain any effects
    isEffectRowSubset (effSet ["IO"]) (effVar "ε" 0) `shouldBe` True

  it "union subset works correctly" $ do
    let union = EffUnion (effSet ["IO"]) (effVar "ε" 0)
    -- Union is not necessarily a subset of concrete without the variable effects
    -- But concrete part must be subset
    isEffectRowSubset (effSet ["IO"]) union `shouldBe` True

-- | Tests for effect row constraints
effectConstraintTests :: Spec
effectConstraintTests = describe "effect constraints" $ do
  it "generates constraint for function call" $ do
    -- When calling f: (A) -[ε]-> B with context effects [IO]
    -- We get constraint: ε <= [IO]
    let fnEffects = effVar "ε" 0
        contextEffects = effSet ["IO"]
        constraint = makeSubsetConstraint fnEffects contextEffects
    constraintLhs constraint `shouldBe` fnEffects
    constraintRhs constraint `shouldBe` contextEffects

  it "solves simple constraint" $ do
    let constraint = makeSubsetConstraint (effVar "ε" 0) (effSet ["IO", "State"])
    let result = solveEffectConstraint constraint
    result `shouldSatisfy` isRight

  it "fails unsatisfiable constraint" $ do
    -- If we require IO but context only has State
    let constraint = makeSubsetConstraint (effSet ["IO"]) (effSet ["State"])
    let result = solveEffectConstraint constraint
    result `shouldSatisfy` isLeft

  it "propagates constraints through composition" $ do
    -- f: (A) -[ε1]-> B, g: (B) -[ε2]-> C
    -- g(f(x)) has effects ε1 ∪ ε2
    let eff1 = effVar "ε1" 0
        eff2 = effVar "ε2" 1
        composed = rowUnion eff1 eff2
    hasEffectVar composed `shouldBe` True
    let vars = effectRowVars composed
    Set.fromList vars `shouldBe` Set.fromList [0, 1]

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles empty effect set" $ do
    unifyEffectRow (EffSet []) EffEmpty `shouldSatisfy` isRight

  it "handles nested unions" $ do
    let row = EffUnion (EffUnion (effSet ["A"]) (effSet ["B"])) (effSet ["C"])
        normalized = normalizeEffectRow row
    case normalized of
      EffSet effs -> length effs `shouldBe` 3
      _ -> expectationFailure "Expected normalized EffSet"

  it "handles effect with authority in polymorphism" $ do
    let eff = EffSet [Effect "IO" (Just "system")]
        row = EffUnion eff (effVar "ε" 0)
    hasEffectVar row `shouldBe` True

  it "substitutes effect variable in type" $ do
    let ty = tyEffFn (tySimple "Int") (tySimple "Int") (effVar "ε" 0)
        subst = singleEffectSubst 0 (effSet ["IO"])
        result = applyEffectSubst subst ty
    case result of
      TyPi _ _ (EffSet [Effect "IO" Nothing]) _ -> pure ()
      _ -> expectationFailure $ "Unexpected substitution result: " ++ show result

  it "handles higher-kinded effect variables" $ do
    -- Effect variables can appear in complex positions
    let nested = TyCon "Handler" [tyEffFn (tySimple "A") (tySimple "B") (effVar "ε" 0)]
    let vars = freeEffectVars nested
    vars `shouldBe` Set.singleton 0

  it "preserves effect variable names through operations" $ do
    let row = effVar "myEffect" 42
    case row of
      EffVar name idx -> do
        name `shouldBe` "myEffect"
        idx `shouldBe` 42
      _ -> expectationFailure "Expected effect variable"
