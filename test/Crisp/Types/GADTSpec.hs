{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.GADTSpec
-- Description : Tests for GADT typing with index refinement
--
-- TDD tests for GADT (Generalized Algebraic Data Types) support including:
-- - GADT declaration validation
-- - Type index refinement during pattern matching
-- - Impossible branch detection
-- - Existential types in constructors
-- - Nested GADT matching with accumulated refinements

module Crisp.Types.GADTSpec (spec) where

import Test.Hspec

import Crisp.Types.GADT
import Crisp.Types.Constructor (TypeDecl(..), ConDecl(..))
import Crisp.Types.Unify (Substitution, applySubst)
import Crisp.Core.Term

import qualified Data.Text as T

-- | Helper to create a simple type
tySimple :: T.Text -> Type
tySimple name = TyCon name []

-- | Helper to create a type constructor application
tyCon :: T.Text -> [Type] -> Type
tyCon = TyCon

-- | Helper to create a type variable
tyVar :: T.Text -> Int -> Type
tyVar = TyVar

-- | Standard kind
kType :: Kind
kType = KiType 0

-- | Helper to create Nat type
tyNat :: Type
tyNat = tySimple "Nat"

-- | Helper to create Int type
tyInt :: Type
tyInt = tySimple "Int"

-- | Helper to create Bool type
tyBool :: Type
tyBool = tySimple "Bool"

-- | Helper for Nat literal type
tyNatLit :: Int -> Type
tyNatLit = TyNatLit

-- | Create standard Expr GADT for testing
exprGadt :: TypeDecl
exprGadt = TypeDecl
  { typeDeclName = "Expr"
  , typeDeclParams = [("A", kType)]
  , typeDeclConstructors =
      [ ConDecl "IntLit" [tyInt] (Just $ tyCon "Expr" [tyInt])
      , ConDecl "BoolLit" [tyBool] (Just $ tyCon "Expr" [tyBool])
      , ConDecl "Add" [tyCon "Expr" [tyInt], tyCon "Expr" [tyInt]]
                      (Just $ tyCon "Expr" [tyInt])
      , ConDecl "If" [tyCon "Expr" [tyBool], tyCon "Expr" [tyVar "A" 0], tyCon "Expr" [tyVar "A" 0]]
                     (Just $ tyCon "Expr" [tyVar "A" 0])
      ]
  }

-- | Create length-indexed Vec GADT
vecGadt :: TypeDecl
vecGadt = TypeDecl
  { typeDeclName = "Vec"
  , typeDeclParams = [("A", kType), ("n", kType)]
  , typeDeclConstructors =
      [ ConDecl "VNil" [] (Just $ tyCon "Vec" [tyVar "A" 0, tyNatLit 0])
      , ConDecl "VCons" [tyVar "A" 0, tyCon "Vec" [tyVar "A" 0, tyVar "n" 1]]
                        (Just $ tyCon "Vec" [tyVar "A" 0, TyAdd (tyVar "n" 1) (tyNatLit 1)])
      ]
  }

-- | Create type equality witness GADT
equalGadt :: TypeDecl
equalGadt = TypeDecl
  { typeDeclName = "Equal"
  , typeDeclParams = [("A", kType), ("B", kType)]
  , typeDeclConstructors =
      [ ConDecl "Refl" [] (Just $ tyCon "Equal" [tyVar "A" 0, tyVar "A" 0])
      ]
  }

spec :: Spec
spec = do
  describe "GADT Typing" $ do
    gadtDeclarationTests
    gadtRefinementTests
    impossibleBranchTests
    existentialTests
    nestedGadtTests
    typeEqualityTests
    gadtUnificationTests
    edgeCaseTests

-- | Tests for GADT declaration validation
gadtDeclarationTests :: Spec
gadtDeclarationTests = describe "GADT declaration" $ do
  it "validates Expr GADT with refined return types" $ do
    let env = emptyGadtEnv
    checkGadtDecl env exprGadt `shouldBe` Right ()

  it "records GADT return types correctly" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
    case getGadtConstructorInfo "IntLit" env of
      Just info -> gadtReturnType info `shouldBe` tyCon "Expr" [tyInt]
      Nothing -> expectationFailure "Constructor not found"

  it "validates Vec GADT with index types" $ do
    let env = emptyGadtEnv
    checkGadtDecl env vecGadt `shouldBe` Right ()

  it "records VNil with zero-length return type" $ do
    let env = addGadtDecl vecGadt emptyGadtEnv
    case getGadtConstructorInfo "VNil" env of
      Just info -> gadtReturnType info `shouldBe` tyCon "Vec" [tyVar "A" 0, tyNatLit 0]
      Nothing -> expectationFailure "Constructor not found"

  it "records VCons with incremented length return type" $ do
    let env = addGadtDecl vecGadt emptyGadtEnv
    case getGadtConstructorInfo "VCons" env of
      Just info -> gadtReturnType info `shouldBe`
                   tyCon "Vec" [tyVar "A" 0, TyAdd (tyVar "n" 1) (tyNatLit 1)]
      Nothing -> expectationFailure "Constructor not found"

  it "validates type equality witness" $ do
    let env = emptyGadtEnv
    checkGadtDecl env equalGadt `shouldBe` Right ()

  it "rejects GADT return type with wrong type constructor" $ do
    let badGadt = TypeDecl
          { typeDeclName = "Foo"
          , typeDeclParams = [("A", kType)]
          , typeDeclConstructors =
              [ ConDecl "Bad" [tyInt] (Just $ tyCon "Bar" [tyInt])  -- Wrong type!
              ]
          }
    checkGadtDecl emptyGadtEnv badGadt `shouldSatisfy` isLeft

  it "accepts default return type (non-GADT)" $ do
    let normalDecl = TypeDecl
          { typeDeclName = "Option"
          , typeDeclParams = [("A", kType)]
          , typeDeclConstructors =
              [ ConDecl "None" [] Nothing
              , ConDecl "Some" [tyVar "A" 0] Nothing
              ]
          }
    checkGadtDecl emptyGadtEnv normalDecl `shouldBe` Right ()

-- | Tests for type refinement during pattern matching
gadtRefinementTests :: Spec
gadtRefinementTests = describe "GADT refinement" $ do
  it "refines type variable to Int in IntLit branch" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyVar "A" 0]
        pattern' = PatCon "IntLit" [PatVar "n"]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement -> do
        gadtRefinedBindings refinement `shouldBe` [("n", tyInt)]
        gadtTypeConstraints refinement `shouldContain` [(tyInt, tyVar "A" 0)]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "refines type variable to Bool in BoolLit branch" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyVar "A" 0]
        pattern' = PatCon "BoolLit" [PatVar "b"]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement -> do
        gadtRefinedBindings refinement `shouldBe` [("b", tyBool)]
        gadtTypeConstraints refinement `shouldContain` [(tyBool, tyVar "A" 0)]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "preserves type variable in If branch" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyVar "A" 0]
        pattern' = PatCon "If" [PatVar "c", PatVar "t", PatVar "e"]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement -> do
        gadtRefinedBindings refinement `shouldContain`
          [("c", tyCon "Expr" [tyBool])]
        gadtRefinedBindings refinement `shouldContain`
          [("t", tyCon "Expr" [tyVar "A" 0])]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "refines Vec length in VCons branch" $ do
    let env = addGadtDecl vecGadt emptyGadtEnv
        -- Matching on Vec(Int, 3) should give tail : Vec(Int, 2)
        scrutineeTy = tyCon "Vec" [tyInt, tyNatLit 3]
        pattern' = PatCon "VCons" [PatVar "h", PatVar "t"]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement -> do
        gadtRefinedBindings refinement `shouldContain` [("h", tyInt)]
        -- Tail should have decremented length
        gadtRefinedBindings refinement `shouldContain`
          [("t", tyCon "Vec" [tyInt, tyNatLit 2])]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "refines concrete scrutinee type" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyInt]  -- Concrete Int
        pattern' = PatCon "IntLit" [PatVar "n"]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement ->
        gadtRefinedBindings refinement `shouldBe` [("n", tyInt)]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "handles wildcard patterns (no refinement)" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyVar "A" 0]
    case refineGadtPattern env scrutineeTy PatWild of
      Right refinement -> do
        gadtRefinedBindings refinement `shouldBe` []
        gadtTypeConstraints refinement `shouldBe` []
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "handles variable patterns" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyVar "A" 0]
    case refineGadtPattern env scrutineeTy (PatVar "x") of
      Right refinement ->
        gadtRefinedBindings refinement `shouldBe` [("x", tyCon "Expr" [tyVar "A" 0])]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

-- | Tests for impossible branch detection
impossibleBranchTests :: Spec
impossibleBranchTests = describe "impossible branches" $ do
  it "detects impossible BoolLit branch for Expr(Int)" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyInt]  -- Expr(Int), not Expr(Bool)
        pattern' = PatCon "BoolLit" [PatVar "b"]
    isImpossibleBranch env scrutineeTy pattern' `shouldBe` True

  it "detects impossible IntLit branch for Expr(Bool)" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyBool]
        pattern' = PatCon "IntLit" [PatVar "n"]
    isImpossibleBranch env scrutineeTy pattern' `shouldBe` True

  it "allows IntLit branch for Expr(Int)" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyInt]
        pattern' = PatCon "IntLit" [PatVar "n"]
    isImpossibleBranch env scrutineeTy pattern' `shouldBe` False

  it "allows any branch for polymorphic Expr(A)" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyVar "A" 0]
        intLitPat = PatCon "IntLit" [PatVar "n"]
        boolLitPat = PatCon "BoolLit" [PatVar "b"]
    isImpossibleBranch env scrutineeTy intLitPat `shouldBe` False
    isImpossibleBranch env scrutineeTy boolLitPat `shouldBe` False

  it "detects impossible VCons branch for Vec(A, 0)" $ do
    let env = addGadtDecl vecGadt emptyGadtEnv
        scrutineeTy = tyCon "Vec" [tyInt, tyNatLit 0]
        pattern' = PatCon "VCons" [PatVar "h", PatVar "t"]
    isImpossibleBranch env scrutineeTy pattern' `shouldBe` True

  it "allows VNil branch for Vec(A, 0)" $ do
    let env = addGadtDecl vecGadt emptyGadtEnv
        scrutineeTy = tyCon "Vec" [tyInt, tyNatLit 0]
        pattern' = PatCon "VNil" []
    isImpossibleBranch env scrutineeTy pattern' `shouldBe` False

  it "detects impossible VNil branch for Vec(A, S(n))" $ do
    let env = addGadtDecl vecGadt emptyGadtEnv
        scrutineeTy = tyCon "Vec" [tyInt, TyAdd (tyVar "n" 0) (tyNatLit 1)]
        pattern' = PatCon "VNil" []
    isImpossibleBranch env scrutineeTy pattern' `shouldBe` True

-- | Tests for existential types in constructors
existentialTests :: Spec
existentialTests = describe "existential types" $ do
  it "handles existential type in GADT constructor" $ do
    -- type Dyn = MkDyn : forall A. A -> Dyn
    let dynGadt = TypeDecl
          { typeDeclName = "Dyn"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "MkDyn" [tyVar "A" 0] (Just $ tySimple "Dyn")
              ]
          }
    checkGadtDecl emptyGadtEnv dynGadt `shouldBe` Right ()

  it "existential type variable is fresh in pattern match" $ do
    let dynGadt = TypeDecl
          { typeDeclName = "Dyn"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "MkDyn" [tyVar "A" 0] (Just $ tySimple "Dyn")
              ]
          }
        env = addGadtDecl dynGadt emptyGadtEnv
        scrutineeTy = tySimple "Dyn"
        pattern' = PatCon "MkDyn" [PatVar "x"]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement -> do
        -- The binding should have an existential type variable
        let bindings = gadtRefinedBindings refinement
        length bindings `shouldBe` 1
        case bindings of
          [(name, ty)] -> do
            name `shouldBe` "x"
            isExistentialType ty `shouldBe` True
          _ -> expectationFailure "Expected single binding"
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "records existential constraints" $ do
    let dynGadt = TypeDecl
          { typeDeclName = "Dyn"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "MkDyn" [tyVar "A" 0] (Just $ tySimple "Dyn")
              ]
          }
        env = addGadtDecl dynGadt emptyGadtEnv
        scrutineeTy = tySimple "Dyn"
        pattern' = PatCon "MkDyn" [PatVar "x"]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement ->
        gadtExistentials refinement `shouldSatisfy` (not . null)
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

-- | Tests for nested GADT pattern matching
nestedGadtTests :: Spec
nestedGadtTests = describe "nested GADT matching" $ do
  it "accumulates refinements from nested patterns" $ do
    -- Pattern: If(BoolLit(b), t, e)
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyVar "A" 0]
        -- Nested pattern: If with BoolLit as condition
        innerPat = PatCon "BoolLit" [PatVar "b"]
        pattern' = PatCon "If" [innerPat, PatVar "t", PatVar "e"]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement -> do
        -- b should be Bool
        gadtRefinedBindings refinement `shouldContain` [("b", tyBool)]
        -- t and e should be Expr(A)
        gadtRefinedBindings refinement `shouldContain`
          [("t", tyCon "Expr" [tyVar "A" 0])]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "propagates constraints through nested patterns" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyInt]
        -- Pattern: Add(IntLit(n), IntLit(m))
        pattern' = PatCon "Add"
          [PatCon "IntLit" [PatVar "n"], PatCon "IntLit" [PatVar "m"]]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement -> do
        gadtRefinedBindings refinement `shouldContain` [("n", tyInt)]
        gadtRefinedBindings refinement `shouldContain` [("m", tyInt)]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "handles nested Vec patterns" $ do
    let env = addGadtDecl vecGadt emptyGadtEnv
        -- Vec(Int, 2)
        scrutineeTy = tyCon "Vec" [tyInt, tyNatLit 2]
        -- Pattern: VCons(x, VCons(y, VNil))
        pattern' = PatCon "VCons"
          [PatVar "x", PatCon "VCons" [PatVar "y", PatCon "VNil" []]]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement -> do
        gadtRefinedBindings refinement `shouldContain` [("x", tyInt)]
        gadtRefinedBindings refinement `shouldContain` [("y", tyInt)]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

-- | Tests for type equality witnesses
typeEqualityTests :: Spec
typeEqualityTests = describe "type equality witness" $ do
  it "Refl constructor equates types" $ do
    let env = addGadtDecl equalGadt emptyGadtEnv
        scrutineeTy = tyCon "Equal" [tyVar "A" 0, tyVar "B" 1]
        pattern' = PatCon "Refl" []
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement ->
        -- Should have constraint A = B
        -- Refl has return type Equal(A, A), which unifies with Equal(A, B)
        -- This gives constraint A = B (or equivalently, B = A)
        gadtTypeConstraints refinement `shouldSatisfy`
          (\cs -> any (\(t1, t2) -> isTypeVarPair t1 t2) cs)
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

  it "uses equality proof for type casting" $ do
    let env = addGadtDecl equalGadt emptyGadtEnv
        -- Equal(Int, A) pattern match proves A = Int
        scrutineeTy = tyCon "Equal" [tyInt, tyVar "A" 0]
        pattern' = PatCon "Refl" []
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement ->
        gadtTypeConstraints refinement `shouldContain`
          [(tyInt, tyVar "A" 0)]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

-- | Tests for GADT-related unification
gadtUnificationTests :: Spec
gadtUnificationTests = describe "GADT unification" $ do
  it "unifies constructor return type with scrutinee" $ do
    let conRetTy = tyCon "Expr" [tyInt]
        scrutineeTy = tyCon "Expr" [tyVar "A" 0]
    case unifyGadtTypes conRetTy scrutineeTy of
      Right subst ->
        applySubst subst (tyVar "A" 0) `shouldBe` tyInt
      Left err -> expectationFailure $ "Unification failed: " ++ show err

  it "fails to unify incompatible index types" $ do
    let conRetTy = tyCon "Expr" [tyInt]
        scrutineeTy = tyCon "Expr" [tyBool]
    unifyGadtTypes conRetTy scrutineeTy `shouldSatisfy` isLeft

  it "unifies Vec length indices" $ do
    let conRetTy = tyCon "Vec" [tyVar "A" 0, tyNatLit 0]
        scrutineeTy = tyCon "Vec" [tyInt, tyVar "n" 1]
    case unifyGadtTypes conRetTy scrutineeTy of
      Right subst -> do
        applySubst subst (tyVar "A" 0) `shouldBe` tyInt
        applySubst subst (tyVar "n" 1) `shouldBe` tyNatLit 0
      Left err -> expectationFailure $ "Unification failed: " ++ show err

  it "handles type-level arithmetic in unification" $ do
    -- VCons return: Vec(A, n + 1)
    -- Scrutinee: Vec(Int, 3)
    let conRetTy = tyCon "Vec" [tyVar "A" 0, TyAdd (tyVar "n" 1) (tyNatLit 1)]
        scrutineeTy = tyCon "Vec" [tyInt, tyNatLit 3]
    case unifyGadtTypes conRetTy scrutineeTy of
      Right subst -> do
        applySubst subst (tyVar "A" 0) `shouldBe` tyInt
        -- n + 1 = 3, so n = 2
        applySubst subst (tyVar "n" 1) `shouldBe` tyNatLit 2
      Left err -> expectationFailure $ "Unification failed: " ++ show err

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles empty constructor list" $ do
    let emptyDecl = TypeDecl "Empty" [("A", kType)] []
    checkGadtDecl emptyGadtEnv emptyDecl `shouldBe` Right ()

  it "handles single-constructor GADT" $ do
    checkGadtDecl emptyGadtEnv equalGadt `shouldBe` Right ()

  it "handles constructor with no params but refined return" $ do
    let unitGadt = TypeDecl
          { typeDeclName = "Unit"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "MkUnit" [] (Just $ tySimple "Unit")
              ]
          }
    checkGadtDecl emptyGadtEnv unitGadt `shouldBe` Right ()

  it "handles deeply nested type indices" $ do
    let nestedGadt = TypeDecl
          { typeDeclName = "Deep"
          , typeDeclParams = [("A", kType)]
          , typeDeclConstructors =
              [ ConDecl "MkDeep" [tyVar "A" 0]
                  (Just $ tyCon "Deep" [tyCon "Option" [tyVar "A" 0]])
              ]
          }
    checkGadtDecl emptyGadtEnv nestedGadt `shouldBe` Right ()

  it "handles multiple type parameters in GADT" $ do
    let multiGadt = TypeDecl
          { typeDeclName = "Multi"
          , typeDeclParams = [("A", kType), ("B", kType), ("C", kType)]
          , typeDeclConstructors =
              [ ConDecl "MkMulti" [tyVar "A" 0, tyVar "B" 1]
                  (Just $ tyCon "Multi" [tyVar "A" 0, tyVar "B" 1, tyVar "C" 2])
              ]
          }
    checkGadtDecl emptyGadtEnv multiGadt `shouldBe` Right ()

  it "preserves order of refined bindings" $ do
    let env = addGadtDecl exprGadt emptyGadtEnv
        scrutineeTy = tyCon "Expr" [tyVar "A" 0]
        pattern' = PatCon "If" [PatVar "c", PatVar "t", PatVar "e"]
    case refineGadtPattern env scrutineeTy pattern' of
      Right refinement -> do
        let names = map fst (gadtRefinedBindings refinement)
        names `shouldBe` ["c", "t", "e"]
      Left err -> expectationFailure $ "Refinement failed: " ++ show err

-- | Helper to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Helper to check if a type is existential (has unbound type variable)
isExistentialType :: Type -> Bool
isExistentialType (TyVar _ _) = True
isExistentialType _ = False

-- | Helper to check if both types are type variables (for equality witness)
isTypeVarPair :: Type -> Type -> Bool
isTypeVarPair (TyVar _ _) (TyVar _ _) = True
isTypeVarPair _ _ = False
