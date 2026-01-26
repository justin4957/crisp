{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.ConstructorSpec
-- Description : Tests for constructor typing
--
-- TDD tests for algebraic data type constructor typing, including
-- constructor declarations, applications, and pattern matching.

module Crisp.Types.ConstructorSpec (spec) where

import Test.Hspec

import Crisp.Types.Constructor
import Crisp.Types.Context
import Crisp.Core.Term

import Data.Either (isRight, isLeft)
import qualified Data.Set as Set
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

-- | Helper to create a function type
tyFn :: Type -> Type -> Type
tyFn from to = TyPi "_" from EffEmpty to

-- | Helper to create a forall type
tyForall :: T.Text -> Type -> Type
tyForall name body = TyForall name (KiType 0) body

spec :: Spec
spec = do
  describe "Type Declaration" $ do
    typeDeclTests

  describe "Constructor Declaration" $ do
    constructorDeclTests

  describe "Constructor Type Synthesis" $ do
    constructorTypeSynthTests

  describe "Constructor Application" $ do
    constructorAppTests

  describe "Pattern Matching" $ do
    patternMatchTests

  describe "Parameterized Types" $ do
    parameterizedTypeTests

  describe "Recursive Types" $ do
    recursiveTypeTests

  describe "GADT-Style Constructors" $ do
    gadtTests

  describe "Type Instantiation" $ do
    typeInstantiationTests

  describe "Edge Cases" $ do
    edgeCaseTests

-- | Tests for type declarations
typeDeclTests :: Spec
typeDeclTests = describe "type declarations" $ do
  it "registers simple type declaration" $ do
    let decl = TypeDecl
          { typeDeclName = "Bool"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "True" [] Nothing
              , ConDecl "False" [] Nothing
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "registers type with parameters" $ do
    let decl = TypeDecl
          { typeDeclName = "Option"
          , typeDeclParams = [("A", KiType 0)]
          , typeDeclConstructors =
              [ ConDecl "None" [] Nothing
              , ConDecl "Some" [tyVar "A" 0] Nothing
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "registers type with multiple parameters" $ do
    let decl = TypeDecl
          { typeDeclName = "Either"
          , typeDeclParams = [("L", KiType 0), ("R", KiType 0)]
          , typeDeclConstructors =
              [ ConDecl "Left" [tyVar "L" 0] Nothing
              , ConDecl "Right" [tyVar "R" 1] Nothing
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "rejects duplicate type names" $ do
    let decl = TypeDecl "Bool" [] [ConDecl "X" [] Nothing]
        env1 = addTypeDecl decl emptyConstructorEnv
    checkTypeDecl env1 decl `shouldSatisfy` isLeft

  it "rejects duplicate constructor names" $ do
    let decl = TypeDecl
          { typeDeclName = "Bad"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "Dup" [] Nothing
              , ConDecl "Dup" [] Nothing
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isLeft

-- | Tests for constructor declarations
constructorDeclTests :: Spec
constructorDeclTests = describe "constructor declarations" $ do
  it "nullary constructor has correct type" $ do
    let decl = TypeDecl "Unit" [] [ConDecl "Unit" [] Nothing]
        env = addTypeDecl decl emptyConstructorEnv
    case lookupConstructorType "Unit" env of
      Just ty -> ty `shouldBe` tySimple "Unit"
      Nothing -> expectationFailure "Constructor not found"

  it "unary constructor has function type" $ do
    let decl = TypeDecl
          { typeDeclName = "Box"
          , typeDeclParams = [("A", KiType 0)]
          , typeDeclConstructors = [ConDecl "Box" [tyVar "A" 0] Nothing]
          }
        env = addTypeDecl decl emptyConstructorEnv
    case lookupConstructorType "Box" env of
      Just ty -> case ty of
        TyForall _ _ (TyPi _ paramTy _ retTy) -> do
          paramTy `shouldBe` tyVar "A" 0
          retTy `shouldBe` tyCon "Box" [tyVar "A" 0]
        _ -> expectationFailure "Expected forall function type"
      Nothing -> expectationFailure "Constructor not found"

  it "binary constructor has curried function type" $ do
    let decl = TypeDecl
          { typeDeclName = "Pair"
          , typeDeclParams = [("A", KiType 0), ("B", KiType 0)]
          , typeDeclConstructors =
              [ConDecl "Pair" [tyVar "A" 0, tyVar "B" 1] Nothing]
          }
        env = addTypeDecl decl emptyConstructorEnv
    case lookupConstructorType "Pair" env of
      Just ty -> case ty of
        TyForall _ _ (TyForall _ _ (TyPi _ _ _ (TyPi _ _ _ retTy))) ->
          -- With nested foralls, indices are: A=0, B=1 in the original declaration
          retTy `shouldBe` tyCon "Pair" [tyVar "A" 0, tyVar "B" 1]
        _ -> expectationFailure $ "Expected nested forall: " ++ show ty
      Nothing -> expectationFailure "Constructor not found"

  it "records constructor return type" $ do
    let decl = TypeDecl
          { typeDeclName = "Bool"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "True" [] Nothing
              , ConDecl "False" [] Nothing
              ]
          }
        env = addTypeDecl decl emptyConstructorEnv
    case getConstructorInfo "True" env of
      Just info -> constructorInfoReturnType info `shouldBe` tySimple "Bool"
      Nothing -> expectationFailure "Constructor not found"

-- | Tests for constructor type synthesis
constructorTypeSynthTests :: Spec
constructorTypeSynthTests = describe "type synthesis" $ do
  it "synthesizes nullary constructor type" $ do
    let env = addTypeDecl
          (TypeDecl "Bool" []
            [ ConDecl "True" [] Nothing
            , ConDecl "False" [] Nothing
            ])
          emptyConstructorEnv
    synthesizeConstructor env "True" [] `shouldBe` Right (tySimple "Bool")

  it "synthesizes parameterized constructor type" $ do
    let env = addTypeDecl
          (TypeDecl "Option" [("A", KiType 0)]
            [ ConDecl "None" [] Nothing
            , ConDecl "Some" [tyVar "A" 0] Nothing
            ])
          emptyConstructorEnv
    case synthesizeConstructor env "None" [] of
      Right ty -> case ty of
        TyForall _ _ (TyCon "Option" [TyVar "A" 0]) -> pure ()
        _ -> expectationFailure $ "Unexpected type: " ++ show ty
      Left err -> expectationFailure $ "Synthesis failed: " ++ show err

  it "returns error for unknown constructor" $ do
    synthesizeConstructor emptyConstructorEnv "Unknown" []
      `shouldSatisfy` isLeft

-- | Tests for constructor application
constructorAppTests :: Spec
constructorAppTests = describe "constructor application" $ do
  it "checks nullary constructor application" $ do
    let env = addTypeDecl
          (TypeDecl "Bool" []
            [ ConDecl "True" [] Nothing
            , ConDecl "False" [] Nothing
            ])
          emptyConstructorEnv
    checkConstructorApp env "True" [] (tySimple "Bool") `shouldSatisfy` isRight

  it "checks unary constructor with correct argument" $ do
    let env = addTypeDecl
          (TypeDecl "Option" [("A", KiType 0)]
            [ ConDecl "None" [] Nothing
            , ConDecl "Some" [tyVar "A" 0] Nothing
            ])
          emptyConstructorEnv
    checkConstructorApp env "Some" [tySimple "Int"] (tyCon "Option" [tySimple "Int"])
      `shouldSatisfy` isRight

  it "rejects constructor with wrong argument type" $ do
    let env = addTypeDecl
          (TypeDecl "IntBox" []
            [ConDecl "IntBox" [tySimple "Int"] Nothing])
          emptyConstructorEnv
    checkConstructorApp env "IntBox" [tySimple "Bool"] (tySimple "IntBox")
      `shouldSatisfy` isLeft

  it "rejects constructor with wrong arity" $ do
    let env = addTypeDecl
          (TypeDecl "Pair" [("A", KiType 0), ("B", KiType 0)]
            [ConDecl "Pair" [tyVar "A" 0, tyVar "B" 1] Nothing])
          emptyConstructorEnv
    checkConstructorApp env "Pair" [tySimple "Int"] (tyCon "Pair" [tySimple "Int", tySimple "Int"])
      `shouldSatisfy` isLeft

  it "handles partial application" $ do
    let env = addTypeDecl
          (TypeDecl "Pair" [("A", KiType 0), ("B", KiType 0)]
            [ConDecl "Pair" [tyVar "A" 0, tyVar "B" 1] Nothing])
          emptyConstructorEnv
    -- Partial application of Pair to [Int] substitutes A=Int, leaving B polymorphic
    -- Result should be: forall B. Int -> B -> Pair(Int, B)
    case partialApplyConstructor env "Pair" [tySimple "Int"] of
      Right ty -> case ty of
        TyForall _ _ (TyPi _ paramTy1 _ (TyPi _ paramTy2 _ retTy)) -> do
          -- First value param: Int (was A, substituted)
          paramTy1 `shouldBe` tySimple "Int"
          -- Second value param: B (index 1 in original, but under forall it stays 1)
          paramTy2 `shouldBe` tyVar "B" 1
          -- Return type: Pair(Int, B) with B at index 1
          retTy `shouldBe` tyCon "Pair" [tySimple "Int", tyVar "B" 1]
        _ -> expectationFailure $ "Expected forall with curried function: " ++ show ty
      Left err -> expectationFailure $ "Partial apply failed: " ++ show err

-- | Tests for pattern matching
patternMatchTests :: Spec
patternMatchTests = describe "pattern matching" $ do
  it "extracts type from constructor pattern" $ do
    let env = addTypeDecl
          (TypeDecl "Option" [("A", KiType 0)]
            [ ConDecl "None" [] Nothing
            , ConDecl "Some" [tyVar "A" 0] Nothing
            ])
          emptyConstructorEnv
        subjectTy = tyCon "Option" [tySimple "Int"]
    case extractPatternBindings env (PatCon "Some" [PatVar "x"]) subjectTy of
      Right bindings -> bindings `shouldBe` [("x", tySimple "Int")]
      Left err -> expectationFailure $ "Extract failed: " ++ show err

  it "extracts types from nested pattern" $ do
    let env = addTypeDecl
          (TypeDecl "Option" [("A", KiType 0)]
            [ ConDecl "None" [] Nothing
            , ConDecl "Some" [tyVar "A" 0] Nothing
            ])
          emptyConstructorEnv
        subjectTy = tyCon "Option" [tyCon "Option" [tySimple "Int"]]
    case extractPatternBindings env (PatCon "Some" [PatCon "Some" [PatVar "x"]]) subjectTy of
      Right bindings -> bindings `shouldBe` [("x", tySimple "Int")]
      Left err -> expectationFailure $ "Extract failed: " ++ show err

  it "handles wildcard pattern" $ do
    let env = addTypeDecl
          (TypeDecl "Option" [("A", KiType 0)]
            [ ConDecl "None" [] Nothing
            , ConDecl "Some" [tyVar "A" 0] Nothing
            ])
          emptyConstructorEnv
        subjectTy = tyCon "Option" [tySimple "Int"]
    case extractPatternBindings env (PatCon "Some" [PatWild]) subjectTy of
      Right bindings -> bindings `shouldBe` []
      Left err -> expectationFailure $ "Extract failed: " ++ show err

  it "handles variable pattern on whole type" $ do
    extractPatternBindings emptyConstructorEnv (PatVar "x") (tySimple "Int")
      `shouldBe` Right [("x", tySimple "Int")]

  it "rejects pattern with wrong constructor" $ do
    let env = addTypeDecl
          (TypeDecl "Bool" []
            [ ConDecl "True" [] Nothing
            , ConDecl "False" [] Nothing
            ])
          emptyConstructorEnv
        subjectTy = tySimple "Int"
    extractPatternBindings env (PatCon "True" []) subjectTy `shouldSatisfy` isLeft

  it "extracts bindings from multi-arg constructor" $ do
    let env = addTypeDecl
          (TypeDecl "Pair" [("A", KiType 0), ("B", KiType 0)]
            [ConDecl "Pair" [tyVar "A" 0, tyVar "B" 1] Nothing])
          emptyConstructorEnv
        subjectTy = tyCon "Pair" [tySimple "Int", tySimple "Bool"]
    case extractPatternBindings env (PatCon "Pair" [PatVar "x", PatVar "y"]) subjectTy of
      Right bindings -> bindings `shouldBe` [("x", tySimple "Int"), ("y", tySimple "Bool")]
      Left err -> expectationFailure $ "Extract failed: " ++ show err

-- | Tests for parameterized types
parameterizedTypeTests :: Spec
parameterizedTypeTests = describe "parameterized types" $ do
  it "instantiates type parameters correctly" $ do
    let env = addTypeDecl
          (TypeDecl "List" [("A", KiType 0)]
            [ ConDecl "Nil" [] Nothing
            , ConDecl "Cons" [tyVar "A" 0, tyCon "List" [tyVar "A" 0]] Nothing
            ])
          emptyConstructorEnv
    -- Cons applied to Int should have type: Int -> List(Int) -> List(Int)
    case instantiateConstructor env "Cons" [tySimple "Int"] of
      Right paramTys ->
        paramTys `shouldBe` [tySimple "Int", tyCon "List" [tySimple "Int"]]
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

  it "handles multiple type parameters" $ do
    let env = addTypeDecl
          (TypeDecl "Either" [("L", KiType 0), ("R", KiType 0)]
            [ ConDecl "Left" [tyVar "L" 0] Nothing
            , ConDecl "Right" [tyVar "R" 1] Nothing
            ])
          emptyConstructorEnv
    case instantiateConstructor env "Left" [tySimple "String", tySimple "Int"] of
      Right paramTys -> paramTys `shouldBe` [tySimple "String"]
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

  it "returns correct result type after instantiation" $ do
    let env = addTypeDecl
          (TypeDecl "Option" [("A", KiType 0)]
            [ ConDecl "None" [] Nothing
            , ConDecl "Some" [tyVar "A" 0] Nothing
            ])
          emptyConstructorEnv
    case instantiateConstructorReturnType env "Some" [tySimple "Int"] of
      Right retTy -> retTy `shouldBe` tyCon "Option" [tySimple "Int"]
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

-- | Tests for recursive types
recursiveTypeTests :: Spec
recursiveTypeTests = describe "recursive types" $ do
  it "handles simple recursive type (List)" $ do
    let decl = TypeDecl
          { typeDeclName = "List"
          , typeDeclParams = [("A", KiType 0)]
          , typeDeclConstructors =
              [ ConDecl "Nil" [] Nothing
              , ConDecl "Cons" [tyVar "A" 0, tyCon "List" [tyVar "A" 0]] Nothing
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "handles Nat type" $ do
    let decl = TypeDecl
          { typeDeclName = "Nat"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "Zero" [] Nothing
              , ConDecl "Succ" [tySimple "Nat"] Nothing
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "handles tree type" $ do
    let decl = TypeDecl
          { typeDeclName = "Tree"
          , typeDeclParams = [("A", KiType 0)]
          , typeDeclConstructors =
              [ ConDecl "Leaf" [tyVar "A" 0] Nothing
              , ConDecl "Node" [tyCon "Tree" [tyVar "A" 0], tyCon "Tree" [tyVar "A" 0]] Nothing
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "handles mutually recursive types" $ do
    let decl1 = TypeDecl
          { typeDeclName = "Even"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "EvenZ" [] Nothing
              , ConDecl "EvenS" [tySimple "Odd"] Nothing
              ]
          }
        decl2 = TypeDecl
          { typeDeclName = "Odd"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "OddS" [tySimple "Even"] Nothing
              ]
          }
        result = checkTypeDeclGroup emptyConstructorEnv [decl1, decl2]
    result `shouldSatisfy` isRight

-- | Tests for GADT-style constructors
gadtTests :: Spec
gadtTests = describe "GADT constructors" $ do
  it "allows refined return type" $ do
    let decl = TypeDecl
          { typeDeclName = "Expr"
          , typeDeclParams = [("A", KiType 0)]
          , typeDeclConstructors =
              [ ConDecl "IntLit" [tySimple "Int"] (Just $ tyCon "Expr" [tySimple "Int"])
              , ConDecl "BoolLit" [tySimple "Bool"] (Just $ tyCon "Expr" [tySimple "Bool"])
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "records GADT return type correctly" $ do
    let decl = TypeDecl
          { typeDeclName = "Expr"
          , typeDeclParams = [("A", KiType 0)]
          , typeDeclConstructors =
              [ ConDecl "IntLit" [tySimple "Int"] (Just $ tyCon "Expr" [tySimple "Int"])
              ]
          }
        env = addTypeDecl decl emptyConstructorEnv
    case getConstructorInfo "IntLit" env of
      Just info -> constructorInfoReturnType info `shouldBe` tyCon "Expr" [tySimple "Int"]
      Nothing -> expectationFailure "Constructor not found"

  it "pattern match with GADT refines type" $ do
    let decl = TypeDecl
          { typeDeclName = "Expr"
          , typeDeclParams = [("A", KiType 0)]
          , typeDeclConstructors =
              [ ConDecl "IntLit" [tySimple "Int"] (Just $ tyCon "Expr" [tySimple "Int"])
              ]
          }
        env = addTypeDecl decl emptyConstructorEnv
        subjectTy = tyCon "Expr" [tySimple "Int"]
    case extractPatternBindings env (PatCon "IntLit" [PatVar "n"]) subjectTy of
      Right bindings -> bindings `shouldBe` [("n", tySimple "Int")]
      Left err -> expectationFailure $ "Extract failed: " ++ show err

-- | Tests for type instantiation
typeInstantiationTests :: Spec
typeInstantiationTests = describe "type instantiation" $ do
  it "substitutes type variable in constructor param" $ do
    let env = addTypeDecl
          (TypeDecl "Box" [("A", KiType 0)]
            [ConDecl "Box" [tyVar "A" 0] Nothing])
          emptyConstructorEnv
    case instantiateConstructor env "Box" [tySimple "Int"] of
      Right paramTys -> paramTys `shouldBe` [tySimple "Int"]
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

  it "substitutes type variable in nested type" $ do
    let env = addTypeDecl
          (TypeDecl "Wrapper" [("A", KiType 0)]
            [ConDecl "Wrap" [tyCon "Option" [tyVar "A" 0]] Nothing])
          emptyConstructorEnv
    case instantiateConstructor env "Wrap" [tySimple "Int"] of
      Right paramTys -> paramTys `shouldBe` [tyCon "Option" [tySimple "Int"]]
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

  it "handles multiple type variables" $ do
    let env = addTypeDecl
          (TypeDecl "Pair" [("A", KiType 0), ("B", KiType 0)]
            [ConDecl "Pair" [tyVar "A" 0, tyVar "B" 1] Nothing])
          emptyConstructorEnv
    case instantiateConstructor env "Pair" [tySimple "Int", tySimple "Bool"] of
      Right paramTys -> paramTys `shouldBe` [tySimple "Int", tySimple "Bool"]
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

  it "preserves non-parameterized types" $ do
    let env = addTypeDecl
          (TypeDecl "IntBoolPair" []
            [ConDecl "MkPair" [tySimple "Int", tySimple "Bool"] Nothing])
          emptyConstructorEnv
    case instantiateConstructor env "MkPair" [] of
      Right paramTys -> paramTys `shouldBe` [tySimple "Int", tySimple "Bool"]
      Left err -> expectationFailure $ "Instantiation failed: " ++ show err

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles empty type (no constructors)" $ do
    let decl = TypeDecl "Void" [] []
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "handles single constructor type" $ do
    let decl = TypeDecl "Unit" [] [ConDecl "Unit" [] Nothing]
        env = addTypeDecl decl emptyConstructorEnv
    lookupConstructorType "Unit" env `shouldSatisfy` \case
      Just _ -> True
      Nothing -> False

  it "handles constructor with many parameters" $ do
    let decl = TypeDecl
          { typeDeclName = "Record"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "Record"
                  [ tySimple "Int"
                  , tySimple "Bool"
                  , tySimple "String"
                  , tySimple "Float"
                  ]
                  Nothing
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "handles higher-kinded type parameter" $ do
    let decl = TypeDecl
          { typeDeclName = "Fix"
          , typeDeclParams = [("F", KiArrow (KiType 0) (KiType 0))]
          , typeDeclConstructors = []
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "handles type with both type and value params" $ do
    let decl = TypeDecl
          { typeDeclName = "Vec"
          , typeDeclParams = [("N", KiType 0), ("A", KiType 0)]
          , typeDeclConstructors =
              [ ConDecl "VNil" [] Nothing
              , ConDecl "VCons" [tyVar "A" 1, tyCon "Vec" [tyVar "N" 0, tyVar "A" 1]] Nothing
              ]
          }
    checkTypeDecl emptyConstructorEnv decl `shouldSatisfy` isRight

  it "preserves constructor order" $ do
    let decl = TypeDecl
          { typeDeclName = "ABC"
          , typeDeclParams = []
          , typeDeclConstructors =
              [ ConDecl "A" [] Nothing
              , ConDecl "B" [] Nothing
              , ConDecl "C" [] Nothing
              ]
          }
        env = addTypeDecl decl emptyConstructorEnv
    case getTypeInfo "ABC" env of
      Just info -> map constructorInfoName (typeInfoConstructors' info) `shouldBe` ["A", "B", "C"]
      Nothing -> expectationFailure "Type not found"
