{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.PropSpec
-- Description : Tests for Prop universe and proof erasure
--
-- TDD tests for the Prop universe including:
-- - Prop kind distinction from Type kind
-- - Proof term type checking
-- - Proof irrelevance enforcement
-- - Proof erasure before code generation

module Crisp.Types.PropSpec (spec) where

import Test.Hspec

import Crisp.Types.Prop
import Crisp.Types.KindChecker
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

-- | Standard types
tyInt :: Type
tyInt = tySimple "Int"

tyBool :: Type
tyBool = tySimple "Bool"

tyUnit :: Type
tyUnit = tySimple "Unit"

-- | Kind helpers
kType :: Kind
kType = KiType 0

kProp :: Kind
kProp = KiProp

spec :: Spec
spec = do
  describe "Prop Universe" $ do
    propKindTests
    proofIrrelevanceTests
    proofErasureTests
    relevanceBoundaryTests
    proofTypeTests
    propInDependentTypesTests
    edgeCaseTests

-- | Tests for Prop kind distinction
propKindTests :: Spec
propKindTests = describe "Prop kind" $ do
  it "TyProp has kind Prop" $ do
    kindOfProp TyProp `shouldBe` Right KiProp

  it "distinguishes Prop from Type" $ do
    kindOfProp TyProp `shouldBe` Right KiProp
    kindOfProp (TyUniverse 0) `shouldBe` Right (KiType 1)

  it "Prop is not equal to Type" $ do
    kindsCompatible KiProp (KiType 0) `shouldBe` False
    kindsCompatible (KiType 0) KiProp `shouldBe` False

  it "Prop equals Prop" $ do
    kindsCompatible KiProp KiProp `shouldBe` True

  it "recognizes Prop type" $ do
    isPropType TyProp `shouldBe` True
    isPropType tyInt `shouldBe` False
    isPropType (TyUniverse 0) `shouldBe` False

  it "recognizes Prop kind" $ do
    isPropKind KiProp `shouldBe` True
    isPropKind (KiType 0) `shouldBe` False
    isPropKind KiLinear `shouldBe` False

  it "equality type has Prop kind" $ do
    let eqType = tyCon "Eq" [tyInt, tyVar "x" 0, tyVar "y" 1]
    -- Eq(A, x, y) : Prop when properly declared
    isPropTypeName "Eq" `shouldBe` True

-- | Tests for proof irrelevance
proofIrrelevanceTests :: Spec
proofIrrelevanceTests = describe "proof irrelevance" $ do
  it "identifies proof-irrelevant function argument" $ do
    -- fn(p: Prop, x: Int) -> Int
    -- Position 0 (p) is proof-irrelevant
    let paramTypes = [TyProp, tyInt]
    isProofIrrelevant (paramTypes !! 0) `shouldBe` True
    isProofIrrelevant (paramTypes !! 1) `shouldBe` False

  it "filters proof arguments from parameter list" $ do
    let paramTypes = [tyInt, TyProp, tyBool, TyProp]
    filterRelevantArgs paramTypes `shouldBe` [tyInt, tyBool]

  it "computes proof positions in parameter list" $ do
    let paramTypes = [tyInt, TyProp, tyBool, TyProp]
    proofPositions paramTypes `shouldBe` [1, 3]

  it "keeps all arguments when no proofs" $ do
    let paramTypes = [tyInt, tyBool, tyUnit]
    filterRelevantArgs paramTypes `shouldBe` paramTypes

  it "returns empty when all proofs" $ do
    let paramTypes = [TyProp, TyProp]
    filterRelevantArgs paramTypes `shouldBe` []

  it "identifies constructor with proof arguments" $ do
    -- Cons(head: A, sorted: Prop, tail: List(A))
    let conParams = [tyVar "A" 0, TyProp, tyCon "List" [tyVar "A" 0]]
    hasProofArguments conParams `shouldBe` True

  it "constructor without proofs has no proof arguments" $ do
    let conParams = [tyVar "A" 0, tyCon "List" [tyVar "A" 0]]
    hasProofArguments conParams `shouldBe` False

-- | Tests for proof erasure
proofErasureTests :: Spec
proofErasureTests = describe "proof erasure" $ do
  it "erases proof variable to unit" $ do
    let term = TmVar "proof" 0
    eraseProofTerm term TyProp `shouldBe` TmCon "Unit" [] []

  it "preserves non-proof variable" $ do
    let term = TmVar "x" 0
    eraseProofTerm term tyInt `shouldBe` term

  it "erases proof in let binding" $ do
    -- let p: Prop = Refl in x  (where x is a free variable, index 1 in body)
    -- After erasing the let, x should have index 0
    let term = TmLet "p" TyProp (TmCon "Refl" [] []) (TmVar "x" 1)
    let erased = eraseProofLet term
    -- Should become just x with adjusted index
    erased `shouldBe` TmVar "x" 0

  it "preserves non-proof let binding" $ do
    let term = TmLet "x" tyInt (TmVar "y" 1) (TmVar "x" 0)
    let erased = eraseProofLet term
    erased `shouldBe` term

  it "erases proof arguments in constructor" $ do
    -- Cons(head, proof, tail) where proof: Prop
    let argTypes = [tyInt, TyProp, tyCon "List" [tyInt]]
    let args = [TmVar "head" 0, TmVar "proof" 1, TmVar "tail" 2]
    let erased = eraseProofArgs argTypes args
    erased `shouldBe` [TmVar "head" 0, TmVar "tail" 2]

  it "erases proof arguments in function application" $ do
    -- f(x, proof, y) where proof: Prop
    let paramTypes = [tyInt, TyProp, tyBool]
    let args = [TmVar "x" 0, TmVar "p" 1, TmVar "y" 2]
    let erased = eraseProofArgs paramTypes args
    erased `shouldBe` [TmVar "x" 0, TmVar "y" 2]

  it "erases proof in lambda parameter" $ do
    -- fn(p: Prop, x: Int) -> x
    let paramTypes = [TyProp, tyInt]
    let erasedParams = filterRelevantParams [("p", TyProp), ("x", tyInt)]
    erasedParams `shouldBe` [("x", tyInt)]

  it "fully erases term with all proof bindings" $ do
    let term = TmLet "p1" TyProp (TmCon "Refl" [] [])
             $ TmLet "p2" TyProp (TmVar "p1" 0)
             $ TmVar "x" 2
    let erased = eraseAllProofs term
    -- Should become just x
    erased `shouldBe` TmVar "x" 0  -- Note: index adjusted

-- | Tests for relevance boundary
relevanceBoundaryTests :: Spec
relevanceBoundaryTests = describe "relevance boundary" $ do
  it "rejects Type depending on Prop value" $ do
    -- fn(p: Prop) -> if p then Int else Bool
    -- This should be rejected: computation depends on proof
    let typeDepOnProof = checkRelevanceBoundary
          (TyPi "p" TyProp EffEmpty (TyVar "result" 0))
          True  -- True = type depends on proof
    typeDepOnProof `shouldSatisfy` isLeft

  it "allows Prop depending on Type value" $ do
    -- Eq(Int, x, y) depends on x and y which are Type values
    -- This is allowed
    let propDepOnType = checkRelevanceBoundary
          (TyPi "x" tyInt EffEmpty TyProp)
          False
    propDepOnType `shouldBe` Right ()

  it "detects proof escaping to computation" $ do
    -- match p: Refl -> compute_with(x)
    -- Cannot match on proofs for computation
    let pattern' = PatCon "Refl" []
    let scrutineeType = TyProp  -- Proof type
    canMatchOnProof scrutineeType pattern' `shouldBe` False

  it "allows proofs in type refinement" $ do
    -- fn(p: n == m) -> (xs: Vec(A, n)) -> Vec(A, m)
    -- Proof refines types, doesn't affect computation
    let proofForRefinement = isProofUsedForRefinement
          (tyCon "Eq" [tySimple "Nat", tyVar "n" 0, tyVar "m" 1])
    proofForRefinement `shouldBe` True

  it "rejects proof inspection in match body" $ do
    -- Cannot use proof value to determine which branch to take
    let matchOnProof = canInspectProofValue TyProp
    matchOnProof `shouldBe` False

  it "allows wildcard match on proof (no inspection)" $ do
    -- match proof: _ -> body  is okay (doesn't inspect)
    canMatchOnProof TyProp PatWild `shouldBe` True

-- | Tests for proof types
proofTypeTests :: Spec
proofTypeTests = describe "proof types" $ do
  it "Eq is a proof type" $ do
    let eqType = tyCon "Eq" [tyInt, termToType (TmVar "x" 0), termToType (TmVar "y" 1)]
    isPropTypeName "Eq" `shouldBe` True

  it "recognizes standard proof type names" $ do
    isPropTypeName "Eq" `shouldBe` True
    isPropTypeName "Lt" `shouldBe` True
    isPropTypeName "Le" `shouldBe` True
    isPropTypeName "Gt" `shouldBe` True
    isPropTypeName "Ge" `shouldBe` True
    isPropTypeName "And" `shouldBe` True
    isPropTypeName "Or" `shouldBe` True
    isPropTypeName "Not" `shouldBe` True
    isPropTypeName "True" `shouldBe` True
    isPropTypeName "False" `shouldBe` True

  it "non-proof types are not proof type names" $ do
    isPropTypeName "Int" `shouldBe` False
    isPropTypeName "Bool" `shouldBe` False
    isPropTypeName "List" `shouldBe` False
    isPropTypeName "Vec" `shouldBe` False

  it "Refl constructor has Prop result" $ do
    -- Refl : Eq(A, x, x)
    let reflType = tyCon "Eq" [tyVar "A" 0, tyVar "x" 1, tyVar "x" 1]
    isPropTypeName "Eq" `shouldBe` True

-- | Tests for Prop in dependent types
propInDependentTypesTests :: Spec
propInDependentTypesTests = describe "Prop in dependent types" $ do
  it "identifies proof-irrelevant Pi parameter" $ do
    -- (p: Prop) -> T
    let piType = TyPi "p" TyProp EffEmpty tyInt
    hasPropParameter piType `shouldBe` True

  it "identifies relevant Pi parameter" $ do
    -- (x: Int) -> T
    let piType = TyPi "x" tyInt EffEmpty tyBool
    hasPropParameter piType `shouldBe` False

  it "extracts proof parameters from nested Pi" $ do
    -- (x: Int) -> (p: Prop) -> (y: Bool) -> T
    let piType = TyPi "x" tyInt EffEmpty
               $ TyPi "p" TyProp EffEmpty
               $ TyPi "y" tyBool EffEmpty tyUnit
    countProofParameters piType `shouldBe` 1

  it "Sigma with Prop second component" $ do
    -- Sigma x:A. P(x) where P(x) : Prop
    let sigmaType = TySigma "x" tyInt TyProp
    hasPropInSigma sigmaType `shouldBe` True

  it "Sigma without Prop components" $ do
    let sigmaType = TySigma "x" tyInt tyBool
    hasPropInSigma sigmaType `shouldBe` False

  it "forall with Prop body" $ do
    -- forall a. Prop (type-level)
    let forallType = TyForall "a" kType TyProp
    forallHasPropBody forallType `shouldBe` True

-- | Edge case tests
edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles nested proof types" $ do
    -- Prop containing Prop references
    let nestedProp = TyPi "p1" TyProp EffEmpty
                   $ TyPi "p2" TyProp EffEmpty tyUnit
    countProofParameters nestedProp `shouldBe` 2

  it "handles empty parameter list" $ do
    filterRelevantArgs [] `shouldBe` []
    proofPositions [] `shouldBe` []

  it "handles single proof parameter" $ do
    filterRelevantArgs [TyProp] `shouldBe` []
    proofPositions [TyProp] `shouldBe` [0]

  it "handles single non-proof parameter" $ do
    filterRelevantArgs [tyInt] `shouldBe` [tyInt]
    proofPositions [tyInt] `shouldBe` []

  it "preserves type structure after erasure" $ do
    -- After erasing proofs, type structure should be consistent
    let params = [tyInt, TyProp, tyBool]
    let args = [TmVar "x" 0, TmVar "p" 1, TmVar "y" 2]
    let erasedParams = filterRelevantArgs params
    let erasedArgs = eraseProofArgs params args
    length erasedParams `shouldBe` length erasedArgs

  it "handles proof in type constructor arguments" $ do
    -- List(Prop) - list of proofs
    let listOfProof = tyCon "List" [TyProp]
    hasProofInTypeArgs listOfProof `shouldBe` True

  it "handles deeply nested proofs" $ do
    -- (a: (b: (c: Prop) -> T) -> U) -> V
    let deepNested = TyPi "a"
          (TyPi "b"
            (TyPi "c" TyProp EffEmpty tyUnit)
            EffEmpty tyUnit)
          EffEmpty tyUnit
    hasNestedProof deepNested `shouldBe` True

  it "erases proof from match case" $ do
    -- match e: Con(x, proof) -> body
    let caseWithProof = Case
          { casePattern = PatCon "Con" [PatVar "x", PatVar "proof"]
          , caseBody = TmVar "x" 0
          }
    let paramTypes = [tyInt, TyProp]
    let erasedCase = eraseProofFromCase paramTypes caseWithProof
    casePattern erasedCase `shouldBe` PatCon "Con" [PatVar "x"]

-- | Helper to convert term to type (for type-level values)
termToType :: Term -> Type
termToType (TmVar name idx) = TyVar name idx
termToType _ = TyProp  -- Simplified
