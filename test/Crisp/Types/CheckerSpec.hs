{-# LANGUAGE OverloadedStrings #-}

module Crisp.Types.CheckerSpec (spec) where

import Test.Hspec

import Crisp.Types.Checker
import Crisp.Types.Context
import Crisp.Core.Term

import Data.Either (isRight, isLeft)

spec :: Spec
spec = do
  describe "synthesize" $ do
    it "synthesizes type of variable" $ do
      let ctx = extendTerm "x" (simpleType "Int") withPrelude
          term = TmVar "x" 0
      let result = synthesize ctx term
      result `shouldSatisfy` isRight

    it "fails on unbound variable" $ do
      let term = TmVar "unknown" 0
      let result = synthesize withPrelude term
      result `shouldSatisfy` isLeft

    it "synthesizes type of constructor" $ do
      let term = TmCon "True" [] []
      let result = synthesize withPrelude term
      result `shouldSatisfy` isRight

    it "synthesizes type of application" $ do
      let funcType = simpleFnType (simpleType "Int") (simpleType "Bool") EffEmpty
          ctx = extendTerm "f" funcType $
                extendTerm "x" (simpleType "Int") withPrelude
          term = TmApp (TmVar "f" 1) (TmVar "x" 0)
      let result = synthesize ctx term
      result `shouldSatisfy` isRight

    it "synthesizes type of let binding" $ do
      let ctx = withPrelude
          term = TmLet "x" (simpleType "Int") (TmCon "Zero" [] []) (TmVar "x" 0)
      -- This would need proper Int constructors to work
      pending

  describe "check" $ do
    it "checks lambda against function type" $ do
      let ctx = withPrelude
          term = TmLam "x" (simpleType "Int") (TmVar "x" 0)
          expected = simpleFnType (simpleType "Int") (simpleType "Int") EffEmpty
      let result = check ctx term expected
      result `shouldSatisfy` isRight

    it "checks type abstraction against forall type" $ do
      let ctx = withPrelude
          term = TmTyAbs "T" (KiType 0) (TmVar "x" 0)
          expected = TyForall "T" (KiType 0) (TyVar "T" 0)
      -- Needs variable in context to work
      pending

  describe "effect tracking" $ do
    it "tracks effects through application" $ do
      pending

    it "removes handled effects" $ do
      pending

  describe "type equality" $ do
    it "considers equal types equal" $ do
      let ty = simpleType "Int"
      let result = check withPrelude (TmCon "Zero" [] []) ty
      -- Would need proper type checking to verify
      pending
