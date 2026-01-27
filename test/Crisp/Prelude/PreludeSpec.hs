{-# LANGUAGE OverloadedStrings #-}

module Crisp.Prelude.PreludeSpec (spec) where

import Test.Hspec

import Crisp.Prelude.Core
import Crisp.Prelude.Types
import Crisp.Prelude.Functions
import Crisp.Prelude.Effects
import Crisp.Core.Term (Type(..), Kind(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "Crisp.Prelude" $ do
  describe "Prelude Types" $ do
    describe "type definitions" $ do
      it "defines Unit type" $ do
        preludeTypeName unitType `shouldBe` "Unit"
        length (preludeTypeConstrs unitType) `shouldBe` 1

      it "defines Bool type with True and False" $ do
        preludeTypeName boolType `shouldBe` "Bool"
        let constrs = map constrName (preludeTypeConstrs boolType)
        constrs `shouldBe` ["True", "False"]

      it "defines Option type with None and Some" $ do
        preludeTypeName optionType `shouldBe` "Option"
        length (preludeTypeParams optionType) `shouldBe` 1
        let constrs = map constrName (preludeTypeConstrs optionType)
        constrs `shouldBe` ["None", "Some"]

      it "defines Result type with Ok and Err" $ do
        preludeTypeName resultType `shouldBe` "Result"
        length (preludeTypeParams resultType) `shouldBe` 2
        let constrs = map constrName (preludeTypeConstrs resultType)
        constrs `shouldBe` ["Ok", "Err"]

      it "defines List type with Nil and Cons" $ do
        preludeTypeName listType `shouldBe` "List"
        length (preludeTypeParams listType) `shouldBe` 1
        let constrs = map constrName (preludeTypeConstrs listType)
        constrs `shouldBe` ["Nil", "Cons"]

      it "defines Pair type" $ do
        preludeTypeName pairType `shouldBe` "Pair"
        length (preludeTypeParams pairType) `shouldBe` 2

      it "defines Ordering type with LT, EQ, GT" $ do
        preludeTypeName orderingType `shouldBe` "Ordering"
        let constrs = map constrName (preludeTypeConstrs orderingType)
        constrs `shouldBe` ["LT", "EQ", "GT"]

      it "defines Either type with Left and Right" $ do
        preludeTypeName eitherType `shouldBe` "Either"
        let constrs = map constrName (preludeTypeConstrs eitherType)
        constrs `shouldBe` ["Left", "Right"]

    describe "type lookup" $ do
      it "finds Unit type" $ do
        lookupPreludeType "Unit" `shouldBe` Just unitType

      it "finds Bool type" $ do
        lookupPreludeType "Bool" `shouldBe` Just boolType

      it "finds Option type" $ do
        lookupPreludeType "Option" `shouldBe` Just optionType

      it "returns Nothing for unknown type" $ do
        lookupPreludeType "Unknown" `shouldBe` Nothing

      it "isPreludeType works correctly" $ do
        isPreludeType "Unit" `shouldBe` True
        isPreludeType "Bool" `shouldBe` True
        isPreludeType "Unknown" `shouldBe` False

    describe "constructor lookup" $ do
      it "finds True constructor" $ do
        case lookupPreludeConstructor "True" of
          Just (c, t) -> do
            constrName c `shouldBe` "True"
            preludeTypeName t `shouldBe` "Bool"
          Nothing -> expectationFailure "Expected to find True"

      it "finds Some constructor" $ do
        case lookupPreludeConstructor "Some" of
          Just (c, t) -> do
            constrName c `shouldBe` "Some"
            preludeTypeName t `shouldBe` "Option"
          Nothing -> expectationFailure "Expected to find Some"

      it "finds Cons constructor" $ do
        isPreludeConstructor "Cons" `shouldBe` True

      it "returns Nothing for unknown constructor" $ do
        lookupPreludeConstructor "Unknown" `shouldBe` Nothing

    describe "constructor types" $ do
      it "Unit constructor has type Unit" $ do
        case lookupPreludeConstructor "Unit" of
          Just (c, _) -> constrResult c `shouldBe` TyCon "Unit" []
          Nothing -> expectationFailure "Expected to find Unit"

      it "Some constructor takes A and returns Option A" $ do
        case lookupPreludeConstructor "Some" of
          Just (c, _) -> do
            length (constrParams c) `shouldBe` 1
            constrResult c `shouldBe` TyCon "Option" [TyVar "A" 0]
          Nothing -> expectationFailure "Expected to find Some"

      it "Cons constructor takes A and List A" $ do
        case lookupPreludeConstructor "Cons" of
          Just (c, _) -> length (constrParams c) `shouldBe` 2
          Nothing -> expectationFailure "Expected to find Cons"

  describe "Prelude Functions" $ do
    describe "identity and composition" $ do
      it "defines id function" $ do
        fnName idFn `shouldBe` "id"
        length (fnTypeParams idFn) `shouldBe` 1
        length (fnParams idFn) `shouldBe` 1

      it "defines const function" $ do
        fnName constFn `shouldBe` "const"
        length (fnTypeParams constFn) `shouldBe` 2

      it "defines compose function" $ do
        fnName composeFn `shouldBe` "compose"
        length (fnTypeParams composeFn) `shouldBe` 3

      it "defines flip function" $ do
        fnName flipFn `shouldBe` "flip"
        length (fnParams flipFn) `shouldBe` 3

    describe "option operations" $ do
      it "defines map_option" $ do
        fnName mapOptionFn `shouldBe` "map_option"
        case fnBody mapOptionFn of
          BodyMatch var _ -> var `shouldBe` "opt"
          _ -> expectationFailure "Expected pattern match body"

      it "defines unwrap_or" $ do
        fnName unwrapOrFn `shouldBe` "unwrap_or"
        length (fnParams unwrapOrFn) `shouldBe` 2

      it "defines is_none" $ do
        fnName isNoneFn `shouldBe` "is_none"
        fnReturnType isNoneFn `shouldBe` TyCon "Bool" []

      it "defines is_some" $ do
        fnName isSomeFn `shouldBe` "is_some"

      it "defines and_then_option" $ do
        fnName andThenOptionFn `shouldBe` "and_then_option"

    describe "result operations" $ do
      it "defines map_result" $ do
        fnName mapResultFn `shouldBe` "map_result"

      it "defines map_error" $ do
        fnName mapErrorFn `shouldBe` "map_error"

      it "defines is_ok" $ do
        fnName isOkFn `shouldBe` "is_ok"

      it "defines is_err" $ do
        fnName isErrFn `shouldBe` "is_err"

      it "defines unwrap_result" $ do
        fnName unwrapResultFn `shouldBe` "unwrap_result"
        case fnBody unwrapResultFn of
          BodyBuiltin name -> name `shouldBe` "unwrap_result"
          _ -> expectationFailure "Expected builtin body"

    describe "list operations" $ do
      it "defines map" $ do
        fnName mapFn `shouldBe` "map"

      it "defines filter" $ do
        fnName filterFn `shouldBe` "filter"

      it "defines foldl" $ do
        fnName foldlFn `shouldBe` "foldl"
        length (fnParams foldlFn) `shouldBe` 3

      it "defines foldr" $ do
        fnName foldrFn `shouldBe` "foldr"

      it "defines length" $ do
        fnName lengthFn `shouldBe` "length"
        fnReturnType lengthFn `shouldBe` TyCon "Int" []

      it "defines append" $ do
        fnName appendFn `shouldBe` "append"

      it "defines reverse" $ do
        fnName reverseFn `shouldBe` "reverse"

      it "defines head" $ do
        fnName headFn `shouldBe` "head"
        case fnBody headFn of
          BodyMatch var cases -> do
            var `shouldBe` "xs"
            length cases `shouldBe` 2
          _ -> expectationFailure "Expected pattern match body"

      it "defines tail" $ do
        fnName tailFn `shouldBe` "tail"

      it "defines null" $ do
        fnName nullFn `shouldBe` "null"
        fnReturnType nullFn `shouldBe` TyCon "Bool" []

    describe "pair operations" $ do
      it "defines fst" $ do
        fnName fstFn `shouldBe` "fst"

      it "defines snd" $ do
        fnName sndFn `shouldBe` "snd"

      it "defines swap" $ do
        fnName swapFn `shouldBe` "swap"

    describe "boolean operations" $ do
      it "defines not" $ do
        fnName notFn `shouldBe` "not"
        length (fnParams notFn) `shouldBe` 1
        fnReturnType notFn `shouldBe` TyCon "Bool" []

      it "defines and" $ do
        fnName andFn `shouldBe` "and"
        length (fnParams andFn) `shouldBe` 2

      it "defines or" $ do
        fnName orFn `shouldBe` "or"

    describe "function lookup" $ do
      it "finds id function" $ do
        lookupPreludeFunction "id" `shouldBe` Just idFn

      it "finds map function" $ do
        isPreludeFunction "map" `shouldBe` True

      it "returns Nothing for unknown function" $ do
        lookupPreludeFunction "unknown" `shouldBe` Nothing

  describe "Prelude Effects" $ do
    describe "effect definitions" $ do
      it "defines State effect" $ do
        effectName stateEffect `shouldBe` "State"
        length (effectTypeParams stateEffect) `shouldBe` 1
        let ops = map opName (effectOperations stateEffect)
        ops `shouldContain` ["get"]
        ops `shouldContain` ["put"]

      it "defines Reader effect" $ do
        effectName readerEffect `shouldBe` "Reader"
        let ops = map opName (effectOperations readerEffect)
        ops `shouldContain` ["ask"]

      it "defines Writer effect" $ do
        effectName writerEffect `shouldBe` "Writer"
        let ops = map opName (effectOperations writerEffect)
        ops `shouldContain` ["tell"]

      it "defines Exception effect" $ do
        effectName exceptionEffect `shouldBe` "Exception"
        let ops = map opName (effectOperations exceptionEffect)
        ops `shouldContain` ["raise"]

      it "defines IO effect" $ do
        effectName ioEffect `shouldBe` "IO"
        length (effectTypeParams ioEffect) `shouldBe` 0
        let ops = map opName (effectOperations ioEffect)
        ops `shouldContain` ["print"]
        ops `shouldContain` ["read_line"]

      it "defines NonDet effect" $ do
        effectName nondetEffect `shouldBe` "NonDet"
        let ops = map opName (effectOperations nondetEffect)
        ops `shouldContain` ["choose"]
        ops `shouldContain` ["fail"]

      it "defines Async effect" $ do
        effectName asyncEffect `shouldBe` "Async"
        let ops = map opName (effectOperations asyncEffect)
        ops `shouldContain` ["fork"]
        ops `shouldContain` ["await"]

    describe "effect lookup" $ do
      it "finds State effect" $ do
        lookupPreludeEffect "State" `shouldBe` Just stateEffect

      it "finds IO effect" $ do
        isPreludeEffect "IO" `shouldBe` True

      it "returns Nothing for unknown effect" $ do
        lookupPreludeEffect "Unknown" `shouldBe` Nothing

    describe "operation lookup" $ do
      it "finds State.get operation" $ do
        case lookupEffectOperation "State" "get" of
          Just (op, eff) -> do
            opName op `shouldBe` "get"
            effectName eff `shouldBe` "State"
          Nothing -> expectationFailure "Expected to find State.get"

      it "finds IO.print operation" $ do
        isEffectOperation "IO" "print" `shouldBe` True

      it "returns Nothing for unknown operation" $ do
        lookupEffectOperation "State" "unknown" `shouldBe` Nothing

  describe "Prelude Context" $ do
    describe "empty context" $ do
      it "has no types" $ do
        Set.null (ctxTypes emptyPreludeContext) `shouldBe` True

      it "has no functions" $ do
        Set.null (ctxFunctions emptyPreludeContext) `shouldBe` True

      it "has no effects" $ do
        Set.null (ctxEffects emptyPreludeContext) `shouldBe` True

    describe "full context" $ do
      it "includes all types" $ do
        Set.member "Unit" (ctxTypes fullPreludeContext) `shouldBe` True
        Set.member "Bool" (ctxTypes fullPreludeContext) `shouldBe` True
        Set.member "Option" (ctxTypes fullPreludeContext) `shouldBe` True
        Set.member "List" (ctxTypes fullPreludeContext) `shouldBe` True

      it "includes all constructors" $ do
        Set.member "True" (ctxConstrs fullPreludeContext) `shouldBe` True
        Set.member "Some" (ctxConstrs fullPreludeContext) `shouldBe` True
        Set.member "Cons" (ctxConstrs fullPreludeContext) `shouldBe` True

      it "includes all functions" $ do
        Set.member "id" (ctxFunctions fullPreludeContext) `shouldBe` True
        Set.member "map" (ctxFunctions fullPreludeContext) `shouldBe` True
        Set.member "foldl" (ctxFunctions fullPreludeContext) `shouldBe` True

      it "includes all effects" $ do
        Set.member "State" (ctxEffects fullPreludeContext) `shouldBe` True
        Set.member "IO" (ctxEffects fullPreludeContext) `shouldBe` True

    describe "minimal context" $ do
      it "includes basic types" $ do
        Set.member "Unit" (ctxTypes minimalPreludeContext) `shouldBe` True
        Set.member "Bool" (ctxTypes minimalPreludeContext) `shouldBe` True

      it "includes id function" $ do
        Set.member "id" (ctxFunctions minimalPreludeContext) `shouldBe` True

      it "excludes effects" $ do
        Set.null (ctxEffects minimalPreludeContext) `shouldBe` True

  describe "Name Resolution" $ do
    it "resolves type names" $ do
      case resolvePreludeName "Unit" of
        ResolvedType t -> preludeTypeName t `shouldBe` "Unit"
        _ -> expectationFailure "Expected ResolvedType"

    it "resolves constructor names" $ do
      case resolvePreludeName "Some" of
        ResolvedConstructor c _ -> constrName c `shouldBe` "Some"
        _ -> expectationFailure "Expected ResolvedConstructor"

    it "resolves function names" $ do
      case resolvePreludeName "map" of
        ResolvedFunction f -> fnName f `shouldBe` "map"
        _ -> expectationFailure "Expected ResolvedFunction"

    it "resolves effect names" $ do
      case resolvePreludeName "State" of
        ResolvedEffect e -> effectName e `shouldBe` "State"
        _ -> expectationFailure "Expected ResolvedEffect"

    it "returns NotFound for unknown names" $ do
      resolvePreludeName "unknown" `shouldBe` NotFound

  describe "Type Checking Support" $ do
    describe "type environment" $ do
      it "maps type names to kinds" $ do
        Map.lookup "Unit" preludeTypeEnv `shouldBe` Just (KiType 0)
        Map.lookup "Bool" preludeTypeEnv `shouldBe` Just (KiType 0)
        Map.lookup "Option" preludeTypeEnv `shouldBe` Just (KiArrow (KiType 0) (KiType 0))

    describe "function environment" $ do
      it "maps function names to types" $ do
        Map.member "id" preludeFunctionEnv `shouldBe` True
        Map.member "map" preludeFunctionEnv `shouldBe` True

    describe "effect environment" $ do
      it "maps effect names to operations" $ do
        case Map.lookup "State" preludeEffectEnv of
          Just ops -> length ops `shouldSatisfy` (> 0)
          Nothing -> expectationFailure "Expected State effect"

  describe "Code Generation" $ do
    it "generates type definition" $ do
      let code = generateTypeDefinition boolType
      code `shouldSatisfy` containsText "type Bool"
      code `shouldSatisfy` containsText "True"
      code `shouldSatisfy` containsText "False"

    it "generates function definition" $ do
      let code = generateFunctionDefinition idFn
      code `shouldSatisfy` containsText "fn id"

    it "generates effect definition" $ do
      let code = generateEffectDefinition stateEffect
      code `shouldSatisfy` containsText "effect State"
      code `shouldSatisfy` containsText "get"
      code `shouldSatisfy` containsText "put"

    it "generates complete prelude module" $ do
      let code = generatePreludeModule
      code `shouldSatisfy` containsText "module Core.Prelude"
      code `shouldSatisfy` containsText "type Unit"
      code `shouldSatisfy` containsText "fn id"
      code `shouldSatisfy` containsText "effect State"

-- Helper functions

containsText :: Text -> Text -> Bool
containsText needle haystack = needle `T.isInfixOf` haystack
