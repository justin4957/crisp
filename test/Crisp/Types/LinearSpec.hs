{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.LinearSpec
-- Description : Linear type checking test suite
--
-- Tests for linear type checking including usage counting,
-- borrow checking, and conditional usage analysis.

module Crisp.Types.LinearSpec (spec) where

import Test.Hspec

import Crisp.Types.Linear
import Crisp.Types.Usage
import Crisp.Core.Term

import Data.Either (isRight, isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

-- | Helper to check for success
shouldSucceed :: (Show a, Show b) => Either a b -> Expectation
shouldSucceed result = result `shouldSatisfy` isRight

-- | Helper to check for failure
shouldFail :: (Show a, Show b) => Either a b -> Expectation
shouldFail result = result `shouldSatisfy` isLeft

-- | Helper to check for specific error
shouldFailWith :: (Show a, Eq a, Show b) => Either a b -> a -> Expectation
shouldFailWith result expected = case result of
  Left err -> err `shouldBe` expected
  Right _ -> expectationFailure "Expected failure but got success"

spec :: Spec
spec = do
  usageTypeTests
  usageCombinationTests
  linearEnvTests
  singleUseTests
  multipleUseTests
  unusedValueTests
  borrowTests
  sharedRefTests
  conditionalTests
  patternMatchTests
  letBindingTests
  functionCallTests
  closureTests
  nestedScopeTests
  edgeCaseTests

-- =============================================================================
-- Usage Type Tests
-- =============================================================================

usageTypeTests :: Spec
usageTypeTests = describe "usage types" $ do

  describe "usage values" $ do
    it "creates Zero usage" $ do
      usageCount Zero `shouldBe` 0

    it "creates One usage" $ do
      usageCount One `shouldBe` 1

    it "creates Many usage" $ do
      usageCount Many `shouldSatisfy` (> 1)

  describe "usage comparison" $ do
    it "Zero equals Zero" $ do
      Zero `shouldBe` Zero

    it "One equals One" $ do
      One `shouldBe` One

    it "Many equals Many" $ do
      Many `shouldBe` Many

    it "Zero not equal to One" $ do
      Zero `shouldNotBe` One

    it "One not equal to Many" $ do
      One `shouldNotBe` Many

  describe "usage predicates" $ do
    it "isZero on Zero" $ do
      isZero Zero `shouldBe` True

    it "isZero on One" $ do
      isZero One `shouldBe` False

    it "isOne on One" $ do
      isOne One `shouldBe` True

    it "isOne on Zero" $ do
      isOne Zero `shouldBe` False

    it "isMany on Many" $ do
      isMany Many `shouldBe` True

    it "isMany on One" $ do
      isMany One `shouldBe` False

-- =============================================================================
-- Usage Combination Tests
-- =============================================================================

usageCombinationTests :: Spec
usageCombinationTests = describe "usage combination" $ do

  describe "sequential usage (both branches execute)" $ do
    it "Zero + Zero = Zero" $ do
      Zero `addUsage` Zero `shouldBe` Zero

    it "Zero + One = One" $ do
      Zero `addUsage` One `shouldBe` One

    it "One + Zero = One" $ do
      One `addUsage` Zero `shouldBe` One

    it "One + One = Many" $ do
      One `addUsage` One `shouldBe` Many

    it "One + Many = Many" $ do
      One `addUsage` Many `shouldBe` Many

    it "Many + Many = Many" $ do
      Many `addUsage` Many `shouldBe` Many

  describe "alternative usage (one branch executes)" $ do
    it "Zero | Zero = Zero" $ do
      Zero `altUsage` Zero `shouldBe` Zero

    it "One | One = One" $ do
      One `altUsage` One `shouldBe` One

    it "Zero | One = ZeroOrOne" $ do
      -- For conditionals: if used in one branch but not other
      let result = Zero `altUsage` One
      result `shouldSatisfy` isInconsistentUsage

    it "Many | Many = Many" $ do
      Many `altUsage` Many `shouldBe` Many

  describe "usage scaling" $ do
    it "Zero * n = Zero" $ do
      scaleUsage Zero 5 `shouldBe` Zero

    it "One * 0 = Zero" $ do
      scaleUsage One 0 `shouldBe` Zero

    it "One * 1 = One" $ do
      scaleUsage One 1 `shouldBe` One

    it "One * 2 = Many" $ do
      scaleUsage One 2 `shouldBe` Many

-- =============================================================================
-- Linear Environment Tests
-- =============================================================================

linearEnvTests :: Spec
linearEnvTests = describe "linear environment" $ do

  describe "environment creation" $ do
    it "creates empty environment" $ do
      emptyLinearEnv `shouldBe` LinearEnv Map.empty

    it "extends environment with binding" $ do
      let env = extendLinearEnv "x" (Unique, Zero) emptyLinearEnv
      lookupUsage "x" env `shouldBe` Just (Unique, Zero)

    it "shadows outer bindings" $ do
      let env1 = extendLinearEnv "x" (Unique, Zero) emptyLinearEnv
          env2 = extendLinearEnv "x" (Borrowed, Zero) env1
      lookupUsage "x" env2 `shouldBe` Just (Borrowed, Zero)

  describe "usage tracking" $ do
    it "marks variable as used" $ do
      let env = extendLinearEnv "x" (Unique, Zero) emptyLinearEnv
          env' = markUsed "x" env
      lookupUsage "x" env' `shouldBe` Just (Unique, One)

    it "marks variable as used multiple times" $ do
      let env = extendLinearEnv "x" (Unique, Zero) emptyLinearEnv
          env' = markUsed "x" $ markUsed "x" env
      lookupUsage "x" env' `shouldBe` Just (Unique, Many)

    it "tracks multiple variables independently" $ do
      let env = extendLinearEnv "y" (Borrowed, Zero) $
                extendLinearEnv "x" (Unique, Zero) emptyLinearEnv
          env' = markUsed "x" env
      lookupUsage "x" env' `shouldBe` Just (Unique, One)
      lookupUsage "y" env' `shouldBe` Just (Borrowed, Zero)

  describe "linearity modes" $ do
    it "recognizes Unique mode" $ do
      isUniqueMode Unique `shouldBe` True
      isUniqueMode Borrowed `shouldBe` False
      isUniqueMode Shared `shouldBe` False

    it "recognizes Borrowed mode" $ do
      isBorrowedMode Borrowed `shouldBe` True
      isBorrowedMode Unique `shouldBe` False

    it "recognizes Shared mode" $ do
      isSharedMode Shared `shouldBe` True
      isSharedMode Unique `shouldBe` False

-- =============================================================================
-- Single Use Tests
-- =============================================================================

singleUseTests :: Spec
singleUseTests = describe "single use of unique value" $ do

  it "accepts single use of unique variable" $ do
    -- fn(f: &unique File) -> Unit = close(f)
    let term = TmApp (TmVar "close" 0) (TmVar "f" 1)
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "close" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "accepts unique value passed to function" $ do
    -- fn(r: &unique Resource) -> Unit = consume(r)
    let term = TmApp (TmVar "consume" 0) (TmVar "r" 1)
        env = extendLinearEnv "r" (Unique, Zero) $
              extendLinearEnv "consume" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "accepts unique value in constructor" $ do
    -- fn(f: &unique File) -> Result = Ok(f)
    let term = TmCon "Ok" [] [TmVar "f" 0]
        env = extendLinearEnv "f" (Unique, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "accepts unique value returned directly" $ do
    -- fn(f: &unique File) -> File = f
    let term = TmVar "f" 0
        env = extendLinearEnv "f" (Unique, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

-- =============================================================================
-- Multiple Use Tests (Errors)
-- =============================================================================

multipleUseTests :: Spec
multipleUseTests = describe "multiple use of unique value (errors)" $ do

  it "rejects double use of unique value" $ do
    -- fn(f: &unique File) -> Unit = { read(f); read(f) }
    let term = TmLet "tmp" (simpleType "String")
                 (TmApp (TmVar "read" 0) (TmVar "f" 1))
                 (TmApp (TmVar "read" 0) (TmVar "f" 2))
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "read" (Unrestricted, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` UsedMoreThanOnce "f"

  it "rejects triple use of unique value" $ do
    -- use(f); use(f); use(f)
    let term = TmLet "a" (simpleType "Unit")
                 (TmApp (TmVar "use" 0) (TmVar "f" 1))
                 (TmLet "b" (simpleType "Unit")
                    (TmApp (TmVar "use" 0) (TmVar "f" 2))
                    (TmApp (TmVar "use" 0) (TmVar "f" 3)))
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "use" (Unrestricted, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` UsedMoreThanOnce "f"

  it "rejects use in both arguments" $ do
    -- pair(f, f)
    let term = TmCon "Pair" [] [TmVar "f" 0, TmVar "f" 0]
        env = extendLinearEnv "f" (Unique, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` UsedMoreThanOnce "f"

-- =============================================================================
-- Unused Value Tests (Errors)
-- =============================================================================

unusedValueTests :: Spec
unusedValueTests = describe "unused unique value (errors)" $ do

  it "rejects unused unique value" $ do
    -- fn(f: &unique File) -> Unit = ()
    let term = TmCon "Unit" [] []
        env = extendLinearEnv "f" (Unique, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` NotUsed "f"

  it "rejects unique value shadowed without use" $ do
    -- fn(f: &unique File) -> Unit = let f = () in ()
    let term = TmLet "f" (simpleType "Unit")
                 (TmCon "Unit" [] [])
                 (TmCon "Unit" [] [])
        env = extendLinearEnv "f" (Unique, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` NotUsed "f"

  it "rejects unique value in dead code" $ do
    -- fn(f: &unique File, g: &unique File) -> Unit = close(f)
    -- g is never used
    let term = TmApp (TmVar "close" 0) (TmVar "f" 1)
        env = extendLinearEnv "g" (Unique, Zero) $
              extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "close" (Unrestricted, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` NotUsed "g"

-- =============================================================================
-- Borrow Tests
-- =============================================================================

borrowTests :: Spec
borrowTests = describe "borrowed references" $ do

  it "allows multiple uses of borrowed reference" $ do
    -- fn(f: &File) -> (String, String) = (read(f), read(f))
    let term = TmCon "Pair" []
                 [TmApp (TmVar "read" 0) (TmVar "f" 1),
                  TmApp (TmVar "read" 0) (TmVar "f" 1)]
        env = extendLinearEnv "f" (Borrowed, Zero) $
              extendLinearEnv "read" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "borrow doesn't consume owner" $ do
    -- This test demonstrates that borrowed references have different semantics
    -- A true borrow check would require type-based analysis to distinguish
    -- &f (borrow) from f (consume). For now, we test that borrowed mode
    -- allows any number of uses.
    let term = TmApp (TmVar "close" 0) (TmVar "f" 1)
        env = extendLinearEnv "f" (Borrowed, Zero) $  -- f is already borrowed
              extendLinearEnv "close" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "allows unused borrowed reference" $ do
    -- fn(f: &File) -> Unit = ()
    let term = TmCon "Unit" [] []
        env = extendLinearEnv "f" (Borrowed, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "allows borrowed in multiple branches" $ do
    -- fn(f: &File, b: Bool) -> String = if b then read(f) else read(f)
    let term = TmMatch (TmVar "b" 0) (simpleType "String")
                 [Case (PatCon "True" []) (TmApp (TmVar "read" 0) (TmVar "f" 1)),
                  Case (PatCon "False" []) (TmApp (TmVar "read" 0) (TmVar "f" 1))]
        env = extendLinearEnv "f" (Borrowed, Zero) $
              extendLinearEnv "b" (Unrestricted, Zero) $
              extendLinearEnv "read" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

-- =============================================================================
-- Shared Reference Tests
-- =============================================================================

sharedRefTests :: Spec
sharedRefTests = describe "shared references" $ do

  it "allows multiple readers of shared reference" $ do
    -- fn(d: &shared Data) -> Unit = { read1(d); read2(d); read3(d) }
    let term = TmLet "a" (simpleType "Unit") (TmApp (TmVar "read1" 0) (TmVar "d" 1)) $
               TmLet "b" (simpleType "Unit") (TmApp (TmVar "read2" 0) (TmVar "d" 2)) $
               TmApp (TmVar "read3" 0) (TmVar "d" 3)
        env = extendLinearEnv "d" (Shared, Zero) $
              extendLinearEnv "read1" (Unrestricted, Zero) $
              extendLinearEnv "read2" (Unrestricted, Zero) $
              extendLinearEnv "read3" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "allows unused shared reference" $ do
    -- fn(d: &shared Data) -> Unit = ()
    let term = TmCon "Unit" [] []
        env = extendLinearEnv "d" (Shared, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "shared reference can be passed multiple times" $ do
    -- fn(d: &shared Data) -> (Data, Data) = (clone(d), clone(d))
    let term = TmCon "Pair" []
                 [TmApp (TmVar "clone" 0) (TmVar "d" 1),
                  TmApp (TmVar "clone" 0) (TmVar "d" 1)]
        env = extendLinearEnv "d" (Shared, Zero) $
              extendLinearEnv "clone" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

-- =============================================================================
-- Conditional Tests
-- =============================================================================

conditionalTests :: Spec
conditionalTests = describe "conditional usage" $ do

  it "allows use in both branches" $ do
    -- fn(f: &unique File, b: Bool) -> Unit = if b then close(f) else close(f)
    let term = TmMatch (TmVar "b" 0) (simpleType "Unit")
                 [Case (PatCon "True" []) (TmApp (TmVar "close" 0) (TmVar "f" 1)),
                  Case (PatCon "False" []) (TmApp (TmVar "close" 0) (TmVar "f" 1))]
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "b" (Unrestricted, Zero) $
              extendLinearEnv "close" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "rejects use only in then branch" $ do
    -- fn(f: &unique File, b: Bool) -> Unit = if b then close(f) else ()
    let term = TmMatch (TmVar "b" 0) (simpleType "Unit")
                 [Case (PatCon "True" []) (TmApp (TmVar "close" 0) (TmVar "f" 1)),
                  Case (PatCon "False" []) (TmCon "Unit" [] [])]
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "b" (Unrestricted, Zero) $
              extendLinearEnv "close" (Unrestricted, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` NotUsedInAllBranches "f"

  it "rejects use only in else branch" $ do
    -- fn(f: &unique File, b: Bool) -> Unit = if b then () else close(f)
    let term = TmMatch (TmVar "b" 0) (simpleType "Unit")
                 [Case (PatCon "True" []) (TmCon "Unit" [] []),
                  Case (PatCon "False" []) (TmApp (TmVar "close" 0) (TmVar "f" 1))]
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "b" (Unrestricted, Zero) $
              extendLinearEnv "close" (Unrestricted, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` NotUsedInAllBranches "f"

  it "allows use in all match branches" $ do
    -- match x: A -> use(f); B -> use(f); C -> use(f)
    let term = TmMatch (TmVar "x" 0) (simpleType "Unit")
                 [Case (PatCon "A" []) (TmApp (TmVar "use" 0) (TmVar "f" 1)),
                  Case (PatCon "B" []) (TmApp (TmVar "use" 0) (TmVar "f" 1)),
                  Case (PatCon "C" []) (TmApp (TmVar "use" 0) (TmVar "f" 1))]
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "x" (Unrestricted, Zero) $
              extendLinearEnv "use" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "rejects use in only some match branches" $ do
    -- match x: A -> use(f); B -> (); C -> use(f)
    let term = TmMatch (TmVar "x" 0) (simpleType "Unit")
                 [Case (PatCon "A" []) (TmApp (TmVar "use" 0) (TmVar "f" 1)),
                  Case (PatCon "B" []) (TmCon "Unit" [] []),
                  Case (PatCon "C" []) (TmApp (TmVar "use" 0) (TmVar "f" 1))]
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "x" (Unrestricted, Zero) $
              extendLinearEnv "use" (Unrestricted, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` NotUsedInAllBranches "f"

-- =============================================================================
-- Pattern Matching Tests
-- =============================================================================

patternMatchTests :: Spec
patternMatchTests = describe "pattern matching on linear values" $ do

  it "transfers ownership through pattern match" $ do
    -- match consume(r): Ok(_) -> (); Err(_) -> ()
    let term = TmMatch (TmApp (TmVar "consume" 0) (TmVar "r" 1)) (simpleType "Unit")
                 [Case (PatCon "Ok" [PatWild]) (TmCon "Unit" [] []),
                  Case (PatCon "Err" [PatWild]) (TmCon "Unit" [] [])]
        env = extendLinearEnv "r" (Unique, Zero) $
              extendLinearEnv "consume" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "binds linear values in pattern" $ do
    -- Pattern-bound variables are added as unrestricted by default in the
    -- current implementation. This test shows basic pattern matching works.
    -- Full linear pattern binding would require type inference integration.
    let term = TmMatch (TmApp (TmVar "get_resource" 0) (TmCon "Unit" [] [])) (simpleType "Unit")
                 [Case (PatCon "Some" [PatVar "r"]) (TmApp (TmVar "close" 0) (TmVar "r" 1)),
                  Case (PatCon "None" []) (TmCon "Unit" [] [])]
        env = extendLinearEnv "get_resource" (Unrestricted, Zero) $
              extendLinearEnv "close" (Unrestricted, Zero) emptyLinearEnv
    -- Pattern-bound variables don't require external binding specification
    shouldSucceed $ checkLinear env term

  it "rejects unused pattern binding" $ do
    -- match get_resource(): Some(r) -> (); None -> ()
    let term = TmMatch (TmApp (TmVar "get" 0) (TmCon "Unit" [] [])) (simpleType "Unit")
                 [Case (PatCon "Some" [PatVar "r"]) (TmCon "Unit" [] []),
                  Case (PatCon "None" []) (TmCon "Unit" [] [])]
        env = extendLinearEnv "get" (Unrestricted, Zero) emptyLinearEnv
    checkLinearWithPatternBindings env term [("r", Unique)]
      `shouldFailWith` NotUsed "r"

-- =============================================================================
-- Let Binding Tests
-- =============================================================================

letBindingTests :: Spec
letBindingTests = describe "let bindings" $ do

  it "accepts linear value used in body" $ do
    -- let x = create() in consume(x)
    let term = TmLet "x" (simpleType "Resource")
                 (TmApp (TmVar "create" 0) (TmCon "Unit" [] []))
                 (TmApp (TmVar "consume" 0) (TmVar "x" 1))
        env = extendLinearEnv "create" (Unrestricted, Zero) $
              extendLinearEnv "consume" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinearWithLetBindings env term [("x", Unique)]

  it "rejects unused linear let binding" $ do
    -- let x = create() in ()
    let term = TmLet "x" (simpleType "Resource")
                 (TmApp (TmVar "create" 0) (TmCon "Unit" [] []))
                 (TmCon "Unit" [] [])
        env = extendLinearEnv "create" (Unrestricted, Zero) emptyLinearEnv
    checkLinearWithLetBindings env term [("x", Unique)]
      `shouldFailWith` NotUsed "x"

  it "allows unrestricted let bindings to be unused" $ do
    -- let x = 42 in ()
    let term = TmLet "x" (simpleType "Int")
                 (TmCon "Int" [] [])  -- placeholder for literal
                 (TmCon "Unit" [] [])
        env = emptyLinearEnv
    shouldSucceed $ checkLinearWithLetBindings env term [("x", Unrestricted)]

  it "tracks usage through nested lets" $ do
    -- let x = create() in let y = x in consume(y)
    let term = TmLet "x" (simpleType "Resource")
                 (TmApp (TmVar "create" 0) (TmCon "Unit" [] []))
                 (TmLet "y" (simpleType "Resource")
                    (TmVar "x" 1)
                    (TmApp (TmVar "consume" 0) (TmVar "y" 2)))
        env = extendLinearEnv "create" (Unrestricted, Zero) $
              extendLinearEnv "consume" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinearWithLetBindings env term [("x", Unique), ("y", Unique)]

-- =============================================================================
-- Function Call Tests
-- =============================================================================

functionCallTests :: Spec
functionCallTests = describe "function calls" $ do

  it "consumes unique argument" $ do
    -- close(f) where close : &unique File -> Unit
    let term = TmApp (TmVar "close" 0) (TmVar "f" 1)
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "close" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "allows borrowed argument without consuming" $ do
    -- read(f) where read : &File -> String
    let term = TmApp (TmVar "read" 0) (TmVar "f" 1)
        env = extendLinearEnv "f" (Borrowed, Zero) $
              extendLinearEnv "read" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinear env term

  it "tracks usage through multiple function calls" $ do
    -- let a = use1(f) in use2(f) -- error: f used twice
    let term = TmLet "a" (simpleType "Unit")
                 (TmApp (TmVar "use1" 0) (TmVar "f" 1))
                 (TmApp (TmVar "use2" 0) (TmVar "f" 2))
        env = extendLinearEnv "f" (Unique, Zero) $
              extendLinearEnv "use1" (Unrestricted, Zero) $
              extendLinearEnv "use2" (Unrestricted, Zero) emptyLinearEnv
    checkLinear env term `shouldFailWith` UsedMoreThanOnce "f"

-- =============================================================================
-- Closure Tests
-- =============================================================================

closureTests :: Spec
closureTests = describe "closures" $ do

  it "captures unique value for single use" $ do
    -- let f = create() in (\x. consume(f))
    let term = TmLet "f" (simpleType "Resource")
                 (TmApp (TmVar "create" 0) (TmCon "Unit" [] []))
                 (TmLam "x" (simpleType "Unit")
                    (TmApp (TmVar "consume" 0) (TmVar "f" 1)))
        env = extendLinearEnv "create" (Unrestricted, Zero) $
              extendLinearEnv "consume" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinearWithLetBindings env term [("f", Unique)]

  it "rejects closure that captures unique but doesn't use" $ do
    -- let f = create() in (\x. x)  -- f captured but unused
    let term = TmLet "f" (simpleType "Resource")
                 (TmApp (TmVar "create" 0) (TmCon "Unit" [] []))
                 (TmLam "x" (simpleType "Unit") (TmVar "x" 0))
        env = extendLinearEnv "create" (Unrestricted, Zero) emptyLinearEnv
    checkLinearWithLetBindings env term [("f", Unique)]
      `shouldFailWith` NotUsed "f"

-- =============================================================================
-- Nested Scope Tests
-- =============================================================================

nestedScopeTests :: Spec
nestedScopeTests = describe "nested scopes" $ do

  it "handles unique value in nested function" $ do
    -- let f = create() in (let g = \x. consume(f) in g())
    let term = TmLet "f" (simpleType "Resource")
                 (TmApp (TmVar "create" 0) (TmCon "Unit" [] []))
                 (TmLet "g" (pureFnType (simpleType "Unit") (simpleType "Unit"))
                    (TmLam "x" (simpleType "Unit")
                       (TmApp (TmVar "consume" 0) (TmVar "f" 1)))
                    (TmApp (TmVar "g" 0) (TmCon "Unit" [] [])))
        env = extendLinearEnv "create" (Unrestricted, Zero) $
              extendLinearEnv "consume" (Unrestricted, Zero) emptyLinearEnv
    shouldSucceed $ checkLinearWithLetBindings env term [("f", Unique), ("g", Unrestricted)]

  it "rejects use in multiple nested functions" $ do
    -- let f = create() in (let g = \x. use(f) in let h = \y. use(f) in ...)
    let term = TmLet "f" (simpleType "Resource")
                 (TmApp (TmVar "create" 0) (TmCon "Unit" [] []))
                 (TmLet "g" (pureFnType (simpleType "Unit") (simpleType "Unit"))
                    (TmLam "x" (simpleType "Unit")
                       (TmApp (TmVar "use" 0) (TmVar "f" 1)))
                    (TmLet "h" (pureFnType (simpleType "Unit") (simpleType "Unit"))
                       (TmLam "y" (simpleType "Unit")
                          (TmApp (TmVar "use" 0) (TmVar "f" 2)))
                       (TmCon "Unit" [] [])))
        env = extendLinearEnv "create" (Unrestricted, Zero) $
              extendLinearEnv "use" (Unrestricted, Zero) emptyLinearEnv
    checkLinearWithLetBindings env term [("f", Unique), ("g", Unrestricted), ("h", Unrestricted)]
      `shouldFailWith` UsedMoreThanOnce "f"

-- =============================================================================
-- Edge Cases
-- =============================================================================

edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do

  describe "empty contexts" $ do
    it "handles empty environment" $ do
      let term = TmCon "Unit" [] []
      shouldSucceed $ checkLinear emptyLinearEnv term

    it "handles literal with no bindings" $ do
      let term = TmCon "Int" [] []  -- placeholder for 42
      shouldSucceed $ checkLinear emptyLinearEnv term

  describe "variable shadowing" $ do
    it "outer unique shadowed by inner unique both used" $ do
      -- let f = create() in let f = create() in consume(f)
      -- outer f unused - error
      let term = TmLet "f" (simpleType "Resource")
                   (TmApp (TmVar "create" 0) (TmCon "Unit" [] []))
                   (TmLet "f" (simpleType "Resource")
                      (TmApp (TmVar "create" 0) (TmCon "Unit" [] []))
                      (TmApp (TmVar "consume" 0) (TmVar "f" 1)))
          env = extendLinearEnv "create" (Unrestricted, Zero) $
                extendLinearEnv "consume" (Unrestricted, Zero) emptyLinearEnv
      checkLinearWithLetBindings env term [("f", Unique), ("f", Unique)]
        `shouldFailWith` NotUsed "f"

  describe "type abstraction" $ do
    it "handles type abstraction" $ do
      let term = TmTyAbs "a" (KiType 0) (TmVar "x" 0)
          env = extendLinearEnv "x" (Unique, Zero) emptyLinearEnv
      shouldSucceed $ checkLinear env term

    it "handles type application" $ do
      let term = TmTyApp (TmVar "id" 0) (simpleType "Int")
          env = extendLinearEnv "id" (Unrestricted, Zero) emptyLinearEnv
      shouldSucceed $ checkLinear env term

  describe "effect operations" $ do
    it "handles perform" $ do
      let term = TmPerform "State" "get" (TmCon "Unit" [] [])
          env = emptyLinearEnv
      shouldSucceed $ checkLinear env term

  describe "handler" $ do
    it "handles simple handler" $ do
      let handler = Handler "State" EffEmpty []
                      (ReturnHandler (PatVar "x") (TmVar "x" 0))
          term = TmHandle handler (TmCon "Unit" [] [])
      shouldSucceed $ checkLinear emptyLinearEnv term

  describe "lazy and force" $ do
    it "handles lazy" $ do
      let term = TmLazy (TmVar "x" 0)
          env = extendLinearEnv "x" (Unique, Zero) emptyLinearEnv
      -- Lazy doesn't immediately use, but the thunk captures it
      shouldSucceed $ checkLinear env term

    it "handles force" $ do
      let term = TmForce (TmVar "thunk" 0)
          env = extendLinearEnv "thunk" (Unrestricted, Zero) emptyLinearEnv
      shouldSucceed $ checkLinear env term
