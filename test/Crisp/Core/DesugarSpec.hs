{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Core.DesugarSpec
-- Description : Tests for desugaring surface AST to core
--
-- Tests for the desugaring phase that transforms surface syntax
-- into the core calculus.

module Crisp.Core.DesugarSpec (spec) where

import Test.Hspec

import Crisp.Core.Desugar
import Crisp.Core.Term as C
import Crisp.Parser.Parser (parseModule)

import Data.Either (isRight, isLeft)
import qualified Data.Text as T

-- | Helper to check that desugaring succeeds
shouldDesugar :: T.Text -> Expectation
shouldDesugar src = case parseModule "test" src of
  Left err -> expectationFailure $ "Parse failed: " ++ show err
  Right m -> case desugarModule m of
    Left err -> expectationFailure $ "Desugar failed: " ++ show err
    Right _ -> pure ()

-- | Helper to check that desugaring fails
shouldNotDesugar :: T.Text -> Expectation
shouldNotDesugar src = case parseModule "test" src of
  Left _ -> pure ()  -- Parse failure is also acceptable
  Right m -> case desugarModule m of
    Left _ -> pure ()
    Right _ -> expectationFailure "Expected desugaring to fail"

spec :: Spec
spec = do
  describe "Crisp.Core.Desugar" $ do
    typeDefinitionTests
    typeAliasTests
    functionTests
    expressionTests
    traitDefinitionTests
    implDefinitionTests

-- =============================================================================
-- Type Definition Tests (issue #246)
-- =============================================================================

typeDefinitionTests :: Spec
typeDefinitionTests = describe "type definitions (issue #246)" $ do
  it "desugars simple enum type" $ do
    let src = T.unlines
          [ "module Test"
          , "type Color:"
          , "  Red"
          , "  Green"
          , "  Blue"
          ]
    shouldDesugar src

  it "desugars type with type parameters" $ do
    let src = T.unlines
          [ "module Test"
          , "type Maybe A:"
          , "  Nothing"
          , "  Just(value: A)"
          ]
    shouldDesugar src

  it "desugars record type" $ do
    let src = T.unlines
          [ "module Test"
          , "type Person:"
          , "  name: String"
          , "  age: Int"
          ]
    shouldDesugar src

  it "desugars type with multiple constructors" $ do
    let src = T.unlines
          [ "module Test"
          , "type Either A B:"
          , "  Left(value: A)"
          , "  Right(value: B)"
          ]
    shouldDesugar src

  it "desugars type with positional constructor args" $ do
    let src = T.unlines
          [ "module Test"
          , "type Pair A B:"
          , "  Pair(A, B)"
          ]
    shouldDesugar src

  it "desugars unit-like constructor (no args)" $ do
    let src = T.unlines
          [ "module Test"
          , "type Unit:"
          , "  Unit"
          ]
    shouldDesugar src

  it "desugars multiple type definitions" $ do
    let src = T.unlines
          [ "module Test"
          , "type Bool:"
          , "  True"
          , "  False"
          , ""
          , "type Option A:"
          , "  None"
          , "  Some(value: A)"
          ]
    shouldDesugar src

-- =============================================================================
-- Type Alias Tests (issue #246)
-- =============================================================================

typeAliasTests :: Spec
typeAliasTests = describe "type aliases (issue #246)" $ do
  it "desugars simple type alias" $ do
    let src = "module Test type MyInt = Int"
    shouldDesugar src

  it "desugars type alias with constraint" $ do
    let src = "module Test type ActiveUser = User where status: Active"
    shouldDesugar src

  it "desugars type alias with extended fields" $ do
    let src = T.unlines
          [ "module Test"
          , "type Extended = Base extended with:"
          , "  extra: Int"
          ]
    shouldDesugar src

-- =============================================================================
-- Function Tests
-- =============================================================================

functionTests :: Spec
functionTests = describe "function definitions" $ do
  it "desugars simple function" $ do
    let src = T.unlines
          [ "module Test"
          , "fn identity(x: Int) -> Int:"
          , "  x"
          ]
    shouldDesugar src

  it "desugars function with multiple params" $ do
    let src = T.unlines
          [ "module Test"
          , "fn add(x: Int, y: Int) -> Int:"
          , "  x"
          ]
    shouldDesugar src

-- =============================================================================
-- Expression Tests
-- =============================================================================

expressionTests :: Spec
expressionTests = describe "expressions" $ do
  it "desugars let expression" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test() -> Int:"
          , "  let x = 1"
          , "  x"
          ]
    shouldDesugar src

  it "desugars if expression" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test(b: Bool) -> Int:"
          , "  if b then 1 else 0"
          ]
    shouldDesugar src

  it "desugars match expression" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test(x: Int) -> Int:"
          , "  match x:"
          , "    0 -> 1"
          , "    _ -> 0"
          ]
    -- Note: literal patterns not yet supported, this will fail
    shouldNotDesugar src

  it "desugars binary operators" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test(x: Int, y: Int) -> Int:"
          , "  x + y"
          ]
    shouldDesugar src

  forLoopDesugarTests

-- =============================================================================
-- For Loop Desugaring Tests (issue #263)
-- =============================================================================

forLoopDesugarTests :: Spec
forLoopDesugarTests = describe "for loop desugaring (issue #263)" $ do
  it "desugars basic for loop" $ do
    -- Use the loop variable in the body to avoid unbound variable errors
    let src = T.unlines
          [ "module Test"
          , "fn process(items: List(Int)) -> Unit:"
          , "  for item in items:"
          , "    item"
          ]
    shouldDesugar src

  it "desugars for loop with let binding in body" $ do
    let src = T.unlines
          [ "module Test"
          , "fn process(ids: List(Int)) -> Unit:"
          , "  for id in ids:"
          , "    let result = id"
          , "    result"
          ]
    shouldDesugar src

  it "desugars nested for loops" $ do
    let src = T.unlines
          [ "module Test"
          , "fn analyze(ids: List(Int), items: List(String)) -> Unit:"
          , "  for id in ids:"
          , "    for item in items:"
          , "      item"
          ]
    shouldDesugar src

  it "desugars for loop with variable iteration" $ do
    let src = T.unlines
          [ "module Test"
          , "fn count(xs: List(Int)) -> Unit:"
          , "  for i in xs:"
          , "    i"
          ]
    shouldDesugar src

  it "desugars for loop to foreach application" $ do
    -- Verify the structure of desugared for loop
    let src = T.unlines
          [ "module Test"
          , "fn test(xs: List(Int)) -> Unit:"
          , "  for x in xs:"
          , "    x"
          ]
    case parseModule "test" src of
      Left err -> expectationFailure $ "Parse failed: " ++ show err
      Right m -> case desugarModule m of
        Left err -> expectationFailure $ "Desugar failed: " ++ show err
        Right terms -> case terms of
          [term] -> case term of
            -- Should be: TmLam "xs" ... (TmApp (TmApp foreach xs) (TmLam "x" ... body))
            C.TmLam _ _ bodyTerm -> case findForEach bodyTerm of
              Just _ -> pure ()  -- Found foreach application
              Nothing -> expectationFailure $ "Expected foreach in desugared output, got: " ++ show term
            other -> expectationFailure $ "Expected lambda, got: " ++ show other
          _ -> expectationFailure "Expected single term"

  it "for loop body has access to loop variable" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test(xs: List(Int)) -> Unit:"
          , "  for x in xs:"
          , "    let y = x"
          , "    y"
          ]
    shouldDesugar src

-- | Helper to find a foreach application in a term
findForEach :: C.Term -> Maybe C.Term
findForEach term = case term of
  C.TmApp (C.TmApp (C.TmVar "foreach" _) _) _ -> Just term
  C.TmApp f a -> findForEach f `orElse` findForEach a
  C.TmLam _ _ body -> findForEach body
  C.TmLet _ _ value body -> findForEach value `orElse` findForEach body
  _ -> Nothing
  where
    orElse Nothing b = b
    orElse a _ = a

-- =============================================================================
-- Trait Definition Tests (issue #264)
-- =============================================================================

traitDefinitionTests :: Spec
traitDefinitionTests = describe "trait definitions (issue #264)" $ do
  it "desugars simple trait with one method" $ do
    let src = T.unlines
          [ "module Test"
          , "trait Show:"
          , "  fn show(self) -> String"
          ]
    shouldDesugar src

  it "desugars trait with type parameter" $ do
    let src = T.unlines
          [ "module Test"
          , "trait Ord A:"
          , "  fn compare(x: A, y: A) -> Int"
          ]
    shouldDesugar src

  it "desugars trait with multiple methods" $ do
    let src = T.unlines
          [ "module Test"
          , "trait Num A:"
          , "  fn add(x: A, y: A) -> A"
          , "  fn sub(x: A, y: A) -> A"
          , "  fn mul(x: A, y: A) -> A"
          ]
    shouldDesugar src

  it "desugars trait with Self type in method signature" $ do
    let src = T.unlines
          [ "module Test"
          , "trait Clone:"
          , "  fn clone(self) -> Self"
          ]
    shouldDesugar src

  it "desugars trait with method returning different type" $ do
    let src = T.unlines
          [ "module Test"
          , "trait Hash:"
          , "  fn hash(self) -> Int"
          ]
    shouldDesugar src

  it "desugars multiple traits in module" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "trait Show:"
          , "  fn show(self) -> String"
          , ""
          , "trait Clone:"
          , "  fn clone(self) -> Self"
          ]
    shouldDesugar src

-- =============================================================================
-- Impl Definition Tests (issue #274)
-- =============================================================================

implDefinitionTests :: Spec
implDefinitionTests = describe "impl definitions (issue #274)" $ do
  it "desugars simple impl with one method" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "trait Show:"
          , "  fn show(self) -> String"
          , ""
          , "type Color:"
          , "  Red"
          , "  Green"
          , ""
          , "impl Show for Color:"
          , "  fn show(c: Color) -> String:"
          , "    \"color\""
          ]
    shouldDesugar src

  it "desugars impl with match expression in method" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "trait Action:"
          , "  fn name(self) -> String"
          , ""
          , "type JudicialAction:"
          , "  Ruling"
          , "  Order"
          , ""
          , "impl Action for JudicialAction:"
          , "  fn name(action: JudicialAction) -> String:"
          , "    match action"
          , "      Ruling -> \"Ruling\""
          , "      Order -> \"Order\""
          ]
    shouldDesugar src

  it "desugars impl with multiple methods" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "trait Describable:"
          , "  fn name(self) -> String"
          , "  fn description(self) -> String"
          , ""
          , "type Item:"
          , "  Book(title: String)"
          , ""
          , "impl Describable for Item:"
          , "  fn name(i: Item) -> String:"
          , "    \"item\""
          , "  fn description(i: Item) -> String:"
          , "    \"an item\""
          ]
    shouldDesugar src

  it "desugars impl for parameterized type" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "trait Show:"
          , "  fn show(self) -> String"
          , ""
          , "type Box(A):"
          , "  value: A"
          , ""
          , "impl Show for Box(Int):"
          , "  fn show(b: Box(Int)) -> String:"
          , "    \"box\""
          ]
    shouldDesugar src

  it "desugars multiple impl blocks in module" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "trait Show:"
          , "  fn show(self) -> String"
          , ""
          , "type Red:"
          , "  Red"
          , ""
          , "type Blue:"
          , "  Blue"
          , ""
          , "impl Show for Red:"
          , "  fn show(r: Red) -> String:"
          , "    \"red\""
          , ""
          , "impl Show for Blue:"
          , "  fn show(b: Blue) -> String:"
          , "    \"blue\""
          ]
    shouldDesugar src

  it "desugars impl with multiple parameters" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "trait Processor:"
          , "  fn process(self, x: Int) -> Int"
          , ""
          , "type Doubler:"
          , "  Doubler"
          , ""
          , "impl Processor for Doubler:"
          , "  fn process(d: Doubler, x: Int) -> Int:"
          , "    x + x"
          ]
    shouldDesugar src
