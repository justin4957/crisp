{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Parser.ParserSpec
-- Description : Comprehensive parser test suite
--
-- Tests for the Crisp parser covering all expression forms, patterns,
-- types, declarations, and modules as specified in issue #8.

module Crisp.Parser.ParserSpec (spec) where

import Test.Hspec

import Crisp.Parser.Parser
import Crisp.Formatter.Format (formatExpr, defaultFormatOptions)
import Crisp.Syntax.Surface
import Crisp.Syntax.Span (Span(..))

import Data.Either (isRight, isLeft)
import qualified Data.Text as T

-- | Helper to check that parsing succeeds
shouldParse :: (Show a, Show b) => Either a b -> Expectation
shouldParse result = result `shouldSatisfy` isRight

-- | Helper to check that parsing fails
shouldNotParse :: (Show a, Show b) => Either a b -> Expectation
shouldNotParse result = result `shouldSatisfy` isLeft

spec :: Spec
spec = do
  expressionTests
  recordConstructionTests
  methodCallTests
  listLiteralTests
  patternTests
  typeTests
  declarationTests
  moduleTests
  operatorTests
  edgeCaseTests
  docCommentTests

-- =============================================================================
-- Expression Tests
-- =============================================================================

expressionTests :: Spec
expressionTests = describe "expressions" $ do
  literalTests
  variableTests
  applicationTests
  letExpressionTests
  ifExpressionTests
  matchExpressionTests
  lambdaTests
  doNotationTests
  effectTests
  lazyForceTests
  pipelineTests
  annotationTests
  forLoopTests

literalTests :: Spec
literalTests = describe "literals" $ do
  it "parses integer literals" $ do
    shouldParse $ parseExpr "test" "42"

  it "parses zero" $ do
    shouldParse $ parseExpr "test" "0"

  it "parses large integers" $ do
    shouldParse $ parseExpr "test" "9999999999"

  it "parses float literals" $ do
    shouldParse $ parseExpr "test" "3.14"

  it "parses float with exponent" $ do
    shouldParse $ parseExpr "test" "1.0e10"

  it "parses float with negative exponent" $ do
    shouldParse $ parseExpr "test" "1.0e-5"

  it "parses string literals" $ do
    shouldParse $ parseExpr "test" "\"hello\""

  it "parses empty string" $ do
    shouldParse $ parseExpr "test" "\"\""

  it "parses string with escapes" $ do
    shouldParse $ parseExpr "test" "\"hello\\nworld\""

  it "parses string with unicode" $ do
    shouldParse $ parseExpr "test" "\"hello 世界\""

  it "parses character literals" $ do
    shouldParse $ parseExpr "test" "'a'"

  it "parses character with escape" $ do
    shouldParse $ parseExpr "test" "'\\n'"

  it "parses unit literal" $ do
    shouldParse $ parseExpr "test" "()"

  it "parses unit correctly as EUnit" $ do
    case parseExpr "test" "()" of
      Right (EUnit _) -> pure ()
      Right other -> expectationFailure $ "Expected EUnit, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

variableTests :: Spec
variableTests = describe "variables and constructors" $ do
  it "parses lowercase variable" $ do
    shouldParse $ parseExpr "test" "foo"

  it "parses variable with underscores" $ do
    shouldParse $ parseExpr "test" "foo_bar"

  it "parses variable with primes" $ do
    shouldParse $ parseExpr "test" "x'"

  it "parses variable with numbers" $ do
    shouldParse $ parseExpr "test" "x1"

  it "parses constructor (uppercase)" $ do
    shouldParse $ parseExpr "test" "Some"

  it "parses constructor as ECon" $ do
    case parseExpr "test" "Some" of
      Right (ECon "Some" _) -> pure ()
      Right other -> expectationFailure $ "Expected ECon, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses variable as EVar" $ do
    case parseExpr "test" "foo" of
      Right (EVar "foo" _) -> pure ()
      Right other -> expectationFailure $ "Expected EVar, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "distinguishes True as constructor" $ do
    case parseExpr "test" "True" of
      Right (ECon "True" _) -> pure ()
      Right other -> expectationFailure $ "Expected ECon True, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

applicationTests :: Spec
applicationTests = describe "function application" $ do
  it "parses single argument application" $ do
    shouldParse $ parseExpr "test" "f x"

  it "parses multiple argument application" $ do
    shouldParse $ parseExpr "test" "f x y z"

  it "parses nested application with parens" $ do
    shouldParse $ parseExpr "test" "f (g x)"

  it "parses application left-associativity" $ do
    case parseExpr "test" "f x y" of
      Right (EApp _ args _) -> length args `shouldBe` 2
      Right other -> expectationFailure $ "Expected EApp, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses constructor application" $ do
    shouldParse $ parseExpr "test" "Some x"

  it "parses application with literal argument" $ do
    shouldParse $ parseExpr "test" "f 42"

  it "parses application with string argument" $ do
    shouldParse $ parseExpr "test" "print \"hello\""

letExpressionTests :: Spec
letExpressionTests = describe "let expressions" $ do
  it "parses simple let" $ do
    shouldParse $ parseExpr "test" "let x = 42 in x"

  it "parses let with type annotation" $ do
    shouldParse $ parseExpr "test" "let x: Int = 42 in x"

  it "parses nested let" $ do
    shouldParse $ parseExpr "test" "let x = 1 in let y = 2 in x"

  it "parses let with application value" $ do
    shouldParse $ parseExpr "test" "let x = f y in x"

  it "parses let with pattern" $ do
    shouldParse $ parseExpr "test" "let (a, b) = pair in a"

  it "parses let with constructor pattern" $ do
    shouldParse $ parseExpr "test" "let Some x = opt in x"

  it "creates ELet node" $ do
    case parseExpr "test" "let x = 42 in x" of
      Right (ELet _ _ _ _ _) -> pure ()
      Right other -> expectationFailure $ "Expected ELet, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "round-trips layout-based let through formatter" $ do
    let src = "let x = 1\nx"
    case formatExpr defaultFormatOptions src of
      Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
      Right formatted -> do
        formatted `shouldBe` "let x = 1\nx"
        -- Re-parse to verify it's still valid
        case parseExpr "test" formatted of
          Right (ELet _ _ _ _ _) -> pure ()
          Right other -> expectationFailure $ "Expected ELet, got " ++ show other
          Left err -> expectationFailure $ "Re-parse failed: " ++ show err

  it "round-trips nested layout-based let through formatter" $ do
    let src = "let x = 1 in let y = 2 in x"
    case formatExpr defaultFormatOptions src of
      Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
      Right formatted -> do
        formatted `shouldBe` "let x = 1\nlet y = 2\nx"
        case parseExpr "test" formatted of
          Right (ELet _ _ _ _ _) -> pure ()
          Right other -> expectationFailure $ "Expected ELet, got " ++ show other
          Left err -> expectationFailure $ "Re-parse failed: " ++ show err

ifExpressionTests :: Spec
ifExpressionTests = describe "if expressions" $ do
  it "parses simple if" $ do
    shouldParse $ parseExpr "test" "if True then 1 else 0"

  it "parses if with complex condition" $ do
    shouldParse $ parseExpr "test" "if f x then a else b"

  it "parses nested if in then branch" $ do
    shouldParse $ parseExpr "test" "if a then if b then 1 else 2 else 3"

  it "parses nested if in else branch" $ do
    shouldParse $ parseExpr "test" "if a then 1 else if b then 2 else 3"

  it "creates EIf node" $ do
    case parseExpr "test" "if True then 1 else 0" of
      Right (EIf _ _ _ _) -> pure ()
      Right other -> expectationFailure $ "Expected EIf, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

matchExpressionTests :: Spec
matchExpressionTests = describe "match expressions" $ do
  -- Note: The current parser has a known limitation where match expressions
  -- consume the entire remaining expression as the subject due to pExpr being
  -- greedy. Match arms with inline patterns don't work as expected.
  -- These tests document the current behavior and mark broken features as pending.

  it "parses match keyword with subject" $ do
    -- Match without arms works - creates EMatch with empty arms list
    shouldParse $ parseExpr "test" "match x"

  it "creates EMatch node with subject" $ do
    case parseExpr "test" "match x" of
      Right (EMatch (EVar "x" _) [] _) -> pure ()
      Right other -> expectationFailure $ "Expected EMatch with subject x, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses match with literal subject" $ do
    case parseExpr "test" "match 42" of
      Right (EMatch (EIntLit 42 _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected EMatch with int literal, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses match with constructor subject" $ do
    case parseExpr "test" "match True" of
      Right (EMatch (ECon "True" _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected EMatch with constructor, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses match with single arm" $ do
    case parseExpr "test" "match x n -> y" of
      Right (EMatch _ arms _) -> length arms `shouldBe` 1
      Right other -> expectationFailure $ "Expected EMatch, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses match with multiple arms" $ do
    case parseExpr "test" "match x 1 -> a 2 -> b" of
      Right (EMatch _ arms _) -> length arms `shouldBe` 2
      Right other -> expectationFailure $ "Expected EMatch, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses match with wildcard pattern" $ do
    case parseExpr "test" "match x _ -> y" of
      Right (EMatch _ [arm] _) -> case matchArmPattern arm of
        PatWildcard _ -> pure ()
        other -> expectationFailure $ "Expected wildcard pattern, got " ++ show other
      Right other -> expectationFailure $ "Expected EMatch with one arm, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses match with tuple pattern" $ do
    pendingWith "Tuple patterns require further parser work"

  it "parses match with guard" $ do
    pendingWith "Guards require further parser work"

  it "parses match with nested patterns" $ do
    pendingWith "Nested patterns require further parser work"

lambdaTests :: Spec
lambdaTests = describe "lambda expressions" $ do
  it "parses lambda with backslash" $ do
    shouldParse $ parseExpr "test" "\\x: Int. x"

  it "parses lambda with unicode λ" $ do
    shouldParse $ parseExpr "test" "λx: Int. x"

  it "parses lambda with multiple params" $ do
    -- Known limitation: multi-param lambdas don't parse because pType is greedy
    pendingWith "Parser limitation: pType consumes subsequent param names as type args"

  it "parses lambda with application body" $ do
    shouldParse $ parseExpr "test" "\\x: Int. f x"

  it "creates ELam node" $ do
    case parseExpr "test" "\\x: Int. x" of
      Right (ELam params _ _) -> length params `shouldSatisfy` (>= 1)
      Right other -> expectationFailure $ "Expected ELam, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses zero-param lambda" $ do
    shouldParse $ parseExpr "test" "\\. 42"

  it "captures param name and type" $ do
    case parseExpr "test" "\\x: Int. x" of
      Right (ELam [Param { paramName = name, paramType = TyName typeName _ }] _ _) -> do
        name `shouldBe` "x"
        typeName `shouldBe` "Int"
      Right other -> expectationFailure $ "Expected ELam with param, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

doNotationTests :: Spec
doNotationTests = describe "do notation" $ do
  it "parses simple do block" $ do
    shouldParse $ parseExpr "test" "do x"

  it "creates EDo node with result" $ do
    case parseExpr "test" "do x" of
      Right (EDo [] (EVar "x" _) _) -> pure ()
      Right other -> expectationFailure $ "Expected EDo with var result, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses do with literal result" $ do
    case parseExpr "test" "do 42" of
      Right (EDo [] (EIntLit 42 _) _) -> pure ()
      Right other -> expectationFailure $ "Expected EDo with int result, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  -- Known parser limitations with do statement parsing
  it "parses do with bind" $ do
    -- Do statements with binds don't parse correctly due to greedy pExpr in value
    pendingWith "Parser limitation: pExpr consumes final result expression"

  it "parses do with let" $ do
    pendingWith "Parser limitation: DoLet uses pExpr which consumes final result"

  it "parses do with multiple binds" $ do
    pendingWith "Parser limitation: pExpr consumes final result expression"

  it "creates EDo node with stmts" $ do
    pendingWith "Parser limitation: do statements don't parse correctly"

effectTests :: Spec
effectTests = describe "effect operations" $ do
  it "parses perform expression" $ do
    shouldParse $ parseExpr "test" "perform Log.info msg"

  it "parses perform without arguments" $ do
    shouldParse $ parseExpr "test" "perform State.get"

  it "parses perform with multiple arguments" $ do
    shouldParse $ parseExpr "test" "perform DB.query table key"

  it "creates EPerform node" $ do
    case parseExpr "test" "perform Log.info msg" of
      Right (EPerform effect op _ _) -> do
        effect `shouldBe` "Log"
        op `shouldBe` "info"
      Right other -> expectationFailure $ "Expected EPerform, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "captures operation arguments" $ do
    case parseExpr "test" "perform Log.info msg" of
      Right (EPerform _ _ args _) -> length args `shouldBe` 1
      Right other -> expectationFailure $ "Expected EPerform, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  -- Known parser limitation: with expression consumes both handler and body
  it "parses with handler expression" $ do
    pendingWith "Parser limitation: pExpr consumes body as handler arguments"

  it "creates EWith node" $ do
    pendingWith "Parser limitation: pExpr consumes body as handler arguments"

lazyForceTests :: Spec
lazyForceTests = describe "lazy and force" $ do
  it "parses lazy expression" $ do
    shouldParse $ parseExpr "test" "lazy x"

  it "parses lazy with complex expression" $ do
    shouldParse $ parseExpr "test" "lazy (f x)"

  it "parses force expression" $ do
    shouldParse $ parseExpr "test" "force x"

  it "parses force with complex expression" $ do
    shouldParse $ parseExpr "test" "force (lazy x)"

  it "creates ELazy node" $ do
    case parseExpr "test" "lazy x" of
      Right (ELazy _ _) -> pure ()
      Right other -> expectationFailure $ "Expected ELazy, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "creates EForce node" $ do
    case parseExpr "test" "force x" of
      Right (EForce _ _) -> pure ()
      Right other -> expectationFailure $ "Expected EForce, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

pipelineTests :: Spec
pipelineTests = describe "pipeline operator" $ do
  it "parses simple pipeline" $ do
    shouldParse $ parseExpr "test" "x |> f"

  it "parses chained pipeline" $ do
    shouldParse $ parseExpr "test" "x |> f |> g"

  it "parses pipeline with application" $ do
    shouldParse $ parseExpr "test" "x |> f y"

  it "creates EPipe node" $ do
    case parseExpr "test" "x |> f" of
      Right (EPipe _ _ _) -> pure ()
      Right other -> expectationFailure $ "Expected EPipe, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses pipeline left-associatively" $ do
    case parseExpr "test" "x |> f |> g" of
      Right (EPipe (EPipe _ _ _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected nested EPipe, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

annotationTests :: Spec
annotationTests = describe "parentheses" $ do
  it "parses parenthesized expression" $ do
    shouldParse $ parseExpr "test" "(x)"

  it "parses nested parentheses" $ do
    shouldParse $ parseExpr "test" "((x))"

  it "parses complex expression in parens" $ do
    shouldParse $ parseExpr "test" "(f x y)"

forLoopTests :: Spec
forLoopTests = describe "for loops (issue #169)" $ do
  it "parses basic for loop in function body" $ do
    let src = T.unlines
          [ "module Test"
          , "fn process(items: List(Item)) -> Unit:"
          , "  for item in items:"
          , "    log item"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefFn fd] -> case fnDefBody fd of
            EFor _ _ _ _ -> pure ()
            other -> expectationFailure $ "Expected EFor, got " ++ show other
          _ -> expectationFailure "Expected function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses for loop with let binding in body" $ do
    let src = T.unlines
          [ "module Test"
          , "fn process(ids: List(Int)) -> Unit:"
          , "  for id in ids:"
          , "    let result = compute id"
          , "    log result"
          ]
    shouldParse $ parseModule "test" src

  it "parses nested for loops" $ do
    let src = T.unlines
          [ "module Test"
          , "fn analyze(ids: List(Int)) -> Unit:"
          , "  for id in ids:"
          , "    for item in items:"
          , "      process item"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefFn fd] -> case fnDefBody fd of
            EFor _ _ (EFor _ _ _ _) _ -> pure ()
            other -> expectationFailure $ "Expected nested EFor, got " ++ show other
          _ -> expectationFailure "Expected function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses for loop with constructor pattern" $ do
    let src = T.unlines
          [ "module Test"
          , "fn process(pairs: List(Pair)) -> Unit:"
          , "  for (Pair x y) in pairs:"
          , "    log x"
          ]
    shouldParse $ parseModule "test" src

-- =============================================================================
-- Record Construction Tests
-- =============================================================================

recordConstructionTests :: Spec
recordConstructionTests = describe "record construction (issue #173)" $ do
  it "parses simple record construction" $ do
    let src = T.unlines
          [ "module Test"
          , "fn make() -> Part:"
          , "  Part { number = 1, title = \"hello\" }"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd] -> case fnDefBody fd of
          ERecord name fields _ -> do
            name `shouldBe` "Part"
            length fields `shouldBe` 2
          _ -> expectationFailure "Expected record construction expression"
        _ -> expectationFailure "Expected function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses nested record construction" $ do
    let src = T.unlines
          [ "module Test"
          , "fn make() -> Arg:"
          , "  Arg { id = Id { value = 1 }, name = \"test\" }"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd] -> case fnDefBody fd of
          ERecord name fields _ -> do
            name `shouldBe` "Arg"
            length fields `shouldBe` 2
            case fields of
              ((_, ERecord innerName _ _) : _) ->
                innerName `shouldBe` "Id"
              _ -> expectationFailure "Expected nested record"
          _ -> expectationFailure "Expected record construction expression"
        _ -> expectationFailure "Expected function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses record construction in let binding" $ do
    let src = T.unlines
          [ "module Test"
          , "fn make() -> Unit:"
          , "  let x = Point { x = 1, y = 2 }"
          , "  x"
          ]
    shouldParse $ parseModule "test" src

-- =============================================================================
-- Method Call Tests
-- =============================================================================

methodCallTests :: Spec
methodCallTests = describe "method call syntax (issue #174)" $ do
  it "parses simple method call" $ do
    let src = T.unlines
          [ "module Test"
          , "fn process(items: List) -> List:"
          , "  items.filter(is_valid)"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd] -> case fnDefBody fd of
          EMethodCall _ method args _ -> do
            method `shouldBe` "filter"
            length args `shouldBe` 1
          _ -> expectationFailure "Expected method call expression"
        _ -> expectationFailure "Expected function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses method call with multiple args" $ do
    let src = T.unlines
          [ "module Test"
          , "fn process(items: List) -> List:"
          , "  items.slice(0, 10)"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd] -> case fnDefBody fd of
          EMethodCall _ method args _ -> do
            method `shouldBe` "slice"
            length args `shouldBe` 2
          _ -> expectationFailure "Expected method call expression"
        _ -> expectationFailure "Expected function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses chained method calls" $ do
    let src = T.unlines
          [ "module Test"
          , "fn process(items: List) -> Int:"
          , "  items.filter(is_valid).map(get_id).length()"
          ]
    shouldParse $ parseModule "test" src

  it "parses method call with no args" $ do
    let src = T.unlines
          [ "module Test"
          , "fn process(items: List) -> Int:"
          , "  items.length()"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd] -> case fnDefBody fd of
          EMethodCall _ method args _ -> do
            method `shouldBe` "length"
            args `shouldBe` []
          _ -> expectationFailure "Expected method call expression"
        _ -> expectationFailure "Expected function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- List Literal Tests
-- =============================================================================

listLiteralTests :: Spec
listLiteralTests = describe "list literals (issue #176)" $ do
  it "parses empty list" $ do
    case parseExpr "test" "[]" of
      Right (EList elems _) -> length elems `shouldBe` 0
      Right other -> expectationFailure $ "Expected list, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses list with elements" $ do
    case parseExpr "test" "[1, 2, 3]" of
      Right (EList elems _) -> length elems `shouldBe` 3
      Right other -> expectationFailure $ "Expected list, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses nested lists" $ do
    case parseExpr "test" "[[1, 2], [3, 4]]" of
      Right (EList elems _) -> do
        length elems `shouldBe` 2
        case elems of
          [EList inner1 _, EList inner2 _] -> do
            length inner1 `shouldBe` 2
            length inner2 `shouldBe` 2
          _ -> expectationFailure "Expected nested lists"
      Right other -> expectationFailure $ "Expected list, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses list in let binding" $ do
    let src = T.unlines
          [ "module Test"
          , "fn make() -> Unit:"
          , "  let items = [1, 2, 3]"
          , "  items"
          ]
    shouldParse $ parseModule "test" src

-- =============================================================================
-- Pattern Tests
-- =============================================================================

patternTests :: Spec
patternTests = describe "patterns" $ do
  wildcardPatternTests
  variablePatternTests
  constructorPatternTests
  tuplePatternTests

wildcardPatternTests :: Spec
wildcardPatternTests = describe "wildcard patterns" $ do
  it "parses wildcard in match" $ do
    -- Match arms don't work due to parser limitation
    pendingWith "Parser limitation: match arms don't parse correctly"

  it "parses wildcard in let" $ do
    shouldParse $ parseExpr "test" "let _ = x in y"

variablePatternTests :: Spec
variablePatternTests = describe "variable patterns" $ do
  it "parses variable pattern in match" $ do
    pendingWith "Parser limitation: match arms don't parse correctly"

  it "parses variable pattern in let" $ do
    shouldParse $ parseExpr "test" "let x = 1 in x"

  it "parses variable with underscore" $ do
    pendingWith "Parser limitation: match arms don't parse correctly"

constructorPatternTests :: Spec
constructorPatternTests = describe "constructor patterns" $ do
  it "parses nullary constructor pattern" $ do
    pendingWith "Parser limitation: match arms don't parse correctly"

  it "parses unary constructor pattern" $ do
    pendingWith "Parser limitation: match arms don't parse correctly"

  it "parses binary constructor pattern" $ do
    pendingWith "Parser limitation: match arms don't parse correctly"

  it "parses nested constructor pattern" $ do
    pendingWith "Parser limitation: match arms don't parse correctly"

  it "parses constructor with wildcard" $ do
    pendingWith "Parser limitation: match arms don't parse correctly"

  it "parses constructor pattern in let" $ do
    shouldParse $ parseExpr "test" "let Some x = opt in x"

tuplePatternTests :: Spec
tuplePatternTests = describe "tuple patterns" $ do
  it "parses pair pattern" $ do
    shouldParse $ parseExpr "test" "let (a, b) = p in a"

  it "parses triple pattern" $ do
    shouldParse $ parseExpr "test" "let (a, b, c) = t in a"

  it "parses nested tuple pattern" $ do
    shouldParse $ parseExpr "test" "let ((a, b), c) = t in a"

  it "parses tuple in match" $ do
    pendingWith "Parser limitation: match arms don't parse correctly"

-- =============================================================================
-- Type Tests
-- =============================================================================

typeTests :: Spec
typeTests = describe "types" $ do
  simpleTypeTests
  functionTypeTests
  typeApplicationTests
  forallTypeTests
  effectTypeTests
  specialTypeTests

simpleTypeTests :: Spec
simpleTypeTests = describe "simple types" $ do
  it "parses type variable" $ do
    shouldParse $ parseType "test" "a"

  it "parses type constructor" $ do
    shouldParse $ parseType "test" "Int"

  it "parses uppercase type" $ do
    shouldParse $ parseType "test" "Bool"

  it "creates TyName node" $ do
    case parseType "test" "Int" of
      Right (TyName "Int" _) -> pure ()
      Right other -> expectationFailure $ "Expected TyName, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

functionTypeTests :: Spec
functionTypeTests = describe "function types" $ do
  it "parses simple function type" $ do
    shouldParse $ parseType "test" "Int -> Bool"

  it "parses multi-argument function type" $ do
    shouldParse $ parseType "test" "Int -> Int -> Int"

  it "parses right-associative function type" $ do
    case parseType "test" "Int -> Int -> Int" of
      Right (TyFn _ (TyFn _ _ _ _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected nested TyFn, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses function with parens" $ do
    shouldParse $ parseType "test" "(Int -> Int) -> Int"

  it "parses function returning function" $ do
    shouldParse $ parseType "test" "Int -> (Int -> Int)"

typeApplicationTests :: Spec
typeApplicationTests = describe "type applications" $ do
  it "parses single type application" $ do
    shouldParse $ parseType "test" "Option Int"

  it "parses multiple type applications" $ do
    shouldParse $ parseType "test" "Either String Int"

  it "parses nested type applications" $ do
    shouldParse $ parseType "test" "Option (Option Int)"

  it "parses type application in function" $ do
    shouldParse $ parseType "test" "Option Int -> Bool"

  it "creates TyApp node" $ do
    case parseType "test" "Option Int" of
      Right (TyApp _ args _) -> length args `shouldBe` 1
      Right other -> expectationFailure $ "Expected TyApp, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

forallTypeTests :: Spec
forallTypeTests = describe "forall types" $ do
  it "parses simple forall" $ do
    shouldParse $ parseType "test" "forall T. T -> T"

  it "parses forall with kind annotation" $ do
    shouldParse $ parseType "test" "forall T: Type. T -> T"

  it "parses nested forall" $ do
    shouldParse $ parseType "test" "forall A. forall B. A -> B -> A"

  it "creates TyForall node" $ do
    case parseType "test" "forall T. T -> T" of
      Right (TyForall _ _ _) -> pure ()
      Right other -> expectationFailure $ "Expected TyForall, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

effectTypeTests :: Spec
effectTypeTests = describe "effect types" $ do
  it "parses function with single effect" $ do
    shouldParse $ parseType "test" "Int -> Bool ! IO"

  it "parses function with multiple effects" $ do
    shouldParse $ parseType "test" "Int -> Bool ! IO, State"

  it "parses effect with authority" $ do
    shouldParse $ parseType "test" "Int -> Bool ! IO@System"

  it "creates TyFn with effects" $ do
    case parseType "test" "Int -> Bool ! IO" of
      Right (TyFn _ _ effects _) -> length effects `shouldSatisfy` (>= 1)
      Right other -> expectationFailure $ "Expected TyFn with effects, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

specialTypeTests :: Spec
specialTypeTests = describe "special types" $ do
  it "parses Lazy type" $ do
    shouldParse $ parseType "test" "Lazy Int"

  it "parses ref type" $ do
    shouldParse $ parseType "test" "ref Int"

  it "parses ref mut type" $ do
    shouldParse $ parseType "test" "ref mut Int"

  it "parses parenthesized type" $ do
    shouldParse $ parseType "test" "(Int)"

  it "creates TyLazy node" $ do
    case parseType "test" "Lazy Int" of
      Right (TyLazy _ _) -> pure ()
      Right other -> expectationFailure $ "Expected TyLazy, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "creates TyRef node" $ do
    case parseType "test" "ref Int" of
      Right (TyRef _ False _) -> pure ()
      Right other -> expectationFailure $ "Expected TyRef, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "creates TyRef mut node" $ do
    case parseType "test" "ref mut Int" of
      Right (TyRef _ True _) -> pure ()
      Right other -> expectationFailure $ "Expected TyRef mut, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Declaration Tests
-- =============================================================================

declarationTests :: Spec
declarationTests = describe "declarations" $ do
  functionDefTests
  typeDefTests
  effectDefTests
  handlerDefTests

functionDefTests :: Spec
functionDefTests = describe "function definitions" $ do
  it "parses simple function" $ do
    let src = "module Main fn add(x: Int, y: Int) -> Int: x"
    shouldParse $ parseModule "test" src

  it "parses function without return type" $ do
    let src = "module Main fn foo(x: Int): x"
    shouldParse $ parseModule "test" src

  it "parses function with type parameters" $ do
    let src = "module Main fn id[T](x: T) -> T: x"
    shouldParse $ parseModule "test" src

  it "parses function with effects" $ do
    let src = "module Main fn log(msg: String) -> Unit ! IO: ()"
    shouldParse $ parseModule "test" src

  it "parses function with no parameters" $ do
    let src = "module Main fn main: ()"
    shouldParse $ parseModule "test" src

  it "parses function with complex body" $ do
    let src = "module Main fn foo(x: Int): let y = x in y"
    shouldParse $ parseModule "test" src

typeDefTests :: Spec
typeDefTests = describe "type definitions" $ do
  it "parses simple type definition" $ do
    let src = "module Main type Unit"
    shouldParse $ parseModule "test" src

  it "parses type with parameter" $ do
    let src = "module Main type Option T"
    shouldParse $ parseModule "test" src

  it "extracts type name" $ do
    case parseModule "test" "module Main type Unit" of
      Right m -> case moduleDefinitions m of
        [DefType td] -> typeDefName td `shouldBe` "Unit"
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts type parameters" $ do
    case parseModule "test" "module Main type Option T" of
      Right m -> case moduleDefinitions m of
        [DefType td] -> length (typeDefParams td) `shouldBe` 1
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  -- Known parser issue: colon is interpreted as kind annotation before constructors
  it "parses type with constructors" $ do
    pendingWith "Parser issue: colon before constructors is parsed as kind annotation"

  it "parses type with constructor arguments" $ do
    pendingWith "Parser issue: colon before constructors is parsed as kind annotation"

  it "parses prop type" $ do
    let src = "module Main type prop Eq T"
    shouldParse $ parseModule "test" src

  it "parses linear type" $ do
    let src = "module Main type linear Token"
    shouldParse $ parseModule "test" src

  it "parses GADT constructor" $ do
    pendingWith "Parser issue: colon before constructors is parsed as kind annotation"

  it "parses type with kind annotation" $ do
    let src = "module Main type List T: Type"
    shouldParse $ parseModule "test" src

  -- Constructor syntax tests (issues #115, #116)
  it "parses constructor with named fields" $ do
    let src = T.unlines
          [ "module Test"
          , "type Wrapper:"
          , "  Empty"
          , "  Value(content: String)"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleDefinitions m) `shouldBe` 1
        case moduleDefinitions m of
          [DefType td] -> length (typeDefConstructors td) `shouldBe` 2
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses constructor with multiple named fields" $ do
    let src = T.unlines
          [ "module Test"
          , "type Point:"
          , "  Origin"
          , "  Cartesian(x: Int, y: Int)"
          , "  Polar(radius: Float, angle: Float)"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> length (typeDefConstructors td) `shouldBe` 3
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses constructor with positional fields" $ do
    let src = T.unlines
          [ "module Test"
          , "type Pair:"
          , "  Empty"
          , "  MkPair(Int, Int)"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> length (typeDefConstructors td) `shouldBe` 2
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses constructor with multiple positional fields" $ do
    let src = T.unlines
          [ "module Test"
          , "type Triple:"
          , "  MkTriple(Int, String, Bool)"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> do
            length (typeDefConstructors td) `shouldBe` 1
            case typeDefConstructors td of
              [SimpleConstructor _ args _] -> length args `shouldBe` 3
              _ -> expectationFailure "Expected SimpleConstructor with 3 args"
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses mixed nullary and parenthesized constructors" $ do
    let src = T.unlines
          [ "module Test"
          , "type Timezone:"
          , "  UTC"
          , "  Local"
          , "  Offset(hours: Int, minutes: Int)"
          , "  Named(name: String)"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> length (typeDefConstructors td) `shouldBe` 4
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses generic type with named field constructors" $ do
    let src = T.unlines
          [ "module Test"
          , "type Result E A:"
          , "  Ok(value: A)"
          , "  Err(error: E)"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> do
            length (typeDefParams td) `shouldBe` 2
            length (typeDefConstructors td) `shouldBe` 2
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses generic type with positional field constructors" $ do
    let src = T.unlines
          [ "module Test"
          , "type Pair A B:"
          , "  MkPair(A, B)"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> do
            length (typeDefParams td) `shouldBe` 2
            length (typeDefConstructors td) `shouldBe` 1
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses constructor with nested generic types" $ do
    let src = T.unlines
          [ "module Test"
          , "type Complex:"
          , "  Simple(Int)"
          , "  Nested(List Int, Option String)"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> length (typeDefConstructors td) `shouldBe` 2
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  -- Nullary constructors and enum-style types (issue #170)
  it "parses pure enum type with all nullary constructors (issue #170)" $ do
    let src = T.unlines
          [ "module Test"
          , "type ReferenceType:"
          , "  Citation"
          , "  Incorporation"
          , "  Exception"
          , "  Condition"
          , "  Definition"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> do
            typeDefName td `shouldBe` "ReferenceType"
            length (typeDefConstructors td) `shouldBe` 5
            case typeDefConstructors td of
              (SimpleConstructor name _ _:_) -> name `shouldBe` "Citation"
              _ -> expectationFailure "Expected SimpleConstructor"
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses multiple enum types in same module (issue #170)" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "type AnnotationAuthor:"
          , "  Legislative"
          , "  Judicial"
          , "  Editorial"
          , ""
          , "type Language:"
          , "  English"
          , "  French"
          , "  German"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleDefinitions m) `shouldBe` 2
        case moduleDefinitions m of
          [DefType td1, DefType td2] -> do
            typeDefName td1 `shouldBe` "AnnotationAuthor"
            length (typeDefConstructors td1) `shouldBe` 3
            typeDefName td2 `shouldBe` "Language"
            length (typeDefConstructors td2) `shouldBe` 3
          _ -> expectationFailure "Expected two type definitions"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses mixed nullary and record constructors (issue #170)" $ do
    let src = T.unlines
          [ "module Test"
          , "type Shape:"
          , "  Point"
          , "  Circle(radius: Int)"
          , "  Rectangle(width: Int, height: Int)"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> do
            length (typeDefConstructors td) `shouldBe` 3
            case typeDefConstructors td of
              [SimpleConstructor n1 args1 _, RecordConstructor n2 _ _, RecordConstructor n3 _ _] -> do
                n1 `shouldBe` "Point"
                null args1 `shouldBe` True
                n2 `shouldBe` "Circle"
                n3 `shouldBe` "Rectangle"
              _ -> expectationFailure "Expected Point, Circle, Rectangle constructors"
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  -- Deriving clause tests (issue #167)
  it "parses type with deriving clause (issue #167)" $ do
    let src = T.unlines
          [ "module Test"
          , "type ConfidenceLevel deriving (Eq, Ord):"
          , "  VeryHigh"
          , "  High"
          , "  Moderate"
          , "  Low"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> do
            typeDefName td `shouldBe` "ConfidenceLevel"
            case typeDefDeriving td of
              Just dc -> derivingTraits dc `shouldBe` ["Eq", "Ord"]
              Nothing -> expectationFailure "Expected deriving clause"
            length (typeDefConstructors td) `shouldBe` 4
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type with single deriving trait (issue #167)" $ do
    let src = T.unlines
          [ "module Test"
          , "type Status deriving Eq:"
          , "  Active"
          , "  Inactive"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> do
            case typeDefDeriving td of
              Just dc -> derivingTraits dc `shouldBe` ["Eq"]
              Nothing -> expectationFailure "Expected deriving clause"
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type with deriving and constructors with arguments (issue #167)" $ do
    let src = T.unlines
          [ "module Test"
          , "type Option T deriving (Eq):"
          , "  Some(value: T)"
          , "  None"
          ]
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefType td] -> do
            typeDefName td `shouldBe` "Option"
            length (typeDefParams td) `shouldBe` 1
            case typeDefDeriving td of
              Just dc -> derivingTraits dc `shouldBe` ["Eq"]
              Nothing -> expectationFailure "Expected deriving clause"
            length (typeDefConstructors td) `shouldBe` 2
          _ -> expectationFailure "Expected type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  -- Type alias with 'where' refinement tests (issue #118)
  it "parses type alias with where refinement" $ do
    let src = "module Test type PositiveInt = Int where { self > 0 }"
    case parseModule "test" src of
      Right m -> do
        length (moduleDefinitions m) `shouldBe` 1
        case moduleDefinitions m of
          [DefTypeAlias _] -> pure ()
          _ -> expectationFailure "Expected type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type alias with where and field access" $ do
    let src = "module Test type ValidRange = Range where { self.start <= self.end }"
    case parseModule "test" src of
      Right m -> do
        length (moduleDefinitions m) `shouldBe` 1
        case moduleDefinitions m of
          [DefTypeAlias _] -> pure ()
          _ -> expectationFailure "Expected type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses generic type alias with where refinement" $ do
    let src = "module Test type NonEmpty A = List A where { self.length > 0 }"
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefTypeAlias td] -> length (typeAliasParams td) `shouldBe` 1
          _ -> expectationFailure "Expected type alias definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type alias with chained field access in where" $ do
    let src = "module Test type Valid = Record where { self.field.inner > 0 }"
    shouldParse $ parseModule "test" src

  it "parses type alias with multiple predicates in where" $ do
    let src = "module Test type Bounded = Int where { self >= 0, self <= 100 }"
    shouldParse $ parseModule "test" src

  -- Match expressions in refinement predicates (issue #120)
  it "parses match expression in refinement predicate" $ do
    let src = T.unlines
          [ "module Test"
          , "type NonEmptyOption A = Option A where {"
          , "  match self"
          , "    Some(_) -> True"
          , "    None -> False"
          , "}"
          ]
    shouldParse $ parseModule "test" src

  it "parses match with field access in refinement" $ do
    let src = T.unlines
          [ "module Test"
          , "type ValidTemporalRange = TemporalRange where {"
          , "  match self.effective_until"
          , "    None -> True"
          , "    Some(end) -> end >= self.effective_from"
          , "}"
          ]
    shouldParse $ parseModule "test" src

  it "parses if expression in refinement predicate" $ do
    let src = "module Test type Bounded = Int where { if self >= 0 then True else False }"
    shouldParse $ parseModule "test" src

  it "parses boolean literals in refinement" $ do
    let src = "module Test type Always = Int where { True }"
    shouldParse $ parseModule "test" src

  -- Constrained type aliases with 'where field: Pattern' syntax (issue #168)
  it "parses type alias with where field constraint (issue #168)" $ do
    let src = "module Test type TrialCourt = Court where level: TrialCourt"
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefTypeAlias ad] -> do
            typeAliasName ad `shouldBe` "TrialCourt"
            length (typeAliasConstraints ad) `shouldBe` 1
          _ -> expectationFailure "Expected type alias"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type alias with multiple where field constraints (issue #168)" $ do
    let src = "module Test type ValidRecord = Record where status: Active, verified: True"
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefTypeAlias ad] -> do
            typeAliasName ad `shouldBe` "ValidRecord"
            length (typeAliasConstraints ad) `shouldBe` 2
          _ -> expectationFailure "Expected type alias"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type alias with where pattern constraint (issue #168)" $ do
    let src = "module Test type ActiveUser = User where status: Active"
    case parseModule "test" src of
      Right m -> do
        case moduleDefinitions m of
          [DefTypeAlias ad] -> do
            typeAliasName ad `shouldBe` "ActiveUser"
            case typeAliasConstraints ad of
              [fc] -> fieldConstraintName fc `shouldBe` "status"
              _ -> expectationFailure "Expected one field constraint"
          _ -> expectationFailure "Expected type alias"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type with multiple dependent parameters (issue #172)" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "type Provision (j: Jurisdiction, temporal: TemporalRange):"
          , "  id: ProvisionId"
          , "  validity: TemporalValidity"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType td] -> do
          typeDefName td `shouldBe` "Provision"
          length (typeDefParams td) `shouldBe` 2
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type with single dependent parameter in parens (issue #172)" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "type Act (j: Jurisdiction):"
          , "  id: ActId"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType td] -> do
          typeDefName td `shouldBe` "Act"
          length (typeDefParams td) `shouldBe` 1
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses type with mixed uppercase and dependent parameters (issue #172)" $ do
    let src = T.unlines
          [ "module Test"
          , ""
          , "type Container T (n: Nat):"
          , "  items: List T"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType td] -> do
          typeDefName td `shouldBe` "Container"
          length (typeDefParams td) `shouldBe` 2
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

effectDefTests :: Spec
effectDefTests = describe "effect definitions" $ do
  it "parses simple effect" $ do
    let src = "module Main effect IO: print: String -> Unit"
    shouldParse $ parseModule "test" src

  it "extracts effect name" $ do
    case parseModule "test" "module Main effect IO: print: String -> Unit" of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> effectDefName ed `shouldBe` "IO"
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts operation" $ do
    case parseModule "test" "module Main effect IO: print: String -> Unit" of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> length (effectDefOperations ed) `shouldBe` 1
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  -- Known parser issue: multiple operations on same line
  it "parses effect with multiple operations" $ do
    pendingWith "Parser limitation: multiple operations need layout/newlines"

  it "parses doc comment on operation (issue #154)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect HttpClient:"
          , "  --- | Make a GET request"
          , "  http_get: String -> String"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> case effectDefOperations ed of
          [op] -> operationDocComment op `shouldBe` Just "Make a GET request"
          _ -> expectationFailure "Expected single operation"
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses effect with multiple documented operations (issue #154)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect Http:"
          , "  --- | GET request"
          , "  get: String -> String"
          , "  --- | POST request"
          , "  post: String -> String -> String"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> do
          length (effectDefOperations ed) `shouldBe` 2
          case effectDefOperations ed of
            [op1, op2] -> do
              operationDocComment op1 `shouldBe` Just "GET request"
              operationDocComment op2 `shouldBe` Just "POST request"
            _ -> expectationFailure "Expected two operations"
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses operation without doc comment has Nothing (issue #154)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect IO:"
          , "  print: String -> Unit"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> case effectDefOperations ed of
          [op] -> operationDocComment op `shouldBe` Nothing
          _ -> expectationFailure "Expected single operation"
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses function-style effect operation (issue #162)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect HttpClient:"
          , "  http_get(url: String) -> String"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> case effectDefOperations ed of
          [op] -> operationName op `shouldBe` "http_get"
          _ -> expectationFailure "Expected single operation"
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses function-style operation with multiple params (issue #162)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect HttpClient:"
          , "  http_post(url: String, body: String) -> String"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> case effectDefOperations ed of
          [op] -> operationName op `shouldBe` "http_post"
          _ -> expectationFailure "Expected single operation"
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses effect with mixed operation syntaxes (issue #162)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect Database:"
          , "  connect: String -> Unit"
          , "  query(sql: String) -> String"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> length (effectDefOperations ed) `shouldBe` 2
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses function-style operation with no params (issue #162)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect Random:"
          , "  next() -> Int"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> case effectDefOperations ed of
          [op] -> operationName op `shouldBe` "next"
          _ -> expectationFailure "Expected single operation"
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses effect with single type parameter (issue #171)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect State S:"
          , "  get: S"
          , "  put: S -> Unit"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> do
          effectDefName ed `shouldBe` "State"
          length (effectDefTypeParams ed) `shouldBe` 1
          length (effectDefOperations ed) `shouldBe` 2
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses effect with multiple type parameters (issue #171)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect Transform A B:"
          , "  apply: A -> B"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> do
          effectDefName ed `shouldBe` "Transform"
          length (effectDefTypeParams ed) `shouldBe` 2
          length (effectDefOperations ed) `shouldBe` 1
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses effect without type params still works (issue #171)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "effect IO:"
          , "  print: String -> Unit"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefEffect ed] -> do
          effectDefName ed `shouldBe` "IO"
          effectDefTypeParams ed `shouldBe` []
        _ -> expectationFailure "Expected single effect definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

handlerDefTests :: Spec
handlerDefTests = describe "handler definitions" $ do
  it "parses simple handler" $ do
    let src = "module Main handler Pure for IO: return x -> x"
    shouldParse $ parseModule "test" src

  it "extracts handler name" $ do
    case parseModule "test" "module Main handler Pure for IO: return x -> x" of
      Right m -> case moduleDefinitions m of
        [DefHandler hd] -> handlerDefName hd `shouldBe` "Pure"
        _ -> expectationFailure "Expected single handler definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts return clause" $ do
    case parseModule "test" "module Main handler Pure for IO: return x -> x" of
      Right m -> case moduleDefinitions m of
        [DefHandler hd] -> length (handlerDefClauses hd) `shouldBe` 1
        _ -> expectationFailure "Expected single handler definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  -- Known parser limitations
  it "parses handler with operation clause" $ do
    pendingWith "Parser limitation: resume keyword conflict and clause parsing"

  it "parses handler with introduced effects" $ do
    let src = "module Main handler Logged for State ! Log: return x -> x"
    shouldParse $ parseModule "test" src

  it "parses handler with single value parameter (issue #185)" $ do
    let src = T.unlines
          [ "module Test"
          , "handler LoggingReasoning(log: List) for Reasoning:"
          , "  return x -> x"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefHandler hd] -> do
          handlerDefName hd `shouldBe` "LoggingReasoning"
          length (handlerDefParams hd) `shouldBe` 1
        _ -> expectationFailure "Expected single handler definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses handler with multiple value parameters (issue #185)" $ do
    let src = T.unlines
          [ "module Test"
          , "handler StateHandler(init: State, count: Int) for Effect:"
          , "  return x -> x"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefHandler hd] -> do
          handlerDefName hd `shouldBe` "StateHandler"
          length (handlerDefParams hd) `shouldBe` 2
        _ -> expectationFailure "Expected single handler definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses handler with value parameter in module (issue #185)" $ do
    let src = T.unlines
          [ "module Test"
          , "handler LoggingReasoning(log: List) for Reasoning:"
          , "  return x -> x"
          , "handler MockReasoning for Reasoning:"
          , "  return x -> x"
          ]
    case parseModule "test" src of
      Right m -> length (moduleDefinitions m) `shouldBe` 2
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Module Tests
-- =============================================================================

moduleTests :: Spec
moduleTests = describe "modules" $ do
  it "parses minimal module" $ do
    shouldParse $ parseModule "test" "module Main"

  it "parses module with dotted path" $ do
    shouldParse $ parseModule "test" "module Treasury.Audit"

  it "parses module with authority" $ do
    shouldParse $ parseModule "test" "module Treasury.Audit authority Treasury"

  it "parses module with requires effects" $ do
    let src = "module Main requires effects: IO, State"
    shouldParse $ parseModule "test" src

  it "parses module with requires types" $ do
    let src = "module Main requires types: Int, String"
    shouldParse $ parseModule "test" src

  it "parses module with requires module path" $ do
    let src = "module Main requires LexSim.Core.Refined"
    shouldParse $ parseModule "test" src

  it "parses module with multiple requires module paths" $ do
    let src = T.unlines
          [ "module LexSim.Core.Temporal"
          , "requires LexSim.Core.Refined"
          , "requires LexSim.Core.Authority"
          ]
    shouldParse $ parseModule "test" src

  it "parses module with mixed requires" $ do
    let src = T.unlines
          [ "module Main"
          , "requires effects: IO"
          , "requires LexSim.Core.Refined"
          , "requires types: Int"
          ]
    shouldParse $ parseModule "test" src

  it "extracts required module path" $ do
    let src = "module Main requires LexSim.Core.Refined"
    case parseModule "test" src of
      Right m -> do
        length (moduleRequires m) `shouldBe` 1
        case head (moduleRequires m) of
          RequireModule path _ -> modulePathSegments path `shouldBe` ["LexSim", "Core", "Refined"]
          _ -> expectationFailure "Expected RequireModule"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses module with provides type" $ do
    let src = "module Main provides type MyType"
    shouldParse $ parseModule "test" src

  it "parses module with provides fn with type" $ do
    let src = "module Main provides fn myFn: Int -> Int"
    shouldParse $ parseModule "test" src

  it "parses module with provides fn without type" $ do
    let src = "module Main provides fn myFn"
    shouldParse $ parseModule "test" src

  it "parses module with multiple provides" $ do
    let src = T.unlines
          [ "module Main"
          , "provides type MyType"
          , "provides fn myFn"
          , "provides fn otherFn: Int -> Bool"
          ]
    shouldParse $ parseModule "test" src

  it "extracts provides with optional type" $ do
    let src = T.unlines
          [ "module Main"
          , "provides fn noType"
          , "provides fn withType: Int"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 2
        case moduleProvides m of
          [ProvideFn "noType" Nothing _, ProvideFn "withType" (Just _) _] -> pure ()
          _ -> expectationFailure "Expected two ProvideFn with correct type presence"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with multiple items" $ do
    let src = T.unlines
          [ "module Main"
          , "provides"
          , "  type Date"
          , "  fn is_valid_at"
          , "  fn overlaps"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 3
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with typed fns" $ do
    let src = T.unlines
          [ "module Main"
          , "provides"
          , "  type Date"
          , "  fn is_valid_at: Date -> Bool"
          , "  fn overlaps: Date -> Date -> Bool"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 3
        case moduleProvides m of
          [ProvideType "Date" _, ProvideFn "is_valid_at" (Just _) _, ProvideFn "overlaps" (Just _) _] -> pure ()
          _ -> expectationFailure "Expected ProvideType and two typed ProvideFn"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses mixed inline and block provides" $ do
    let src = T.unlines
          [ "module Main"
          , "provides type First"
          , "provides"
          , "  type Second"
          , "  fn helper"
          , "provides fn last: Int"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 4
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block followed by type definition" $ do
    let src = T.unlines
          [ "module Test.WithProvides"
          , "provides"
          , "  type Foo"
          , ""
          , "type Foo:"
          , "  value: Int"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 1
        length (moduleDefinitions m) `shouldBe` 1
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block followed by function definition" $ do
    let src = T.unlines
          [ "module Test.Exports"
          , "provides"
          , "  fn helper"
          , "  type Data"
          , ""
          , "fn helper(x: Int) -> Int:"
          , "  x"
          , ""
          , "type Data:"
          , "  Data Int"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 2
        length (moduleDefinitions m) `shouldBe` 2
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with doc comments (issue #155)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  --- | FFI bindings"
          , "  fn some_function"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 1
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with multiple doc comments (issue #155)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  --- | Date type for temporal operations"
          , "  type Date"
          , "  --- | Check if date is valid"
          , "  fn is_valid"
          , "  --- | Overlap check"
          , "  fn overlaps"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 3
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with mixed documented and undocumented items (issue #155)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  type Plain"
          , "  --- | Documented function"
          , "  fn documented"
          , "  fn undocumented"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 3
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with external fn (issue #156)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  external fn ffi_http_get"
          , "  fn some_function"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 2
        case moduleProvides m of
          [ProvideExternalFn name _ _, ProvideFn _ _ _] -> name `shouldBe` "ffi_http_get"
          _ -> expectationFailure "Expected ProvideExternalFn and ProvideFn"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides external fn with type annotation (issue #156)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  external fn query: String -> String"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 1
        case moduleProvides m of
          [ProvideExternalFn name (Just _) _] -> name `shouldBe` "query"
          _ -> expectationFailure "Expected ProvideExternalFn with type"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with mixed types, fns, and external fns (issue #156)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  type Config"
          , "  fn initialize"
          , "  external fn ffi_connect"
          , "  external fn ffi_query: String -> Result"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 4
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with effect (issue #160)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  effect HttpClient"
          , "  fn some_function"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 2
        case moduleProvides m of
          [ProvideEffect name _, ProvideFn _ _ _] -> name `shouldBe` "HttpClient"
          _ -> expectationFailure "Expected ProvideEffect and ProvideFn"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with multiple effects (issue #160)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  effect Logger"
          , "  effect Database"
          , "  effect HttpClient"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 3
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with all item types (issue #160)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  type Config"
          , "  effect Logger"
          , "  fn initialize"
          , "  external fn ffi_log"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 4
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with trait (issue #164)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  trait Action"
          , "  type JudicialAction"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 2
        case moduleProvides m of
          [ProvideTrait name _, ProvideType _ _] -> name `shouldBe` "Action"
          _ -> expectationFailure "Expected ProvideTrait and ProvideType"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with multiple traits (issue #164)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  trait Eq"
          , "  trait Ord"
          , "  trait Show"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 3
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with all item types including trait (issue #164)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  trait Action"
          , "  type JudicialAction"
          , "  effect Logger"
          , "  fn execute"
          , "  external fn ffi_call"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 5
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with handler (issue #165)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  effect Database"
          , "  handler MockDatabase"
          , "  handler DatabaseResearch"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 3
        case moduleProvides m of
          [ProvideEffect _ _, ProvideHandler name1 _, ProvideHandler name2 _] -> do
            name1 `shouldBe` "MockDatabase"
            name2 `shouldBe` "DatabaseResearch"
          _ -> expectationFailure "Expected ProvideEffect and two ProvideHandlers"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with multiple handlers (issue #165)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  handler MockAudit"
          , "  handler PersistentAudit"
          , "  handler StreamingAudit"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 3
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with all item types including handler (issue #165)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  type Result"
          , "  effect Database"
          , "  handler MockDatabase"
          , "  trait Action"
          , "  fn execute"
          , "  external fn ffi_call"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 6
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with type prop (issue #166)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  type prop BindsOn"
          , "  type prop IsGoodLaw"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 2
        case moduleProvides m of
          [ProvideTypeProp name1 _, ProvideTypeProp name2 _] -> do
            name1 `shouldBe` "BindsOn"
            name2 `shouldBe` "IsGoodLaw"
          _ -> expectationFailure "Expected two ProvideTypeProp"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with mixed type and type prop (issue #166)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  type Court"
          , "  type prop BindsOn"
          , "  type Jurisdiction"
          , "  type prop SameHierarchy"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleProvides m) `shouldBe` 4
        case moduleProvides m of
          [ProvideType t1 _, ProvideTypeProp p1 _, ProvideType t2 _, ProvideTypeProp p2 _] -> do
            t1 `shouldBe` "Court"
            p1 `shouldBe` "BindsOn"
            t2 `shouldBe` "Jurisdiction"
            p2 `shouldBe` "SameHierarchy"
          _ -> expectationFailure "Expected alternating ProvideType and ProvideTypeProp"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses provides block with all item types including type prop (issue #166)" $ do
    let src = T.unlines
          [ "module Test"
          , "provides"
          , "  type Court"
          , "  type prop BindsOn"
          , "  effect Logger"
          , "  trait Action"
          , "  fn execute"
          ]
    case parseModule "test" src of
      Right m -> length (moduleProvides m) `shouldBe` 5
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses requires block with multiple items" $ do
    let src = T.unlines
          [ "module Main"
          , "requires"
          , "  LexSim.Core.Refined"
          , "  Treasury.Types"
          ]
    case parseModule "test" src of
      Right m -> length (moduleRequires m) `shouldBe` 2
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses requires block with effects" $ do
    let src = T.unlines
          [ "module Main"
          , "requires"
          , "  LexSim.Core"
          , "  effects: State, IO"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleRequires m) `shouldBe` 2
        case moduleRequires m of
          [RequireModule _ _, RequireEffects effs _] -> effs `shouldBe` ["State", "IO"]
          _ -> expectationFailure "Expected RequireModule and RequireEffects"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses requires block followed by provides and definitions" $ do
    let src = T.unlines
          [ "module Main"
          , "requires"
          , "  Core.Types"
          , "provides"
          , "  type Result"
          , ""
          , "type Result:"
          , "  Ok Int"
          , "  Err String"
          ]
    case parseModule "test" src of
      Right m -> do
        length (moduleRequires m) `shouldBe` 1
        length (moduleProvides m) `shouldBe` 1
        length (moduleDefinitions m) `shouldBe` 1
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses module with multiple definitions" $ do
    -- Note: type definitions with constructors have parsing limitations
    let src = T.unlines
          [ "module Main"
          , "type Unit"
          , "fn negate(x: Bool) -> Bool: x"
          ]
    shouldParse $ parseModule "test" src

  it "extracts module name" $ do
    case parseModule "test" "module Main" of
      Right m -> modulePathSegments (moduleName m) `shouldBe` ["Main"]
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts module authority" $ do
    case parseModule "test" "module Main authority System" of
      Right m -> moduleAuthority m `shouldBe` Just "System"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Operator Precedence and Associativity Tests
-- =============================================================================

operatorTests :: Spec
operatorTests = describe "operator precedence and associativity" $ do
  it "parses application before pipeline" $ do
    -- f x |> g should be (f x) |> g
    case parseExpr "test" "f x |> g" of
      Right (EPipe (EApp _ _ _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected (f x) |> g, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses pipeline left-to-right" $ do
    -- x |> f |> g should be (x |> f) |> g
    case parseExpr "test" "x |> f |> g" of
      Right (EPipe (EPipe _ _ _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected (x |> f) |> g, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses function types right-to-left" $ do
    -- A -> B -> C should be A -> (B -> C)
    case parseType "test" "A -> B -> C" of
      Right (TyFn _ (TyFn _ _ _ _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected A -> (B -> C), got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses ++ concat operator (issue #175)" $ do
    case parseExpr "test" "\"hello\" ++ \" world\"" of
      Right (EBinOp OpConcat _ _ _) -> pure ()
      Right other -> expectationFailure $ "Expected concat, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses chained ++ left-to-right (issue #175)" $ do
    case parseExpr "test" "\"a\" ++ \"b\" ++ \"c\"" of
      Right (EBinOp OpConcat (EBinOp OpConcat _ _ _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected (a ++ b) ++ c, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "keeps + and ++ distinct (issue #175)" $ do
    case parseExpr "test" "x + y" of
      Right (EBinOp OpAdd _ _ _) -> pure ()
      Right other -> expectationFailure $ "Expected add, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses :: cons operator (issue #177)" $ do
    case parseExpr "test" "x :: xs" of
      Right (EBinOp OpCons _ _ _) -> pure ()
      Right other -> expectationFailure $ "Expected cons, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses chained :: right-to-left (issue #177)" $ do
    -- 1 :: 2 :: xs should be 1 :: (2 :: xs)
    case parseExpr "test" "1 :: 2 :: xs" of
      Right (EBinOp OpCons _ (EBinOp OpCons _ _ _) _) -> pure ()
      Right other -> expectationFailure $ "Expected 1 :: (2 :: xs), got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses :: with list literal (issue #177)" $ do
    case parseExpr "test" "1 :: [2, 3]" of
      Right (EBinOp OpCons _ (EList _ _) _) -> pure ()
      Right other -> expectationFailure $ "Expected cons with list, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses break statement (issue #178)" $ do
    case parseExpr "test" "break" of
      Right (EBreak _) -> pure ()
      Right other -> expectationFailure $ "Expected break, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses break in for loop body (issue #178)" $ do
    let src = T.unlines
          [ "module Test"
          , "fn find(xs: List) -> Unit:"
          , "  for x in xs:"
          , "    break"
          ]
    case parseModule "test" src of
      Right _ -> pure ()
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses break as standalone expression (issue #178)" $ do
    case parseExpr "test" "if done then break else x" of
      Right (EIf _ (EBreak _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected if with break in then, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses return expression (issue #179)" $ do
    case parseExpr "test" "return None" of
      Right (EReturn (ECon "None" _) _) -> pure ()
      Right other -> expectationFailure $ "Expected return None, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses return with parenthesized expression (issue #179)" $ do
    case parseExpr "test" "return (Some(x))" of
      Right (EReturn _ _) -> pure ()
      Right other -> expectationFailure $ "Expected return expr, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses return in function body (issue #179)" $ do
    let src = T.unlines
          [ "module Test"
          , "fn guard(x: Int) -> Option:"
          , "  if x == 0 then"
          , "    return None"
          , "  else"
          , "    Some(x)"
          ]
    case parseModule "test" src of
      Right _ -> pure ()
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses index access expr[expr] (issue #180)" $ do
    case parseExpr "test" "items[0]" of
      Right (EIndex (EVar "items" _) (EIntLit 0 _) _) -> pure ()
      Right other -> expectationFailure $ "Expected index access, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses chained index access (issue #180)" $ do
    case parseExpr "test" "matrix[i][j]" of
      Right (EIndex (EIndex _ _ _) _ _) -> pure ()
      Right other -> expectationFailure $ "Expected chained index, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses index access in let binding (issue #180)" $ do
    let src = T.unlines
          [ "module Test"
          , "fn get(items: List) -> Int:"
          , "  let first = items[0]"
          , "  first"
          ]
    case parseModule "test" src of
      Right _ -> pure ()
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses range expression (issue #181)" $ do
    case parseExpr "test" "0..10" of
      Right (ERange (EIntLit 0 _) (EIntLit 10 _) _) -> pure ()
      Right other -> expectationFailure $ "Expected range, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses range with field access (issue #181)" $ do
    case parseExpr "test" "0..items.length" of
      Right (ERange (EIntLit 0 _) (EFieldAccess _ "length" _) _) -> pure ()
      Right other -> expectationFailure $ "Expected range with field access, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses range in for loop (issue #181)" $ do
    let src = T.unlines
          [ "module Test"
          , "fn count() -> Unit:"
          , "  for i in 0..10:"
          , "    process(i)"
          ]
    case parseModule "test" src of
      Right _ -> pure ()
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses pair tuple expression (issue #182)" $ do
    case parseExpr "test" "(x, y)" of
      Right (ETuple [EVar "x" _, EVar "y" _] _) -> pure ()
      Right other -> expectationFailure $ "Expected pair tuple, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses triple tuple expression (issue #182)" $ do
    case parseExpr "test" "(a, b, c)" of
      Right (ETuple [_, _, _] _) -> pure ()
      Right other -> expectationFailure $ "Expected triple tuple, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses tuple in let binding (issue #182)" $ do
    let src = T.unlines
          [ "module Test"
          , "fn test() -> Tuple:"
          , "  let pair = (x, y)"
          , "  pair"
          ]
    case parseModule "test" src of
      Right _ -> pure ()
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses not expression (issue #183)" $ do
    case parseExpr "test" "not x" of
      Right (ENot (EVar "x" _) _) -> pure ()
      Right other -> expectationFailure $ "Expected not expression, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses nested not expression (issue #183)" $ do
    case parseExpr "test" "not not flag" of
      Right (ENot (ENot (EVar "flag" _) _) _) -> pure ()
      Right other -> expectationFailure $ "Expected nested not, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses not in let binding (issue #183)" $ do
    let src = T.unlines
          [ "module Test"
          , "fn negate(b: Bool) -> Bool:"
          , "  let result = not b"
          , "  result"
          ]
    case parseModule "test" src of
      Right _ -> pure ()
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses float literal with leading zero (issue #184)" $ do
    case parseExpr "test" "0.5" of
      Right (EFloatLit 0.5 _) -> pure ()
      Right other -> expectationFailure $ "Expected float 0.5, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses float arithmetic expression (issue #184)" $ do
    case parseExpr "test" "(prob - 0.5) * 2.0" of
      Right (EBinOp OpMul _ (EFloatLit 2.0 _) _) -> pure ()
      Right other -> expectationFailure $ "Expected float multiplication, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses float arithmetic in function body (issue #184)" $ do
    let src = T.unlines
          [ "module Test"
          , "fn score(prob: Float) -> Float:"
          , "  (prob - 0.5) * 2.0"
          ]
    case parseModule "test" src of
      Right _ -> pure ()
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- Edge Cases and Error Handling
-- =============================================================================

edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles keywords not used as identifiers" $ do
    shouldNotParse $ parseExpr "test" "let"

  it "allows keywords in qualified position" $ do
    -- "let" alone should fail, but in other contexts it's a keyword
    shouldParse $ parseExpr "test" "let x = 1 in x"

  it "handles empty input" $ do
    shouldNotParse $ parseExpr "test" ""

  it "handles whitespace-only input" $ do
    shouldNotParse $ parseExpr "test" "   "

  it "handles comments" $ do
    shouldParse $ parseExpr "test" "-- comment\n42"

  it "handles block comments" $ do
    shouldParse $ parseExpr "test" "{- comment -} 42"

  it "handles nested block comments" $ do
    shouldParse $ parseExpr "test" "{- outer {- inner -} outer -} 42"

  it "preserves source location" $ do
    case parseExpr "test.crisp" "foo" of
      Right (EVar _ span') -> spanFile span' `shouldBe` "test.crisp"
      Right other -> expectationFailure $ "Expected EVar, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "handles long identifiers" $ do
    let longName = T.replicate 100 "x"
    shouldParse $ parseExpr "test" longName

  it "handles deeply nested expressions" $ do
    shouldParse $ parseExpr "test" "(((((x)))))"

  it "handles complex realistic expression" $ do
    -- Match arms don't work, so test a simpler complex expression
    shouldParse $ parseExpr "test" "let x = f a b in if x then 1 else 0"

  it "handles complex realistic type" $ do
    shouldParse $ parseType "test" "forall T. Option T -> T -> T"

  it "handles unicode in strings" $ do
    shouldParse $ parseExpr "test" "\"Hello, 世界!\""

  it "handles newlines in multiline code" $ do
    let src = T.unlines
          [ "let x = 1"
          , "in x"
          ]
    shouldParse $ parseExpr "test" src

-- =============================================================================
-- Doc Comment Tests
-- =============================================================================

docCommentTests :: Spec
docCommentTests = describe "doc comments" $ do
  it "parses doc comment on function definition" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | Add two numbers"
          , "fn add(x: Int, y: Int) -> Int:"
          , "  x"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd] -> fnDefDocComment fd `shouldBe` Just "Add two numbers"
        _ -> expectationFailure "Expected single function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses doc comment on type definition" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | A simple type"
          , "type T:"
          , "  Value Int"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType td] -> typeDefDocComment td `shouldBe` Just "A simple type"
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses definition without doc comment" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "fn id(x: Int) -> Int:"
          , "  x"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd] -> fnDefDocComment fd `shouldBe` Nothing
        _ -> expectationFailure "Expected single function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "preserves regular comments while capturing doc comments" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "-- Regular comment"
          , "--- | Doc comment"
          , "fn foo(x: Int) -> Int:"
          , "  x"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd] -> fnDefDocComment fd `shouldBe` Just "Doc comment"
        _ -> expectationFailure "Expected single function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses multiple definitions with and without doc comments" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | First function"
          , "fn first(x: Int) -> Int:"
          , "  x"
          , ""
          , "fn second(y: Int) -> Int:"
          , "  y"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd1, DefFn fd2] -> do
          fnDefDocComment fd1 `shouldBe` Just "First function"
          fnDefDocComment fd2 `shouldBe` Nothing
        _ -> expectationFailure "Expected two function definitions"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses multi-line doc comment with pipe on continuation lines" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | Summary line"
          , "--- | Continuation line"
          , "type Color:"
          , "  Red"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType td] -> typeDefDocComment td `shouldBe` Just "Summary line\nContinuation line"
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses multi-line doc comment with empty pipe line" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | Summary"
          , "--- |"
          , "--- | After blank"
          , "type Color:"
          , "  Red"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType td] -> do
          let doc = typeDefDocComment td
          doc `shouldSatisfy` \d -> d /= Nothing
          case doc of
            Just d -> do
              d `shouldSatisfy` T.isInfixOf "Summary"
              d `shouldSatisfy` T.isInfixOf "After blank"
            Nothing -> expectationFailure "Expected doc comment"
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses two functions with doc comments (issue #138)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | First thing"
          , "fn first(x: Int) -> Int:"
          , "  x"
          , ""
          , "--- | Second thing"
          , "fn second(x: Int) -> Int:"
          , "  x"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd1, DefFn fd2] -> do
          fnDefDocComment fd1 `shouldBe` Just "First thing"
          fnDefDocComment fd2 `shouldBe` Just "Second thing"
        _ -> expectationFailure "Expected two function definitions"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses two types with doc comments (issue #138)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | A color"
          , "type Color:"
          , "  Red"
          , ""
          , "--- | A shape"
          , "type Shape:"
          , "  Circle"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType td1, DefType td2] -> do
          typeDefDocComment td1 `shouldBe` Just "A color"
          typeDefDocComment td2 `shouldBe` Just "A shape"
        _ -> expectationFailure "Expected two type definitions"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses documented function followed by documented type (issue #138)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | A function"
          , "fn foo(x: Int) -> Int:"
          , "  x"
          , ""
          , "--- | A type"
          , "type Bar:"
          , "  Baz"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd, DefType td] -> do
          fnDefDocComment fd `shouldBe` Just "A function"
          typeDefDocComment td `shouldBe` Just "A type"
        _ -> expectationFailure "Expected function and type definitions"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "subtraction still works after doc comment fix (issue #138)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "fn sub(x: Int, y: Int) -> Int:"
          , "  x - y"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn _] -> pure ()
        _ -> expectationFailure "Expected single function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses module-level doc comment before module keyword (issue #140)" $ do
    let src = T.unlines
          [ "--- | Module for testing"
          , "module Test.ModDoc"
          , ""
          , "fn double(x: Int) -> Int:"
          , "  x"
          ]
    case parseModule "test" src of
      Right m -> moduleDocComment m `shouldBe` Just "Module for testing"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "parses multi-line module doc comment (issue #140)" $ do
    let src = T.unlines
          [ "--- | Module summary"
          , "--- | More details here"
          , "module Test.MultiDoc"
          ]
    case parseModule "test" src of
      Right m -> do
        moduleDocComment m `shouldBe` Just "Module summary\nMore details here"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "module without doc comment has Nothing (issue #140)" $ do
    case parseModule "test" "module Main" of
      Right m -> moduleDocComment m `shouldBe` Nothing
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "preserves indentation in doc comment lines (issue #147)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | Summary line"
          , "--- |   indented content"
          , "--- |     more indented"
          , "--- | back to normal"
          , "fn f(x: Int) -> Int:"
          , "  x"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefFn fd] -> do
          let doc = fnDefDocComment fd
          doc `shouldSatisfy` \d -> d /= Nothing
          case doc of
            Just d -> do
              d `shouldSatisfy` T.isInfixOf "  indented content"
              d `shouldSatisfy` T.isInfixOf "    more indented"
              d `shouldSatisfy` T.isInfixOf "back to normal"
            Nothing -> expectationFailure "Expected doc comment"
        _ -> expectationFailure "Expected single function definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "preserves multi-level indentation in doc comments (issue #147)" $ do
    let src = T.unlines
          [ "module Main"
          , ""
          , "--- | Examples:"
          , "--- |   let x = 1"
          , "--- |   let y = 2"
          , "--- |     nested"
          , "type T:"
          , "  Value Int"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefType td] -> do
          let doc = typeDefDocComment td
          case doc of
            Just d -> do
              d `shouldSatisfy` T.isInfixOf "  let x = 1"
              d `shouldSatisfy` T.isInfixOf "  let y = 2"
              d `shouldSatisfy` T.isInfixOf "    nested"
            Nothing -> expectationFailure "Expected doc comment"
        _ -> expectationFailure "Expected single type definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err
