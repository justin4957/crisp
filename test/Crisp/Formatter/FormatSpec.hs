{-# LANGUAGE OverloadedStrings #-}

module Crisp.Formatter.FormatSpec (spec) where

import Test.Hspec
import Crisp.Formatter.Format
import Crisp.Parser.Parser (parseModule)
import Crisp.Syntax.Surface
  ( Module(..)
  )
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "Crisp.Formatter.Format" $ do
  describe "Format Options" $ do
    it "has sensible defaults" $ do
      optIndentWidth defaultFormatOptions `shouldBe` 2
      optMaxLineWidth defaultFormatOptions `shouldBe` 80
      optTrailingNewline defaultFormatOptions `shouldBe` True
      optAlignArrows defaultFormatOptions `shouldBe` True

  describe "Expression Formatting" $ do
    describe "literals" $ do
      it "formats integer literals" $ do
        formatExpr defaultFormatOptions "42" `shouldBe` Right "42"

      it "formats float literals" $ do
        formatExpr defaultFormatOptions "3.14" `shouldBe` Right "3.14"

      it "formats string literals" $ do
        formatExpr defaultFormatOptions "\"hello\"" `shouldBe` Right "\"hello\""

      it "formats unit literal" $ do
        formatExpr defaultFormatOptions "()" `shouldBe` Right "()"

    describe "variables and constructors" $ do
      it "formats variable" $ do
        formatExpr defaultFormatOptions "foo" `shouldBe` Right "foo"

      it "formats constructor" $ do
        formatExpr defaultFormatOptions "Some" `shouldBe` Right "Some"

    describe "application" $ do
      it "formats simple application" $ do
        formatExpr defaultFormatOptions "f x" `shouldBe` Right "f x"

      it "formats multi-arg application" $ do
        formatExpr defaultFormatOptions "f x y z" `shouldBe` Right "f x y z"

    describe "let expressions" $ do
      it "formats simple let with layout-based style" $ do
        formatExpr defaultFormatOptions "let x = 1 in x" `shouldBe` Right "let x = 1\nx"

      it "formats let with type annotation using layout-based style" $ do
        formatExpr defaultFormatOptions "let x: Int = 1 in x" `shouldBe` Right "let x: Int = 1\nx"

      it "formats nested let chain with each binding on its own line" $ do
        formatExpr defaultFormatOptions "let x = 1 in let y = 2 in x"
          `shouldBe` Right "let x = 1\nlet y = 2\nx"

    describe "if expressions" $ do
      it "formats if-then-else" $ do
        formatExpr defaultFormatOptions "if True then 1 else 2"
          `shouldBe` Right "if True then 1 else 2"

    describe "lambda expressions" $ do
      it "formats lambda" $ do
        formatExpr defaultFormatOptions "\\x: Int. x" `shouldBe` Right "\\x: Int. x"

    describe "pipeline operator" $ do
      it "formats pipeline" $ do
        formatExpr defaultFormatOptions "x |> f" `shouldBe` Right "x |> f"

    describe "lazy and force" $ do
      it "formats lazy" $ do
        formatExpr defaultFormatOptions "lazy 42" `shouldBe` Right "lazy 42"

      it "formats force" $ do
        formatExpr defaultFormatOptions "force x" `shouldBe` Right "force x"

    describe "perform" $ do
      it "formats perform expression" $ do
        formatExpr defaultFormatOptions "perform State.get"
          `shouldBe` Right "perform State.get"

      it "formats perform with arguments" $ do
        formatExpr defaultFormatOptions "perform State.put 42"
          `shouldBe` Right "perform State.put 42"

  describe "Module Formatting" $ do
    it "formats minimal module" $ do
      let src = "module Test"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isPrefixOf "module Test"
        Left _ -> expectationFailure "Expected Right"

    it "formats module with function definition" $ do
      let src = "module Test\n\nfn id(x: Int) -> Int:\n  x"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight

    it "formats module with type definition" $ do
      let src = "module Test\n\ntype Bool:\n  True\n  False"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight

    it "formats module with effect definition" $ do
      let src = "module Test\n\neffect State:\n  get: Unit -> Int\n  put: Int -> Unit"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight

    it "formats module with provides block" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  type MyType"
            , "  fn myFn"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "provides type MyType"
          formatted `shouldSatisfy` T.isInfixOf "provides fn myFn"
        Left _ -> expectationFailure "Expected Right"

    it "formats module with provides block and typed fns" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  type Date"
            , "  fn is_valid: Date -> Bool"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "provides type Date"
          formatted `shouldSatisfy` T.isInfixOf "provides fn is_valid: Date -> Bool"
        Left _ -> expectationFailure "Expected Right"

    it "formats module with requires block" $ do
      let src = T.unlines
            [ "module Test"
            , "requires"
            , "  Core.Types"
            , "  Utils.Helpers"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "requires Core.Types"
          formatted `shouldSatisfy` T.isInfixOf "requires Utils.Helpers"
        Left _ -> expectationFailure "Expected Right"

    it "formats module with provides block followed by definitions" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  type Foo"
            , ""
            , "type Foo:"
            , "  Foo Int"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "provides type Foo"
          formatted `shouldSatisfy` T.isInfixOf "type Foo"
        Left _ -> expectationFailure "Expected Right"

    it "formats module with external fn in provides block (issue #156)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  external fn ffi_http_get"
            , "  fn some_function"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "provides external fn ffi_http_get"
          formatted `shouldSatisfy` T.isInfixOf "provides fn some_function"
        Left _ -> expectationFailure "Expected Right"

    it "formats external fn with type annotation in provides block (issue #156)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  external fn query: String -> String"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "provides external fn query: String -> String"
        Left _ -> expectationFailure "Expected Right"

    it "formats external fn in provides block idempotently (issue #156)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  external fn ffi_connect"
            , "  external fn ffi_query: String -> Result"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats module with effect in provides block (issue #160)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  effect HttpClient"
            , "  fn some_function"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "provides effect HttpClient"
          formatted `shouldSatisfy` T.isInfixOf "provides fn some_function"
        Left _ -> expectationFailure "Expected Right"

    it "formats effect in provides block idempotently (issue #160)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  effect Logger"
            , "  effect Database"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats module with trait in provides block (issue #164)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  trait Action"
            , "  type JudicialAction"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "provides trait Action"
          formatted `shouldSatisfy` T.isInfixOf "provides type JudicialAction"
        Left _ -> expectationFailure "Expected Right"

    it "formats trait in provides block idempotently (issue #164)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  trait Eq"
            , "  trait Ord"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats module with handler in provides block (issue #165)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  effect Database"
            , "  handler MockDatabase"
            , "  handler DatabaseResearch"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "provides effect Database"
          formatted `shouldSatisfy` T.isInfixOf "provides handler MockDatabase"
          formatted `shouldSatisfy` T.isInfixOf "provides handler DatabaseResearch"
        Left _ -> expectationFailure "Expected Right"

    it "formats handler in provides block idempotently (issue #165)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  handler MockAudit"
            , "  handler PersistentAudit"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats module with type prop in provides block (issue #166)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  type prop BindsOn"
            , "  type prop IsGoodLaw"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "provides type prop BindsOn"
          formatted `shouldSatisfy` T.isInfixOf "provides type prop IsGoodLaw"
        Left _ -> expectationFailure "Expected Right"

    it "formats type prop in provides block idempotently (issue #166)" $ do
      let src = T.unlines
            [ "module Test"
            , "provides"
            , "  type Court"
            , "  type prop BindsOn"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats type with deriving clause (issue #167)" $ do
      let src = T.unlines
            [ "module Test"
            , "type ConfidenceLevel deriving (Eq, Ord):"
            , "  VeryHigh"
            , "  High"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "deriving (Eq, Ord)"
          formatted `shouldSatisfy` T.isInfixOf "VeryHigh"
        Left _ -> expectationFailure "Expected Right"

    it "formats type with deriving clause idempotently (issue #167)" $ do
      let src = T.unlines
            [ "module Test"
            , "type Status deriving Eq:"
            , "  Active"
            , "  Inactive"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats type with named field constructors" $ do
      let src = T.unlines
            [ "module Test"
            , "type Point:"
            , "  Origin"
            , "  Cartesian(x: Int, y: Int)"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "Origin"
          formatted `shouldSatisfy` T.isInfixOf "Cartesian"
          formatted `shouldSatisfy` T.isInfixOf "x: Int"
        Left _ -> expectationFailure "Expected Right"

    it "formats type with positional field constructors" $ do
      let src = T.unlines
            [ "module Test"
            , "type Pair:"
            , "  MkPair(Int, String)"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "MkPair"
          formatted `shouldSatisfy` T.isInfixOf "Int"
          formatted `shouldSatisfy` T.isInfixOf "String"
        Left _ -> expectationFailure "Expected Right"

    it "formats type with mixed constructors" $ do
      let src = T.unlines
            [ "module Test"
            , "type Timezone:"
            , "  UTC"
            , "  Local"
            , "  Offset(hours: Int, minutes: Int)"
            , "  Named(name: String)"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "UTC"
          formatted `shouldSatisfy` T.isInfixOf "Local"
          formatted `shouldSatisfy` T.isInfixOf "Offset"
          formatted `shouldSatisfy` T.isInfixOf "Named"
        Left _ -> expectationFailure "Expected Right"

    it "formats type alias with where refinement" $ do
      let src = "module Test type PositiveInt = Int where { self > 0 }"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "PositiveInt"
          formatted `shouldSatisfy` T.isInfixOf "Int"
          formatted `shouldSatisfy` T.isInfixOf "self > 0"
        Left _ -> expectationFailure "Expected Right"

    it "formats type alias with where and field access" $ do
      let src = "module Test type ValidRange = Range where { self.start <= self.end }"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "ValidRange"
          formatted `shouldSatisfy` T.isInfixOf "self.start"
          formatted `shouldSatisfy` T.isInfixOf "self.end"
        Left _ -> expectationFailure "Expected Right"

    it "formats refinement with match expression" $ do
      let src = T.unlines
            [ "module Test"
            , "type NonEmptyOption A = Option A where {"
            , "  match self"
            , "    Some(_) -> True"
            , "    None -> False"
            , "}"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "match self"
          formatted `shouldSatisfy` T.isInfixOf "Some"
          formatted `shouldSatisfy` T.isInfixOf "None"
        Left _ -> expectationFailure "Expected Right"

    it "formats refinement with if expression" $ do
      let src = "module Test type Bounded = Int where { if self >= 0 then True else False }"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "if"
          formatted `shouldSatisfy` T.isInfixOf "then"
          formatted `shouldSatisfy` T.isInfixOf "else"
        Left _ -> expectationFailure "Expected Right"

    it "formats type alias with where field constraint (issue #168)" $ do
      let src = "module Test type TrialCourt = Court where level: TrialCourt"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "where level: TrialCourt"
        Left _ -> expectationFailure "Expected Right"

    it "formats type alias with where field constraint idempotently (issue #168)" $ do
      let src = "module Test type ActiveUser = User where status: Active"
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

  describe "Idempotence" $ do
    it "formatting twice produces same result for simple module" $ do
      let src = "module Test\n\nfn id(x: Int) -> Int:\n  x"
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formatting twice produces same result for expressions" $ do
      let src = "let x = 1\nx"
      case formatExpr defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatExpr defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formatting twice produces same result for module with type" $ do
      let src = T.unlines
            [ "module Treasury.Audit"
            , ""
            , "type Amount:"
            , "  Amount Int"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

  describe "Definition Formatting" $ do
    describe "type definitions" $ do
      it "formats simple type" $ do
        let src = "module Test\n\ntype Unit:\n  Unit"
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted ->
            formatted `shouldSatisfy` T.isInfixOf "type Unit"

      it "formats type with parameters" $ do
        let src = "module Test\n\ntype Option A:\n  None\n  Some A"
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted ->
            formatted `shouldSatisfy` T.isInfixOf "type Option A"

    describe "function definitions" $ do
      it "formats simple function" $ do
        let src = "module Test\n\nfn const(x: Int, y: Int) -> Int:\n  x"
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted ->
            formatted `shouldSatisfy` T.isInfixOf "fn const"

      it "formats function with effects" $ do
        let src = "module Test\n\nfn getState() -> Int ! State:\n  perform State.get"
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted ->
            formatted `shouldSatisfy` T.isInfixOf "! State"

    describe "effect definitions" $ do
      it "formats effect" $ do
        let src = "module Test\n\neffect Reader:\n  ask: Unit -> Int"
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted ->
            formatted `shouldSatisfy` T.isInfixOf "effect Reader"

      it "formats effect with documented operations (issue #154)" $ do
        let src = T.unlines
              [ "module Test"
              , ""
              , "effect HttpClient:"
              , "  --- | Make a GET request"
              , "  http_get: String -> String"
              ]
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted -> do
            formatted `shouldSatisfy` T.isInfixOf "--- | Make a GET request"
            formatted `shouldSatisfy` T.isInfixOf "http_get: String -> String"

      it "effect with documented operations is idempotent (issue #154)" $ do
        let src = T.unlines
              [ "module Test"
              , ""
              , "effect Http:"
              , "  --- | GET request"
              , "  get: String -> String"
              , "  --- | POST request"
              , "  post: String -> String -> String"
              ]
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted1 ->
            case formatSource defaultFormatOptions formatted1 of
              Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
              Right formatted2 -> formatted1 `shouldBe` formatted2

      it "formats function-style effect operation (issue #162)" $ do
        let src = T.unlines
              [ "module Test"
              , ""
              , "effect HttpClient:"
              , "  http_get(url: String) -> String"
              ]
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted -> do
            -- Function-style gets converted to type signature
            formatted `shouldSatisfy` T.isInfixOf "http_get: String -> String"

      it "function-style effect operation is idempotent (issue #162)" $ do
        let src = T.unlines
              [ "module Test"
              , ""
              , "effect HttpClient:"
              , "  http_post(url: String, body: String) -> String"
              ]
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted1 ->
            case formatSource defaultFormatOptions formatted1 of
              Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
              Right formatted2 -> formatted1 `shouldBe` formatted2

    describe "external function definitions" $ do
      it "formats external fn" $ do
        let src = "module Test\n\nexternal fn log(msg: String) -> Unit = (\"console\", \"log\")"
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted ->
            formatted `shouldSatisfy` T.isInfixOf "external fn log"

  describe "Trailing Newline Option" $ do
    it "adds trailing newline by default" $ do
      let src = "module Test"
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> T.isSuffixOf "\n" formatted `shouldBe` True
        Left _ -> expectationFailure "Expected Right"

    it "can disable trailing newline" $ do
      let opts = defaultFormatOptions { optTrailingNewline = False }
          src = "module Test"
          result = formatSource opts src
      case result of
        Right formatted -> T.isSuffixOf "\n" formatted `shouldBe` False
        Left _ -> expectationFailure "Expected Right"

  describe "Error Handling" $ do
    it "returns error for invalid syntax" $ do
      let src = "this is not valid crisp"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isLeft

    it "returns error for incomplete expression" $ do
      let result = formatExpr defaultFormatOptions "let x ="
      result `shouldSatisfy` isLeft

  describe "prettyModule function" $ do
    it "produces valid output for parsed module" $ do
      let src = "module Test\n\nfn test() -> Int:\n  42"
      case parseModule "<test>" src of
        Left _ -> expectationFailure "Failed to parse"
        Right mod' -> do
          let formatted = formatModule defaultFormatOptions mod'
          formatted `shouldSatisfy` T.isInfixOf "module Test"
          formatted `shouldSatisfy` T.isInfixOf "fn test"

  describe "prettyExpr function" $ do
    it "pretty prints constructors" $ do
      let result = formatExpr defaultFormatOptions "None"
      result `shouldBe` Right "None"

    it "pretty prints nested applications" $ do
      let result = formatExpr defaultFormatOptions "f (g x)"
      result `shouldBe` Right "f (g x)"

    it "pretty prints binary operations" $ do
      let result = formatExpr defaultFormatOptions "1"
      result `shouldBe` Right "1"

    it "pretty prints addition" $ do
      let result = formatExpr defaultFormatOptions "1 + 2"
      result `shouldBe` Right "1 + 2"

    it "pretty prints comparison operators" $ do
      let result = formatExpr defaultFormatOptions "x >= 0"
      result `shouldBe` Right "x >= 0"

    it "pretty prints logical operators" $ do
      let result = formatExpr defaultFormatOptions "a && b"
      result `shouldBe` Right "a && b"

    it "pretty prints field access" $ do
      let result = formatExpr defaultFormatOptions "x.field"
      result `shouldBe` Right "x.field"

    it "pretty prints chained field access" $ do
      let result = formatExpr defaultFormatOptions "x.a.b"
      result `shouldBe` Right "x.a.b"

  describe "prettyType function" $ do
    it "formats function types in module" $ do
      let src = "module Test\n\nfn apply(f: Int -> Bool, x: Int) -> Bool:\n  f x"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "->"
        Left _ -> expectationFailure "Expected Right"

    it "formats type application in module" $ do
      let src = "module Test\n\nfn test(x: List Int) -> Int:\n  0"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "List Int"
        Left _ -> expectationFailure "Expected Right"

  describe "prettyPattern function" $ do
    it "formats simple variable patterns" $ do
      -- Test via lambda parameter which includes pattern
      let result = formatExpr defaultFormatOptions "\\x: Int. x"
      result `shouldBe` Right "\\x: Int. x"

    it "formats let patterns" $ do
      let result = formatExpr defaultFormatOptions "let x = 1 in x"
      result `shouldBe` Right "let x = 1\nx"

  describe "Doc Comment Preservation" $ do
    it "preserves doc comment on function definition" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "--- | Add two numbers"
            , "fn add(x: Int, y: Int) -> Int:"
            , "  x"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "--- | Add two numbers"
        Left _ -> expectationFailure "Expected Right"

    it "preserves doc comment on type definition" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "--- | A simple type"
            , "type T:"
            , "  Value Int"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "--- | A simple type"
        Left _ -> expectationFailure "Expected Right"

    it "preserves doc comments on multiple definitions" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "--- | A type"
            , "type T:"
            , "  Value Int"
            , ""
            , "--- | Get value"
            , "fn get_value(t: T) -> Int:"
            , "  0"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "--- | A type"
          formatted `shouldSatisfy` T.isInfixOf "--- | Get value"
        Left _ -> expectationFailure "Expected Right"

    it "doc comment appears before definition in output" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "--- | My function"
            , "fn foo(x: Int) -> Int:"
            , "  x"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          let docPos = T.breakOn "--- | My function" formatted
              fnPos = T.breakOn "fn foo" formatted
          -- Doc comment line should come before function definition
          T.length (fst docPos) `shouldSatisfy` (< T.length (fst fnPos))
        Left _ -> expectationFailure "Expected Right"

    it "formats multi-line doc comment with prefix on every line (issue #139)" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "--- | A range with start and end bounds"
            , "--- | Examples:"
            , "--- |   let r = Range(0, 10)"
            , "type Range:"
            , "  start: Int"
            , "  end: Int"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "--- | A range with start and end bounds"
          formatted `shouldSatisfy` T.isInfixOf "--- | Examples:"
          formatted `shouldSatisfy` T.isInfixOf "--- |   let r = Range(0, 10)"
        Left _ -> expectationFailure "Expected Right"

    it "multi-line doc comment formatting is idempotent (issue #139)" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "--- | Summary line"
            , "--- | Detail line"
            , "type T:"
            , "  Value Int"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats single-line doc comment unchanged (issue #139)" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "--- | Just one line"
            , "fn foo(x: Int) -> Int:"
            , "  x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "--- | Just one line"
          -- Should not have duplicate prefix lines
          let docLines = filter (T.isPrefixOf "--- |") (T.lines formatted)
          length docLines `shouldBe` 1

    it "formats module-level doc comment before module keyword (issue #140)" $ do
      let src = T.unlines
            [ "--- | Module for testing"
            , "module Test.ModDoc"
            , ""
            , "fn double(x: Int) -> Int:"
            , "  x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "--- | Module for testing"
          -- Doc comment should appear before module keyword
          let docPos = T.breakOn "--- | Module" formatted
              modPos = T.breakOn "module Test" formatted
          T.length (fst docPos) `shouldSatisfy` (< T.length (fst modPos))

    it "module-level doc comment formatting is idempotent (issue #140)" $ do
      let src = T.unlines
            [ "--- | Module docs"
            , "module Test"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "preserves indentation in doc comment continuation lines (issue #147)" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "--- | Summary line"
            , "--- |   indented content"
            , "--- |     more indented"
            , "--- | back to normal"
            , "fn f(x: Int) -> Int:"
            , "  x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "--- | Summary line"
          formatted `shouldSatisfy` T.isInfixOf "--- |   indented content"
          formatted `shouldSatisfy` T.isInfixOf "--- |     more indented"
          formatted `shouldSatisfy` T.isInfixOf "--- | back to normal"

    it "indented doc comment formatting is idempotent (issue #147)" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "--- | Examples:"
            , "--- |   let x = 1"
            , "--- |   let y = 2"
            , "fn f(x: Int) -> Int:"
            , "  x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

  describe "Match Arm Body Formatting (Issue #152)" $ do
    it "formats function application in match arm body with parentheses" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "fn check(r: Range) -> Int:"
            , "  match r"
            , "    Range(s, e) -> get_start(r)"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "get_start(r)"

    it "match arm body with application is idempotent" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "fn check(r: Range) -> Int:"
            , "  match r"
            , "    Range(s, e) -> get_start(r)"
            , ""
            , "--- | Next"
            , "fn next(x: Int) -> Int:"
            , "  x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats nested application in match arm body with parentheses" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "fn check(x: Int) -> Int:"
            , "  match x"
            , "    0 -> add(mul(x, 2), 1)"
            , "    n -> n"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "add(mul(x, 2), 1)"

    it "formats multiple args in match arm body application" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "fn check(x: Int, y: Int) -> Int:"
            , "  match x"
            , "    0 -> compute(x, y, z)"
            , "    n -> n"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "compute(x, y, z)"

-- Helper functions

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
