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

      it "formats triple-quoted string" $ do
        let src = "\"\"\"hello world\"\"\""
            result = formatExpr defaultFormatOptions src
        result `shouldSatisfy` isRight
        case result of
          Right formatted -> formatted `shouldSatisfy` T.isInfixOf "\"\"\""
          Left _ -> expectationFailure "Expected Right"

      it "formats triple-quoted multiline string" $ do
        let src = "\"\"\"\n  line 1\n  line 2\n\"\"\""
            result = formatExpr defaultFormatOptions src
        result `shouldSatisfy` isRight
        case result of
          Right formatted -> do
            formatted `shouldSatisfy` T.isInfixOf "\"\"\""
            formatted `shouldSatisfy` T.isInfixOf "line 1"
            formatted `shouldSatisfy` T.isInfixOf "line 2"
          Left _ -> expectationFailure "Expected Right"

      it "regular string formatting unchanged" $ do
        formatExpr defaultFormatOptions "\"hello\\nworld\"" `shouldBe` Right "\"hello\\nworld\""

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

    it "formats authority as field name (issue #221)" $ do
      let src = T.unlines
            [ "module Test"
            , "type AuthorityCited:"
            , "  authority: AuthorityRef"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "authority"
        Left _ -> expectationFailure "Expected Right"

    it "formats module with authority annotation (issue #221)" $ do
      let src = "module Treasury.Audit authority Treasury"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "authority Treasury"
        Left _ -> expectationFailure "Expected Right"

    it "formats authority as identifier idempotently (issue #221)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn evaluate(authority: Int) -> Int:"
            , "  authority"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats total as variable name (issue #222)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn sum_items() -> Int:"
            , "  let total = 0"
            , "  total"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "total"
        Left _ -> expectationFailure "Expected Right"

    it "formats total as field name idempotently (issue #222)" $ do
      let src = T.unlines
            [ "module Test"
            , "type AuditMetrics:"
            , "  total: Int"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats resume as parameter name (issue #265)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn call_resume(resume: Unit -> Unit) -> Unit:"
            , "  resume(())"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "resume"
        Left _ -> expectationFailure "Expected Right"

    it "formats resume as field name idempotently (issue #265)" $ do
      let src = T.unlines
            [ "module Test"
            , "type Handler:"
            , "  resume: Unit -> Unit"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

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

    it "formats function with paren type params to bracket syntax (issue #266)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn identity(A)(x: A) -> A:"
            , "  x"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          -- Should normalize to bracket syntax
          formatted `shouldSatisfy` T.isInfixOf "fn identity[A](x: A)"
        Left _ -> expectationFailure "Expected Right"

    it "formats function with multiple paren type params (issue #266)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn map_fn(A, B)(list: List(A), f: fn(A) -> B) -> List(B):"
            , "  list"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          -- Should normalize to bracket syntax
          formatted `shouldSatisfy` T.isInfixOf "fn map_fn[A, B]"
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

    -- Deriving clause position consistency tests (issue #278)
    it "formats deriving after sum type constructors (issue #278)" $ do
      let src = T.unlines
            [ "module Test"
            , "type Color:"
            , "  Red"
            , "  Green"
            , "  Blue"
            , "  deriving (Eq, Ord)"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "deriving"
          formatted `shouldSatisfy` T.isInfixOf "Red"
        Left _ -> expectationFailure "Expected Right"

    it "formats deriving after sum type idempotently (issue #278)" $ do
      let src = T.unlines
            [ "module Test"
            , "type ThreatLevel:"
            , "  Low"
            , "  Medium"
            , "  High"
            , "  deriving (Eq)"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    -- Type inheritance syntax tests (issue #279)
    it "formats type with trait implementation (issue #279)" $ do
      let src = T.unlines
            [ "module Test"
            , "type JudicialAction deriving (Eq): Action"
            , "  Ruling"
            , "  Order"
            , "  Judgment"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf ": Action"
          formatted `shouldSatisfy` T.isInfixOf "deriving"
          formatted `shouldSatisfy` T.isInfixOf "Ruling"
        Left _ -> expectationFailure "Expected Right"

    it "formats type with trait implementation idempotently (issue #279)" $ do
      let src = T.unlines
            [ "module Test"
            , "type JudicialAction deriving (Eq): Action"
            , "  Ruling"
            , "  Order"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats type implementing trait without deriving (issue #279)" $ do
      let src = T.unlines
            [ "module Test"
            , "type MyAction: Actionable"
            , "  DoSomething"
            , "  DoNothing"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf ": Actionable"
          formatted `shouldSatisfy` T.isInfixOf "DoSomething"
        Left _ -> expectationFailure "Expected Right"

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

    it "formats pure enum type with all nullary constructors (issue #170)" $ do
      let src = T.unlines
            [ "module Test"
            , "type ReferenceType:"
            , "  Citation"
            , "  Incorporation"
            , "  Exception"
            , "  Condition"
            , "  Definition"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "Citation"
          formatted `shouldSatisfy` T.isInfixOf "Incorporation"
          formatted `shouldSatisfy` T.isInfixOf "Exception"
          formatted `shouldSatisfy` T.isInfixOf "Condition"
          formatted `shouldSatisfy` T.isInfixOf "Definition"
        Left _ -> expectationFailure "Expected Right"

    it "formats enum type idempotently (issue #170)" $ do
      let src = T.unlines
            [ "module Test"
            , ""
            , "type Language:"
            , "  English"
            , "  French"
            , "  German"
            , ""
            , "type FormFieldType:"
            , "  TextField"
            , "  DateField"
            , "  NumberField"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

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

    it "formats type alias with extended with (issue #239)" $ do
      let src = T.unlines
            [ "module Test"
            , "type Extended = Base extended with:"
            , "  extra: Int"
            , "  another: String"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "extended with:"
          formatted `shouldSatisfy` T.isInfixOf "extra: Int"
          formatted `shouldSatisfy` T.isInfixOf "another: String"
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

    it "formats float literal in refinement predicate (issue #220)" $ do
      let src = "module Test type PositiveFloat = Float where { self > 0.0 }"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "self > 0.0"
        Left _ -> expectationFailure "Expected Right"

    it "formats float with decimal places in refinement (issue #220)" $ do
      let src = "module Test type SmallFloat = Float where { self < 3.14159 }"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "3.14159"
        Left _ -> expectationFailure "Expected Right"

    it "formats integer refinement unchanged with float support (issue #220)" $ do
      let src = "module Test type PositiveInt = Int where { self > 0 }"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "self > 0"
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

    it "formats OR pattern field constraint (issue #218)" $ do
      let src = "module Test type DissentOpinion = Opinion where opinion_type: Dissent | DissentInPart"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "Dissent | DissentInPart"
        Left _ -> expectationFailure "Expected Right"

    it "formats OR pattern field constraint idempotently (issue #218)" $ do
      let src = "module Test type DissentOpinion = Opinion where opinion_type: Dissent | DissentInPart"
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats triple OR pattern field constraint (issue #218)" $ do
      let src = "module Test type StrongInterp = Interpretation where strength: Strong | Definitive | Binding"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "Strong | Definitive | Binding"
        Left _ -> expectationFailure "Expected Right"

    -- Constructor-level where constraints (issue #243)
    it "formats constructor-level where constraint (issue #243)" $ do
      let src = "module Test type RedOnly = Color where Red"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          -- Constructor constraint should not have field: prefix
          formatted `shouldSatisfy` T.isInfixOf "where Red"
          formatted `shouldSatisfy` (not . T.isInfixOf ": Red")
        Left _ -> expectationFailure "Expected Right"

    it "formats constructor-level where constraint with args (issue #243)" $ do
      let src = "module Test type MandatoryBinding = BindingForce where Mandatory(_)"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          -- Formatter outputs constructor patterns with spaces: Mandatory _
          formatted `shouldSatisfy` T.isInfixOf "where Mandatory _"
        Left _ -> expectationFailure "Expected Right"

    it "formats OR constructor constraints (issue #243)" $ do
      let src = "module Test type WarmColor = Color where Red | Orange | Yellow"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "where Red | Orange | Yellow"
        Left _ -> expectationFailure "Expected Right"

    it "formats constructor-level where constraint idempotently (issue #243)" $ do
      let src = "module Test type RedOnly = Color where Red"
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats basic for loop (issue #169)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn process(items: List(Item)) -> Unit:"
            , "  for item in items:"
            , "    log item"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "for item in items:"
          formatted `shouldSatisfy` T.isInfixOf "log"
        Left _ -> expectationFailure "Expected Right"

    it "formats nested for loops (issue #169)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn analyze(ids: List(Int)) -> Unit:"
            , "  for id in ids:"
            , "    for item in items:"
            , "      process item"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "for id in ids:"
          formatted `shouldSatisfy` T.isInfixOf "for item in items:"
        Left _ -> expectationFailure "Expected Right"

    it "formats for loop idempotently (issue #169)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn process(items: List(Item)) -> Unit:"
            , "  for item in items:"
            , "    log item"
            ]
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> do
          let result2 = formatSource defaultFormatOptions formatted
          case result2 of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left _ -> expectationFailure "Second format failed"
        Left _ -> expectationFailure "First format failed"

    it "formats record construction (issue #173)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn make() -> Part:"
            , "  Part { number = 1, title = \"hello\" }"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "Part { number = 1, title = \"hello\" }"

    it "formats record construction idempotently (issue #173)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn make() -> Arg:"
            , "  Arg { id = Id { value = 1 }, name = \"test\" }"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats method call (issue #174)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn process(items: List) -> List:"
            , "  items.filter(is_valid)"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "items.filter(is_valid)"

    it "formats chained method calls idempotently (issue #174)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn process(items: List) -> Int:"
            , "  items.filter(is_valid).map(get_id).length()"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats ++ concat operator (issue #175)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn greet(name: String) -> String:"
            , "  \"Hello, \" ++ name"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "++"

    it "formats ++ concat operator idempotently (issue #175)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn greet(name: String) -> String:"
            , "  (\"Hello, \" ++ name) ++ \"!\""
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats list literal (issue #176)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn make() -> List:"
            , "  [1, 2, 3]"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "[1, 2, 3]"

    it "formats list literal idempotently (issue #176)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn make() -> List:"
            , "  [[1, 2], [3, 4]]"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats cons operator (issue #177)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn prepend(x: Int, xs: List) -> List:"
            , "  x :: xs"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "x :: xs"

    it "formats cons operator idempotently (issue #177)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn build() -> List:"
            , "  1 :: 2 :: 3 :: []"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats break statement (issue #178)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn stop() -> Unit:"
            , "  break"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "break"

    it "formats break statement idempotently (issue #178)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn find(xs: List) -> Unit:"
            , "  for x in xs:"
            , "    break"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats return expression (issue #179)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn guard(x: Int) -> Option:"
            , "  return None"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "return None"

    it "formats return expression idempotently (issue #179)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn guard(x: Int) -> Option:"
            , "  return None"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats index access (issue #180)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn first(items: List) -> Int:"
            , "  items[0]"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "items[0]"

    it "formats chained index access idempotently (issue #180)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn get(matrix: List, i: Int, j: Int) -> Int:"
            , "  matrix[i][j]"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats range expression (issue #181)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn count() -> Unit:"
            , "  for i in 0..10:"
            , "    process(i)"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "0..10"

    it "formats range expression idempotently (issue #181)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn test(items: List) -> Unit:"
            , "  for i in 0..items.length:"
            , "    process(items[i])"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats tuple expression (issue #182)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn pair(x: Int, y: Int) -> Tuple:"
            , "  (x, y)"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "(x, y)"

    it "formats tuple expression idempotently (issue #182)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn triple(a: Int, b: Int, c: Int) -> Tuple:"
            , "  (a, b, c)"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats not expression (issue #183)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn negate(b: Bool) -> Bool:"
            , "  not b"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "not b"

    it "formats not expression idempotently (issue #183)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn doubleNegate(b: Bool) -> Bool:"
            , "  not not b"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats as cast expression (issue #240)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn cast(x: Int) -> Bool:"
            , "  x as Bool"
            ]
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "x as Bool"
        Left _ -> expectationFailure "Expected Right"

    it "formats as cast expression idempotently (issue #240)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn cast(x: Int) -> Bool:"
            , "  x as Bool"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats handler with value parameter (issue #185)" $ do
      let src = T.unlines
            [ "module Test"
            , "handler LoggingReasoning(log: List) for Reasoning:"
            , "  return x -> x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "handler LoggingReasoning(log: List) for Reasoning:"

    it "formats handler with multiple value parameters idempotently (issue #185)" $ do
      let src = T.unlines
            [ "module Test"
            , "handler StateHandler(init: State, count: Int) for Effect:"
            , "  return x -> x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats qualified constructor record construction (issue #186)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn prove() -> Option:"
            , "  Some(GeographicCovers.national_covers_all { })"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "GeographicCovers.national_covers_all { }"

    it "formats qualified constructor record idempotently (issue #186)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn prove() -> Option:"
            , "  GeographicCovers.national_covers_all { x = 1 }"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats mutable assignment (issue #187)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn update(x: Int) -> Int:"
            , "  let count = 0"
            , "  count = 1"
            , "  count"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted ->
          formatted `shouldSatisfy` T.isInfixOf "count = 1"

    it "formats mutable assignment idempotently (issue #187)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn update(items: List) -> List:"
            , "  let results = items"
            , "  results = process(items)"
            , "  results"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

    it "formats float arithmetic expression (issue #184)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn score(prob: Float) -> Float:"
            , "  (prob - 0.5) * 2.0"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "0.5"
          formatted `shouldSatisfy` T.isInfixOf "2.0"

    it "formats float arithmetic idempotently (issue #184)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn adjust(x: Float, y: Float) -> Float:"
            , "  x / y + 0.1"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ T.unpack err
        Right formatted1 ->
          case formatSource defaultFormatOptions formatted1 of
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
            Right formatted2 -> formatted1 `shouldBe` formatted2

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

      it "formats type with multiple dependent parameters (issue #172)" $ do
        let src = T.unlines
              [ "module Test"
              , ""
              , "type Provision (j: Jurisdiction, temporal: TemporalRange):"
              , "  id: ProvisionId"
              , "  validity: TemporalValidity"
              ]
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted -> do
            formatted `shouldSatisfy` T.isInfixOf "(j: Jurisdiction, temporal: TemporalRange)"

      it "type with multiple dependent parameters is idempotent (issue #172)" $ do
        let src = T.unlines
              [ "module Test"
              , ""
              , "type Provision (j: Jurisdiction, temporal: TemporalRange):"
              , "  id: ProvisionId"
              , "  validity: TemporalValidity"
              ]
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted1 ->
            case formatSource defaultFormatOptions formatted1 of
              Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
              Right formatted2 -> formatted1 `shouldBe` formatted2

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

      it "formats effect with type parameters (issue #171)" $ do
        let src = T.unlines
              [ "module Test"
              , ""
              , "effect State S:"
              , "  get: S"
              , "  put: S -> Unit"
              ]
        case formatSource defaultFormatOptions src of
          Left err -> expectationFailure $ T.unpack err
          Right formatted -> do
            formatted `shouldSatisfy` T.isInfixOf "effect State S:"
            formatted `shouldSatisfy` T.isInfixOf "get: S"

      it "effect with type parameters is idempotent (issue #171)" $ do
        let src = T.unlines
              [ "module Test"
              , ""
              , "effect State S:"
              , "  get: S"
              , "  put: S -> Unit"
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

  describe "tuple types" $ do
    it "formats tuple type in function parameter" $ do
      let src = "module Test\n\nfn first(pair: (Int, Bool)) -> Int:\n  0"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "(Int, Bool)"
        Left _ -> expectationFailure "Expected Right"

    it "formats tuple type as return type" $ do
      let src = "module Test\n\nfn swap(x: Int, y: Bool) -> (Bool, Int):\n  (y, x)"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "(Bool, Int)"
        Left _ -> expectationFailure "Expected Right"

    it "tuple type round-trips idempotently" $ do
      let src = "module Test\n\nfn first(pair: (Int, Bool)) -> Int:\n  0"
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> formatSource defaultFormatOptions formatted `shouldBe` Right formatted
        Left err -> expectationFailure $ "First format failed: " ++ show err

    it "formats triple tuple type" $ do
      let src = "module Test\n\nfn test(t: (Int, Bool, String)) -> Int:\n  0"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "(Int, Bool, String)"
        Left _ -> expectationFailure "Expected Right"

    it "formats nested tuple type" $ do
      let src = "module Test\n\nfn test(t: ((Int, Bool), String)) -> Int:\n  0"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "((Int, Bool), String)"
        Left _ -> expectationFailure "Expected Right"

  describe "wildcard type arguments" $ do
    it "formats wildcard type argument in function parameter" $ do
      let src = "module Test\n\nfn test(c: Court _) -> Int:\n  0"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "Court _"
        Left _ -> expectationFailure "Expected Right"

    it "formats wildcard type in return type" $ do
      let src = "module Test\n\nfn lookup(c: Citation) -> Option _:\n  None"
          result = formatSource defaultFormatOptions src
      result `shouldSatisfy` isRight
      case result of
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "Option _"
        Left _ -> expectationFailure "Expected Right"

    it "wildcard type round-trips idempotently" $ do
      let src = "module Test\n\nfn test(c: Court _) -> Int:\n  0"
          result = formatSource defaultFormatOptions src
      case result of
        Right formatted -> formatSource defaultFormatOptions formatted `shouldBe` Right formatted
        Left err -> expectationFailure $ "First format failed: " ++ show err

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

    it "formats parameterless trait definition (issue #212)" $ do
      let src = T.unlines
            [ "module Test"
            , "trait Action:"
            , "  describe: String"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "trait Action:"
          -- Should NOT have a trailing space before colon
          formatted `shouldSatisfy` (not . T.isInfixOf "trait Action :")

    it "formats parameterless trait idempotently (issue #212)" $ do
      let src = T.unlines
            [ "module Test"
            , "trait Action:"
            , "  describe: String"
            , "  debug: String"
            ]
      case formatSource defaultFormatOptions src of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err

    it "formats parameterized trait with parameter (issue #212)" $ do
      let src = T.unlines
            [ "module Test"
            , "trait Eq A:"
            , "  eq: A -> A -> Bool"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "trait Eq A:"

    it "formats fn-style trait method with self (issue #213)" $ do
      let src = T.unlines
            [ "module Test"
            , "trait Action:"
            , "  fn describe(self) -> String"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "fn describe(self) -> String"

    it "formats fn-style trait method with self and extra params (issue #213)" $ do
      let src = T.unlines
            [ "module Test"
            , "trait Action:"
            , "  fn implies(self, other: Self) -> Bool"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "fn implies(self, other: Self) -> Bool"

    it "formats fn-style trait method idempotently (issue #213)" $ do
      let src = T.unlines
            [ "module Test"
            , "trait Action:"
            , "  fn describe(self) -> String"
            , "  fn implies(self, other: Self) -> Bool"
            ]
      case formatSource defaultFormatOptions src of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err

    it "formats sig-style trait method still works (issue #213 regression)" $ do
      let src = T.unlines
            [ "module Test"
            , "trait Eq A:"
            , "  eq: A -> A -> Bool"
            ]
      case formatSource defaultFormatOptions src of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err

    it "formats impl with self parameter (issue #213)" $ do
      let src = T.unlines
            [ "module Test"
            , "impl Action for JudicialAction:"
            , "  fn describe(self) -> String:"
            , "    \"judicial\""
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "fn describe(self) -> String:"

    it "formats impl with self parameter idempotently (issue #213)" $ do
      let src = T.unlines
            [ "module Test"
            , "impl Action for JudicialAction:"
            , "  fn describe(self) -> String:"
            , "    \"judicial\""
            ]
      case formatSource defaultFormatOptions src of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err

    it "formats mixed fn-style and sig-style methods in trait (issue #213)" $ do
      let src = T.unlines
            [ "module Test"
            , "trait Action:"
            , "  describe: String"
            , "  fn implies(self, other: Self) -> Bool"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "describe: String"
          formatted `shouldSatisfy` T.isInfixOf "fn implies(self, other: Self) -> Bool"

    it "formats mixed trait methods idempotently (issue #213)" $ do
      let src = T.unlines
            [ "module Test"
            , "trait Action:"
            , "  describe: String"
            , "  fn implies(self, other: Self) -> Bool"
            ]
      case formatSource defaultFormatOptions src of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err

    it "formats fn closure expression round-trip (issue #214)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn main() -> Int:"
            , "  fn(x) -> x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "fn(x) -> x"

    it "formats fn closure with typed param round-trip (issue #214)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn main() -> Int:"
            , "  fn(x: Int) -> x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "fn(x: Int) -> x"

    it "formats fn closure idempotently (issue #214)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn main() -> Int:"
            , "  fn(x) -> x"
            ]
      case formatSource defaultFormatOptions src of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err

    it "backslash lambda still formats correctly (issue #214 regression)" $ do
      let src = T.unlines
            [ "module Test"
            , "fn main() -> Int:"
            , "  \\x: Int. x"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "\\x: Int. x"

    it "formats top-level let binding (issue #215)" $ do
      let src = T.unlines
            [ "module Test"
            , "let x = 42"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "let x = 42"

    it "formats top-level let with type annotation (issue #215)" $ do
      let src = T.unlines
            [ "module Test"
            , "let x: Int = 42"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "let x: Int = 42"

    it "formats top-level let idempotently (issue #215)" $ do
      let src = T.unlines
            [ "module Test"
            , "let x = 42"
            ]
      case formatSource defaultFormatOptions src of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err

    it "formats top-level let with type annotation idempotently (issue #215)" $ do
      let src = T.unlines
            [ "module Test"
            , "let name: String = \"hello\""
            ]
      case formatSource defaultFormatOptions src of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err

    it "formats qualified let binding (issue #276)" $ do
      let src = T.unlines
            [ "module Test"
            , "let Date.max = 42"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "let Date.max = 42"

    it "formats qualified let binding idempotently (issue #276)" $ do
      let src = T.unlines
            [ "module Test"
            , "let Date.max = 42"
            ]
      case formatSource defaultFormatOptions src of
        Right formatted -> do
          case formatSource defaultFormatOptions formatted of
            Right formatted2 -> formatted2 `shouldBe` formatted
            Left err -> expectationFailure $ "Re-format failed: " ++ T.unpack err
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err

    it "formats qualified let with record value (issue #276)" $ do
      let src = T.unlines
            [ "module Test"
            , "let Date.max = Date { year = 9999, month = 12, day = 31 }"
            ]
      case formatSource defaultFormatOptions src of
        Left err -> expectationFailure $ "Format failed: " ++ T.unpack err
        Right formatted -> do
          formatted `shouldSatisfy` T.isInfixOf "let Date.max"
          formatted `shouldSatisfy` T.isInfixOf "Date { year = 9999"

-- Helper functions

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
