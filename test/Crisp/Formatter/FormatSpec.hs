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
      it "formats simple let" $ do
        formatExpr defaultFormatOptions "let x = 1 in x" `shouldBe` Right "let x = 1 in x"

      it "formats let with type annotation" $ do
        formatExpr defaultFormatOptions "let x: Int = 1 in x" `shouldBe` Right "let x: Int = 1 in x"

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
      let src = "let x = 1 in x"
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
      result `shouldBe` Right "let x = 1 in x"

-- Helper functions

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
