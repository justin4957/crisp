{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Lexer.ErrorSpec
-- Description : Tests for lexer error recovery and reporting
--
-- Tests for the lexer's error recovery functionality, including
-- detection of unterminated strings, comments, and character literals,
-- as well as error formatting and multiple error collection.

module Crisp.Lexer.ErrorSpec (spec) where

import Test.Hspec
import Crisp.Lexer.Lexer
import Crisp.Lexer.Error
import Crisp.Syntax.Span
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Error Recovery" $ do
    describe "Unterminated string literals" $ do
      it "reports unterminated string at end of line" $ do
        let result = lexFileWithRecovery "test" "\"hello"
        result `shouldSatisfy` isLeft

      it "reports unterminated string at newline" $ do
        let result = lexFileWithRecovery "test" "\"hello\nworld"
        result `shouldSatisfy` isLeft

      it "includes error span for unterminated string" $ do
        case lexFileWithRecovery "test" "\"hello" of
          Left (err:_) -> do
            lexErrorKind err `shouldBe` UnterminatedString
            posLine (spanStart (lexErrorSpan err)) `shouldBe` 1
          Left [] -> expectationFailure "Expected at least one error"
          Right _ -> expectationFailure "Expected error"

    describe "Unterminated block comments" $ do
      it "reports unterminated block comment" $ do
        let result = lexFileWithRecovery "test" "{- comment"
        result `shouldSatisfy` isLeft

      it "reports unterminated nested block comment" $ do
        let result = lexFileWithRecovery "test" "{- outer {- inner -}"
        result `shouldSatisfy` isLeft

      it "includes error kind for unterminated block comment" $ do
        case lexFileWithRecovery "test" "{- comment" of
          Left (err:_) -> lexErrorKind err `shouldBe` UnterminatedBlockComment
          Left [] -> expectationFailure "Expected at least one error"
          Right _ -> expectationFailure "Expected error"

    describe "Unterminated character literals" $ do
      it "reports unterminated character literal" $ do
        let result = lexFileWithRecovery "test" "'a"
        result `shouldSatisfy` isLeft

      it "reports unterminated character literal at newline" $ do
        let result = lexFileWithRecovery "test" "'a\nb"
        result `shouldSatisfy` isLeft

    describe "Invalid character literals" $ do
      it "reports empty character literal" $ do
        let result = lexFileWithRecovery "test" "''"
        result `shouldSatisfy` isLeft

      it "reports multi-character literal" $ do
        let result = lexFileWithRecovery "test" "'abc'"
        result `shouldSatisfy` isLeft

    describe "Unexpected characters" $ do
      it "reports unexpected character" $ do
        let result = lexFileWithRecovery "test" "let x = `"
        result `shouldSatisfy` isLeft

      it "includes the problematic character in error" $ do
        case lexFileWithRecovery "test" "`" of
          Left (err:_) ->
            case lexErrorKind err of
              UnexpectedCharacter c -> c `shouldBe` '`'
              _ -> expectationFailure "Expected UnexpectedCharacter"
          Left [] -> expectationFailure "Expected at least one error"
          Right _ -> expectationFailure "Expected error"

  describe "Multiple Error Collection" $ do
    it "can continue after an error to find valid tokens" $ do
      -- After an invalid char, should still be able to lex remaining tokens
      let result = lexFileWithRecovery "test" "` let"
      result `shouldSatisfy` isLeft

    it "collects errors while producing partial token stream" $ do
      -- The recovery mechanism allows continuing past errors
      let result = lexFileWithRecovery "test" "` `"
      case result of
        Left errors -> length errors `shouldSatisfy` (>= 1)
        Right _ -> expectationFailure "Expected errors"

  describe "Error Formatting" $ do
    describe "formatLexError" $ do
      it "includes error code in output" $ do
        let err = unterminatedString (pointSpan (Position 1 1) "test") Nothing
            source = "\"hello"
            formatted = formatLexError source err
        formatted `shouldSatisfy` T.isInfixOf "L001"

      it "includes file position in output" $ do
        let err = unterminatedString (pointSpan (Position 3 5) "test.crisp") Nothing
            source = "line1\nline2\n    \"hello"
            formatted = formatLexError source err
        formatted `shouldSatisfy` T.isInfixOf "test.crisp:3:5"

      it "includes error message in output" $ do
        let err = unterminatedBlockComment (pointSpan (Position 1 1) "test")
            source = "{- comment"
            formatted = formatLexError source err
        formatted `shouldSatisfy` T.isInfixOf "unterminated block comment"

      it "includes help text when available" $ do
        let err = unterminatedString (pointSpan (Position 1 1) "test") Nothing
            source = "\"hello"
            formatted = formatLexError source err
        formatted `shouldSatisfy` T.isInfixOf "help:"

    describe "formatLexErrors" $ do
      it "formats multiple errors" $ do
        let err1 = unterminatedString (pointSpan (Position 1 1) "test") Nothing
            err2 = unexpectedCharacter '`' (pointSpan (Position 2 1) "test")
            source = "\"hello\n`"
            formatted = formatLexErrors source [err1, err2]
        formatted `shouldSatisfy` T.isInfixOf "L001"
        formatted `shouldSatisfy` T.isInfixOf "L007"

  describe "Error Codes" $ do
    it "assigns L001 to unterminated string" $ do
      errorCode UnterminatedString `shouldBe` "L001"

    it "assigns L002 to unterminated block comment" $ do
      errorCode UnterminatedBlockComment `shouldBe` "L002"

    it "assigns L003 to unterminated char literal" $ do
      errorCode UnterminatedCharLiteral `shouldBe` "L003"

    it "assigns L004 to invalid escape sequence" $ do
      errorCode (InvalidEscapeSequence 'x') `shouldBe` "L004"

    it "assigns L005 to invalid char literal" $ do
      errorCode (InvalidCharLiteral "abc") `shouldBe` "L005"

    it "assigns L006 to invalid numeric literal" $ do
      errorCode (InvalidNumericLiteral "bad") `shouldBe` "L006"

    it "assigns L007 to unexpected character" $ do
      errorCode (UnexpectedCharacter '`') `shouldBe` "L007"

    it "assigns L008 to tab character" $ do
      errorCode TabCharacter `shouldBe` "L008"

  describe "Error Construction" $ do
    it "creates unterminated string error with context" $ do
      let span' = pointSpan (Position 1 1) "test"
          err = unterminatedString span' (Just "hello")
      lexErrorKind err `shouldBe` UnterminatedString
      lexErrorContext err `shouldBe` Just "hello"

    it "creates unterminated block comment error" $ do
      let span' = pointSpan (Position 1 1) "test"
          err = unterminatedBlockComment span'
      lexErrorKind err `shouldBe` UnterminatedBlockComment
      lexErrorContext err `shouldBe` Nothing

    it "creates invalid escape sequence error" $ do
      let span' = pointSpan (Position 1 1) "test"
          err = invalidEscapeSequence 'q' span'
      lexErrorKind err `shouldBe` InvalidEscapeSequence 'q'

    it "creates unexpected character error" $ do
      let span' = pointSpan (Position 1 1) "test"
          err = unexpectedCharacter '`' span'
      lexErrorKind err `shouldBe` UnexpectedCharacter '`'

    it "creates tab character error" $ do
      let span' = pointSpan (Position 1 1) "test"
          err = tabCharacter span'
      lexErrorKind err `shouldBe` TabCharacter

  describe "Error Predicates" $ do
    describe "isLexError" $ do
      it "returns True for Left with errors" $ do
        isLexError (Left [unterminatedString (pointSpan (Position 1 1) "test") Nothing] :: Either [LexError] ()) `shouldBe` True

      it "returns False for Left with empty list" $ do
        isLexError (Left [] :: Either [LexError] ()) `shouldBe` False

      it "returns False for Right" $ do
        isLexError (Right () :: Either [LexError] ()) `shouldBe` False

    describe "getLexErrors" $ do
      it "extracts errors from Left" $ do
        let err = unterminatedString (pointSpan (Position 1 1) "test") Nothing
        getLexErrors (Left [err]) `shouldBe` [err]

      it "returns empty list for Right" $ do
        getLexErrors (Right () :: Either [LexError] ()) `shouldBe` []

  describe "Valid Input Still Works" $ do
    it "lexes valid code without errors" $ do
      let result = lexFileWithRecovery "test" "let x = 42"
      result `shouldSatisfy` isRight

    it "lexes valid string literals" $ do
      let result = lexFileWithRecovery "test" "\"hello world\""
      result `shouldSatisfy` isRight

    it "lexes valid block comments" $ do
      let result = lexFileWithRecovery "test" "{- comment -} let x = 1"
      result `shouldSatisfy` isRight

    it "lexes valid character literals" $ do
      let result = lexFileWithRecovery "test" "'a' 'b' 'c'"
      result `shouldSatisfy` isRight

    it "lexes nested block comments" $ do
      let result = lexFileWithRecovery "test" "{- outer {- inner -} outer -} x"
      result `shouldSatisfy` isRight
