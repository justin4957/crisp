{-# LANGUAGE OverloadedStrings #-}

module Crisp.Lexer.LexerSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Crisp.Lexer.Lexer
import Crisp.Lexer.Token

import Data.Either (isRight)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "lexFile" $ do
    it "lexes simple keywords" $ do
      let result = lexFile "test" "fn let match"
      result `shouldSatisfy` isRight

    it "lexes identifiers" $ do
      let result = lexFile "test" "foo Bar baz'"
      result `shouldSatisfy` isRight

    it "lexes integer literals" $ do
      let result = lexFile "test" "42 0xFF 0b1010"
      result `shouldSatisfy` isRight

    it "lexes the arrow operator" $ do
      let result = lexFile "test" "a -> b"
      result `shouldSatisfy` isRight

    it "handles line comments" $ do
      let result = lexFile "test" "foo -- comment\nbar"
      result `shouldSatisfy` isRight

    it "handles block comments" $ do
      let result = lexFile "test" "foo {- block comment -} bar"
      result `shouldSatisfy` isRight

    it "handles nested block comments" $ do
      let result = lexFile "test" "foo {- outer {- inner -} outer -} bar"
      result `shouldSatisfy` isRight

  describe "token kinds" $ do
    it "identifies keywords correctly" $ do
      isKeyword KwFn `shouldBe` True
      isKeyword KwLet `shouldBe` True
      isKeyword (LowerIdent "foo") `shouldBe` False

    it "identifies layout tokens" $ do
      isLayoutToken Indent `shouldBe` True
      isLayoutToken Dedent `shouldBe` True
      isLayoutToken Newline `shouldBe` True
      isLayoutToken KwFn `shouldBe` False

    it "identifies literals" $ do
      isLiteral (IntLit 42) `shouldBe` True
      isLiteral (FloatLit 3.14) `shouldBe` True
      isLiteral (StringLit "hello") `shouldBe` True
      isLiteral Unit `shouldBe` True
      isLiteral KwFn `shouldBe` False

  describe "keyword lookup" $ do
    it "looks up keywords correctly" $ do
      lookupKeyword "fn" `shouldBe` Just KwFn
      lookupKeyword "let" `shouldBe` Just KwLet
      lookupKeyword "foo" `shouldBe` Nothing
