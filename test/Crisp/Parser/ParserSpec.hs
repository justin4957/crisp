{-# LANGUAGE OverloadedStrings #-}

module Crisp.Parser.ParserSpec (spec) where

import Test.Hspec

import Crisp.Parser.Parser
import Crisp.Syntax.Surface

import Data.Either (isRight, isLeft)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "parses integer literals" $ do
      let result = parseExpr "test" "42"
      result `shouldSatisfy` isRight

    it "parses float literals" $ do
      let result = parseExpr "test" "3.14"
      result `shouldSatisfy` isRight

    it "parses string literals" $ do
      let result = parseExpr "test" "\"hello\""
      result `shouldSatisfy` isRight

    it "parses unit literal" $ do
      let result = parseExpr "test" "()"
      result `shouldSatisfy` isRight

    it "parses variable references" $ do
      let result = parseExpr "test" "foo"
      result `shouldSatisfy` isRight

    it "parses constructor references" $ do
      let result = parseExpr "test" "Some"
      result `shouldSatisfy` isRight

    it "parses function application" $ do
      let result = parseExpr "test" "f x y"
      result `shouldSatisfy` isRight

    it "parses let expressions" $ do
      let result = parseExpr "test" "let x = 42 in x"
      result `shouldSatisfy` isRight

    it "parses if expressions" $ do
      let result = parseExpr "test" "if True then 1 else 0"
      result `shouldSatisfy` isRight

    it "parses lambda expressions" $ do
      let result = parseExpr "test" "\\x: Int. x"
      result `shouldSatisfy` isRight

    it "parses perform expressions" $ do
      let result = parseExpr "test" "perform Log.info msg"
      result `shouldSatisfy` isRight

    it "parses pipeline expressions" $ do
      let result = parseExpr "test" "x |> f |> g"
      result `shouldSatisfy` isRight

  describe "parseType" $ do
    it "parses simple types" $ do
      let result = parseType "test" "Int"
      result `shouldSatisfy` isRight

    it "parses function types" $ do
      let result = parseType "test" "Int -> Bool"
      result `shouldSatisfy` isRight

    it "parses type applications" $ do
      let result = parseType "test" "Option Int"
      result `shouldSatisfy` isRight

    it "parses effectful function types" $ do
      let result = parseType "test" "Int -> Bool ! Log"
      result `shouldSatisfy` isRight

    it "parses forall types" $ do
      let result = parseType "test" "forall T. T -> T"
      result `shouldSatisfy` isRight

    it "parses lazy types" $ do
      let result = parseType "test" "Lazy Int"
      result `shouldSatisfy` isRight

    it "parses ref types" $ do
      let result = parseType "test" "ref Int"
      result `shouldSatisfy` isRight

  describe "parseModule" $ do
    it "parses minimal module" $ do
      let source = "module Main"
      let result = parseModule "test" source
      result `shouldSatisfy` isRight

    it "parses module with authority" $ do
      let source = "module Treasury.Audit authority Treasury"
      let result = parseModule "test" source
      result `shouldSatisfy` isRight
