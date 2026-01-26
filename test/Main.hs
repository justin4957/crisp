{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec

import qualified Crisp.Lexer.LexerSpec
import qualified Crisp.Lexer.ErrorSpec
import qualified Crisp.Parser.ParserSpec
import qualified Crisp.Types.CheckerSpec
import qualified Crisp.Types.UnifySpec
import qualified Crisp.Types.InferSpec
import qualified Crisp.Types.KindSpec
import qualified Crisp.Types.ExhaustiveSpec
import qualified Crisp.Effects.TypingSpec
import qualified Crisp.Effects.PolymorphismSpec
import qualified Crisp.Core.PatternSpec

main :: IO ()
main = hspec $ do
  describe "Crisp.Lexer" Crisp.Lexer.LexerSpec.spec
  describe "Crisp.Lexer.Error" Crisp.Lexer.ErrorSpec.spec
  describe "Crisp.Parser" Crisp.Parser.ParserSpec.spec
  describe "Crisp.Types.Checker" Crisp.Types.CheckerSpec.spec
  describe "Crisp.Types.Unify" Crisp.Types.UnifySpec.spec
  describe "Crisp.Types.Infer" Crisp.Types.InferSpec.spec
  describe "Crisp.Types.Kind" Crisp.Types.KindSpec.spec
  describe "Crisp.Types.Exhaustive" Crisp.Types.ExhaustiveSpec.spec
  describe "Crisp.Effects.Typing" Crisp.Effects.TypingSpec.spec
  describe "Crisp.Effects.Polymorphism" Crisp.Effects.PolymorphismSpec.spec
  describe "Crisp.Core.Pattern" Crisp.Core.PatternSpec.spec
