{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec

import qualified Crisp.Lexer.LexerSpec
import qualified Crisp.Lexer.ErrorSpec
import qualified Crisp.Parser.ParserSpec
import qualified Crisp.Types.CheckerSpec
import qualified Crisp.Core.PatternSpec

main :: IO ()
main = hspec $ do
  describe "Crisp.Lexer" Crisp.Lexer.LexerSpec.spec
  describe "Crisp.Lexer.Error" Crisp.Lexer.ErrorSpec.spec
  describe "Crisp.Parser" Crisp.Parser.ParserSpec.spec
  describe "Crisp.Types.Checker" Crisp.Types.CheckerSpec.spec
  describe "Crisp.Core.Pattern" Crisp.Core.PatternSpec.spec
