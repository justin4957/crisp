{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec

import qualified Crisp.Lexer.LexerSpec
import qualified Crisp.Parser.ParserSpec
import qualified Crisp.Types.CheckerSpec

main :: IO ()
main = hspec $ do
  describe "Crisp.Lexer" Crisp.Lexer.LexerSpec.spec
  describe "Crisp.Parser" Crisp.Parser.ParserSpec.spec
  describe "Crisp.Types.Checker" Crisp.Types.CheckerSpec.spec
