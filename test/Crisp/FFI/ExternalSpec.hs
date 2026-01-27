{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.FFI.ExternalSpec
-- Description : Test suite for foreign function interface (FFI)
--
-- Tests for parsing external function definitions, registering them
-- in the context, and LLIR generation for external calls.

module Crisp.FFI.ExternalSpec (spec) where

import Test.Hspec

import Crisp.Parser.Parser
import Crisp.Syntax.Surface
import Crisp.Types.Context
import Crisp.Core.Term (Type(..), Kind(..), simpleType, pureFnType)
import Crisp.IR.LLIR

import Data.Either (isRight, isLeft)
import Data.Maybe (isJust)
import qualified Data.Text as T

-- | Helper to check that parsing succeeds
shouldParse :: (Show a, Show b) => Either a b -> Expectation
shouldParse result = result `shouldSatisfy` isRight

-- | Helper to check that parsing fails
shouldNotParse :: (Show a, Show b) => Either a b -> Expectation
shouldNotParse result = result `shouldSatisfy` isLeft

spec :: Spec
spec = do
  externalFnParsingTests
  externalExprParsingTests
  externalContextTests
  llirExternalTests

-- =============================================================================
-- External Function Definition Parsing Tests
-- =============================================================================

externalFnParsingTests :: Spec
externalFnParsingTests = describe "external function definitions" $ do
  it "parses simple external function" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn log(msg: String) -> Unit = (\"console\", \"log\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses external function with multiple params" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn query(db: String, sql: String) -> String = (\"postgres\", \"query\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses external function returning complex type" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn fetch(url: String) -> Option String = (\"http\", \"get\")"
          ]
    shouldParse $ parseModule "test" src

  it "extracts external function name" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn log(msg: String) -> Unit = (\"console\", \"log\")"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefExternal ext] -> extFnDefName ext `shouldBe` "log"
        _ -> expectationFailure "Expected single external definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts external module name" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn log(msg: String) -> Unit = (\"console\", \"log\")"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefExternal ext] -> externalModule (extFnDefExternal ext) `shouldBe` "console"
        _ -> expectationFailure "Expected single external definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts external function reference" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn query(sql: String) -> String = (\"postgres\", \"execute\")"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefExternal ext] -> externalFunction (extFnDefExternal ext) `shouldBe` "execute"
        _ -> expectationFailure "Expected single external definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts external function parameters" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn query(db: String, sql: String) -> String = (\"postgres\", \"query\")"
          ]
    case parseModule "test" src of
      Right m -> case moduleDefinitions m of
        [DefExternal ext] -> length (extFnDefParams ext) `shouldBe` 2
        _ -> expectationFailure "Expected single external definition"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- External Expression Parsing Tests
-- =============================================================================

externalExprParsingTests :: Spec
externalExprParsingTests = describe "external expression calls" $ do
  it "parses external call in expression" $ do
    let src = T.unlines
          [ "module Main"
          , "fn main() -> Unit:"
          , "  external(\"console\", \"log\") \"Hello\""
          ]
    shouldParse $ parseModule "test" src

  it "parses external call with multiple arguments" $ do
    let src = T.unlines
          [ "module Main"
          , "fn main() -> String:"
          , "  external(\"postgres\", \"query\") db sql"
          ]
    shouldParse $ parseModule "test" src

  it "parses external call without arguments" $ do
    let src = T.unlines
          [ "module Main"
          , "fn get_time() -> Int:"
          , "  external(\"system\", \"time\")"
          ]
    shouldParse $ parseModule "test" src

  it "extracts external expression module" $ do
    let src = "external(\"wasi\", \"fd_write\") fd buf"
    case parseExpr "test" src of
      Right (EExternal extRef _ _) -> externalModule extRef `shouldBe` "wasi"
      Right other -> expectationFailure $ "Expected EExternal, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

  it "extracts external expression function" $ do
    let src = "external(\"wasi\", \"fd_write\") fd buf"
    case parseExpr "test" src of
      Right (EExternal extRef _ _) -> externalFunction extRef `shouldBe` "fd_write"
      Right other -> expectationFailure $ "Expected EExternal, got " ++ show other
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- External Context Tests
-- =============================================================================

externalContextTests :: Spec
externalContextTests = describe "external context" $ do
  it "registers external function" $ do
    let extInfo = ExternalInfo "log" "console" "log"
          (pureFnType (simpleType "String") (simpleType "Unit"))
    let ctx = registerExternal extInfo emptyContext
    lookupExternal "log" ctx `shouldBe` Just extInfo

  it "registers multiple external functions" $ do
    let ext1 = ExternalInfo "log" "console" "log"
          (pureFnType (simpleType "String") (simpleType "Unit"))
    let ext2 = ExternalInfo "query" "postgres" "execute"
          (pureFnType (simpleType "String") (simpleType "String"))
    let ctx = registerExternal ext2 $ registerExternal ext1 emptyContext
    lookupExternal "log" ctx `shouldSatisfy` isJust
    lookupExternal "query" ctx `shouldSatisfy` isJust

  it "looks up nonexistent external returns Nothing" $ do
    lookupExternal "nonexistent" emptyContext `shouldBe` Nothing

  it "gets all externals from context" $ do
    let ext1 = ExternalInfo "log" "console" "log"
          (pureFnType (simpleType "String") (simpleType "Unit"))
    let ext2 = ExternalInfo "query" "postgres" "execute"
          (pureFnType (simpleType "String") (simpleType "String"))
    let ctx = registerExternal ext2 $ registerExternal ext1 emptyContext
    length (allExternals ctx) `shouldBe` 2

  it "external info preserves module name" $ do
    let extInfo = ExternalInfo "fetch" "http" "get"
          (pureFnType (simpleType "String") (simpleType "String"))
    externalInfoModule extInfo `shouldBe` "http"

  it "external info preserves function name" $ do
    let extInfo = ExternalInfo "fetch" "http" "get"
          (pureFnType (simpleType "String") (simpleType "String"))
    externalInfoFunction extInfo `shouldBe` "get"

-- =============================================================================
-- LLIR External Tests
-- =============================================================================

llirExternalTests :: Spec
llirExternalTests = describe "LLIR external calls" $ do
  it "creates LlirCallExternal instruction" $ do
    let instr = LlirCallExternal "console" "log" 1
    llirInstrIsCallExternal instr `shouldBe` True

  it "LlirCallExternal is not regular call" $ do
    let instr = LlirCallExternal "console" "log" 1
    llirInstrIsCall instr `shouldBe` False

  it "LlirCallExternal is not indirect call" $ do
    let instr = LlirCallExternal "console" "log" 1
    llirInstrIsCallIndirect instr `shouldBe` False

  it "regular call is not external call" $ do
    let instr = LlirCall "some_func" 1
    llirInstrIsCallExternal instr `shouldBe` False
