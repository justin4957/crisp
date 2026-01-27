{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.LSP.ServerSpec
-- Description : Test suite for LSP server functionality
--
-- Tests for diagnostics, hover, completion, symbols, and formatting.

module Crisp.LSP.ServerSpec (spec) where

import Test.Hspec

import Crisp.LSP.Server
import Crisp.Parser.Parser (parseModule)
import Crisp.Syntax.Span (Span(..), Position(..))

import Data.Maybe (isJust, isNothing)
import Data.Either (isRight, isLeft)
import qualified Data.Text as T

spec :: Spec
spec = do
  configTests
  capabilitiesTests
  documentStateTests
  diagnosticsTests
  hoverTests
  definitionTests
  symbolsTests
  completionTests
  formattingTests

-- =============================================================================
-- Configuration Tests
-- =============================================================================

configTests :: Spec
configTests = describe "LspConfig" $ do
  it "has sensible defaults" $ do
    lspConfigLogLevel defaultLspConfig `shouldBe` LogInfo
    lspConfigFormatOnSave defaultLspConfig `shouldBe` False
    lspConfigMaxDiagnostics defaultLspConfig `shouldBe` 100

-- =============================================================================
-- Capabilities Tests
-- =============================================================================

capabilitiesTests :: Spec
capabilitiesTests = describe "ServerCapabilities" $ do
  it "enables text document sync by default" $ do
    capTextDocumentSync defaultCapabilities `shouldBe` True

  it "enables hover by default" $ do
    capHoverProvider defaultCapabilities `shouldBe` True

  it "enables definition by default" $ do
    capDefinitionProvider defaultCapabilities `shouldBe` True

  it "enables completion by default" $ do
    capCompletionProvider defaultCapabilities `shouldBe` True

  it "enables document symbols by default" $ do
    capDocumentSymbolProvider defaultCapabilities `shouldBe` True

  it "enables formatting by default" $ do
    capFormattingProvider defaultCapabilities `shouldBe` True

  it "enables diagnostics by default" $ do
    capDiagnosticProvider defaultCapabilities `shouldBe` True

-- =============================================================================
-- Document State Tests
-- =============================================================================

documentStateTests :: Spec
documentStateTests = describe "DocumentState" $ do
  it "creates empty state with URI" $ do
    let state = emptyDocumentState "file:///test.crisp"
    docUri state `shouldBe` "file:///test.crisp"
    docVersion state `shouldBe` 0
    docContent state `shouldBe` ""
    docParsed state `shouldBe` Nothing
    docSymbols state `shouldBe` []
    docDiagnostics state `shouldBe` []

  it "updateDocument parses valid content" $ do
    let content = T.unlines
          [ "module Test"
          , "fn hello() -> Unit:"
          , "  Unit"
          ]
    let state = updateDocument "file:///test.crisp" 1 content
    docVersion state `shouldBe` 1
    docParsed state `shouldSatisfy` isJust

  it "updateDocument extracts symbols" $ do
    let content = T.unlines
          [ "module Test"
          , "fn hello() -> Unit:"
          , "  Unit"
          , "fn world() -> Unit:"
          , "  Unit"
          ]
    let state = updateDocument "file:///test.crisp" 1 content
    length (docSymbols state) `shouldBe` 2

  it "updateDocument generates diagnostics for invalid content" $ do
    let content = "this is not valid crisp code {{{"
    let state = updateDocument "file:///test.crisp" 1 content
    docDiagnostics state `shouldSatisfy` (not . null)

-- =============================================================================
-- Diagnostics Tests
-- =============================================================================

diagnosticsTests :: Spec
diagnosticsTests = describe "Diagnostics" $ do
  it "returns no diagnostics for valid code" $ do
    let content = T.unlines
          [ "module Test"
          , "fn main() -> Unit:"
          , "  Unit"
          ]
    let diags = computeDiagnostics "test.crisp" content
    diags `shouldBe` []

  it "returns diagnostics for parse errors" $ do
    let content = "module Test\nfn broken {{{"
    let diags = computeDiagnostics "test.crisp" content
    length diags `shouldSatisfy` (> 0)

  it "diagnostics have error severity for parse errors" $ do
    let content = "invalid syntax!!!"
    let diags = computeDiagnostics "test.crisp" content
    case diags of
      (d:_) -> diagSeverity d `shouldBe` SeverityError
      [] -> expectationFailure "Expected at least one diagnostic"

  it "diagnostics include source" $ do
    let content = "invalid"
    let diags = computeDiagnostics "test.crisp" content
    case diags of
      (d:_) -> diagSource d `shouldBe` "crisp"
      [] -> expectationFailure "Expected diagnostic"

-- =============================================================================
-- Hover Tests
-- =============================================================================

hoverTests :: Spec
hoverTests = describe "Hover" $ do
  it "returns Nothing when no symbol at position" $ do
    let state = emptyDocumentState "test.crisp"
    getHoverInfo state (Position 1 1) `shouldBe` Nothing

  it "returns hover info for known symbol" $ do
    let content = T.unlines
          [ "module Test"
          , "fn hello() -> Unit:"
          , "  Unit"
          ]
    let state = updateDocument "file:///test.crisp" 1 content
    -- If we have symbols, hover at their position should work
    case docSymbols state of
      (sym:_) -> do
        let pos = spanStart (defSpan sym)
        getHoverInfo state pos `shouldSatisfy` isJust
      [] -> return ()  -- No symbols to test

  it "HoverResult contains markdown" $ do
    let info = DefinitionInfo "test" SkFunction dummySpan (Just "Unit")
    let content = formatHoverContent info
    content `shouldSatisfy` T.isInfixOf "```crisp"
    content `shouldSatisfy` T.isInfixOf "fn test"

-- =============================================================================
-- Go to Definition Tests
-- =============================================================================

definitionTests :: Spec
definitionTests = describe "Go to Definition" $ do
  it "returns Nothing when no symbol at position" $ do
    let state = emptyDocumentState "test.crisp"
    getDefinition state (Position 1 1) `shouldBe` Nothing

  it "returns location for known symbol" $ do
    let content = T.unlines
          [ "module Test"
          , "fn hello() -> Unit:"
          , "  Unit"
          ]
    let state = updateDocument "file:///test.crisp" 1 content
    case docSymbols state of
      (sym:_) -> do
        let pos = spanStart (defSpan sym)
        let result = getDefinition state pos
        result `shouldSatisfy` isJust
        case result of
          Just loc -> locUri loc `shouldBe` "file:///test.crisp"
          Nothing -> expectationFailure "Expected location"
      [] -> return ()

-- =============================================================================
-- Document Symbols Tests
-- =============================================================================

symbolsTests :: Spec
symbolsTests = describe "Document Symbols" $ do
  it "extracts module symbol" $ do
    let content = T.unlines
          [ "module Test"
          , "fn main() -> Unit:"
          , "  Unit"
          ]
    case parseModule "test.crisp" content of
      Right mod -> do
        let symbols = getDocumentSymbols mod
        length symbols `shouldBe` 1
        symKind (head symbols) `shouldBe` SkModule
        symName (head symbols) `shouldBe` "Test"
      Left _ -> expectationFailure "Parse failed"

  it "extracts function symbols as children" $ do
    let content = T.unlines
          [ "module Test"
          , "fn hello() -> Unit:"
          , "  Unit"
          , "fn world() -> Unit:"
          , "  Unit"
          ]
    case parseModule "test.crisp" content of
      Right mod -> do
        let symbols = getDocumentSymbols mod
        let modSym = head symbols
        length (symChildren modSym) `shouldBe` 2
        map symKind (symChildren modSym) `shouldBe` [SkFunction, SkFunction]
      Left _ -> expectationFailure "Parse failed"

  it "extracts type symbols with constructors" $ do
    let content = T.unlines
          [ "module Test"
          , "type Color:"
          , "  Red"
          , "  Green"
          , "  Blue"
          ]
    case parseModule "test.crisp" content of
      Right mod -> do
        let symbols = getDocumentSymbols mod
        let modSym = head symbols
        length (symChildren modSym) `shouldBe` 1
        let typeSym = head (symChildren modSym)
        symKind typeSym `shouldBe` SkType
        -- Constructors should be extracted as children
        length (symChildren typeSym) `shouldSatisfy` (>= 1)
        all (== SkConstructor) (map symKind (symChildren typeSym)) `shouldBe` True
      Left _ -> expectationFailure "Parse failed"

  it "symbolKindName returns readable names" $ do
    symbolKindName SkFunction `shouldBe` "fn"
    symbolKindName SkType `shouldBe` "type"
    symbolKindName SkEffect `shouldBe` "effect"
    symbolKindName SkModule `shouldBe` "module"

-- =============================================================================
-- Completion Tests
-- =============================================================================

completionTests :: Spec
completionTests = describe "Completions" $ do
  it "returns keyword completions" $ do
    let state = emptyDocumentState "test.crisp"
    let completions = getCompletions state (Position 1 1) ""
    let labels = map compLabel completions
    labels `shouldSatisfy` elem "fn"
    labels `shouldSatisfy` elem "type"
    labels `shouldSatisfy` elem "match"
    labels `shouldSatisfy` elem "if"

  it "returns prelude function completions" $ do
    let state = emptyDocumentState "test.crisp"
    let completions = getCompletions state (Position 1 1) ""
    let labels = map compLabel completions
    labels `shouldSatisfy` elem "id"
    labels `shouldSatisfy` elem "map"
    labels `shouldSatisfy` elem "filter"

  it "returns prelude type completions" $ do
    let state = emptyDocumentState "test.crisp"
    let completions = getCompletions state (Position 1 1) ""
    let labels = map compLabel completions
    labels `shouldSatisfy` elem "Option"
    labels `shouldSatisfy` elem "Result"
    labels `shouldSatisfy` elem "List"

  it "returns constructor completions" $ do
    let state = emptyDocumentState "test.crisp"
    let completions = getCompletions state (Position 1 1) ""
    let labels = map compLabel completions
    labels `shouldSatisfy` elem "Some"
    labels `shouldSatisfy` elem "None"
    labels `shouldSatisfy` elem "Ok"
    labels `shouldSatisfy` elem "Err"

  it "returns effect completions" $ do
    let state = emptyDocumentState "test.crisp"
    let completions = getCompletions state (Position 1 1) ""
    let labels = map compLabel completions
    labels `shouldSatisfy` elem "State"
    labels `shouldSatisfy` elem "IO"

  it "filters by prefix" $ do
    let state = emptyDocumentState "test.crisp"
    let completions = getCompletions state (Position 1 1) "ma"
    let labels = map compLabel completions
    labels `shouldSatisfy` elem "map"
    labels `shouldSatisfy` elem "match"
    labels `shouldSatisfy` (not . elem "filter")

  it "prefix matching is case-insensitive" $ do
    let state = emptyDocumentState "test.crisp"
    let completions = getCompletions state (Position 1 1) "SO"
    let labels = map compLabel completions
    labels `shouldSatisfy` elem "Some"

  it "function completions have type details" $ do
    let state = emptyDocumentState "test.crisp"
    let completions = getCompletions state (Position 1 1) "id"
    case filter (\c -> compLabel c == "id") completions of
      (c:_) -> compDetail c `shouldSatisfy` isJust
      [] -> expectationFailure "Expected id completion"

-- =============================================================================
-- Formatting Tests
-- =============================================================================

formattingTests :: Spec
formattingTests = describe "Formatting" $ do
  it "formats valid code successfully" $ do
    let content = T.unlines
          [ "module Test"
          , "fn main() -> Unit:"
          , "  Unit"
          ]
    formatDocument content `shouldSatisfy` isRight

  it "returns error for invalid code" $ do
    let content = "this is {{ invalid"
    formatDocument content `shouldSatisfy` isLeft

  it "preserves module structure" $ do
    let content = T.unlines
          [ "module Test"
          , "fn main() -> Unit:"
          , "  Unit"
          ]
    case formatDocument content of
      Right formatted -> do
        formatted `shouldSatisfy` T.isInfixOf "module Test"
        formatted `shouldSatisfy` T.isInfixOf "fn main"
      Left _ -> expectationFailure "Formatting failed"

-- =============================================================================
-- Helpers
-- =============================================================================

dummySpan :: Span
dummySpan = Span (Position 1 1) (Position 1 10) "test.crisp"
