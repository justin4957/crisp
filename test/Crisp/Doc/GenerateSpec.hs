{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Doc.GenerateSpec
-- Description : Test suite for documentation generator

module Crisp.Doc.GenerateSpec (spec) where

import Test.Hspec

import Crisp.Doc.Generate
import Crisp.Parser.Parser (parseModule)

import Data.Either (isRight, isLeft)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

spec :: Spec
spec = do
  docCommentParsingTests
  moduleDocExtractionTests
  markdownRenderingTests
  htmlRenderingTests
  docFormatTests

-- =============================================================================
-- Doc Comment Parsing Tests
-- =============================================================================

docCommentParsingTests :: Spec
docCommentParsingTests = describe "Doc Comment Parsing" $ do
  it "extracts simple doc comment" $ do
    let content = T.unlines
          [ "--- | A simple function"
          , "fn hello() -> Unit:"
          , "  Unit"
          ]
    let docs = extractDocComments content
    Map.size docs `shouldBe` 1

  it "parses summary from doc comment" $ do
    let block = ["--- | This is the summary"]
    let doc = parseDocComment block
    docSummary doc `shouldBe` Just "This is the summary"

  it "parses multi-line doc comments" $ do
    let block =
          [ "--- | Brief summary"
          , "---"
          , "--- This is the longer description"
          , "--- that spans multiple lines."
          ]
    let doc = parseDocComment block
    docSummary doc `shouldBe` Just "Brief summary"
    docDescription doc `shouldSatisfy` isJust

  it "extracts examples section" $ do
    let block =
          [ "--- | A function"
          , "---"
          , "--- Examples:"
          , "---   add(1, 2)"
          , "---   add(3, 4)"
          ]
    let doc = parseDocComment block
    length (docExamples doc) `shouldSatisfy` (>= 1)

  it "extracts see also section" $ do
    let block =
          [ "--- | A function"
          , "---"
          , "--- See also:"
          , "---   other_fn"
          ]
    let doc = parseDocComment block
    length (docSeeAlso doc) `shouldSatisfy` (>= 1)

  it "extracts since version" $ do
    let block =
          [ "--- | A function"
          , "---"
          , "--- Since:"
          , "---   1.0.0"
          ]
    let doc = parseDocComment block
    docSince doc `shouldSatisfy` isJust

  it "handles empty doc comment" $ do
    let block = ["---"]
    let doc = parseDocComment block
    docSummary doc `shouldBe` Nothing

  it "handles doc comment without pipe" $ do
    let block = ["--- This is also valid"]
    let doc = parseDocComment block
    docSummary doc `shouldBe` Just "This is also valid"

-- =============================================================================
-- Module Documentation Extraction Tests
-- =============================================================================

moduleDocExtractionTests :: Spec
moduleDocExtractionTests = describe "Module Documentation Extraction" $ do
  it "extracts module name" $ do
    let content = T.unlines
          [ "module Test"
          , "fn main() -> Unit:"
          , "  Unit"
          ]
    case generateModuleDocs Markdown "test.crisp" content of
      Right doc -> modDocName doc `shouldBe` "Test"
      Left err -> expectationFailure $ T.unpack err

  it "extracts function documentation" $ do
    let content = T.unlines
          [ "module Test"
          , ""
          , "--- | Add two numbers"
          , "fn add(x: Int, y: Int) -> Int:"
          , "  x"
          ]
    case generateModuleDocs Markdown "test.crisp" content of
      Right doc -> do
        length (modDocItems doc) `shouldBe` 1
        case head (modDocItems doc) of
          ItemFunction fn -> fnDocName fn `shouldBe` "add"
          _ -> expectationFailure "Expected function"
      Left err -> expectationFailure $ T.unpack err

  it "extracts function signature" $ do
    let content = T.unlines
          [ "module Test"
          , ""
          , "fn greet(name: String) -> String:"
          , "  name"
          ]
    case generateModuleDocs Markdown "test.crisp" content of
      Right doc -> case modDocItems doc of
        [ItemFunction fn] -> do
          fnDocSignature fn `shouldSatisfy` T.isInfixOf "name: String"
          fnDocSignature fn `shouldSatisfy` T.isInfixOf "-> String"
        _ -> expectationFailure "Expected one function"
      Left err -> expectationFailure $ T.unpack err

  it "extracts type documentation" $ do
    let content = T.unlines
          [ "module Test"
          , ""
          , "--- | A color type"
          , "type Color:"
          , "  Red"
          , "  Green"
          , "  Blue"
          ]
    case generateModuleDocs Markdown "test.crisp" content of
      Right doc -> case modDocItems doc of
        [ItemType ty] -> do
          tyDocName ty `shouldBe` "Color"
          length (tyDocConstructors ty) `shouldSatisfy` (>= 1)
        _ -> expectationFailure "Expected one type"
      Left err -> expectationFailure $ T.unpack err

  it "extracts effect documentation" $ do
    let content = T.unlines
          [ "module Test"
          , ""
          , "--- | Logging effect"
          , "effect Log:"
          , "  log: String -> Unit"
          ]
    case generateModuleDocs Markdown "test.crisp" content of
      Right doc -> case modDocItems doc of
        [ItemEffect eff] -> do
          effDocName eff `shouldBe` "Log"
          length (effDocOperations eff) `shouldSatisfy` (>= 1)
        _ -> expectationFailure "Expected one effect"
      Left err -> expectationFailure $ T.unpack err

  it "extracts external function documentation" $ do
    let content = T.unlines
          [ "module Test"
          , ""
          , "--- | Get current time"
          , "external fn now() -> Int = (\"time\", \"now\")"
          ]
    case generateModuleDocs Markdown "test.crisp" content of
      Right doc -> case modDocItems doc of
        [ItemExternal fn] -> fnDocName fn `shouldBe` "now"
        _ -> expectationFailure "Expected one external"
      Left err -> expectationFailure $ T.unpack err

  it "extracts type alias documentation" $ do
    let content = T.unlines
          [ "module Test"
          , ""
          , "--- | User identifier"
          , "type UserId = Int"
          ]
    case generateModuleDocs Markdown "test.crisp" content of
      Right doc -> case modDocItems doc of
        [ItemTypeAlias ty] -> tyDocName ty `shouldBe` "UserId"
        _ -> expectationFailure "Expected one type alias"
      Left err -> expectationFailure $ T.unpack err

  it "handles multiple items" $ do
    let content = T.unlines
          [ "module Test"
          , ""
          , "fn foo() -> Unit:"
          , "  Unit"
          , ""
          , "fn bar() -> Unit:"
          , "  Unit"
          , ""
          , "type Baz:"
          , "  Qux"
          ]
    case generateModuleDocs Markdown "test.crisp" content of
      Right doc -> length (modDocItems doc) `shouldBe` 3
      Left err -> expectationFailure $ T.unpack err

  it "returns error for invalid code" $ do
    let content = "this is {{ not valid"
    generateModuleDocs Markdown "test.crisp" content `shouldSatisfy` isLeft

-- =============================================================================
-- Markdown Rendering Tests
-- =============================================================================

markdownRenderingTests :: Spec
markdownRenderingTests = describe "Markdown Rendering" $ do
  it "renders module header" $ do
    let doc = ModuleDoc
          { modDocName = "Test.Module"
          , modDocSummary = Just "A test module"
          , modDocDescription = Nothing
          , modDocItems = []
          , modDocSourceFile = "test.crisp"
          }
    let md = renderMarkdown doc
    md `shouldSatisfy` T.isInfixOf "# Test.Module"

  it "renders module summary" $ do
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Just "This is the summary"
          , modDocDescription = Nothing
          , modDocItems = []
          , modDocSourceFile = "test.crisp"
          }
    let md = renderMarkdown doc
    md `shouldSatisfy` T.isInfixOf "This is the summary"

  it "renders function documentation" $ do
    let fn = FunctionDoc
          { fnDocName = "myFunc"
          , fnDocSignature = "fn myFunc(x: Int) -> Int"
          , fnDocSummary = Just "A function"
          , fnDocDescription = Nothing
          , fnDocExamples = []
          , fnDocSeeAlso = []
          }
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = [ItemFunction fn]
          , modDocSourceFile = "test.crisp"
          }
    let md = renderMarkdown doc
    md `shouldSatisfy` T.isInfixOf "### `myFunc`"
    md `shouldSatisfy` T.isInfixOf "```crisp"
    md `shouldSatisfy` T.isInfixOf "fn myFunc(x: Int) -> Int"

  it "renders function examples" $ do
    let fn = FunctionDoc
          { fnDocName = "add"
          , fnDocSignature = "fn add(x: Int, y: Int) -> Int"
          , fnDocSummary = Nothing
          , fnDocDescription = Nothing
          , fnDocExamples = ["add(1, 2)", "add(3, 4)"]
          , fnDocSeeAlso = []
          }
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = [ItemFunction fn]
          , modDocSourceFile = "test.crisp"
          }
    let md = renderMarkdown doc
    md `shouldSatisfy` T.isInfixOf "**Examples:**"
    md `shouldSatisfy` T.isInfixOf "add(1, 2)"

  it "renders type documentation" $ do
    let ty = TypeDoc
          { tyDocName = "Color"
          , tyDocParams = []
          , tyDocSummary = Just "A color type"
          , tyDocDescription = Nothing
          , tyDocConstructors =
              [ ConstructorDoc "Red" [] Nothing
              , ConstructorDoc "Green" [] Nothing
              ]
          }
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = [ItemType ty]
          , modDocSourceFile = "test.crisp"
          }
    let md = renderMarkdown doc
    md `shouldSatisfy` T.isInfixOf "### `Color`"
    md `shouldSatisfy` T.isInfixOf "**Constructors:**"
    md `shouldSatisfy` T.isInfixOf "`Red`"

  it "renders type with parameters" $ do
    let ty = TypeDoc
          { tyDocName = "Option"
          , tyDocParams = ["a"]
          , tyDocSummary = Nothing
          , tyDocDescription = Nothing
          , tyDocConstructors = []
          }
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = [ItemType ty]
          , modDocSourceFile = "test.crisp"
          }
    let md = renderMarkdown doc
    md `shouldSatisfy` T.isInfixOf "### `Option a`"

  it "renders effect documentation" $ do
    let eff = EffectDoc
          { effDocName = "State"
          , effDocParams = ["s"]
          , effDocSummary = Just "Stateful computation"
          , effDocDescription = Nothing
          , effDocOperations =
              [ OperationDoc "get" "Unit -> s" Nothing
              , OperationDoc "put" "s -> Unit" Nothing
              ]
          }
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = [ItemEffect eff]
          , modDocSourceFile = "test.crisp"
          }
    let md = renderMarkdown doc
    md `shouldSatisfy` T.isInfixOf "### `effect State s`"
    md `shouldSatisfy` T.isInfixOf "**Operations:**"
    md `shouldSatisfy` T.isInfixOf "`get`"

  it "renders table of contents" $ do
    let fn1 = FunctionDoc "foo" "fn foo() -> Unit" Nothing Nothing [] []
    let fn2 = FunctionDoc "bar" "fn bar() -> Unit" Nothing Nothing [] []
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = [ItemFunction fn1, ItemFunction fn2]
          , modDocSourceFile = "test.crisp"
          }
    let md = renderMarkdown doc
    md `shouldSatisfy` T.isInfixOf "## Contents"
    md `shouldSatisfy` T.isInfixOf "[`foo`]"
    md `shouldSatisfy` T.isInfixOf "[`bar`]"

  it "renders source file reference" $ do
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = []
          , modDocSourceFile = "src/Test.crisp"
          }
    let md = renderMarkdown doc
    md `shouldSatisfy` T.isInfixOf "src/Test.crisp"

-- =============================================================================
-- HTML Rendering Tests
-- =============================================================================

htmlRenderingTests :: Spec
htmlRenderingTests = describe "HTML Rendering" $ do
  it "renders valid HTML document" $ do
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = []
          , modDocSourceFile = "test.crisp"
          }
    let html = renderHtml doc
    html `shouldSatisfy` T.isInfixOf "<!DOCTYPE html>"
    html `shouldSatisfy` T.isInfixOf "<html"
    html `shouldSatisfy` T.isInfixOf "</html>"

  it "includes module name in title" $ do
    let doc = ModuleDoc
          { modDocName = "MyModule"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = []
          , modDocSourceFile = "test.crisp"
          }
    let html = renderHtml doc
    html `shouldSatisfy` T.isInfixOf "<title>MyModule"

  it "includes CSS styles" $ do
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = []
          , modDocSourceFile = "test.crisp"
          }
    let html = renderHtml doc
    html `shouldSatisfy` T.isInfixOf "<style>"
    html `shouldSatisfy` T.isInfixOf "</style>"

  it "escapes HTML special characters" $ do
    let fn = FunctionDoc
          { fnDocName = "compare"
          , fnDocSignature = "fn compare(x: Int, y: Int) -> Bool"
          , fnDocSummary = Just "Check if x < y"
          , fnDocDescription = Nothing
          , fnDocExamples = []
          , fnDocSeeAlso = []
          }
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = [ItemFunction fn]
          , modDocSourceFile = "test.crisp"
          }
    let html = renderHtml doc
    html `shouldSatisfy` T.isInfixOf "&lt;"

  it "renders navigation links" $ do
    let fn = FunctionDoc "testFn" "fn testFn() -> Unit" Nothing Nothing [] []
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = [ItemFunction fn]
          , modDocSourceFile = "test.crisp"
          }
    let html = renderHtml doc
    html `shouldSatisfy` T.isInfixOf "<nav>"
    html `shouldSatisfy` T.isInfixOf "href=\"#testfn\""

  it "renders footer with source file" $ do
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = []
          , modDocSourceFile = "src/Test.crisp"
          }
    let html = renderHtml doc
    html `shouldSatisfy` T.isInfixOf "<footer>"
    html `shouldSatisfy` T.isInfixOf "src/Test.crisp"

-- =============================================================================
-- DocFormat Tests
-- =============================================================================

docFormatTests :: Spec
docFormatTests = describe "DocFormat" $ do
  it "Markdown format renders markdown" $ do
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = []
          , modDocSourceFile = "test.crisp"
          }
    let output = renderModuleDoc Markdown doc
    output `shouldSatisfy` T.isInfixOf "# Test"
    output `shouldSatisfy` (not . T.isInfixOf "<!DOCTYPE")

  it "HTML format renders HTML" $ do
    let doc = ModuleDoc
          { modDocName = "Test"
          , modDocSummary = Nothing
          , modDocDescription = Nothing
          , modDocItems = []
          , modDocSourceFile = "test.crisp"
          }
    let output = renderModuleDoc HTML doc
    output `shouldSatisfy` T.isInfixOf "<!DOCTYPE html>"
    output `shouldSatisfy` (not . T.isInfixOf "# Test")

  it "DocFormat equality works" $ do
    Markdown `shouldBe` Markdown
    HTML `shouldBe` HTML
    Markdown `shouldNotBe` HTML
