{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Module.ModuleSpec
-- Description : Module system test suite
--
-- Tests for the Crisp module system including import parsing,
-- name resolution, visibility, and circular import detection.

module Crisp.Module.ModuleSpec (spec) where

import Test.Hspec

import Crisp.Module.Types
import Crisp.Module.Import
import Crisp.Module.Interface

import Data.Either (isRight, isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Helper to check for success
shouldSucceed :: (Show a, Show b) => Either a b -> Expectation
shouldSucceed result = result `shouldSatisfy` isRight

-- | Helper to check for failure
shouldFail :: (Show a, Show b) => Either a b -> Expectation
shouldFail result = result `shouldSatisfy` isLeft

spec :: Spec
spec = do
  importParsingTests
  importFormTests
  resolutionTests
  visibilityTests
  cycleDetectionTests
  interfaceTests
  modulePathTests
  qualifiedNameTests
  reexportTests
  edgeCaseTests

-- =============================================================================
-- Import Parsing Tests
-- =============================================================================

importParsingTests :: Spec
importParsingTests = describe "import parsing" $ do

  describe "module paths" $ do
    it "parses simple module path" $ do
      parseModulePath "Foo" `shouldBe` Right (ModPath ["Foo"])

    it "parses dotted module path" $ do
      parseModulePath "Foo.Bar" `shouldBe` Right (ModPath ["Foo", "Bar"])

    it "parses long module path" $ do
      parseModulePath "Foo.Bar.Baz.Qux" `shouldBe` Right (ModPath ["Foo", "Bar", "Baz", "Qux"])

    it "rejects empty path" $ do
      shouldFail $ parseModulePath ""

    it "rejects trailing dot" $ do
      shouldFail $ parseModulePath "Foo."

    it "rejects leading dot" $ do
      shouldFail $ parseModulePath ".Foo"

    it "rejects lowercase segment" $ do
      shouldFail $ parseModulePath "foo"

    it "rejects lowercase in path" $ do
      shouldFail $ parseModulePath "Foo.bar"

  describe "import declarations" $ do
    it "parses unqualified import" $ do
      let expected = ImportDecl (ModPath ["M"]) Unqualified
      parseImport "import M" `shouldBe` Right expected

    it "parses qualified import with alias" $ do
      let expected = ImportDecl (ModPath ["Long", "Name"]) (Qualified "L")
      parseImport "import qualified Long.Name as L" `shouldBe` Right expected

    it "parses selective import" $ do
      let expected = ImportDecl (ModPath ["M"]) (Selective (Set.fromList ["foo", "bar"]))
      parseImport "import M (foo, bar)" `shouldBe` Right expected

    it "parses hiding import" $ do
      let expected = ImportDecl (ModPath ["M"]) (Hiding (Set.fromList ["secret"]))
      parseImport "import M hiding (secret)" `shouldBe` Right expected

    it "parses selective with single item" $ do
      let expected = ImportDecl (ModPath ["M"]) (Selective (Set.fromList ["only"]))
      parseImport "import M (only)" `shouldBe` Right expected

    it "parses hiding with multiple items" $ do
      let expected = ImportDecl (ModPath ["M"]) (Hiding (Set.fromList ["a", "b", "c"]))
      parseImport "import M hiding (a, b, c)" `shouldBe` Right expected

    it "parses qualified with dotted path" $ do
      let expected = ImportDecl (ModPath ["Core", "Prelude"]) (Qualified "P")
      parseImport "import qualified Core.Prelude as P" `shouldBe` Right expected

    it "rejects qualified without alias" $ do
      shouldFail $ parseImport "import qualified M"

    it "rejects import without module" $ do
      shouldFail $ parseImport "import"

-- =============================================================================
-- Import Form Tests
-- =============================================================================

importFormTests :: Spec
importFormTests = describe "import forms" $ do

  describe "unqualified imports" $ do
    it "creates Unqualified form" $ do
      importForm Unqualified `shouldBe` "unqualified"

    it "brings all names into scope unqualified" $ do
      let form = Unqualified
      importsName form "foo" `shouldBe` True
      importsName form "bar" `shouldBe` True

  describe "qualified imports" $ do
    it "creates Qualified form with alias" $ do
      importForm (Qualified "M") `shouldBe` "qualified as M"

    it "requires qualifier prefix" $ do
      let form = Qualified "M"
      requiresQualifier form `shouldBe` True

  describe "selective imports" $ do
    it "only imports specified names" $ do
      let form = Selective (Set.fromList ["foo", "bar"])
      importsName form "foo" `shouldBe` True
      importsName form "bar" `shouldBe` True
      importsName form "baz" `shouldBe` False

    it "handles empty selection" $ do
      let form = Selective Set.empty
      importsName form "anything" `shouldBe` False

  describe "hiding imports" $ do
    it "imports all except hidden" $ do
      let form = Hiding (Set.fromList ["secret"])
      hidesName form "secret" `shouldBe` True
      hidesName form "public" `shouldBe` False

    it "handles multiple hidden names" $ do
      let form = Hiding (Set.fromList ["a", "b", "c"])
      hidesName form "a" `shouldBe` True
      hidesName form "b" `shouldBe` True
      hidesName form "c" `shouldBe` True
      hidesName form "d" `shouldBe` False

-- =============================================================================
-- Resolution Tests
-- =============================================================================

resolutionTests :: Spec
resolutionTests = describe "name resolution" $ do

  describe "unqualified resolution" $ do
    it "resolves unqualified imported names" $ do
      let imports = [ImportDecl (ModPath ["M"]) (Selective (Set.fromList ["foo"]))]
          available = Map.singleton (ModPath ["M"]) (Set.fromList ["foo", "bar"])
      resolveName imports available "foo" `shouldBe` Just (ModPath ["M"], "foo")

    it "fails for non-imported names" $ do
      let imports = [ImportDecl (ModPath ["M"]) (Selective (Set.fromList ["foo"]))]
          available = Map.singleton (ModPath ["M"]) (Set.fromList ["foo", "bar"])
      resolveName imports available "bar" `shouldBe` Nothing

    it "resolves hidden names correctly" $ do
      let imports = [ImportDecl (ModPath ["M"]) (Hiding (Set.fromList ["secret"]))]
          available = Map.singleton (ModPath ["M"]) (Set.fromList ["public", "secret"])
      resolveName imports available "public" `shouldBe` Just (ModPath ["M"], "public")
      resolveName imports available "secret" `shouldBe` Nothing

  describe "qualified resolution" $ do
    it "resolves qualified names" $ do
      let imports = [ImportDecl (ModPath ["Long", "Name"]) (Qualified "L")]
          available = Map.singleton (ModPath ["Long", "Name"]) (Set.fromList ["func"])
      resolveQualifiedName imports available "L" "func"
        `shouldBe` Just (ModPath ["Long", "Name"], "func")

    it "fails for wrong qualifier" $ do
      let imports = [ImportDecl (ModPath ["Long", "Name"]) (Qualified "L")]
          available = Map.singleton (ModPath ["Long", "Name"]) (Set.fromList ["func"])
      resolveQualifiedName imports available "M" "func" `shouldBe` Nothing

    it "fails for non-existent name" $ do
      let imports = [ImportDecl (ModPath ["M"]) (Qualified "M")]
          available = Map.singleton (ModPath ["M"]) (Set.fromList ["foo"])
      resolveQualifiedName imports available "M" "bar" `shouldBe` Nothing

  describe "ambiguity detection" $ do
    it "detects ambiguous imports" $ do
      let imports = [ ImportDecl (ModPath ["A"]) Unqualified
                    , ImportDecl (ModPath ["B"]) Unqualified
                    ]
          available = Map.fromList
            [ (ModPath ["A"], Set.fromList ["foo"])
            , (ModPath ["B"], Set.fromList ["foo"])
            ]
      checkAmbiguity imports available "foo" `shouldBe`
        Just [ModPath ["A"], ModPath ["B"]]

    it "returns Nothing for unambiguous names" $ do
      let imports = [ ImportDecl (ModPath ["A"]) Unqualified
                    , ImportDecl (ModPath ["B"]) Unqualified
                    ]
          available = Map.fromList
            [ (ModPath ["A"], Set.fromList ["foo"])
            , (ModPath ["B"], Set.fromList ["bar"])
            ]
      checkAmbiguity imports available "foo" `shouldBe` Nothing

-- =============================================================================
-- Visibility Tests
-- =============================================================================

visibilityTests :: Spec
visibilityTests = describe "visibility" $ do

  describe "export lists" $ do
    it "exports declared names" $ do
      let exports = ExportList (Set.fromList ["public_fn"])
      visibility exports "public_fn" `shouldBe` Public
      visibility exports "private_fn" `shouldBe` Private

    it "exports all when no export list" $ do
      let exports = ExportAll
      visibility exports "any_fn" `shouldBe` Public
      visibility exports "another_fn" `shouldBe` Public

    it "handles empty export list" $ do
      let exports = ExportList Set.empty
      visibility exports "anything" `shouldBe` Private

  describe "export forms" $ do
    it "exports single names" $ do
      let exports = ExportList (Set.fromList ["foo"])
      isExported exports "foo" `shouldBe` True
      isExported exports "bar" `shouldBe` False

    it "exports multiple names" $ do
      let exports = ExportList (Set.fromList ["a", "b", "c"])
      isExported exports "a" `shouldBe` True
      isExported exports "b" `shouldBe` True
      isExported exports "c" `shouldBe` True
      isExported exports "d" `shouldBe` False

    it "handles ExportAll" $ do
      isExported ExportAll "anything" `shouldBe` True

-- =============================================================================
-- Circular Import Detection Tests
-- =============================================================================

cycleDetectionTests :: Spec
cycleDetectionTests = describe "cycle detection" $ do

  describe "simple cycles" $ do
    it "detects direct circular import" $ do
      let deps = Map.fromList
            [ (ModPath ["A"], [ModPath ["B"]])
            , (ModPath ["B"], [ModPath ["A"]])
            ]
      checkCycles deps `shouldBe` Just [ModPath ["A"], ModPath ["B"], ModPath ["A"]]

    it "returns Nothing for acyclic imports" $ do
      let deps = Map.fromList
            [ (ModPath ["A"], [ModPath ["B"]])
            , (ModPath ["B"], [ModPath ["C"]])
            , (ModPath ["C"], [])
            ]
      checkCycles deps `shouldBe` Nothing

  describe "complex cycles" $ do
    it "detects transitive cycle" $ do
      let deps = Map.fromList
            [ (ModPath ["A"], [ModPath ["B"]])
            , (ModPath ["B"], [ModPath ["C"]])
            , (ModPath ["C"], [ModPath ["A"]])
            ]
      case checkCycles deps of
        Just cycle' -> length cycle' `shouldSatisfy` (>= 3)
        Nothing -> expectationFailure "Expected cycle detection"

    it "detects self-import" $ do
      let deps = Map.fromList
            [ (ModPath ["A"], [ModPath ["A"]])
            ]
      checkCycles deps `shouldBe` Just [ModPath ["A"], ModPath ["A"]]

    it "handles diamond dependencies (no cycle)" $ do
      let deps = Map.fromList
            [ (ModPath ["A"], [ModPath ["B"], ModPath ["C"]])
            , (ModPath ["B"], [ModPath ["D"]])
            , (ModPath ["C"], [ModPath ["D"]])
            , (ModPath ["D"], [])
            ]
      checkCycles deps `shouldBe` Nothing

    it "handles empty dependency graph" $ do
      checkCycles Map.empty `shouldBe` Nothing

-- =============================================================================
-- Interface Tests
-- =============================================================================

interfaceTests :: Spec
interfaceTests = describe "interface files" $ do

  describe "interface generation" $ do
    it "generates interface from module" $ do
      let modInfo = ModuleInfo
            { modInfoPath = ModPath ["Test"]
            , modInfoExports = ExportList (Set.fromList ["foo", "bar"])
            , modInfoTypes = Map.fromList [("Foo", TypeInfo "Foo" KindType)]
            , modInfoFunctions = Map.fromList [("foo", "Int -> Int")]
            }
          iface = generateInterface modInfo
      interfaceExports iface `shouldBe` Set.fromList ["foo", "bar"]

    it "includes type information" $ do
      let modInfo = ModuleInfo
            { modInfoPath = ModPath ["Test"]
            , modInfoExports = ExportAll
            , modInfoTypes = Map.fromList
                [ ("Foo", TypeInfo "Foo" KindType)
                , ("Bar", TypeInfo "Bar" (KindArrow KindType KindType))
                ]
            , modInfoFunctions = Map.empty
            }
          iface = generateInterface modInfo
      Map.size (interfaceTypes iface) `shouldBe` 2

    it "includes function signatures" $ do
      let modInfo = ModuleInfo
            { modInfoPath = ModPath ["Test"]
            , modInfoExports = ExportAll
            , modInfoTypes = Map.empty
            , modInfoFunctions = Map.fromList [("add", "Int -> Int -> Int")]
            }
          iface = generateInterface modInfo
      Map.lookup "add" (interfaceFunctions iface) `shouldBe` Just "Int -> Int -> Int"

  describe "interface serialization" $ do
    it "serializes interface to JSON" $ do
      let iface = Interface
            { interfacePath = ModPath ["Test"]
            , interfaceExports = Set.fromList ["foo"]
            , interfaceTypes = Map.empty
            , interfaceFunctions = Map.singleton "foo" "Int -> Int"
            }
      shouldSucceed $ serializeInterface iface

    it "deserializes interface from JSON" $ do
      let iface = Interface
            { interfacePath = ModPath ["Test"]
            , interfaceExports = Set.fromList ["foo"]
            , interfaceTypes = Map.empty
            , interfaceFunctions = Map.singleton "foo" "Int -> Int"
            }
      case serializeInterface iface of
        Right json -> deserializeInterface json `shouldBe` Right iface
        Left err -> expectationFailure $ "Serialization failed: " ++ T.unpack err

    it "round-trips interface" $ do
      let iface = Interface
            { interfacePath = ModPath ["Core", "Prelude"]
            , interfaceExports = Set.fromList ["map", "filter", "fold"]
            , interfaceTypes = Map.fromList [("List", TypeInfo "List" (KindArrow KindType KindType))]
            , interfaceFunctions = Map.fromList
                [ ("map", "forall a b. (a -> b) -> List a -> List b")
                , ("filter", "forall a. (a -> Bool) -> List a -> List a")
                ]
            }
      case serializeInterface iface >>= deserializeInterface of
        Right iface' -> iface' `shouldBe` iface
        Left err -> expectationFailure $ "Round-trip failed: " ++ T.unpack err

-- =============================================================================
-- Module Path Tests
-- =============================================================================

modulePathTests :: Spec
modulePathTests = describe "module paths" $ do

  describe "path operations" $ do
    it "converts path to file path" $ do
      modulePathToFile (ModPath ["Core", "Prelude"]) `shouldBe` "Core/Prelude.crisp"

    it "converts path to interface file path" $ do
      modulePathToInterfaceFile (ModPath ["Core", "Prelude"]) `shouldBe` "Core/Prelude.crispi"

    it "converts file path to module path" $ do
      fileToModulePath "Core/Prelude.crisp" `shouldBe` Right (ModPath ["Core", "Prelude"])

    it "handles single segment paths" $ do
      modulePathToFile (ModPath ["Main"]) `shouldBe` "Main.crisp"

    it "renders path as text" $ do
      renderModulePath (ModPath ["Core", "Prelude"]) `shouldBe` "Core.Prelude"

  describe "path comparison" $ do
    it "compares equal paths" $ do
      ModPath ["A", "B"] `shouldBe` ModPath ["A", "B"]

    it "compares different paths" $ do
      ModPath ["A", "B"] `shouldNotBe` ModPath ["A", "C"]

    it "orders paths lexicographically" $ do
      ModPath ["A"] `shouldSatisfy` (< ModPath ["B"])
      ModPath ["A", "A"] `shouldSatisfy` (< ModPath ["A", "B"])

-- =============================================================================
-- Qualified Name Tests
-- =============================================================================

qualifiedNameTests :: Spec
qualifiedNameTests = describe "qualified names" $ do

  describe "qualified name parsing" $ do
    it "parses simple qualified name" $ do
      parseQualifiedName "M.foo" `shouldBe` Right ("M", "foo")

    it "parses multi-segment qualifier" $ do
      parseQualifiedName "Core.Prelude.map" `shouldBe` Right ("Core.Prelude", "map")

    it "rejects unqualified name" $ do
      shouldFail $ parseQualifiedName "foo"

    it "rejects trailing dot" $ do
      shouldFail $ parseQualifiedName "M."

  describe "qualified name construction" $ do
    it "constructs qualified name" $ do
      makeQualifiedName (ModPath ["Core"]) "foo" `shouldBe` "Core.foo"

    it "constructs with long path" $ do
      makeQualifiedName (ModPath ["Core", "Data", "List"]) "map"
        `shouldBe` "Core.Data.List.map"

-- =============================================================================
-- Re-export Tests
-- =============================================================================

reexportTests :: Spec
reexportTests = describe "re-exports" $ do

  describe "re-export declarations" $ do
    it "parses module re-export" $ do
      let expected = ReExport (ModPath ["Core", "Prelude"]) ReExportAll
      parseReExport "export module Core.Prelude" `shouldBe` Right expected

    it "parses selective re-export" $ do
      let expected = ReExport (ModPath ["M"]) (ReExportSome (Set.fromList ["foo", "bar"]))
      parseReExport "export M (foo, bar)" `shouldBe` Right expected

  describe "re-export resolution" $ do
    it "includes re-exported names in exports" $ do
      let reexports = [ReExport (ModPath ["M"]) ReExportAll]
          available = Map.singleton (ModPath ["M"]) (Set.fromList ["foo", "bar"])
      resolveReExports reexports available `shouldBe` Set.fromList ["foo", "bar"]

    it "includes selective re-exports" $ do
      let reexports = [ReExport (ModPath ["M"]) (ReExportSome (Set.fromList ["foo"]))]
          available = Map.singleton (ModPath ["M"]) (Set.fromList ["foo", "bar"])
      resolveReExports reexports available `shouldBe` Set.fromList ["foo"]

-- =============================================================================
-- Edge Cases
-- =============================================================================

edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do

  describe "unicode handling" $ do
    it "handles unicode in module paths" $ do
      -- Module paths should be ASCII identifiers only
      shouldFail $ parseModulePath "Foo.Bär"

  describe "special characters" $ do
    it "handles underscores in names" $ do
      let form = Selective (Set.fromList ["foo_bar"])
      importsName form "foo_bar" `shouldBe` True

    it "handles primes in names" $ do
      let form = Selective (Set.fromList ["x'"])
      importsName form "x'" `shouldBe` True

  describe "empty cases" $ do
    it "handles module with no exports" $ do
      let exports = ExportList Set.empty
      visibility exports "anything" `shouldBe` Private

    it "handles module with no imports" $ do
      resolveName [] Map.empty "foo" `shouldBe` Nothing

  describe "case sensitivity" $ do
    it "distinguishes case in names" $ do
      let form = Selective (Set.fromList ["Foo"])
      importsName form "Foo" `shouldBe` True
      importsName form "foo" `shouldBe` False

    it "distinguishes case in module paths" $ do
      ModPath ["Foo"] `shouldNotBe` ModPath ["FOO"]
