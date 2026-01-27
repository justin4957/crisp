{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Codegen.WasmSpec
-- Description : Tests for effect-to-import mapping in Wasm codegen
--
-- Tests for generating Wasm imports from effect definitions and
-- resolving effect operations to import indices.

module Crisp.Codegen.WasmSpec (spec) where

import Test.Hspec

import Crisp.Codegen.Wasm
import Crisp.IR.TypedIR
import Crisp.Core.Term (Type(..), EffectRow(..), Term(..), simpleType)

import Data.Text (Text)
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  effectImportTests
  typeConversionTests
  importMapTests
  integrationTests

-- =============================================================================
-- Effect Import Generation Tests
-- =============================================================================

effectImportTests :: Spec
effectImportTests = describe "generateEffectImports" $ do
  it "generates no imports for module without effects" $ do
    let tirMod = newModule "Test" Nothing []
    generateEffectImports tirMod `shouldBe` []

  it "generates import for single effect operation" $ do
    let effect = mkEffect "IO" [("print", simpleType "String", simpleType "Unit")]
    let tirMod = newModule "Test" Nothing [TirEffect effect]
    let imports = generateEffectImports tirMod
    length imports `shouldBe` 1
    wiModule (head imports) `shouldBe` "IO"
    wiName (head imports) `shouldBe` "print"

  it "generates imports for multiple operations" $ do
    let effect = mkEffect "IO"
          [ ("read", simpleType "Unit", simpleType "String")
          , ("write", simpleType "String", simpleType "Unit")
          ]
    let tirMod = newModule "Test" Nothing [TirEffect effect]
    let imports = generateEffectImports tirMod
    length imports `shouldBe` 2
    map wiName imports `shouldBe` ["read", "write"]

  it "generates imports for multiple effects" $ do
    let ioEffect = mkEffect "IO" [("print", simpleType "String", simpleType "Unit")]
    let stateEffect = mkEffect "State"
          [ ("get", simpleType "Unit", simpleType "Int")
          , ("put", simpleType "Int", simpleType "Unit")
          ]
    let tirMod = newModule "Test" Nothing [TirEffect ioEffect, TirEffect stateEffect]
    let imports = generateEffectImports tirMod
    length imports `shouldBe` 3
    map wiModule imports `shouldBe` ["IO", "State", "State"]

  it "ignores non-effect definitions" $ do
    let fn = TirFn $ TypedFunction
          "main"
          []
          []
          (simpleType "Int")
          EffEmpty
          (TmVar "x" 0)
          []
    let tirMod = newModule "Test" Nothing [fn]
    generateEffectImports tirMod `shouldBe` []

-- =============================================================================
-- Type Conversion Tests
-- =============================================================================

typeConversionTests :: Spec
typeConversionTests = describe "typeToWasmValType" $ do
  it "converts Int to i32" $ do
    typeToWasmValType (simpleType "Int") `shouldBe` WasmI32

  it "converts Int32 to i32" $ do
    typeToWasmValType (simpleType "Int32") `shouldBe` WasmI32

  it "converts Int64 to i64" $ do
    typeToWasmValType (simpleType "Int64") `shouldBe` WasmI64

  it "converts Float to f32" $ do
    typeToWasmValType (simpleType "Float") `shouldBe` WasmF32

  it "converts Float32 to f32" $ do
    typeToWasmValType (simpleType "Float32") `shouldBe` WasmF32

  it "converts Float64 to f64" $ do
    typeToWasmValType (simpleType "Float64") `shouldBe` WasmF64

  it "converts Double to f64" $ do
    typeToWasmValType (simpleType "Double") `shouldBe` WasmF64

  it "converts Bool to i32" $ do
    typeToWasmValType (simpleType "Bool") `shouldBe` WasmI32

  it "converts Char to i32" $ do
    typeToWasmValType (simpleType "Char") `shouldBe` WasmI32

  it "converts Unit to i32" $ do
    typeToWasmValType (simpleType "Unit") `shouldBe` WasmI32

  it "converts String to i32 (pointer)" $ do
    typeToWasmValType (simpleType "String") `shouldBe` WasmI32

  it "converts ADT to i32 (pointer)" $ do
    typeToWasmValType (TyCon "Option" [simpleType "Int"]) `shouldBe` WasmI32

  it "converts function type to i32 (function index)" $ do
    let fnType = TyPi "x" (simpleType "Int") EffEmpty (simpleType "Int")
    typeToWasmValType fnType `shouldBe` WasmI32

  it "converts type variable to i32 (default)" $ do
    typeToWasmValType (TyVar "A" 0) `shouldBe` WasmI32

-- =============================================================================
-- Import Map Tests
-- =============================================================================

importMapTests :: Spec
importMapTests = describe "lookupEffectImport" $ do
  it "finds import by effect and operation name" $ do
    let effect = mkEffect "IO" [("print", simpleType "String", simpleType "Unit")]
    let tirMod = newModule "Test" Nothing [TirEffect effect]
    let imports = generateEffectImports tirMod
    let importMap = buildImportMap imports
    lookupEffectImport "IO" "print" importMap `shouldBe` Just 0

  it "returns Nothing for unknown effect" $ do
    let effect = mkEffect "IO" [("print", simpleType "String", simpleType "Unit")]
    let tirMod = newModule "Test" Nothing [TirEffect effect]
    let imports = generateEffectImports tirMod
    let importMap = buildImportMap imports
    lookupEffectImport "State" "get" importMap `shouldBe` Nothing

  it "returns Nothing for unknown operation" $ do
    let effect = mkEffect "IO" [("print", simpleType "String", simpleType "Unit")]
    let tirMod = newModule "Test" Nothing [TirEffect effect]
    let imports = generateEffectImports tirMod
    let importMap = buildImportMap imports
    lookupEffectImport "IO" "read" importMap `shouldBe` Nothing

  it "assigns sequential indices to multiple imports" $ do
    let effect = mkEffect "State"
          [ ("get", simpleType "Unit", simpleType "Int")
          , ("put", simpleType "Int", simpleType "Unit")
          ]
    let tirMod = newModule "Test" Nothing [TirEffect effect]
    let imports = generateEffectImports tirMod
    let importMap = buildImportMap imports
    lookupEffectImport "State" "get" importMap `shouldBe` Just 0
    lookupEffectImport "State" "put" importMap `shouldBe` Just 1

  it "handles multiple effects with correct indices" $ do
    let ioEffect = mkEffect "IO" [("print", simpleType "String", simpleType "Unit")]
    let stateEffect = mkEffect "State" [("get", simpleType "Unit", simpleType "Int")]
    let tirMod = newModule "Test" Nothing [TirEffect ioEffect, TirEffect stateEffect]
    let imports = generateEffectImports tirMod
    let importMap = buildImportMap imports
    lookupEffectImport "IO" "print" importMap `shouldBe` Just 0
    lookupEffectImport "State" "get" importMap `shouldBe` Just 1

-- =============================================================================
-- Integration Tests
-- =============================================================================

integrationTests :: Spec
integrationTests = describe "compileToWasm integration" $ do
  it "compiles module with effect to Wasm with imports" $ do
    let effect = mkEffect "IO" [("print", simpleType "String", simpleType "Unit")]
    let tirMod = newModule "Test" Nothing [TirEffect effect]
    case compileToWasm tirMod of
      Right wasmMod -> do
        length (wasmImports wasmMod) `shouldBe` 1
        wiModule (head (wasmImports wasmMod)) `shouldBe` "IO"
      Left err -> expectationFailure $ "Compilation failed: " ++ show err

  it "import type signature matches operation types" $ do
    let effect = mkEffect "IO" [("read", simpleType "Unit", simpleType "String")]
    let tirMod = newModule "Test" Nothing [TirEffect effect]
    let imports = generateEffectImports tirMod
    let WasmType params results = wiType (head imports)
    params `shouldBe` [WasmI32]   -- Unit -> i32
    results `shouldBe` [WasmI32]  -- String -> i32 (pointer)

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- | Create an effect definition with operations
mkEffect :: Text -> [(Text, Type, Type)] -> TypedEffectDef
mkEffect name ops = TypedEffectDef
  { teName = name
  , teOperations = map mkOp ops
  }
  where
    mkOp (opName, inputTy, outputTy) = TypedOperation
      { toName = opName
      , toInputType = inputTy
      , toOutputType = outputTy
      }

-- | Build import map from imports (wrapper for testing)
buildImportMap :: [WasmImport] -> EffectImportMap
buildImportMap imports =
  Map.fromList [((wiModule imp, wiName imp), idx) | (idx, imp) <- zip [0..] imports]
