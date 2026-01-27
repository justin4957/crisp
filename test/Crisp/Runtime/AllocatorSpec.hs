{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Runtime.AllocatorSpec
-- Description : Tests for runtime memory allocator
--
-- Tests for the bump allocator implementation including:
-- - Allocator configuration
-- - Generated Wasm function structure
-- - Allocation instruction generation
-- - Memory growth behavior

module Crisp.Runtime.AllocatorSpec (spec) where

import Test.Hspec

import Crisp.Runtime.Allocator
import Crisp.Codegen.Wasm (WasmInstr(..), WasmValType(..))
import Crisp.Codegen.WasmBinary (WasmCodeEntry(..), wasmCode)

spec :: Spec
spec = do
  configTests
  indexTests
  runtimeImportTests
  allocatorGenerationTests
  instructionHelperTests

-- =============================================================================
-- Configuration Tests
-- =============================================================================

configTests :: Spec
configTests = describe "AllocatorConfig" $ do
  it "has reasonable default heap base" $ do
    acHeapBase defaultAllocatorConfig `shouldBe` 65536  -- 64KB

  it "has reasonable initial pages" $ do
    acInitialPages defaultAllocatorConfig `shouldBe` 2  -- 128KB total

  it "has max pages limit" $ do
    acMaxPages defaultAllocatorConfig `shouldBe` Just 256  -- 16MB max

  it "has 8-byte default alignment" $ do
    acAlignment defaultAllocatorConfig `shouldBe` 8

  it "allows custom configuration" $ do
    let config = AllocatorConfig
          { acHeapBase = 4096
          , acInitialPages = 1
          , acMaxPages = Nothing
          , acAlignment = 4
          , acGrowIncrement = 2
          }
    acHeapBase config `shouldBe` 4096
    acMaxPages config `shouldBe` Nothing

-- =============================================================================
-- Index Tests
-- =============================================================================

indexTests :: Spec
indexTests = describe "function and global indices" $ do
  it "alloc function is at index 0" $ do
    allocFuncIndex `shouldBe` 0
    allocatorFunctionIndex `shouldBe` 0

  it "free function is at index 1" $ do
    freeFuncIndex `shouldBe` 1

  it "memory grow helper is at index 2" $ do
    memoryGrowFuncIndex `shouldBe` 2

  it "heap base global is at index 0" $ do
    heapBaseGlobalIndex `shouldBe` 0

  it "heap pointer global is at index 1" $ do
    heapPtrGlobalIndex `shouldBe` 1
    allocatorGlobalIndex `shouldBe` 1

-- =============================================================================
-- Runtime Import Tests
-- =============================================================================

runtimeImportTests :: Spec
runtimeImportTests = describe "runtime imports" $ do
  it "provides alloc import specification" $ do
    let imports = runtimeImports
    length imports `shouldSatisfy` (>= 1)
    let allocImport = head imports
    riModule allocImport `shouldBe` "crisp_runtime"
    riName allocImport `shouldBe` "alloc"
    riParams allocImport `shouldBe` [WasmI32]
    riResult allocImport `shouldBe` [WasmI32]

  it "provides alloc_aligned import specification" $ do
    let alignedImport = runtimeImports !! 1
    riName alignedImport `shouldBe` "alloc_aligned"
    riParams alignedImport `shouldBe` [WasmI32, WasmI32]
    riResult alignedImport `shouldBe` [WasmI32]

  it "provides free import specification" $ do
    let freeImport = runtimeImports !! 2
    riName freeImport `shouldBe` "free"
    riParams freeImport `shouldBe` [WasmI32]
    riResult freeImport `shouldBe` []

  it "generates import declarations" $ do
    let imports = generateAllocatorImport
    length imports `shouldBe` 2  -- alloc and free
    let (mod1, name1, _) = head imports
    mod1 `shouldBe` "crisp_runtime"
    name1 `shouldBe` "alloc"

-- =============================================================================
-- Allocator Generation Tests
-- =============================================================================

allocatorGenerationTests :: Spec
allocatorGenerationTests = describe "generateAllocator" $ do
  it "generates three functions" $ do
    let (funcs, _, _) = generateAllocator defaultAllocatorConfig
    length funcs `shouldBe` 3  -- alloc, free, ensure_memory

  it "generates two global variables" $ do
    let (_, globals, _) = generateAllocator defaultAllocatorConfig
    length globals `shouldBe` 2  -- heap_base, heap_ptr

  it "heap_base global is immutable" $ do
    let (_, globals, _) = generateAllocator defaultAllocatorConfig
    let (ty, mutable, _) = head globals
    ty `shouldBe` WasmI32
    mutable `shouldBe` False  -- immutable

  it "heap_ptr global is mutable" $ do
    let (_, globals, _) = generateAllocator defaultAllocatorConfig
    let (ty, mutable, _) = globals !! 1
    ty `shouldBe` WasmI32
    mutable `shouldBe` True  -- mutable

  it "globals initialized to heap base" $ do
    let config = defaultAllocatorConfig { acHeapBase = 12345 }
    let (_, globals, _) = generateAllocator config
    let (_, _, initVal1) = head globals
    let (_, _, initVal2) = globals !! 1
    initVal1 `shouldBe` 12345  -- heap_base
    initVal2 `shouldBe` 12345  -- heap_ptr starts at base

  it "returns memory configuration" $ do
    let (_, _, memory) = generateAllocator defaultAllocatorConfig
    memory `shouldBe` (2, Just 256)  -- 2 initial pages, 256 max

  it "respects custom memory configuration" $ do
    let config = defaultAllocatorConfig
          { acInitialPages = 4
          , acMaxPages = Just 100
          }
    let (_, _, memory) = generateAllocator config
    memory `shouldBe` (4, Just 100)

  it "alloc function has locals for result and new_ptr" $ do
    let (funcs, _, _) = generateAllocator defaultAllocatorConfig
    let allocFunc = head funcs
    -- Should have 2 locals (result and new_ptr)
    length (wceLocals allocFunc) `shouldBe` 2

  it "alloc function body is non-empty" $ do
    let (funcs, _, _) = generateAllocator defaultAllocatorConfig
    let allocFunc = head funcs
    length (wceBody allocFunc) `shouldSatisfy` (> 10)

  it "free function body is empty (bump allocator)" $ do
    let (funcs, _, _) = generateAllocator defaultAllocatorConfig
    let freeFunc = funcs !! 1
    wceBody freeFunc `shouldBe` []

-- =============================================================================
-- Instruction Helper Tests
-- =============================================================================

instructionHelperTests :: Spec
instructionHelperTests = describe "instruction helpers" $ do
  it "allocInstrs generates call to alloc" $ do
    allocInstrs `shouldBe` [WCall 0]

  it "allocAlignedInstrs generates call to alloc" $ do
    allocAlignedInstrs 8 `shouldBe` [WCall 0]

  it "freeInstrs generates drop (no-op for bump allocator)" $ do
    freeInstrs `shouldBe` [WDrop]

  describe "allocation pattern" $ do
    it "size + allocInstrs produces valid allocation sequence" $ do
      let size = 16
      let instrs = [WI32Const size] ++ allocInstrs
      instrs `shouldBe` [WI32Const 16, WCall 0]

    it "multiple allocations are independent" $ do
      let alloc1 = [WI32Const 8] ++ allocInstrs
      let alloc2 = [WI32Const 16] ++ allocInstrs
      alloc1 ++ alloc2 `shouldBe`
        [WI32Const 8, WCall 0, WI32Const 16, WCall 0]
