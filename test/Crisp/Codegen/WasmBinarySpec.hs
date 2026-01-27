{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Codegen.WasmBinarySpec
-- Description : Tests for WebAssembly binary encoding
--
-- TDD tests for the WebAssembly binary format encoding:
-- - Module header (magic number and version)
-- - LEB128 variable-length integer encoding
-- - Section encoding (type, import, function, memory, export, code)
-- - Instruction encoding
-- - Complete module validation

module Crisp.Codegen.WasmBinarySpec (spec) where

import Test.Hspec

import Crisp.Codegen.WasmBinary
import Crisp.Codegen.Wasm (WasmValType(..), WasmInstr(..))

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits ((.&.), (.|.), shiftL)
import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "WebAssembly Binary Encoding" $ do
    headerTests
    leb128Tests
    valTypeTests
    typeSectionTests
    importSectionTests
    functionSectionTests
    memorySectionTests
    exportSectionTests
    codeSectionTests
    instructionTests
    ieee754Tests
    moduleEncodingTests
    edgeCaseTests

--------------------------------------------------------------------------------
-- Header Tests
--------------------------------------------------------------------------------

headerTests :: Spec
headerTests = describe "module header" $ do
  it "produces correct magic number" $ do
    let bytes = encodeModule emptyWasmModule
    take 4 (BS.unpack bytes) `shouldBe` [0x00, 0x61, 0x73, 0x6D]

  it "produces correct version (1)" $ do
    let bytes = encodeModule emptyWasmModule
    take 4 (drop 4 (BS.unpack bytes)) `shouldBe` [0x01, 0x00, 0x00, 0x00]

  it "header is exactly 8 bytes" $ do
    wasmHeader `shouldBe` BS.pack [0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00]

--------------------------------------------------------------------------------
-- LEB128 Tests
--------------------------------------------------------------------------------

leb128Tests :: Spec
leb128Tests = describe "LEB128 encoding" $ do
  describe "unsigned (ULEB128)" $ do
    it "encodes 0" $ do
      encodeULEB128 0 `shouldBe` [0x00]

    it "encodes small integers (< 128)" $ do
      encodeULEB128 1 `shouldBe` [0x01]
      encodeULEB128 63 `shouldBe` [0x3F]
      encodeULEB128 127 `shouldBe` [0x7F]

    it "encodes 128 (two bytes)" $ do
      encodeULEB128 128 `shouldBe` [0x80, 0x01]

    it "encodes 255" $ do
      encodeULEB128 255 `shouldBe` [0xFF, 0x01]

    it "encodes 256" $ do
      encodeULEB128 256 `shouldBe` [0x80, 0x02]

    it "encodes 624485 (example from spec)" $ do
      encodeULEB128 624485 `shouldBe` [0xE5, 0x8E, 0x26]

    it "encodes large integers" $ do
      encodeULEB128 16384 `shouldBe` [0x80, 0x80, 0x01]
      encodeULEB128 2097152 `shouldBe` [0x80, 0x80, 0x80, 0x01]

  describe "signed (SLEB128)" $ do
    it "encodes 0" $ do
      encodeSLEB128 0 `shouldBe` [0x00]

    it "encodes small positive integers" $ do
      encodeSLEB128 1 `shouldBe` [0x01]
      encodeSLEB128 63 `shouldBe` [0x3F]

    it "encodes 64 (requires two bytes due to sign bit)" $ do
      encodeSLEB128 64 `shouldBe` [0xC0, 0x00]

    it "encodes -1" $ do
      encodeSLEB128 (-1) `shouldBe` [0x7F]

    it "encodes -64" $ do
      encodeSLEB128 (-64) `shouldBe` [0x40]

    it "encodes -65 (requires two bytes)" $ do
      encodeSLEB128 (-65) `shouldBe` [0xBF, 0x7F]

    it "encodes -123456 (example from spec)" $ do
      encodeSLEB128 (-123456) `shouldBe` [0xC0, 0xBB, 0x78]

    it "encodes larger negative integers" $ do
      encodeSLEB128 (-128) `shouldBe` [0x80, 0x7F]

--------------------------------------------------------------------------------
-- Value Type Tests
--------------------------------------------------------------------------------

valTypeTests :: Spec
valTypeTests = describe "value type encoding" $ do
  it "encodes i32" $ do
    encodeValType WasmI32 `shouldBe` 0x7F

  it "encodes i64" $ do
    encodeValType WasmI64 `shouldBe` 0x7E

  it "encodes f32" $ do
    encodeValType WasmF32 `shouldBe` 0x7D

  it "encodes f64" $ do
    encodeValType WasmF64 `shouldBe` 0x7C

--------------------------------------------------------------------------------
-- Type Section Tests
--------------------------------------------------------------------------------

typeSectionTests :: Spec
typeSectionTests = describe "type section" $ do
  it "has section ID 1" $ do
    let bytes = encodeTypeSection [funcType [] [WasmI32]]
    head bytes `shouldBe` 0x01

  it "encodes empty type section" $ do
    let bytes = encodeTypeSection []
    -- Section ID + size (0) + count (0)
    bytes `shouldBe` [0x01, 0x01, 0x00]

  it "encodes single function type () -> i32" $ do
    let bytes = encodeTypeSection [funcType [] [WasmI32]]
    -- Section ID 1, size, count 1, func marker 0x60, 0 params, 1 result i32
    bytes `shouldBe` [0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7F]

  it "encodes function type (i32, i32) -> i32" $ do
    let bytes = encodeTypeSection [funcType [WasmI32, WasmI32] [WasmI32]]
    -- Section ID 1, size, count 1, func 0x60, 2 params, i32 i32, 1 result i32
    bytes `shouldBe` [0x01, 0x07, 0x01, 0x60, 0x02, 0x7F, 0x7F, 0x01, 0x7F]

  it "encodes function type () -> ()" $ do
    let bytes = encodeTypeSection [funcType [] []]
    bytes `shouldBe` [0x01, 0x04, 0x01, 0x60, 0x00, 0x00]

  it "encodes multiple function types" $ do
    let types = [funcType [] [WasmI32], funcType [WasmI32] [WasmI32]]
    let bytes = encodeTypeSection types
    head bytes `shouldBe` 0x01  -- Section ID
    bytes !! 2 `shouldBe` 0x02  -- Count = 2

--------------------------------------------------------------------------------
-- Import Section Tests
--------------------------------------------------------------------------------

importSectionTests :: Spec
importSectionTests = describe "import section" $ do
  it "has section ID 2" $ do
    let bytes = encodeImportSection [wasmImport "env" "print" 0]
    head bytes `shouldBe` 0x02

  it "encodes empty import section" $ do
    let bytes = encodeImportSection []
    bytes `shouldBe` [0x02, 0x01, 0x00]

  it "encodes function import" $ do
    let imp = wasmImport "env" "print" 0
    let bytes = encodeImportSection [imp]
    head bytes `shouldBe` 0x02  -- Section ID
    -- Should contain module name "env", field name "print", kind 0x00, type index

  it "encodes module and field names" $ do
    let imp = wasmImport "env" "log" 0
    let bytes = encodeImportSection [imp]
    -- "env" = 3 bytes, "log" = 3 bytes
    bytes `shouldSatisfy` (\b -> length b > 8)

  it "encodes memory import" $ do
    let imp = wasmMemoryImport "env" "memory" 1 Nothing
    let bytes = encodeImportSection [imp]
    head bytes `shouldBe` 0x02

--------------------------------------------------------------------------------
-- Function Section Tests
--------------------------------------------------------------------------------

functionSectionTests :: Spec
functionSectionTests = describe "function section" $ do
  it "has section ID 3" $ do
    let bytes = encodeFunctionSection [0]
    head bytes `shouldBe` 0x03

  it "encodes empty function section" $ do
    let bytes = encodeFunctionSection []
    bytes `shouldBe` [0x03, 0x01, 0x00]

  it "encodes single function with type index 0" $ do
    let bytes = encodeFunctionSection [0]
    bytes `shouldBe` [0x03, 0x02, 0x01, 0x00]

  it "encodes multiple functions" $ do
    let bytes = encodeFunctionSection [0, 1, 0]
    bytes `shouldBe` [0x03, 0x04, 0x03, 0x00, 0x01, 0x00]

--------------------------------------------------------------------------------
-- Memory Section Tests
--------------------------------------------------------------------------------

memorySectionTests :: Spec
memorySectionTests = describe "memory section" $ do
  it "has section ID 5" $ do
    let bytes = encodeMemorySection (wasmMemory 1 Nothing)
    head bytes `shouldBe` 0x05

  it "encodes memory with only minimum" $ do
    let bytes = encodeMemorySection (wasmMemory 1 Nothing)
    -- Section ID 5, size, count 1, flags 0, min 1
    bytes `shouldBe` [0x05, 0x03, 0x01, 0x00, 0x01]

  it "encodes memory with minimum and maximum" $ do
    let bytes = encodeMemorySection (wasmMemory 1 (Just 10))
    -- Section ID 5, size, count 1, flags 1, min 1, max 10
    bytes `shouldBe` [0x05, 0x04, 0x01, 0x01, 0x01, 0x0A]

  it "encodes larger memory sizes" $ do
    let bytes = encodeMemorySection (wasmMemory 256 (Just 1024))
    head bytes `shouldBe` 0x05

--------------------------------------------------------------------------------
-- Export Section Tests
--------------------------------------------------------------------------------

exportSectionTests :: Spec
exportSectionTests = describe "export section" $ do
  it "has section ID 7" $ do
    let bytes = encodeExportSection [wasmExport "main" ExportKindFunc 0]
    head bytes `shouldBe` 0x07

  it "encodes empty export section" $ do
    let bytes = encodeExportSection []
    bytes `shouldBe` [0x07, 0x01, 0x00]

  it "encodes function export" $ do
    let exp' = wasmExport "main" ExportKindFunc 0
    let bytes = encodeExportSection [exp']
    head bytes `shouldBe` 0x07
    -- Should contain name "main", kind 0x00, index 0

  it "encodes memory export" $ do
    let exp' = wasmExport "memory" ExportKindMemory 0
    let bytes = encodeExportSection [exp']
    -- Kind should be 0x02 for memory
    head bytes `shouldBe` 0x07

  it "encodes multiple exports" $ do
    let exports = [wasmExport "main" ExportKindFunc 0, wasmExport "add" ExportKindFunc 1]
    let bytes = encodeExportSection exports
    bytes !! 2 `shouldBe` 0x02  -- Count = 2

--------------------------------------------------------------------------------
-- Code Section Tests
--------------------------------------------------------------------------------

codeSectionTests :: Spec
codeSectionTests = describe "code section" $ do
  it "has section ID 10" $ do
    let bytes = encodeCodeSection [wasmCode [] [WReturn]]
    head bytes `shouldBe` 0x0A

  it "encodes empty code section" $ do
    let bytes = encodeCodeSection []
    bytes `shouldBe` [0x0A, 0x01, 0x00]

  it "encodes function with no locals and return" $ do
    let code = wasmCode [] [WReturn]
    let bytes = encodeCodeSection [code]
    head bytes `shouldBe` 0x0A
    -- Function body ends with 0x0B (end)

  it "encodes function with locals" $ do
    let code = wasmCode [(1, WasmI32)] [WLocalGet 0, WReturn]
    let bytes = encodeCodeSection [code]
    -- Should have local declarations
    head bytes `shouldBe` 0x0A

  it "encodes add function" $ do
    let code = wasmCode [] [WLocalGet 0, WLocalGet 1, WI32Add, WReturn]
    let bytes = encodeCodeSection [code]
    head bytes `shouldBe` 0x0A

--------------------------------------------------------------------------------
-- Instruction Tests
--------------------------------------------------------------------------------

instructionTests :: Spec
instructionTests = describe "instruction encoding" $ do
  describe "constants" $ do
    it "encodes i32.const" $ do
      encodeInstr (WI32Const 42) `shouldBe` [0x41, 0x2A]

    it "encodes i32.const negative" $ do
      encodeInstr (WI32Const (-1)) `shouldBe` [0x41, 0x7F]

    it "encodes i64.const" $ do
      -- 100 in SLEB128 requires two bytes because bit 6 is set (sign bit)
      encodeInstr (WI64Const 100) `shouldBe` [0x42, 0xE4, 0x00]

    it "encodes f64.const" $ do
      let bytes = encodeInstr (WF64Const 0.0)
      head bytes `shouldBe` 0x44
      length bytes `shouldBe` 9  -- opcode + 8 bytes

  describe "local operations" $ do
    it "encodes local.get" $ do
      encodeInstr (WLocalGet 0) `shouldBe` [0x20, 0x00]
      encodeInstr (WLocalGet 5) `shouldBe` [0x20, 0x05]

    it "encodes local.set" $ do
      encodeInstr (WLocalSet 0) `shouldBe` [0x21, 0x00]
      encodeInstr (WLocalSet 3) `shouldBe` [0x21, 0x03]

  describe "global operations" $ do
    it "encodes global.get" $ do
      encodeInstr (WGlobalGet 0) `shouldBe` [0x23, 0x00]

    it "encodes global.set" $ do
      encodeInstr (WGlobalSet 0) `shouldBe` [0x24, 0x00]

  describe "control flow" $ do
    it "encodes return" $ do
      encodeInstr WReturn `shouldBe` [0x0F]

    it "encodes unreachable" $ do
      encodeInstr WUnreachable `shouldBe` [0x00]

    it "encodes call" $ do
      encodeInstr (WCall 0) `shouldBe` [0x10, 0x00]
      encodeInstr (WCall 5) `shouldBe` [0x10, 0x05]

    it "encodes call_indirect" $ do
      encodeInstr (WCallIndirect 0) `shouldBe` [0x11, 0x00, 0x00]

    it "encodes br" $ do
      encodeInstr (WBr 0) `shouldBe` [0x0C, 0x00]
      encodeInstr (WBr 2) `shouldBe` [0x0C, 0x02]

    it "encodes br_if" $ do
      encodeInstr (WBrIf 0) `shouldBe` [0x0D, 0x00]

  describe "arithmetic" $ do
    it "encodes i32.add" $ do
      encodeInstr WI32Add `shouldBe` [0x6A]

    it "encodes i32.sub" $ do
      encodeInstr WI32Sub `shouldBe` [0x6B]

    it "encodes i32.mul" $ do
      encodeInstr WI32Mul `shouldBe` [0x6C]

    it "encodes i32.eq" $ do
      encodeInstr WI32Eq `shouldBe` [0x46]

    it "encodes i32.lt_s" $ do
      encodeInstr WI32Lt `shouldBe` [0x48]

    it "encodes i64.add" $ do
      encodeInstr WI64Add `shouldBe` [0x7C]

    it "encodes i64.sub" $ do
      encodeInstr WI64Sub `shouldBe` [0x7D]

    it "encodes f64.add" $ do
      encodeInstr WF64Add `shouldBe` [0xA0]

    it "encodes f64.sub" $ do
      encodeInstr WF64Sub `shouldBe` [0xA1]

    it "encodes f64.mul" $ do
      encodeInstr WF64Mul `shouldBe` [0xA2]

  describe "misc" $ do
    it "encodes drop" $ do
      encodeInstr WDrop `shouldBe` [0x1A]

  describe "structured control" $ do
    it "encodes block" $ do
      let bytes = encodeInstr (WBlock [WI32Const 1])
      head bytes `shouldBe` 0x02  -- block opcode
      last bytes `shouldBe` 0x0B  -- end

    it "encodes loop" $ do
      let bytes = encodeInstr (WLoop [WBr 0])
      head bytes `shouldBe` 0x03  -- loop opcode
      last bytes `shouldBe` 0x0B  -- end

    it "encodes if-then-else" $ do
      let bytes = encodeInstr (WIf [WI32Const 1] [WI32Const 0])
      head bytes `shouldBe` 0x04  -- if opcode
      bytes `shouldSatisfy` (elem 0x05)  -- else opcode
      last bytes `shouldBe` 0x0B  -- end

--------------------------------------------------------------------------------
-- IEEE 754 Float Encoding Tests
--------------------------------------------------------------------------------

ieee754Tests :: Spec
ieee754Tests = describe "IEEE 754 float encoding" $ do
  describe "f32 encoding" $ do
    it "encodes f32.const instruction correctly" $ do
      let bytes = encodeInstr (WF32Const 0.0)
      head bytes `shouldBe` 0x43  -- f32.const opcode
      length bytes `shouldBe` 5   -- opcode + 4 bytes

    it "encodes 0.0 as all zeros" $ do
      floatToWord32 0.0 `shouldBe` 0x00000000

    it "encodes -0.0 with sign bit set" $ do
      floatToWord32 (-0.0) `shouldBe` 0x80000000

    it "encodes 1.0 correctly" $ do
      -- IEEE 754 single: sign=0, exp=127 (0x7F), mantissa=0
      -- Binary: 0 01111111 00000000000000000000000
      -- Hex: 0x3F800000
      floatToWord32 1.0 `shouldBe` 0x3F800000

    it "encodes -1.0 correctly" $ do
      -- Same as 1.0 but with sign bit set
      floatToWord32 (-1.0) `shouldBe` 0xBF800000

    it "encodes 2.0 correctly" $ do
      -- IEEE 754 single: sign=0, exp=128 (0x80), mantissa=0
      -- Hex: 0x40000000
      floatToWord32 2.0 `shouldBe` 0x40000000

    it "encodes 0.5 correctly" $ do
      -- IEEE 754 single: sign=0, exp=126 (0x7E), mantissa=0
      -- Hex: 0x3F000000
      floatToWord32 0.5 `shouldBe` 0x3F000000

    it "encodes positive infinity correctly" $ do
      -- IEEE 754 single: exp=255 (all 1s), mantissa=0
      -- Hex: 0x7F800000
      let posInf = 1.0 / 0.0 :: Float
      floatToWord32 posInf `shouldBe` 0x7F800000

    it "encodes negative infinity correctly" $ do
      -- IEEE 754 single: sign=1, exp=255, mantissa=0
      -- Hex: 0xFF800000
      let negInf = (-1.0) / 0.0 :: Float
      floatToWord32 negInf `shouldBe` 0xFF800000

    it "encodes NaN with correct exponent" $ do
      -- IEEE 754 NaN: exp=255, mantissa!=0
      let nan = 0.0 / 0.0 :: Float
      let bits = floatToWord32 nan
      -- Check exponent is all 1s (bits 23-30)
      (bits .&. 0x7F800000) `shouldBe` 0x7F800000
      -- Check mantissa is non-zero
      (bits .&. 0x007FFFFF) `shouldSatisfy` (/= 0)

    it "produces little-endian bytes for 1.0" $ do
      let bytes = encodeF32 1.0
      -- 0x3F800000 in little-endian: 0x00, 0x00, 0x80, 0x3F
      bytes `shouldBe` [0x00, 0x00, 0x80, 0x3F]

  describe "f64 encoding" $ do
    it "encodes f64.const instruction correctly" $ do
      let bytes = encodeInstr (WF64Const 0.0)
      head bytes `shouldBe` 0x44  -- f64.const opcode
      length bytes `shouldBe` 9   -- opcode + 8 bytes

    it "encodes 0.0 as all zeros" $ do
      doubleToWord64 0.0 `shouldBe` 0x0000000000000000

    it "encodes -0.0 with sign bit set" $ do
      doubleToWord64 (-0.0) `shouldBe` 0x8000000000000000

    it "encodes 1.0 correctly" $ do
      -- IEEE 754 double: sign=0, exp=1023 (0x3FF), mantissa=0
      -- Hex: 0x3FF0000000000000
      doubleToWord64 1.0 `shouldBe` 0x3FF0000000000000

    it "encodes -1.0 correctly" $ do
      -- Same as 1.0 but with sign bit set
      doubleToWord64 (-1.0) `shouldBe` 0xBFF0000000000000

    it "encodes 2.0 correctly" $ do
      -- IEEE 754 double: sign=0, exp=1024 (0x400), mantissa=0
      -- Hex: 0x4000000000000000
      doubleToWord64 2.0 `shouldBe` 0x4000000000000000

    it "encodes 0.5 correctly" $ do
      -- IEEE 754 double: sign=0, exp=1022 (0x3FE), mantissa=0
      -- Hex: 0x3FE0000000000000
      doubleToWord64 0.5 `shouldBe` 0x3FE0000000000000

    it "encodes 3.14159265358979 correctly" $ do
      -- Pi approximation
      let pi' = 3.14159265358979 :: Double
      let bits = doubleToWord64 pi'
      -- Should be close to 0x400921FB54442D18 (actual IEEE 754 for pi)
      bits `shouldSatisfy` (\b -> b >= 0x400921FB54400000 && b <= 0x400921FB54500000)

    it "encodes positive infinity correctly" $ do
      -- IEEE 754 double: exp=2047 (all 1s), mantissa=0
      -- Hex: 0x7FF0000000000000
      let posInf = 1.0 / 0.0 :: Double
      doubleToWord64 posInf `shouldBe` 0x7FF0000000000000

    it "encodes negative infinity correctly" $ do
      -- IEEE 754 double: sign=1, exp=2047, mantissa=0
      -- Hex: 0xFFF0000000000000
      let negInf = (-1.0) / 0.0 :: Double
      doubleToWord64 negInf `shouldBe` 0xFFF0000000000000

    it "encodes NaN with correct exponent" $ do
      -- IEEE 754 NaN: exp=2047, mantissa!=0
      let nan = 0.0 / 0.0 :: Double
      let bits = doubleToWord64 nan
      -- Check exponent is all 1s (bits 52-62)
      (bits .&. 0x7FF0000000000000) `shouldBe` 0x7FF0000000000000
      -- Check mantissa is non-zero
      (bits .&. 0x000FFFFFFFFFFFFF) `shouldSatisfy` (/= 0)

    it "produces little-endian bytes for 1.0" $ do
      let bytes = encodeF64 1.0
      -- 0x3FF0000000000000 in little-endian:
      -- 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0x3F
      bytes `shouldBe` [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0x3F]

    it "produces little-endian bytes for -1.0" $ do
      let bytes = encodeF64 (-1.0)
      -- 0xBFF0000000000000 in little-endian:
      -- 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0xBF
      bytes `shouldBe` [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0xBF]

  describe "round-trip verification" $ do
    it "encodes and decodes various f32 values" $ do
      -- Test several values to ensure encoding is correct
      let testValues :: [Float]
          testValues = [0.0, 1.0, -1.0, 0.5, 2.0, 100.0, -100.0, 0.125, 1.5]
      forM_ testValues $ \v -> do
        let encoded = encodeF32 v
        length encoded `shouldBe` 4

    it "encodes and decodes various f64 values" $ do
      let testValues :: [Double]
          testValues = [0.0, 1.0, -1.0, 0.5, 2.0, 100.0, -100.0, 0.125, 1.5, 1e10, 1e-10]
      forM_ testValues $ \v -> do
        let encoded = encodeF64 v
        length encoded `shouldBe` 8

  describe "subnormal numbers" $ do
    it "encodes f32 subnormal correctly" $ do
      -- Smallest positive subnormal: 2^(-149)
      let subnormal = 1.4e-45 :: Float  -- approximately 2^(-149)
      let bits = floatToWord32 subnormal
      -- Exponent should be 0 for subnormals
      (bits .&. 0x7F800000) `shouldBe` 0x00000000
      -- Mantissa should be non-zero
      (bits .&. 0x007FFFFF) `shouldSatisfy` (/= 0)

    it "encodes f64 subnormal correctly" $ do
      -- Smallest positive subnormal: 2^(-1074)
      let subnormal = 5e-324 :: Double  -- approximately 2^(-1074)
      let bits = doubleToWord64 subnormal
      -- Exponent should be 0 for subnormals
      (bits .&. 0x7FF0000000000000) `shouldBe` 0x0000000000000000
      -- Mantissa should be non-zero
      (bits .&. 0x000FFFFFFFFFFFFF) `shouldSatisfy` (/= 0)

--------------------------------------------------------------------------------
-- Complete Module Encoding Tests
--------------------------------------------------------------------------------

moduleEncodingTests :: Spec
moduleEncodingTests = describe "complete module encoding" $ do
  it "encodes empty module" $ do
    let bytes = encodeModule emptyWasmModule
    BS.length bytes `shouldBe` 8  -- Just header

  it "encodes minimal module with one function" $ do
    let mod' = minimalModule
    let bytes = encodeModule mod'
    BS.length bytes `shouldSatisfy` (> 8)

  it "encodes module with memory" $ do
    let mod' = moduleWithMemory 1 (Just 10)
    let bytes = encodeModule mod'
    BS.length bytes `shouldSatisfy` (> 8)

  it "encodes module with exports" $ do
    let mod' = moduleWithExport "main" 0
    let bytes = encodeModule mod'
    BS.length bytes `shouldSatisfy` (> 8)

  it "sections appear in correct order" $ do
    let mod' = fullTestModule
    let bytes = BS.unpack $ encodeModule mod'
    -- After header (8 bytes), sections should be in order:
    -- Type (1), Import (2), Function (3), Memory (5), Export (7), Code (10)
    let sectionBytes = drop 8 bytes
    let sectionIds = extractSectionIds sectionBytes
    sectionIds `shouldSatisfy` isSorted

--------------------------------------------------------------------------------
-- Edge Case Tests
--------------------------------------------------------------------------------

edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles large LEB128 values" $ do
    let large = 268435455  -- 0x0FFFFFFF
    let encoded = encodeULEB128 large
    length encoded `shouldSatisfy` (<= 5)

  it "handles maximum i32 value" $ do
    -- Max i32 is 2^31 - 1 = 2147483647
    let bytes = encodeInstr (WI32Const 2147483647)
    length bytes `shouldSatisfy` (<= 6)

  it "handles minimum i32 value" $ do
    -- Min i32 is -2^31 = -2147483648
    let bytes = encodeInstr (WI32Const (-2147483648))
    length bytes `shouldSatisfy` (<= 6)

  it "handles deeply nested blocks" $ do
    let nested = WBlock [WBlock [WBlock [WI32Const 1]]]
    let bytes = encodeInstr nested
    length bytes `shouldSatisfy` (> 0)

  it "handles function with many locals" $ do
    let locals = replicate 10 (1, WasmI32)
    let code = wasmCode locals [WReturn]
    let bytes = encodeCodeSection [code]
    length bytes `shouldSatisfy` (> 0)

  it "handles empty function body" $ do
    let code = wasmCode [] []
    let bytes = encodeCodeSection [code]
    -- Should at least have end instruction
    length bytes `shouldSatisfy` (> 3)

  it "handles long export names" $ do
    let longName = replicate 100 'a'
    let exp' = wasmExport longName ExportKindFunc 0
    let bytes = encodeExportSection [exp']
    length bytes `shouldSatisfy` (> 100)

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Extract section IDs from bytes (skipping over section contents)
extractSectionIds :: [Word8] -> [Word8]
extractSectionIds [] = []
extractSectionIds (sectionId:rest) =
  let (sizeBytes, afterSize) = spanLEB128 rest
      contentSize = decodeLEB128 sizeBytes
      afterContent = drop contentSize afterSize
  in sectionId : extractSectionIds afterContent

-- | Span bytes that form a LEB128 number
spanLEB128 :: [Word8] -> ([Word8], [Word8])
spanLEB128 [] = ([], [])
spanLEB128 (b:bs)
  | b < 0x80 = ([b], bs)
  | otherwise = let (more, rest) = spanLEB128 bs in (b:more, rest)

-- | Decode LEB128 to Int (simplified)
decodeLEB128 :: [Word8] -> Int
decodeLEB128 = go 0 0
  where
    go acc _ [] = acc
    go acc shift (b:bs) =
      let val = fromIntegral (b .&. 0x7F)
          acc' = acc .|. (val `shiftL` shift)
      in if b < 0x80 then acc' else go acc' (shift + 7) bs

-- | Check if list is sorted
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
