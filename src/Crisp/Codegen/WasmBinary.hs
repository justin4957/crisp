{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Codegen.WasmBinary
-- Description : WebAssembly binary format encoding
--
-- Implements WebAssembly binary encoding following the Wasm specification:
-- - Module header (magic number and version)
-- - LEB128 variable-length integer encoding
-- - Section encoding (type, import, function, memory, export, code)
-- - Instruction encoding
--
-- Reference: https://webassembly.github.io/spec/core/binary/

module Crisp.Codegen.WasmBinary
  ( -- * Module Encoding
    encodeModule
  , wasmHeader
  , emptyWasmModule
  , minimalModule
  , moduleWithMemory
  , moduleWithExport
  , fullTestModule
    -- * LEB128 Encoding
  , encodeULEB128
  , encodeSLEB128
    -- * Value Types
  , encodeValType
    -- * Function Types
  , funcType
  , WasmFuncType(..)
    -- * Sections
  , encodeTypeSection
  , encodeImportSection
  , encodeFunctionSection
  , encodeMemorySection
  , encodeExportSection
  , encodeCodeSection
    -- * Imports
  , WasmImportDesc(..)
  , wasmImport
  , wasmMemoryImport
    -- * Memory
  , WasmMemoryType(..)
  , wasmMemory
    -- * Exports
  , ExportKind(..)
  , WasmExportDesc(..)
  , wasmExport
    -- * Code
  , WasmCodeEntry(..)
  , wasmCode
    -- * Instructions
  , encodeInstr
    -- * Wasm Module Type
  , WasmBinaryModule(..)
  ) where

import Crisp.Codegen.Wasm (WasmValType(..), WasmInstr(..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits ((.&.), (.|.), shiftR, shiftL, testBit)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

--------------------------------------------------------------------------------
-- Module Structure
--------------------------------------------------------------------------------

-- | A WebAssembly binary module
data WasmBinaryModule = WasmBinaryModule
  { wbmTypes    :: ![WasmFuncType]
  , wbmImports  :: ![WasmImportDesc]
  , wbmFuncs    :: ![Int]            -- ^ Type indices
  , wbmMemory   :: !(Maybe WasmMemoryType)
  , wbmExports  :: ![WasmExportDesc]
  , wbmCode     :: ![WasmCodeEntry]
  }
  deriving stock (Eq, Show)

-- | Empty module
emptyWasmModule :: WasmBinaryModule
emptyWasmModule = WasmBinaryModule [] [] [] Nothing [] []

-- | Minimal module with one function
minimalModule :: WasmBinaryModule
minimalModule = WasmBinaryModule
  { wbmTypes = [funcType [] [WasmI32]]
  , wbmImports = []
  , wbmFuncs = [0]
  , wbmMemory = Nothing
  , wbmExports = []
  , wbmCode = [wasmCode [] [WI32Const 42, WReturn]]
  }

-- | Module with memory
moduleWithMemory :: Int -> Maybe Int -> WasmBinaryModule
moduleWithMemory minPages maxPages = emptyWasmModule
  { wbmMemory = Just (wasmMemory minPages maxPages)
  }

-- | Module with an export
moduleWithExport :: String -> Int -> WasmBinaryModule
moduleWithExport name idx = WasmBinaryModule
  { wbmTypes = [funcType [] [WasmI32]]
  , wbmImports = []
  , wbmFuncs = [0]
  , wbmMemory = Nothing
  , wbmExports = [wasmExport name ExportKindFunc idx]
  , wbmCode = [wasmCode [] [WI32Const 0, WReturn]]
  }

-- | Full test module with all sections
fullTestModule :: WasmBinaryModule
fullTestModule = WasmBinaryModule
  { wbmTypes = [funcType [] [WasmI32], funcType [WasmI32, WasmI32] [WasmI32]]
  , wbmImports = [wasmImport "env" "print" 0]
  , wbmFuncs = [0, 1]
  , wbmMemory = Just (wasmMemory 1 (Just 10))
  , wbmExports = [wasmExport "main" ExportKindFunc 1, wasmExport "memory" ExportKindMemory 0]
  , wbmCode = [wasmCode [] [WI32Const 0, WReturn], wasmCode [] [WLocalGet 0, WLocalGet 1, WI32Add, WReturn]]
  }

--------------------------------------------------------------------------------
-- Module Encoding
--------------------------------------------------------------------------------

-- | Wasm magic number and version
wasmHeader :: ByteString
wasmHeader = BS.pack [0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00]

-- | Encode a complete Wasm module
encodeModule :: WasmBinaryModule -> ByteString
encodeModule mod' = BS.concat
  [ wasmHeader
  , BS.pack $ concat
      [ if null (wbmTypes mod') then [] else encodeTypeSection (wbmTypes mod')
      , if null (wbmImports mod') then [] else encodeImportSection (wbmImports mod')
      , if null (wbmFuncs mod') then [] else encodeFunctionSection (wbmFuncs mod')
      , maybe [] (encodeMemorySection) (wbmMemory mod')
      , if null (wbmExports mod') then [] else encodeExportSection (wbmExports mod')
      , if null (wbmCode mod') then [] else encodeCodeSection (wbmCode mod')
      ]
  ]

--------------------------------------------------------------------------------
-- LEB128 Encoding
--------------------------------------------------------------------------------

-- | Encode unsigned integer as ULEB128
encodeULEB128 :: Int -> [Word8]
encodeULEB128 n
  | n < 0 = error "encodeULEB128: negative number"
  | n < 128 = [fromIntegral n]
  | otherwise =
      let byte = fromIntegral (n .&. 0x7F) .|. 0x80
          rest = n `shiftR` 7
      in byte : encodeULEB128 rest

-- | Encode signed integer as SLEB128
encodeSLEB128 :: Int -> [Word8]
encodeSLEB128 n = go n
  where
    go val =
      let byte = fromIntegral (val .&. 0x7F)
          val' = val `shiftR` 7
          -- Check if we need more bytes
          signBit = testBit byte 6
          done = (val' == 0 && not signBit) || (val' == -1 && signBit)
      in if done
         then [byte]
         else (byte .|. 0x80) : go val'

--------------------------------------------------------------------------------
-- Value Types
--------------------------------------------------------------------------------

-- | Encode a Wasm value type
encodeValType :: WasmValType -> Word8
encodeValType = \case
  WasmI32 -> 0x7F
  WasmI64 -> 0x7E
  WasmF32 -> 0x7D
  WasmF64 -> 0x7C

--------------------------------------------------------------------------------
-- Function Types
--------------------------------------------------------------------------------

-- | Wasm function type
data WasmFuncType = WasmFuncType
  { wftParams  :: ![WasmValType]
  , wftResults :: ![WasmValType]
  }
  deriving stock (Eq, Show)

-- | Create a function type
funcType :: [WasmValType] -> [WasmValType] -> WasmFuncType
funcType = WasmFuncType

-- | Encode a function type
encodeFuncType :: WasmFuncType -> [Word8]
encodeFuncType ft =
  [0x60]  -- func type marker
  ++ encodeVec (map encodeValType (wftParams ft))
  ++ encodeVec (map encodeValType (wftResults ft))

-- | Encode a vector (length-prefixed sequence)
encodeVec :: [Word8] -> [Word8]
encodeVec items = encodeULEB128 (length items) ++ items

-- | Encode a vector of multi-byte items
encodeVecOf :: (a -> [Word8]) -> [a] -> [Word8]
encodeVecOf enc items = encodeULEB128 (length items) ++ concatMap enc items

--------------------------------------------------------------------------------
-- Sections
--------------------------------------------------------------------------------

-- | Section IDs
sectionType, sectionImport, sectionFunction, sectionTable :: Word8
sectionMemory, sectionGlobal, sectionExport, sectionStart :: Word8
sectionElement, sectionCode, sectionData :: Word8

sectionType = 0x01
sectionImport = 0x02
sectionFunction = 0x03
sectionTable = 0x04
sectionMemory = 0x05
sectionGlobal = 0x06
sectionExport = 0x07
sectionStart = 0x08
sectionElement = 0x09
sectionCode = 0x0A
sectionData = 0x0B

-- | Wrap content in a section
encodeSection :: Word8 -> [Word8] -> [Word8]
encodeSection sectionId content =
  [sectionId] ++ encodeULEB128 (length content) ++ content

-- | Encode type section
encodeTypeSection :: [WasmFuncType] -> [Word8]
encodeTypeSection types =
  let content = encodeVecOf encodeFuncType types
  in encodeSection sectionType content

--------------------------------------------------------------------------------
-- Import Section
--------------------------------------------------------------------------------

-- | Import description
data WasmImportDesc = WasmImportDesc
  { widModule :: !String
  , widName   :: !String
  , widKind   :: !ImportKind
  }
  deriving stock (Eq, Show)

-- | Import kind
data ImportKind
  = ImportFunc !Int      -- ^ Type index
  | ImportTable          -- ^ Table (simplified)
  | ImportMemory !Int !(Maybe Int)  -- ^ Memory min/max
  | ImportGlobal         -- ^ Global (simplified)
  deriving stock (Eq, Show)

-- | Create a function import
wasmImport :: String -> String -> Int -> WasmImportDesc
wasmImport modName name typeIdx = WasmImportDesc modName name (ImportFunc typeIdx)

-- | Create a memory import
wasmMemoryImport :: String -> String -> Int -> Maybe Int -> WasmImportDesc
wasmMemoryImport modName name minPages maxPages =
  WasmImportDesc modName name (ImportMemory minPages maxPages)

-- | Encode import section
encodeImportSection :: [WasmImportDesc] -> [Word8]
encodeImportSection imports =
  let content = encodeVecOf encodeImport imports
  in encodeSection sectionImport content

-- | Encode a single import
encodeImport :: WasmImportDesc -> [Word8]
encodeImport imp =
  encodeName (widModule imp)
  ++ encodeName (widName imp)
  ++ encodeImportKind (widKind imp)

-- | Encode import kind
encodeImportKind :: ImportKind -> [Word8]
encodeImportKind = \case
  ImportFunc typeIdx -> [0x00] ++ encodeULEB128 typeIdx
  ImportTable -> [0x01, 0x70, 0x00, 0x00]  -- Simplified
  ImportMemory minP maxP -> [0x02] ++ encodeLimits minP maxP
  ImportGlobal -> [0x03, 0x7F, 0x00]  -- Simplified: i32 immutable

-- | Encode limits (for memory/table)
encodeLimits :: Int -> Maybe Int -> [Word8]
encodeLimits minP Nothing = [0x00] ++ encodeULEB128 minP
encodeLimits minP (Just maxP) = [0x01] ++ encodeULEB128 minP ++ encodeULEB128 maxP

-- | Encode a name (UTF-8 with length prefix)
encodeName :: String -> [Word8]
encodeName s =
  let bytes = BS.unpack $ encodeUtf8 $ T.pack s
  in encodeULEB128 (length bytes) ++ bytes

--------------------------------------------------------------------------------
-- Function Section
--------------------------------------------------------------------------------

-- | Encode function section (maps function index to type index)
encodeFunctionSection :: [Int] -> [Word8]
encodeFunctionSection typeIndices =
  let content = encodeVecOf encodeULEB128 typeIndices
  in encodeSection sectionFunction content

--------------------------------------------------------------------------------
-- Memory Section
--------------------------------------------------------------------------------

-- | Memory type
data WasmMemoryType = WasmMemoryType
  { wmtMin :: !Int
  , wmtMax :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

-- | Create a memory type
wasmMemory :: Int -> Maybe Int -> WasmMemoryType
wasmMemory = WasmMemoryType

-- | Encode memory section
encodeMemorySection :: WasmMemoryType -> [Word8]
encodeMemorySection mem =
  let content = encodeULEB128 1 ++ encodeLimits (wmtMin mem) (wmtMax mem)
  in encodeSection sectionMemory content

--------------------------------------------------------------------------------
-- Export Section
--------------------------------------------------------------------------------

-- | Export kind
data ExportKind
  = ExportKindFunc
  | ExportKindTable
  | ExportKindMemory
  | ExportKindGlobal
  deriving stock (Eq, Show)

-- | Export description
data WasmExportDesc = WasmExportDesc
  { wedName :: !String
  , wedKind :: !ExportKind
  , wedIdx  :: !Int
  }
  deriving stock (Eq, Show)

-- | Create an export
wasmExport :: String -> ExportKind -> Int -> WasmExportDesc
wasmExport = WasmExportDesc

-- | Encode export section
encodeExportSection :: [WasmExportDesc] -> [Word8]
encodeExportSection exports =
  let content = encodeVecOf encodeExport exports
  in encodeSection sectionExport content

-- | Encode a single export
encodeExport :: WasmExportDesc -> [Word8]
encodeExport exp' =
  encodeName (wedName exp')
  ++ [encodeExportKind (wedKind exp')]
  ++ encodeULEB128 (wedIdx exp')

-- | Encode export kind
encodeExportKind :: ExportKind -> Word8
encodeExportKind = \case
  ExportKindFunc -> 0x00
  ExportKindTable -> 0x01
  ExportKindMemory -> 0x02
  ExportKindGlobal -> 0x03

--------------------------------------------------------------------------------
-- Code Section
--------------------------------------------------------------------------------

-- | Code entry (function body)
data WasmCodeEntry = WasmCodeEntry
  { wceLocals :: ![(Int, WasmValType)]  -- ^ (count, type) pairs
  , wceBody   :: ![WasmInstr]
  }
  deriving stock (Eq, Show)

-- | Create a code entry
wasmCode :: [(Int, WasmValType)] -> [WasmInstr] -> WasmCodeEntry
wasmCode = WasmCodeEntry

-- | Encode code section
encodeCodeSection :: [WasmCodeEntry] -> [Word8]
encodeCodeSection codes =
  let content = encodeVecOf encodeCodeEntry codes
  in encodeSection sectionCode content

-- | Encode a single code entry (function body)
encodeCodeEntry :: WasmCodeEntry -> [Word8]
encodeCodeEntry entry =
  let locals = encodeLocals (wceLocals entry)
      body = concatMap encodeInstr (wceBody entry) ++ [0x0B]  -- end opcode
      funcBody = locals ++ body
  in encodeULEB128 (length funcBody) ++ funcBody

-- | Encode local declarations
encodeLocals :: [(Int, WasmValType)] -> [Word8]
encodeLocals locals =
  encodeULEB128 (length locals)
  ++ concatMap encodeLocalEntry locals

-- | Encode a single local entry
encodeLocalEntry :: (Int, WasmValType) -> [Word8]
encodeLocalEntry (count, ty) =
  encodeULEB128 count ++ [encodeValType ty]

--------------------------------------------------------------------------------
-- Instruction Encoding
--------------------------------------------------------------------------------

-- | Encode a Wasm instruction
encodeInstr :: WasmInstr -> [Word8]
encodeInstr = \case
  -- Constants
  WI32Const n -> [0x41] ++ encodeSLEB128 n
  WI64Const n -> [0x42] ++ encodeSLEB128 (fromIntegral n)
  WF64Const d -> [0x44] ++ encodeF64 d

  -- Local operations
  WLocalGet idx -> [0x20] ++ encodeULEB128 idx
  WLocalSet idx -> [0x21] ++ encodeULEB128 idx

  -- Global operations
  WGlobalGet idx -> [0x23] ++ encodeULEB128 idx
  WGlobalSet idx -> [0x24] ++ encodeULEB128 idx

  -- Control flow
  WCall idx -> [0x10] ++ encodeULEB128 idx
  WCallIndirect typeIdx -> [0x11] ++ encodeULEB128 typeIdx ++ [0x00]  -- table index
  WReturn -> [0x0F]
  WBr depth -> [0x0C] ++ encodeULEB128 depth
  WBrIf depth -> [0x0D] ++ encodeULEB128 depth
  WUnreachable -> [0x00]

  -- Structured control
  WBlock body ->
    [0x02, 0x40]  -- block, void type
    ++ concatMap encodeInstr body
    ++ [0x0B]  -- end
  WLoop body ->
    [0x03, 0x40]  -- loop, void type
    ++ concatMap encodeInstr body
    ++ [0x0B]  -- end
  WIf thenBranch elseBranch ->
    [0x04, 0x40]  -- if, void type
    ++ concatMap encodeInstr thenBranch
    ++ [0x05]  -- else
    ++ concatMap encodeInstr elseBranch
    ++ [0x0B]  -- end

  -- i32 arithmetic
  WI32Add -> [0x6A]
  WI32Sub -> [0x6B]
  WI32Mul -> [0x6C]
  WI32Eq -> [0x46]
  WI32Lt -> [0x48]

  -- i64 arithmetic
  WI64Add -> [0x7C]
  WI64Sub -> [0x7D]

  -- f64 arithmetic
  WF64Add -> [0xA0]
  WF64Sub -> [0xA1]
  WF64Mul -> [0xA2]

  -- Misc
  WDrop -> [0x1A]

-- | Encode f64 as little-endian bytes
encodeF64 :: Double -> [Word8]
encodeF64 d = BS.unpack $ BS.reverse $ BS.pack bytes
  where
    -- This is a simplified encoding; proper implementation would use
    -- IEEE 754 double-precision format
    bytes = encodeDoubleLE d

-- | Encode double as little-endian (simplified)
encodeDoubleLE :: Double -> [Word8]
encodeDoubleLE d =
  -- Use the Foreign module for proper encoding
  -- For now, use a simplified approach for 0.0
  if d == 0.0
  then [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
  else
    -- For other values, we'd need proper IEEE 754 encoding
    -- This is a placeholder that works for simple cases
    let bits = doubleToWord64 d
    in word64ToBytes bits

-- | Convert double to Word64 bits (placeholder - would use Foreign in real code)
doubleToWord64 :: Double -> Word64
doubleToWord64 d =
  -- Simplified: only handles 0.0 correctly
  if d == 0.0 then 0 else 0x3FF0000000000000  -- 1.0 as placeholder

-- | Convert Word64 to little-endian bytes
word64ToBytes :: Word64 -> [Word8]
word64ToBytes w =
  [ fromIntegral (w .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 24) .&. 0xFF)
  , fromIntegral ((w `shiftR` 32) .&. 0xFF)
  , fromIntegral ((w `shiftR` 40) .&. 0xFF)
  , fromIntegral ((w `shiftR` 48) .&. 0xFF)
  , fromIntegral ((w `shiftR` 56) .&. 0xFF)
  ]

-- | Word64 type alias
type Word64 = Integer
