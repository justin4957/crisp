{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Codegen.Wasm
-- Description : WebAssembly Code Generation
--
-- Generates WebAssembly binary format from the low-level IR.
-- This is a placeholder for the future Wasm backend.

module Crisp.Codegen.Wasm
  ( -- * Compilation
    compileToWasm
  , WasmModule(..)
  , WasmError(..)
    -- * Wasm structures (simplified)
  , WasmFunc(..)
  , WasmType(..)
  , WasmImport(..)
  , WasmExport(..)
  ) where

import Crisp.IR.ENIR
import Crisp.IR.TypedIR (TypedModule(..), TypedDefinition(..), TypedFunction(..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

-- | Wasm compilation errors
data WasmError
  = UnsupportedFeature !Text
  | CodegenError !Text
  deriving stock (Eq, Show)

-- | A compiled Wasm module
data WasmModule = WasmModule
  { wasmTypes   :: ![WasmType]
  , wasmImports :: ![WasmImport]
  , wasmFuncs   :: ![WasmFunc]
  , wasmExports :: ![WasmExport]
  , wasmBytes   :: !ByteString
  } deriving stock (Eq, Show)

-- | Wasm function type signature
data WasmType = WasmType
  { wtParams  :: ![WasmValType]
  , wtResults :: ![WasmValType]
  } deriving stock (Eq, Show)

-- | Wasm value types
data WasmValType
  = WasmI32
  | WasmI64
  | WasmF32
  | WasmF64
  deriving stock (Eq, Show)

-- | Wasm import
data WasmImport = WasmImport
  { wiModule :: !Text
  , wiName   :: !Text
  , wiType   :: !WasmType
  } deriving stock (Eq, Show)

-- | Wasm function
data WasmFunc = WasmFunc
  { wfName   :: !Text
  , wfType   :: !WasmType
  , wfLocals :: ![WasmValType]
  , wfBody   :: ![WasmInstr]
  } deriving stock (Eq, Show)

-- | Wasm export
data WasmExport = WasmExport
  { weName :: !Text
  , weKind :: !WasmExportKind
  , weIdx  :: !Int
  } deriving stock (Eq, Show)

-- | Export kinds
data WasmExportKind
  = ExportFunc
  | ExportMemory
  | ExportGlobal
  | ExportTable
  deriving stock (Eq, Show)

-- | Wasm instructions (simplified subset)
data WasmInstr
  = WI32Const !Int
  | WI64Const !Integer
  | WF64Const !Double
  | WLocalGet !Int
  | WLocalSet !Int
  | WGlobalGet !Int
  | WGlobalSet !Int
  | WCall !Int
  | WCallIndirect !Int
  | WReturn
  | WBlock ![WasmInstr]
  | WLoop ![WasmInstr]
  | WBr !Int
  | WBrIf !Int
  | WIf ![WasmInstr] ![WasmInstr]
  | WI32Add
  | WI32Sub
  | WI32Mul
  | WI32Eq
  | WI32Lt
  | WI64Add
  | WI64Sub
  | WF64Add
  | WF64Sub
  | WF64Mul
  | WDrop
  | WUnreachable
  deriving stock (Eq, Show)

-- | Compile a typed module to Wasm
compileToWasm :: TypedModule -> Either WasmError WasmModule
compileToWasm tirModule = do
  -- Generate imports for effects
  let imports = generateEffectImports tirModule

  -- Compile functions
  funcs <- traverse compileDefinition (tirDefinitions tirModule)

  -- Generate exports
  let exports = generateExports (tirModuleName tirModule) (concat funcs)

  -- Generate type section
  let types = generateTypes funcs imports

  -- Encode to binary
  let bytes = encodeWasmModule types imports (concat funcs) exports

  pure $ WasmModule
    { wasmTypes = types
    , wasmImports = imports
    , wasmFuncs = concat funcs
    , wasmExports = exports
    , wasmBytes = bytes
    }

-- | Generate imports for effect operations
generateEffectImports :: TypedModule -> [WasmImport]
generateEffectImports _mod = []  -- Placeholder

-- | Compile a definition to Wasm functions
compileDefinition :: TypedDefinition -> Either WasmError [WasmFunc]
compileDefinition def = case def of
  TirFn fn -> do
    func <- compileFunction fn
    pure [func]
  _ -> pure []  -- Other definitions don't generate functions directly

-- | Compile a function to Wasm
compileFunction :: TypedFunction -> Either WasmError WasmFunc
compileFunction fn = do
  -- Convert to ENIR first
  let enir = toENIR (tfBody fn)

  -- Generate Wasm instructions
  body <- compileENIR enir

  pure $ WasmFunc
    { wfName = tfName fn
    , wfType = WasmType [WasmI32] [WasmI32]  -- Placeholder type
    , wfLocals = []
    , wfBody = body
    }

-- | Compile ENIR to Wasm instructions
compileENIR :: ENIRTerm -> Either WasmError [WasmInstr]
compileENIR term = case term of
  ENIRReturn val -> compileValue val
  ENIRVar _ idx -> pure [WLocalGet idx]
  ENIRLam _ _ body -> compileENIR body  -- Lambda bodies need closure conversion
  ENIRApp func arg -> do
    funcInstrs <- compileENIR func
    argInstrs <- compileValue arg
    pure $ argInstrs ++ funcInstrs ++ [WCall 0]  -- Placeholder call index
  ENIRLet _ _ val body -> do
    valInstrs <- compileENIR val
    bodyInstrs <- compileENIR body
    pure $ valInstrs ++ [WLocalSet 0] ++ bodyInstrs  -- Placeholder local index
  ENIRCon name args -> do
    argInstrs <- traverse compileValue args
    pure $ concat argInstrs ++ [WI32Const (tagFor name)]  -- Constructor tagging
  ENIRMatch val cases -> do
    valInstrs <- compileValue val
    -- Match compilation would generate br_table or nested ifs
    pure $ valInstrs ++ [WUnreachable]  -- Placeholder
  ENIRCall effect op arg cont -> do
    argInstrs <- compileValue arg
    -- Effect calls become imports
    pure $ argInstrs ++ [WCall 0]  -- Placeholder import index
  where
    tagFor _ = 0  -- Placeholder tag assignment

-- | Compile a value to Wasm instructions
compileValue :: ENIRValue -> Either WasmError [WasmInstr]
compileValue val = case val of
  ENIRVVar _ idx -> pure [WLocalGet idx]
  ENIRVCon name args -> do
    argInstrs <- traverse compileValue args
    pure $ concat argInstrs ++ [WI32Const (tagFor name)]
  ENIRVLam _ _ body -> do
    -- Lambda values need closure conversion
    bodyInstrs <- compileENIR body
    pure bodyInstrs
  where
    tagFor _ = 0

-- | Generate exports
generateExports :: Text -> [WasmFunc] -> [WasmExport]
generateExports _modName funcs =
  [ WasmExport (wfName f) ExportFunc i
  | (i, f) <- zip [0..] funcs
  ]

-- | Generate type section
generateTypes :: [[WasmFunc]] -> [WasmImport] -> [WasmType]
generateTypes _ _ = [WasmType [WasmI32] [WasmI32]]  -- Placeholder

-- | Encode module to Wasm binary format
encodeWasmModule :: [WasmType] -> [WasmImport] -> [WasmFunc] -> [WasmExport] -> ByteString
encodeWasmModule types imports funcs exports =
  BS.pack $ wasmMagic ++ wasmVersion ++ sections
  where
    wasmMagic = [0x00, 0x61, 0x73, 0x6D]  -- \0asm
    wasmVersion = [0x01, 0x00, 0x00, 0x00]  -- version 1

    sections = concat
      [ encodeTypeSection types
      , encodeImportSection imports
      , encodeFunctionSection funcs
      , encodeExportSection exports
      , encodeCodeSection funcs
      ]

-- Placeholder section encoders
encodeTypeSection :: [WasmType] -> [Word8]
encodeTypeSection _ = []  -- Would encode type section

encodeImportSection :: [WasmImport] -> [Word8]
encodeImportSection _ = []

encodeFunctionSection :: [WasmFunc] -> [Word8]
encodeFunctionSection _ = []

encodeExportSection :: [WasmExport] -> [Word8]
encodeExportSection _ = []

encodeCodeSection :: [WasmFunc] -> [Word8]
encodeCodeSection _ = []
