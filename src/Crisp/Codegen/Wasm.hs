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
    -- * Value Types and Instructions
  , WasmValType(..)
  , WasmInstr(..)
  , WasmExportKind(..)
    -- * Effect-to-Import Mapping
  , generateEffectImports
  , EffectImportMap
  , lookupEffectImport
  , typeToWasmValType
  ) where

import Crisp.IR.ENIR
import Crisp.IR.TypedIR
import Crisp.Core.Term (Type(..), EffectRow(..), Effect(..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

-- | Map from (effect name, operation name) to import index
-- Used to resolve effect calls to the correct Wasm import
type EffectImportMap = Map (Text, Text) Int

-- | Look up the import index for an effect operation
lookupEffectImport :: Text -> Text -> EffectImportMap -> Maybe Int
lookupEffectImport effectName opName = Map.lookup (effectName, opName)

-- | Wasm instructions
data WasmInstr
  -- Constants
  = WI32Const !Int
  | WI64Const !Integer
  | WF32Const !Float
  | WF64Const !Double
  -- Local variables
  | WLocalGet !Int
  | WLocalSet !Int
  | WLocalTee !Int
  -- Global variables
  | WGlobalGet !Int
  | WGlobalSet !Int
  -- Function calls
  | WCall !Int
  | WCallIndirect !Int
  -- Control flow
  | WReturn
  | WBlock ![WasmInstr]
  | WLoop ![WasmInstr]
  | WBr !Int
  | WBrIf !Int
  | WBrTable ![Int] !Int  -- ^ labels, default
  | WIf ![WasmInstr] ![WasmInstr]
  | WNop
  | WUnreachable
  -- i32 arithmetic
  | WI32Add
  | WI32Sub
  | WI32Mul
  | WI32DivS
  | WI32DivU
  | WI32RemS
  | WI32RemU
  -- i32 bitwise
  | WI32And
  | WI32Or
  | WI32Xor
  | WI32Shl
  | WI32ShrS
  | WI32ShrU
  -- i32 comparisons
  | WI32Eq
  | WI32Ne
  | WI32LtS
  | WI32LtU
  | WI32GtS
  | WI32GtU
  | WI32LeS
  | WI32LeU
  | WI32GeS
  | WI32GeU
  | WI32Lt  -- ^ Alias for WI32LtS (kept for compatibility)
  | WI32Eqz
  -- i64 arithmetic
  | WI64Add
  | WI64Sub
  | WI64Mul
  | WI64DivS
  -- f32 arithmetic
  | WF32Add
  | WF32Sub
  | WF32Mul
  | WF32Div
  -- f64 arithmetic
  | WF64Add
  | WF64Sub
  | WF64Mul
  | WF64Div
  -- Memory operations
  | WI32Load !Int    -- ^ offset
  | WI64Load !Int
  | WF32Load !Int
  | WF64Load !Int
  | WI32Store !Int   -- ^ offset
  | WI64Store !Int
  | WF32Store !Int
  | WF64Store !Int
  -- Memory management
  | WMemoryGrow
  | WMemorySize
  -- Misc
  | WDrop
  | WSelect
  -- Conversions
  | WI32WrapI64
  | WI64ExtendI32S
  | WI64ExtendI32U
  | WF32ConvertI32S
  | WF64ConvertI32S
  | WI32TruncF32S
  | WI32TruncF64S
  deriving stock (Eq, Show)

-- | Compile a typed module to Wasm
compileToWasm :: TypedModule -> Either WasmError WasmModule
compileToWasm tirModule = do
  -- Generate imports for effects
  let imports = generateEffectImports tirModule

  -- Build effect import map for resolving effect calls
  let importMap = buildEffectImportMap imports

  -- Compile functions with the import map
  funcs <- traverse (compileDefinition importMap) (tirDefinitions tirModule)

  -- Generate exports (function indices start after imports)
  let numImports = length imports
  let exports = generateExports (tirModuleName tirModule) numImports (concat funcs)

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
-- Each effect operation becomes a Wasm import with:
--   - Module name: effect name (e.g., "IO", "State")
--   - Import name: operation name (e.g., "read", "write")
--   - Type: converted from operation input/output types
generateEffectImports :: TypedModule -> [WasmImport]
generateEffectImports tirMod = concatMap effectToImports effectDefs
  where
    -- Extract effect definitions from the module
    effectDefs :: [TypedEffectDef]
    effectDefs = [e | TirEffect e <- tirDefinitions tirMod]

    -- Convert an effect definition to a list of imports (one per operation)
    effectToImports :: TypedEffectDef -> [WasmImport]
    effectToImports eff =
      map (operationToImport (teName eff)) (teOperations eff)

    -- Convert a single operation to a Wasm import
    operationToImport :: Text -> TypedOperation -> WasmImport
    operationToImport effectName op = WasmImport
      { wiModule = effectName
      , wiName   = toName op
      , wiType   = operationToWasmType op
      }

    -- Convert operation types to Wasm function type
    operationToWasmType :: TypedOperation -> WasmType
    operationToWasmType op = WasmType
      { wtParams  = [typeToWasmValType (toInputType op)]
      , wtResults = [typeToWasmValType (toOutputType op)]
      }

-- | Build an import index map from a list of imports
-- Returns a map from (module, name) -> import index
buildEffectImportMap :: [WasmImport] -> EffectImportMap
buildEffectImportMap imports =
  Map.fromList [((wiModule imp, wiName imp), idx) | (idx, imp) <- zip [0..] imports]

-- | Convert a Crisp type to a Wasm value type
-- Most types become i32 (pointer/reference), primitives map directly
typeToWasmValType :: Type -> WasmValType
typeToWasmValType ty = case ty of
  -- Primitive types
  TyCon "Int" []    -> WasmI32
  TyCon "Int32" []  -> WasmI32
  TyCon "Int64" []  -> WasmI64
  TyCon "Float" []  -> WasmF32
  TyCon "Float32" [] -> WasmF32
  TyCon "Float64" [] -> WasmF64
  TyCon "Double" [] -> WasmF64
  TyCon "Bool" []   -> WasmI32  -- Booleans are i32 (0 or 1)
  TyCon "Char" []   -> WasmI32  -- Characters are i32 (Unicode codepoint)
  TyCon "Unit" []   -> WasmI32  -- Unit is i32 (always 0)
  TyCon "Nat" []    -> WasmI32  -- Natural numbers are i32

  -- String is a pointer to heap-allocated data
  TyCon "String" [] -> WasmI32

  -- Function types become function references (i32 index)
  TyPi {} -> WasmI32

  -- Generic type constructors (ADTs, etc.) are heap-allocated pointers
  TyCon _ _ -> WasmI32

  -- Type variables and other types default to i32 (pointer)
  TyVar _ _ -> WasmI32
  TyForall {} -> WasmI32
  TyForallDep {} -> WasmI32
  TyLazy _ -> WasmI32
  TyLinear _ -> WasmI32
  TyRef _ -> WasmI32
  TyRefMut _ -> WasmI32
  TyUniverse _ -> WasmI32
  TyProp -> WasmI32
  TySigma {} -> WasmI32
  TyNatLit _ -> WasmI32
  TyAdd {} -> WasmI32
  TyEffect {} -> WasmI32
  TyRefined {} -> WasmI32

-- | Compile a definition to Wasm functions
compileDefinition :: EffectImportMap -> TypedDefinition -> Either WasmError [WasmFunc]
compileDefinition importMap def = case def of
  TirFn fn -> do
    func <- compileFunction importMap fn
    pure [func]
  _ -> pure []  -- Other definitions don't generate functions directly

-- | Compile a function to Wasm
compileFunction :: EffectImportMap -> TypedFunction -> Either WasmError WasmFunc
compileFunction importMap fn = do
  -- Convert to ENIR first
  let enir = toENIR (tfBody fn)

  -- Generate Wasm instructions with import map for effect resolution
  body <- compileENIR importMap enir

  -- Generate function type from parameters and return type
  let paramTypes = map (typeToWasmValType . tpType) (tfParams fn)
  let resultTypes = [typeToWasmValType (tfReturnType fn)]

  pure $ WasmFunc
    { wfName = tfName fn
    , wfType = WasmType paramTypes resultTypes
    , wfLocals = []
    , wfBody = body
    }

-- | Compile ENIR to Wasm instructions
compileENIR :: EffectImportMap -> ENIRTerm -> Either WasmError [WasmInstr]
compileENIR importMap term = case term of
  ENIRReturn val -> compileValue importMap val
  ENIRVar _ idx -> pure [WLocalGet idx]
  ENIRLam _ _ body -> compileENIR importMap body  -- Lambda bodies need closure conversion
  ENIRApp func arg -> do
    funcInstrs <- compileENIR importMap func
    argInstrs <- compileValue importMap arg
    pure $ argInstrs ++ funcInstrs ++ [WCall 0]  -- Placeholder call index
  ENIRLet _ _ val body -> do
    valInstrs <- compileENIR importMap val
    bodyInstrs <- compileENIR importMap body
    pure $ valInstrs ++ [WLocalSet 0] ++ bodyInstrs  -- Placeholder local index
  ENIRCon name args -> do
    argInstrs <- traverse (compileValue importMap) args
    pure $ concat argInstrs ++ [WI32Const (tagFor name)]  -- Constructor tagging
  ENIRMatch val cases -> do
    valInstrs <- compileValue importMap val
    -- Match compilation would generate br_table or nested ifs
    pure $ valInstrs ++ [WUnreachable]  -- Placeholder
  ENIRCall effect op arg _cont -> do
    argInstrs <- compileValue importMap arg
    -- Effect calls become imports - look up the import index
    case lookupEffectImport effect op importMap of
      Just importIdx -> pure $ argInstrs ++ [WCall importIdx]
      Nothing -> Left $ CodegenError $
        "Unknown effect operation: " <> effect <> "." <> op
  where
    tagFor _ = 0  -- Placeholder tag assignment

-- | Compile a value to Wasm instructions
compileValue :: EffectImportMap -> ENIRValue -> Either WasmError [WasmInstr]
compileValue importMap val = case val of
  ENIRVVar _ idx -> pure [WLocalGet idx]
  ENIRVCon name args -> do
    argInstrs <- traverse (compileValue importMap) args
    pure $ concat argInstrs ++ [WI32Const (tagFor name)]
  ENIRVLam _ _ body -> do
    -- Lambda values need closure conversion
    bodyInstrs <- compileENIR importMap body
    pure bodyInstrs
  where
    tagFor _ = 0

-- | Generate exports
-- Function indices in Wasm start after imports, so we offset by numImports
generateExports :: Text -> Int -> [WasmFunc] -> [WasmExport]
generateExports _modName numImports funcs =
  [ WasmExport (wfName f) ExportFunc (numImports + i)
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
