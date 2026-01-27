{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Codegen.Compile
-- Description : LLIR to WebAssembly compilation
--
-- Compiles LLIR (Low-Level IR) to WebAssembly instructions:
-- - Arithmetic and comparison operations
-- - Local and global variable access
-- - Control flow (if, blocks, loops, branches)
-- - Function calls (direct and indirect)
-- - Data type constructors and pattern matching
-- - Closures with captured variables
-- - Memory operations (load/store)

module Crisp.Codegen.Compile
  ( -- * Compilation
    compileModule
  , compileInstr
  , compileInstrs
  , compileInstrsWithContext
    -- * Constructor Compilation
  , compileConstructor
  , compileFieldAccess
  , compileTagLoad
  , TagInfo(..)
    -- * Pattern Match Compilation
  , compileMatch
  , compileMatchWithDefault
    -- * Closure Compilation
  , compileClosureAlloc
  , compileClosureCall
  , compileCaptureStore
  , compileCaptureLoad
  , ClosureInfo(..)
    -- * Compilation Context
  , CompileContext(..)
  , defaultCompileContext
    -- * Result Types
  , WasmCompiledModule(..)
  , WasmCompiledFunc(..)
  , CompileError(..)
    -- * Accessors
  , wasmModuleFunctions
  , wasmModuleExports
  , wasmModuleImports
  , wasmModuleHasMemory
  , wasmFunctionParamCount
  , wasmFunctionLocalCount
    -- * Utilities
  , emptyLlirModule
  ) where

import Crisp.IR.LLIR
import Crisp.Codegen.Wasm (WasmInstr(..), WasmValType(..))
import Crisp.Runtime.Allocator (allocInstrs)

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM)

--------------------------------------------------------------------------------
-- Compilation Error
--------------------------------------------------------------------------------

-- | Compilation errors
data CompileError
  = UnknownFunction !Text
  | UnknownGlobal !Text
  | UnknownLabel !Text
  | InvalidType !Text
  | UnsupportedFeature !Text
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Compilation Context
--------------------------------------------------------------------------------

-- | Context for compilation
data CompileContext = CompileContext
  { ccFunctions :: ![(Text, Int)]     -- ^ Function name to index
  , ccGlobals   :: ![(Text, Int)]     -- ^ Global name to index
  , ccLabels    :: ![(Text, Int)]     -- ^ Label name to depth
  , ccImports   :: ![(Text, Int)]     -- ^ Import name to index
  }
  deriving stock (Eq, Show)

-- | Default compilation context
defaultCompileContext :: CompileContext
defaultCompileContext = CompileContext [] [] [] []

--------------------------------------------------------------------------------
-- Compiled Module Types
--------------------------------------------------------------------------------

-- | A compiled WebAssembly module
data WasmCompiledModule = WasmCompiledModule
  { wcmFunctions :: ![WasmCompiledFunc]
  , wcmExports   :: ![(Text, Int)]       -- ^ Export name and function index
  , wcmImports   :: ![(Text, Text, Int)] -- ^ Module, name, type index
  , wcmMemory    :: !(Maybe (Int, Maybe Int))  -- ^ Min pages, max pages
  , wcmTypes     :: ![([WasmValType], [WasmValType])]  -- ^ Function types
  }
  deriving stock (Eq, Show)

-- | A compiled WebAssembly function
data WasmCompiledFunc = WasmCompiledFunc
  { wcfName      :: !Text
  , wcfParams    :: ![WasmValType]
  , wcfResult    :: ![WasmValType]
  , wcfLocals    :: ![WasmValType]
  , wcfBody      :: ![WasmInstr]
  }
  deriving stock (Eq, Show)

-- | Accessors for WasmCompiledModule
wasmModuleFunctions :: WasmCompiledModule -> [WasmCompiledFunc]
wasmModuleFunctions = wcmFunctions

wasmModuleExports :: WasmCompiledModule -> [(Text, Int)]
wasmModuleExports = wcmExports

wasmModuleImports :: WasmCompiledModule -> [(Text, Text, Int)]
wasmModuleImports = wcmImports

wasmModuleHasMemory :: WasmCompiledModule -> Bool
wasmModuleHasMemory m = case wcmMemory m of
  Just _ -> True
  Nothing -> False

-- | Accessors for WasmCompiledFunc
wasmFunctionParamCount :: WasmCompiledFunc -> Int
wasmFunctionParamCount = length . wcfParams

wasmFunctionLocalCount :: WasmCompiledFunc -> Int
wasmFunctionLocalCount = length . wcfLocals

--------------------------------------------------------------------------------
-- Tag and Closure Info
--------------------------------------------------------------------------------

-- | Tag information for constructors
data TagInfo = TagInfo
  { tagValue      :: !Int    -- ^ Tag value (constructor index)
  , tagPayloadSize :: !Int   -- ^ Total payload size
  }
  deriving stock (Eq, Show)

-- | Closure information
data ClosureInfo = ClosureInfo
  { closureFuncName   :: !Text
  , closureFuncIdx    :: !Int
  , closureCaptures   :: ![(Text, LlirType)]
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Empty Module
--------------------------------------------------------------------------------

-- | Create an empty LLIR module for testing
emptyLlirModule :: LlirModule
emptyLlirModule = LlirModule
  { llirModFunctions = []
  , llirModTypes = []
  , llirModMemory = LlirMemory 0 Nothing
  , llirModImports = []
  , llirModExports = []
  }

--------------------------------------------------------------------------------
-- Module Compilation
--------------------------------------------------------------------------------

-- | Compile an LLIR module to WebAssembly
compileModule :: LlirModule -> Either CompileError WasmCompiledModule
compileModule llirMod = do
  -- Build function index map
  let funcNames = map llirFuncName (llirModFunctions llirMod)
  let importCount = length (llirModImports llirMod)
  let funcIndices = zip funcNames [importCount..]

  -- Build context
  let ctx = CompileContext
        { ccFunctions = funcIndices
        , ccGlobals = []
        , ccLabels = []
        , ccImports = []
        }

  -- Compile functions
  funcs <- forM (llirModFunctions llirMod) (compileFunction ctx)

  -- Build exports
  let exports = [(llirExportName e, lookupFuncIdx funcIndices (getExportedFunc e))
                | e <- llirModExports llirMod]

  -- Build imports
  let imports = [(llirImportModule i, llirImportName i, 0)
                | i <- llirModImports llirMod]

  -- Build memory
  let memory = case llirModMemory llirMod of
        LlirMemory 0 Nothing -> Nothing
        LlirMemory minPages maxPages -> Just (minPages, maxPages)

  pure $ WasmCompiledModule
    { wcmFunctions = funcs
    , wcmExports = exports
    , wcmImports = imports
    , wcmMemory = memory
    , wcmTypes = []
    }
  where
    lookupFuncIdx indices name =
      case lookup name indices of
        Just idx -> idx
        Nothing -> 0  -- Fallback

    getExportedFunc (LlirExport _ (LlirExportFunc name)) = name
    getExportedFunc _ = ""

-- | Compile an LLIR function to WebAssembly
compileFunction :: CompileContext -> LlirFunc -> Either CompileError WasmCompiledFunc
compileFunction ctx func = do
  let params = map (llirTypeToWasm . snd) (llirFuncParams func)
  let result = case llirFuncReturn func of
        LlirUnit -> []
        ty -> [llirTypeToWasm ty]
  let locals = map (llirTypeToWasm . snd) (llirFuncLocals func)
  let body = compileInstrsWithContext ctx (llirFuncBody func)

  pure $ WasmCompiledFunc
    { wcfName = llirFuncName func
    , wcfParams = params
    , wcfResult = result
    , wcfLocals = locals
    , wcfBody = body
    }

--------------------------------------------------------------------------------
-- Type Conversion
--------------------------------------------------------------------------------

-- | Convert LLIR type to Wasm value type
llirTypeToWasm :: LlirType -> WasmValType
llirTypeToWasm = \case
  LlirI32 -> WasmI32
  LlirI64 -> WasmI64
  LlirF32 -> WasmF32
  LlirF64 -> WasmF64
  LlirPtr -> WasmI32  -- Pointers are i32 in wasm32
  LlirUnit -> WasmI32  -- Unit represented as i32 (dummy value)
  LlirStruct _ -> WasmI32  -- Structs are pointers
  LlirTaggedUnion _ -> WasmI32  -- Tagged unions are pointers
  LlirArray _ -> WasmI32  -- Arrays are pointers

--------------------------------------------------------------------------------
-- Instruction Compilation
--------------------------------------------------------------------------------

-- | Compile a single LLIR instruction to Wasm instructions
compileInstr :: LlirInstr -> [WasmInstr]
compileInstr = compileInstrWithContext defaultCompileContext

-- | Compile multiple LLIR instructions
compileInstrs :: [LlirInstr] -> [WasmInstr]
compileInstrs = compileInstrsWithContext defaultCompileContext

-- | Compile instructions with context
compileInstrsWithContext :: CompileContext -> [LlirInstr] -> [WasmInstr]
compileInstrsWithContext ctx = concatMap (compileInstrWithContext ctx)

-- | Compile instruction with context
compileInstrWithContext :: CompileContext -> LlirInstr -> [WasmInstr]
compileInstrWithContext ctx = \case
  -- Constants
  LlirConst (LlirValI32 n) -> [WI32Const n]
  LlirConst (LlirValI64 n) -> [WI64Const n]
  LlirConst (LlirValF32 n) -> [WF32Const n]
  LlirConst (LlirValF64 n) -> [WF64Const n]
  LlirConst LlirValNull -> [WI32Const 0]

  -- Local variables
  LlirLocalGet idx -> [WLocalGet idx]
  LlirLocalSet idx -> [WLocalSet idx]
  LlirLocalTee idx -> [WLocalTee idx]

  -- Global variables
  LlirGlobalGet name ->
    case lookup name (ccGlobals ctx) of
      Just idx -> [WGlobalGet idx]
      Nothing -> [WUnreachable]  -- Error case

  LlirGlobalSet name ->
    case lookup name (ccGlobals ctx) of
      Just idx -> [WGlobalSet idx]
      Nothing -> [WUnreachable]

  -- Memory operations
  LlirLoad LlirI32 offset -> [WI32Load offset]
  LlirLoad LlirI64 offset -> [WI64Load offset]
  LlirLoad LlirF32 offset -> [WF32Load offset]
  LlirLoad LlirF64 offset -> [WF64Load offset]
  LlirLoad LlirPtr offset -> [WI32Load offset]
  LlirLoad _ offset -> [WI32Load offset]  -- Default to i32 for other types

  LlirStore LlirI32 offset -> [WI32Store offset]
  LlirStore LlirI64 offset -> [WI64Store offset]
  LlirStore LlirF32 offset -> [WF32Store offset]
  LlirStore LlirF64 offset -> [WF64Store offset]
  LlirStore LlirPtr offset -> [WI32Store offset]
  LlirStore _ offset -> [WI32Store offset]

  -- Function calls
  LlirCall name _arity ->
    case lookup name (ccFunctions ctx) of
      Just idx -> [WCall idx]
      Nothing -> [WUnreachable]

  LlirCallIndirect funcType -> [WCallIndirect 0]  -- Type index would be computed

  -- Control flow
  LlirDrop -> [WDrop]
  LlirSelect -> [WSelect]
  LlirReturn -> [WReturn]
  LlirUnreachable -> [WUnreachable]
  LlirNop -> [WNop]

  LlirBlock label body ->
    let ctx' = pushLabel label ctx
        bodyInstrs = compileInstrsWithContext ctx' body
    in [WBlock bodyInstrs]

  LlirLoop label body ->
    let ctx' = pushLabel label ctx
        bodyInstrs = compileInstrsWithContext ctx' body
    in [WLoop bodyInstrs]

  LlirIf resultTy thenBranch elseBranch ->
    let thenInstrs = compileInstrsWithContext ctx thenBranch
        elseInstrs = compileInstrsWithContext ctx elseBranch
    in [WIf thenInstrs elseInstrs]

  LlirBr label ->
    case lookupLabelDepth label ctx of
      Just depth -> [WBr depth]
      Nothing -> [WUnreachable]

  LlirBrIf label ->
    case lookupLabelDepth label ctx of
      Just depth -> [WBrIf depth]
      Nothing -> [WUnreachable]

  LlirBrTable labels defaultLabel ->
    let labelDepths = map (\l -> maybe 0 id (lookupLabelDepth l ctx)) labels
        defaultDepth = maybe 0 id (lookupLabelDepth defaultLabel ctx)
    in [WBrTable labelDepths defaultDepth]

  -- Binary operations
  LlirBinOp op -> compileBinOp op

  -- Comparison operations
  LlirCmpOp op -> compileCmpOp op

  -- Unary operations (simplified)
  LlirUnaryOp "neg" -> [WI32Const 0, WI32Sub]  -- 0 - x
  LlirUnaryOp "not" -> [WI32Eqz]
  LlirUnaryOp _ -> []

  -- Type conversions
  LlirConvert LlirI32 LlirI64 -> [WI64ExtendI32S]
  LlirConvert LlirI64 LlirI32 -> [WI32WrapI64]
  LlirConvert LlirI32 LlirF64 -> [WF64ConvertI32S]
  LlirConvert LlirI32 LlirF32 -> [WF32ConvertI32S]
  LlirConvert LlirF64 LlirI32 -> [WI32TruncF64S]
  LlirConvert LlirF32 LlirI32 -> [WI32TruncF32S]
  LlirConvert _ _ -> []  -- Unsupported conversions

  -- Memory management
  LlirMemoryGrow -> [WMemoryGrow]
  LlirMemorySize -> [WMemorySize]

-- | Compile binary operation
compileBinOp :: LlirBinOp -> [WasmInstr]
compileBinOp = \case
  LlirAdd -> [WI32Add]
  LlirSub -> [WI32Sub]
  LlirMul -> [WI32Mul]
  LlirDiv -> [WI32DivS]
  LlirRem -> [WI32RemS]
  LlirAnd -> [WI32And]
  LlirOr -> [WI32Or]
  LlirXor -> [WI32Xor]
  LlirShl -> [WI32Shl]
  LlirShr -> [WI32ShrS]
  LlirShrU -> [WI32ShrU]

-- | Compile comparison operation
compileCmpOp :: LlirCmpOp -> [WasmInstr]
compileCmpOp = \case
  LlirEq -> [WI32Eq]
  LlirNe -> [WI32Ne]
  LlirLt -> [WI32LtS]
  LlirLe -> [WI32LeS]
  LlirGt -> [WI32GtS]
  LlirGe -> [WI32GeS]
  LlirLtU -> [WI32LtU]
  LlirLeU -> [WI32LeU]
  LlirGtU -> [WI32GtU]
  LlirGeU -> [WI32GeU]

-- | Push a label onto the context
pushLabel :: Text -> CompileContext -> CompileContext
pushLabel label ctx = ctx
  { ccLabels = (label, 0) : map incDepth (ccLabels ctx)
  }
  where
    incDepth (l, d) = (l, d + 1)

-- | Look up label depth in context
lookupLabelDepth :: Text -> CompileContext -> Maybe Int
lookupLabelDepth label ctx = lookup label (ccLabels ctx)

--------------------------------------------------------------------------------
-- Constructor Compilation
--------------------------------------------------------------------------------

-- | Compile a data type constructor
-- Returns instructions that allocate memory and store tag + fields
compileConstructor :: Text -> Text -> [LlirType] -> TagInfo -> [WasmInstr]
compileConstructor _typeName _constrName fields tagInfo =
  let -- Calculate total size: 4 (tag) + payload
      totalSize = 4 + tagPayloadSize tagInfo

      -- Allocate memory using the runtime allocator
      allocateInstrs =
        [ WI32Const totalSize
        ] ++ allocInstrs  -- Use allocator from Runtime.Allocator

      -- Store tag at offset 0
      storeTagInstrs =
        [ WLocalTee 0     -- Save pointer to local 0
        , WI32Const (tagValue tagInfo)
        , WI32Store 0     -- Store tag at offset 0
        ]

      -- Store fields at offsets 4, 8, 12, etc.
      -- (Fields are expected to be on the stack already)
      storeFieldInstrs = concatMap storeField (zip [0..] fields)

      -- Return the pointer
      returnPtr = [WLocalGet 0]

  in allocateInstrs ++ storeTagInstrs ++ storeFieldInstrs ++ returnPtr
  where
    storeField (idx, fieldTy) =
      let offset = 4 + idx * 4  -- Simplified: all fields are 4 bytes
      in [ WLocalGet 0             -- Get pointer
         , WI32Const offset
         , WI32Add                 -- ptr + offset
         -- Field value should be on stack from previous computation
         , storeInstr fieldTy offset
         ]

    storeInstr LlirI32 _ = WI32Store 0
    storeInstr LlirI64 _ = WI64Store 0
    storeInstr LlirF64 _ = WF64Store 0
    storeInstr LlirPtr _ = WI32Store 0
    storeInstr _ _ = WI32Store 0

-- | Compile field access (load field from a data structure)
compileFieldAccess :: Int -> Int -> [LlirInstr]
compileFieldAccess fieldIdx baseOffset =
  let offset = baseOffset + fieldIdx * 4
  in [ LlirLocalGet 0              -- Pointer to structure
     , LlirLoad LlirI32 offset     -- Load field
     ]

-- | Compile tag load (load constructor tag from a tagged union)
compileTagLoad :: [LlirInstr]
compileTagLoad =
  [ LlirLocalGet 0     -- Pointer to tagged union
  , LlirLoad LlirI32 0 -- Load tag at offset 0
  ]

--------------------------------------------------------------------------------
-- Pattern Match Compilation
--------------------------------------------------------------------------------

-- | Compile a pattern match
-- Takes the result type and a list of (tag, body instructions) pairs
compileMatch :: LlirType -> [(Int, [LlirInstr])] -> [LlirInstr]
compileMatch resultTy branches =
  case branches of
    [(_, body)] -> body  -- Single branch: just execute body

    [(0, body0), (1, body1)] ->
      -- Two branches (like Bool): use if-then-else
      -- Assume tag is on stack
      [ LlirIf resultTy body1 body0 ]  -- Tag 1 = true = then, Tag 0 = false = else

    _ ->
      -- Multiple branches: use br_table
      -- Generate blocks for each branch
      let numBranches = length branches
          sortedBranches = sortBranchesByTag branches
          labels = ["case" <> T.pack (show i) | i <- [0..numBranches-1]]
          defaultLabel = "default"

          -- Wrap in nested blocks
          wrapInBlocks body [] = body
          wrapInBlocks body (l:ls) = wrapInBlocks [LlirBlock l body] ls

          -- Create branch table instruction
          brTableInstr = LlirBrTable labels defaultLabel

          -- Create body: load tag, branch to appropriate case
          matchBody = compileTagLoad ++ [brTableInstr]

          -- Each case block contains its body and br to exit
          caseBlocks = concatMap makeCaseBlock (zip labels sortedBranches)

      in [LlirBlock "exit" (caseBlocks ++ matchBody)]
  where
    sortBranchesByTag = id  -- Assume already sorted

    makeCaseBlock (label, (_, body)) =
      [LlirBlock label (body ++ [LlirBr "exit"])]

-- | Compile a pattern match with a default branch
compileMatchWithDefault :: LlirType -> [(Int, [LlirInstr])] -> [LlirInstr] -> [LlirInstr]
compileMatchWithDefault resultTy branches defaultBranch =
  let allBranches = branches ++ [(-1, defaultBranch)]
  in compileMatch resultTy allBranches

--------------------------------------------------------------------------------
-- Closure Compilation
--------------------------------------------------------------------------------

-- | Compile closure allocation
compileClosureAlloc :: ClosureInfo -> [WasmInstr]
compileClosureAlloc closure =
  let -- Size: 4 (func ptr) + captures
      captureSize = sum [typeSize ty | (_, ty) <- closureCaptures closure]
      totalSize = 4 + captureSize

      -- Allocate using the runtime allocator
      allocateInstrs =
        [ WI32Const totalSize
        ] ++ allocInstrs  -- Use allocator from Runtime.Allocator

      -- Store function pointer at offset 0
      storeFuncPtr =
        [ WLocalTee 0  -- Save closure pointer
        , WI32Const (closureFuncIdx closure)
        , WI32Store 0  -- Store at offset 0
        ]

      -- Store captures at offsets 4, 8, 12, etc.
      -- Captured values should already be on the stack
      storeCaptures = []  -- Would need capture values

  in allocateInstrs ++ storeFuncPtr ++ storeCaptures ++ [WLocalGet 0]
  where
    typeSize LlirI32 = 4
    typeSize LlirI64 = 8
    typeSize LlirF64 = 8
    typeSize LlirPtr = 4
    typeSize _ = 4

-- | Compile closure call
compileClosureCall :: ClosureInfo -> [LlirType] -> LlirType -> [WasmInstr]
compileClosureCall closure argTypes resultType =
  let -- Load function index from closure
      loadFuncIdx =
        [ WLocalGet 0  -- Closure pointer
        , WI32Load 0   -- Load function index
        ]

      -- Call indirect
      callInstr = [WCallIndirect 0]  -- Type index would be computed

  in loadFuncIdx ++ callInstr

-- | Compile capture store (store a captured variable into closure)
compileCaptureStore :: Int -> Int -> [LlirInstr]
compileCaptureStore captureIdx offset =
  [ LlirLocalGet 0           -- Closure pointer
  , LlirConst (LlirValI32 (4 + captureIdx * 4))
  , LlirBinOp LlirAdd        -- ptr + offset
  -- Value to store should be on stack
  , LlirStore LlirI32 0
  ]

-- | Compile capture load (load a captured variable from closure)
compileCaptureLoad :: Int -> Int -> [LlirInstr]
compileCaptureLoad captureIdx offset =
  [ LlirLocalGet 0           -- Closure pointer
  , LlirLoad LlirI32 (4 + captureIdx * 4)  -- Load at offset
  ]
