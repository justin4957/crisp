{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.IR.LLIR
-- Description : Low-Level IR for WebAssembly code generation
--
-- Implements the Low-Level IR (LLIR) representation for WebAssembly:
-- - Monomorphic, explicitly-typed code
-- - Explicit memory layouts
-- - No type abstractions or applications
-- - Direct translation to WebAssembly instructions
--
-- LLIR is the final IR before WebAssembly code generation, after:
-- 1. Type checking (TypedIR)
-- 2. CPS transformation (CPS)
-- 3. Proof erasure (ENIR)
-- 4. Monomorphization (this module)

module Crisp.IR.LLIR
  ( -- * Types
    LlirType(..)
  , LlirFuncType(..)
    -- * Type Operations
  , llirTypeSize
  , llirTypeAlign
  , llirFuncTypeArity
  , llirFuncTypeReturn
  , toLlirType
  , fromLlirType
    -- * Values
  , LlirValue(..)
    -- * Instructions
  , LlirInstr(..)
  , LlirBinOp(..)
  , LlirCmpOp(..)
    -- * Instruction Predicates
  , llirInstrIsConst
  , llirInstrIsLocalGet
  , llirInstrIsLocalSet
  , llirInstrIsLoad
  , llirInstrIsStore
  , llirInstrIsCall
  , llirInstrIsCallIndirect
  , llirInstrIsCallExternal
  , llirInstrIsIf
  , llirInstrIsBlock
  , llirInstrIsLoop
  , llirInstrIsBr
  , llirInstrIsBrIf
  , llirInstrIsReturn
  , llirInstrIsBinOp
  , llirInstrIsCmpOp
  , llirCallTarget
    -- * Functions
  , LlirFunc(..)
  , llirFuncArity
  , llirFuncLocalCount
  , llirFuncFrameSize
  , isExportedFunc
    -- * Modules
  , LlirModule(..)
  , LlirTypeDef(..)
  , LlirMemory(..)
  , LlirImport(..)
  , LlirImportType(..)
  , LlirExport(..)
  , LlirExportKind(..)
  ) where

import Crisp.Core.Term (Type(..), Kind(..), EffectRow(..))

import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- LLIR Types
--------------------------------------------------------------------------------

-- | Low-level types suitable for WebAssembly
data LlirType
  = LlirI32              -- ^ 32-bit integer
  | LlirI64              -- ^ 64-bit integer
  | LlirF32              -- ^ 32-bit float
  | LlirF64              -- ^ 64-bit float
  | LlirPtr              -- ^ Pointer (i32 in wasm32)
  | LlirUnit             -- ^ Unit type (no value)
  | LlirStruct ![LlirType]  -- ^ Struct with fields
  | LlirTaggedUnion ![LlirType]  -- ^ Tagged union (max of alternatives)
  | LlirArray !LlirType  -- ^ Array of elements
  deriving stock (Eq, Show)

-- | Function type signature
data LlirFuncType = LlirFuncType
  { llirFuncTypeParams :: ![LlirType]
  , llirFuncTypeResult :: !LlirType
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Type Operations
--------------------------------------------------------------------------------

-- | Get the size of a type in bytes
llirTypeSize :: LlirType -> Int
llirTypeSize = \case
  LlirI32 -> 4
  LlirI64 -> 8
  LlirF32 -> 4
  LlirF64 -> 8
  LlirPtr -> 4  -- wasm32
  LlirUnit -> 0
  LlirStruct fields -> sum (map llirTypeSize fields)
  LlirTaggedUnion alts -> 4 + maximum (0 : map llirTypeSize alts)  -- tag + max payload
  LlirArray _ -> 4  -- Just the pointer; actual array is on heap

-- | Get the alignment of a type in bytes
llirTypeAlign :: LlirType -> Int
llirTypeAlign = \case
  LlirI32 -> 4
  LlirI64 -> 8
  LlirF32 -> 4
  LlirF64 -> 8
  LlirPtr -> 4
  LlirUnit -> 1
  LlirStruct [] -> 1
  LlirStruct fields -> maximum (map llirTypeAlign fields)
  LlirTaggedUnion [] -> 4  -- At least tag alignment
  LlirTaggedUnion alts -> max 4 (maximum (map llirTypeAlign alts))
  LlirArray _ -> 4

-- | Get the arity of a function type
llirFuncTypeArity :: LlirFuncType -> Int
llirFuncTypeArity = length . llirFuncTypeParams

-- | Get the return type of a function type
llirFuncTypeReturn :: LlirFuncType -> LlirType
llirFuncTypeReturn = llirFuncTypeResult

-- | Convert a Crisp type to LLIR type
toLlirType :: Type -> LlirType
toLlirType = \case
  TyCon "Int" [] -> LlirI32
  TyCon "Int32" [] -> LlirI32
  TyCon "Int64" [] -> LlirI64
  TyCon "Float" [] -> LlirF64
  TyCon "Float32" [] -> LlirF32
  TyCon "Float64" [] -> LlirF64
  TyCon "Bool" [] -> LlirI32  -- Bools are i32 in wasm
  TyCon "Unit" [] -> LlirUnit
  TyCon "String" [] -> LlirPtr  -- Strings are heap-allocated
  TyCon "Char" [] -> LlirI32  -- Unicode codepoint
  TyProp -> LlirUnit  -- Proofs are erased
  TyVar _ _ -> LlirPtr  -- Type variables become pointers (shouldn't happen after mono)
  TyPi _ _ _ _ -> LlirPtr  -- Function types are closures
  TyForall _ _ _ -> LlirPtr  -- Polymorphic types shouldn't exist after mono
  TyForallDep _ _ _ -> LlirPtr
  TySigma _ _ _ -> LlirPtr  -- Dependent pairs are heap-allocated
  TyLazy _ -> LlirPtr  -- Lazy values are thunks
  TyLinear t -> toLlirType t  -- Linear wrapper doesn't change representation
  TyRef _ -> LlirPtr  -- References are pointers
  TyRefMut _ -> LlirPtr
  TyUniverse _ -> LlirUnit  -- Type universe erased
  TyNatLit _ -> LlirI32  -- Type-level nats become runtime ints (if needed)
  TyAdd _ _ -> LlirI32
  TyEffect _ _ -> LlirPtr
  TyCon _ _ -> LlirPtr  -- Other type constructors are heap-allocated

-- | Convert LLIR type back to Crisp type (for testing)
fromLlirType :: LlirType -> Type
fromLlirType = \case
  LlirI32 -> TyCon "Int" []
  LlirI64 -> TyCon "Int64" []
  LlirF32 -> TyCon "Float32" []
  LlirF64 -> TyCon "Float" []
  LlirPtr -> TyCon "Ptr" []
  LlirUnit -> TyCon "Unit" []
  LlirStruct _ -> TyCon "Struct" []
  LlirTaggedUnion _ -> TyCon "Union" []
  LlirArray t -> TyCon "Array" [fromLlirType t]

--------------------------------------------------------------------------------
-- LLIR Values
--------------------------------------------------------------------------------

-- | Constant values
data LlirValue
  = LlirValI32 !Int
  | LlirValI64 !Integer
  | LlirValF32 !Float
  | LlirValF64 !Double
  | LlirValNull
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- LLIR Instructions
--------------------------------------------------------------------------------

-- | Binary operations
data LlirBinOp
  = LlirAdd | LlirSub | LlirMul | LlirDiv | LlirRem
  | LlirAnd | LlirOr | LlirXor
  | LlirShl | LlirShr | LlirShrU
  deriving stock (Eq, Show)

-- | Comparison operations
data LlirCmpOp
  = LlirEq | LlirNe
  | LlirLt | LlirLe | LlirGt | LlirGe
  | LlirLtU | LlirLeU | LlirGtU | LlirGeU
  deriving stock (Eq, Show)

-- | LLIR instructions (stack-based like WebAssembly)
data LlirInstr
  = LlirConst !LlirValue           -- ^ Push constant
  | LlirLocalGet !Int              -- ^ Get local variable
  | LlirLocalSet !Int              -- ^ Set local variable
  | LlirLocalTee !Int              -- ^ Set local and keep value on stack
  | LlirGlobalGet !Text            -- ^ Get global variable
  | LlirGlobalSet !Text            -- ^ Set global variable
  | LlirLoad !LlirType !Int        -- ^ Load from memory with offset
  | LlirStore !LlirType !Int       -- ^ Store to memory with offset
  | LlirCall !Text !Int            -- ^ Call function with arity
  | LlirCallIndirect !LlirFuncType -- ^ Indirect call through function table
  | LlirCallExternal !Text !Text !Int -- ^ Call external (FFI) function: module, name, arity
  | LlirDrop                       -- ^ Drop top of stack
  | LlirSelect                     -- ^ Select between two values
  | LlirBinOp !LlirBinOp           -- ^ Binary operation
  | LlirCmpOp !LlirCmpOp           -- ^ Comparison operation
  | LlirUnaryOp !Text              -- ^ Unary operation (neg, not, etc.)
  | LlirConvert !LlirType !LlirType  -- ^ Type conversion
  | LlirBlock !Text ![LlirInstr]   -- ^ Block with label
  | LlirLoop !Text ![LlirInstr]    -- ^ Loop with label
  | LlirIf !LlirType ![LlirInstr] ![LlirInstr]  -- ^ If-then-else
  | LlirBr !Text                   -- ^ Branch to label
  | LlirBrIf !Text                 -- ^ Conditional branch
  | LlirBrTable ![Text] !Text      -- ^ Branch table
  | LlirReturn                     -- ^ Return from function
  | LlirUnreachable                -- ^ Unreachable (trap)
  | LlirNop                        -- ^ No operation
  | LlirMemoryGrow                 -- ^ Grow memory
  | LlirMemorySize                 -- ^ Get memory size
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Instruction Predicates
--------------------------------------------------------------------------------

llirInstrIsConst :: LlirInstr -> Bool
llirInstrIsConst (LlirConst _) = True
llirInstrIsConst _ = False

llirInstrIsLocalGet :: LlirInstr -> Bool
llirInstrIsLocalGet (LlirLocalGet _) = True
llirInstrIsLocalGet _ = False

llirInstrIsLocalSet :: LlirInstr -> Bool
llirInstrIsLocalSet (LlirLocalSet _) = True
llirInstrIsLocalSet _ = False

llirInstrIsLoad :: LlirInstr -> Bool
llirInstrIsLoad (LlirLoad _ _) = True
llirInstrIsLoad _ = False

llirInstrIsStore :: LlirInstr -> Bool
llirInstrIsStore (LlirStore _ _) = True
llirInstrIsStore _ = False

llirInstrIsCall :: LlirInstr -> Bool
llirInstrIsCall (LlirCall _ _) = True
llirInstrIsCall _ = False

llirInstrIsCallIndirect :: LlirInstr -> Bool
llirInstrIsCallIndirect (LlirCallIndirect _) = True
llirInstrIsCallIndirect _ = False

llirInstrIsCallExternal :: LlirInstr -> Bool
llirInstrIsCallExternal (LlirCallExternal _ _ _) = True
llirInstrIsCallExternal _ = False

llirInstrIsIf :: LlirInstr -> Bool
llirInstrIsIf (LlirIf _ _ _) = True
llirInstrIsIf _ = False

llirInstrIsBlock :: LlirInstr -> Bool
llirInstrIsBlock (LlirBlock _ _) = True
llirInstrIsBlock _ = False

llirInstrIsLoop :: LlirInstr -> Bool
llirInstrIsLoop (LlirLoop _ _) = True
llirInstrIsLoop _ = False

llirInstrIsBr :: LlirInstr -> Bool
llirInstrIsBr (LlirBr _) = True
llirInstrIsBr _ = False

llirInstrIsBrIf :: LlirInstr -> Bool
llirInstrIsBrIf (LlirBrIf _) = True
llirInstrIsBrIf _ = False

llirInstrIsReturn :: LlirInstr -> Bool
llirInstrIsReturn LlirReturn = True
llirInstrIsReturn _ = False

llirInstrIsBinOp :: LlirInstr -> Bool
llirInstrIsBinOp (LlirBinOp _) = True
llirInstrIsBinOp _ = False

llirInstrIsCmpOp :: LlirInstr -> Bool
llirInstrIsCmpOp (LlirCmpOp _) = True
llirInstrIsCmpOp _ = False

-- | Get the target of a call instruction
llirCallTarget :: LlirInstr -> Maybe Text
llirCallTarget (LlirCall name _) = Just name
llirCallTarget _ = Nothing

--------------------------------------------------------------------------------
-- LLIR Functions
--------------------------------------------------------------------------------

-- | LLIR function definition
data LlirFunc = LlirFunc
  { llirFuncName   :: !Text
  , llirFuncParams :: ![(Text, LlirType)]
  , llirFuncReturn :: !LlirType
  , llirFuncLocals :: ![(Text, LlirType)]
  , llirFuncBody   :: ![LlirInstr]
  }
  deriving stock (Eq, Show)

-- | Get the arity of a function
llirFuncArity :: LlirFunc -> Int
llirFuncArity = length . llirFuncParams

-- | Get the total number of locals (params + locals)
llirFuncLocalCount :: LlirFunc -> Int
llirFuncLocalCount f = length (llirFuncParams f) + length (llirFuncLocals f)

-- | Compute the stack frame size for a function
llirFuncFrameSize :: LlirFunc -> Int
llirFuncFrameSize f =
  sum (map (llirTypeSize . snd) (llirFuncParams f)) +
  sum (map (llirTypeSize . snd) (llirFuncLocals f))

-- | Check if a function should be exported
isExportedFunc :: Text -> Bool
isExportedFunc name =
  name == "main" ||
  name == "_start" ||
  not (T.isPrefixOf "_" name)

--------------------------------------------------------------------------------
-- LLIR Module
--------------------------------------------------------------------------------

-- | LLIR module
data LlirModule = LlirModule
  { llirModFunctions :: ![LlirFunc]
  , llirModTypes     :: ![LlirTypeDef]
  , llirModMemory    :: !LlirMemory
  , llirModImports   :: ![LlirImport]
  , llirModExports   :: ![LlirExport]
  }
  deriving stock (Eq, Show)

-- | Type definition
data LlirTypeDef = LlirTypeDef
  { llirTypeDefName :: !Text
  , llirTypeDefType :: !LlirType
  }
  deriving stock (Eq, Show)

-- | Memory configuration
data LlirMemory = LlirMemory
  { llirMemPages    :: !Int        -- ^ Initial pages (64KB each)
  , llirMemMaxPages :: !(Maybe Int) -- ^ Maximum pages
  }
  deriving stock (Eq, Show)

-- | Import declaration
data LlirImport = LlirImport
  { llirImportModule :: !Text
  , llirImportName   :: !Text
  , llirImportType   :: !LlirImportType
  }
  deriving stock (Eq, Show)

-- | Import type
data LlirImportType
  = LlirImportFunc !LlirFuncType
  | LlirImportGlobal !LlirType !Bool  -- ^ Type and mutability
  | LlirImportMemory !Int !(Maybe Int)  -- ^ Min and max pages
  | LlirImportTable !Int !(Maybe Int)   -- ^ Min and max elements
  deriving stock (Eq, Show)

-- | Export declaration
data LlirExport = LlirExport
  { llirExportName :: !Text
  , llirExportKind :: !LlirExportKind
  }
  deriving stock (Eq, Show)

-- | Export kind
data LlirExportKind
  = LlirExportFunc !Text    -- ^ Export function by internal name
  | LlirExportGlobal !Text  -- ^ Export global
  | LlirExportMemory        -- ^ Export memory
  | LlirExportTable         -- ^ Export function table
  deriving stock (Eq, Show)
