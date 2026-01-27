{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Runtime.Allocator
-- Description : Runtime memory allocator for WebAssembly
--
-- Provides a simple bump allocator for WebAssembly memory management.
-- This allocator is bundled with compiled Wasm modules to handle
-- heap allocation for data structures, closures, and strings.
--
-- == Allocator Design
--
-- The bump allocator maintains a single heap pointer (global variable)
-- that starts after the data section and grows upward. Each allocation:
--
-- 1. Aligns the current heap pointer to the requested alignment
-- 2. Returns the aligned pointer as the allocation result
-- 3. Advances the heap pointer by the allocation size
-- 4. Grows memory if needed
--
-- == Memory Layout
--
-- @
-- +------------------+
-- |   Data Section   |  (static data, strings)
-- +------------------+
-- |   Heap Start     |  <- heapBase global
-- |        |         |
-- |        v         |
-- |   Heap Growth    |  <- heapPtr global (current allocation point)
-- |                  |
-- |   (free space)   |
-- |                  |
-- +------------------+
-- |   Stack (grows   |
-- |   down from top) |
-- +------------------+
-- @
--
-- == Limitations
--
-- This is a simple bump allocator with no deallocation support.
-- For production use, consider:
--
-- * Implementing a free list allocator
-- * Using garbage collection
-- * Importing an external allocator from the host environment

module Crisp.Runtime.Allocator
  ( -- * Allocator Configuration
    AllocatorConfig(..)
  , defaultAllocatorConfig
    -- * Allocator Generation
  , generateAllocator
  , generateAllocatorImport
  , allocatorFunctionIndex
  , allocatorGlobalIndex
    -- * Runtime Imports
  , RuntimeImport(..)
  , runtimeImports
    -- * Wasm Instructions
  , allocInstrs
  , allocAlignedInstrs
  , freeInstrs
    -- * Constants
  , heapBaseGlobalIndex
  , heapPtrGlobalIndex
  , allocFuncIndex
  , freeFuncIndex
  , memoryGrowFuncIndex
  ) where

import Crisp.Codegen.Wasm (WasmInstr(..), WasmValType(..))
import Crisp.Codegen.WasmBinary
  ( WasmFuncType(..)
  , funcType
  , WasmCodeEntry(..)
  , wasmCode
  )

import Data.Text (Text)

--------------------------------------------------------------------------------
-- Allocator Configuration
--------------------------------------------------------------------------------

-- | Configuration for the allocator
data AllocatorConfig = AllocatorConfig
  { acHeapBase       :: !Int    -- ^ Starting address of the heap (after data section)
  , acInitialPages   :: !Int    -- ^ Initial memory pages (64KB each)
  , acMaxPages       :: !(Maybe Int)  -- ^ Maximum memory pages (Nothing = unlimited)
  , acAlignment      :: !Int    -- ^ Default alignment (typically 8 for 64-bit values)
  , acGrowIncrement  :: !Int    -- ^ Pages to grow when out of memory
  }
  deriving stock (Eq, Show)

-- | Default allocator configuration
-- Starts heap at page 1 (after 64KB of potential data section)
defaultAllocatorConfig :: AllocatorConfig
defaultAllocatorConfig = AllocatorConfig
  { acHeapBase = 65536      -- Start at 64KB (page 1)
  , acInitialPages = 2      -- Start with 2 pages (128KB total)
  , acMaxPages = Just 256   -- Max 16MB
  , acAlignment = 8         -- 8-byte alignment by default
  , acGrowIncrement = 1     -- Grow by 1 page at a time
  }

--------------------------------------------------------------------------------
-- Function and Global Indices
--------------------------------------------------------------------------------

-- | Index of the alloc function in the module
-- This is typically 0 if alloc is the first defined function
allocFuncIndex :: Int
allocFuncIndex = 0

-- | Index of the free function (stub - no-op in bump allocator)
freeFuncIndex :: Int
freeFuncIndex = 1

-- | Index of the memory grow helper function
memoryGrowFuncIndex :: Int
memoryGrowFuncIndex = 2

-- | Index of the heap base global variable
heapBaseGlobalIndex :: Int
heapBaseGlobalIndex = 0

-- | Index of the heap pointer global variable
heapPtrGlobalIndex :: Int
heapPtrGlobalIndex = 1

-- | Get the allocator function index
allocatorFunctionIndex :: Int
allocatorFunctionIndex = allocFuncIndex

-- | Get the allocator global index (heap pointer)
allocatorGlobalIndex :: Int
allocatorGlobalIndex = heapPtrGlobalIndex

--------------------------------------------------------------------------------
-- Runtime Imports
--------------------------------------------------------------------------------

-- | Runtime import specification
data RuntimeImport = RuntimeImport
  { riModule :: !Text           -- ^ Module name (e.g., "crisp_runtime")
  , riName   :: !Text           -- ^ Function name
  , riParams :: ![WasmValType]  -- ^ Parameter types
  , riResult :: ![WasmValType]  -- ^ Result types
  }
  deriving stock (Eq, Show)

-- | List of runtime imports required if using external allocator
-- An alternative to bundling the allocator is to import these functions
runtimeImports :: [RuntimeImport]
runtimeImports =
  [ RuntimeImport
      { riModule = "crisp_runtime"
      , riName = "alloc"
      , riParams = [WasmI32]    -- size in bytes
      , riResult = [WasmI32]    -- pointer to allocated memory
      }
  , RuntimeImport
      { riModule = "crisp_runtime"
      , riName = "alloc_aligned"
      , riParams = [WasmI32, WasmI32]  -- size, alignment
      , riResult = [WasmI32]            -- pointer
      }
  , RuntimeImport
      { riModule = "crisp_runtime"
      , riName = "free"
      , riParams = [WasmI32]    -- pointer
      , riResult = []           -- void
      }
  ]

-- | Generate import declarations for external allocator
generateAllocatorImport :: [(Text, Text, WasmFuncType)]
generateAllocatorImport =
  [ ("crisp_runtime", "alloc", funcType [WasmI32] [WasmI32])
  , ("crisp_runtime", "free", funcType [WasmI32] [])
  ]

--------------------------------------------------------------------------------
-- Allocator Generation
--------------------------------------------------------------------------------

-- | Generate the complete allocator as Wasm functions
-- Returns: (functions, globals, memory config)
generateAllocator :: AllocatorConfig
                  -> ([WasmCodeEntry], [(WasmValType, Bool, Int)], (Int, Maybe Int))
generateAllocator config =
  let -- Functions
      allocFunc = generateAllocFunc config
      freeFunc = generateFreeFunc
      ensureMemoryFunc = generateEnsureMemoryFunc config

      -- Globals: (type, mutable, initial value)
      globals =
        [ (WasmI32, False, acHeapBase config)     -- heap_base (immutable)
        , (WasmI32, True, acHeapBase config)      -- heap_ptr (mutable, starts at base)
        ]

      -- Memory configuration
      memory = (acInitialPages config, acMaxPages config)

  in ([allocFunc, freeFunc, ensureMemoryFunc], globals, memory)

-- | Generate the alloc function
-- alloc(size: i32) -> i32
--
-- Implementation:
--   1. Align current heap_ptr to 8-byte boundary
--   2. Save aligned pointer as result
--   3. Compute new heap_ptr = result + size
--   4. If new_ptr > memory_size * PAGE_SIZE, grow memory
--   5. Update heap_ptr
--   6. Return result
generateAllocFunc :: AllocatorConfig -> WasmCodeEntry
generateAllocFunc config = wasmCode
  [(1, WasmI32), (1, WasmI32)]  -- locals: [result, new_ptr]
  allocBody
  where
    alignment = acAlignment config
    localResult = 1   -- Local index for result
    localNewPtr = 2   -- Local index for new_ptr

    allocBody =
      -- Align heap_ptr: aligned = (heap_ptr + (align-1)) & ~(align-1)
      [ WGlobalGet heapPtrGlobalIndex           -- heap_ptr
      , WI32Const (alignment - 1)               -- align - 1
      , WI32Add                                 -- heap_ptr + (align - 1)
      , WI32Const (negate alignment)            -- ~(align - 1) = -align in two's complement
      , WI32And                                 -- aligned result
      , WLocalTee localResult                   -- store in result, keep on stack

      -- Compute new_ptr = result + size
      , WLocalGet 0                             -- size parameter
      , WI32Add                                 -- result + size
      , WLocalSet localNewPtr                   -- store in new_ptr

      -- Check if we need to grow memory
      -- if (new_ptr > memory.size * PAGE_SIZE)
      , WLocalGet localNewPtr                   -- new_ptr
      , WMemorySize                             -- current pages
      , WI32Const 16                            -- log2(PAGE_SIZE) = 16
      , WI32Shl                                 -- pages * 65536
      , WI32GtU                                 -- new_ptr > memory_size?
      , WIf
          -- Then: grow memory
          [ WLocalGet localNewPtr               -- new_ptr
            , WI32Const 16
            , WI32ShrU                          -- new_ptr / PAGE_SIZE
            , WI32Const 1
            , WI32Add                           -- + 1 page for safety
            , WMemorySize                       -- current pages
            , WI32Sub                           -- pages_needed - current_pages
            , WMemoryGrow                       -- grow memory
            , WI32Const (-1)                    -- check for failure
            , WI32Eq
            , WIf
                [WUnreachable]                  -- Out of memory - trap
                []
          ]
          -- Else: do nothing
          []

      -- Update heap_ptr
      , WLocalGet localNewPtr
      , WGlobalSet heapPtrGlobalIndex

      -- Return result
      , WLocalGet localResult
      ]

-- | Generate the free function (stub - no-op for bump allocator)
-- free(ptr: i32) -> void
generateFreeFunc :: WasmCodeEntry
generateFreeFunc = wasmCode [] []  -- No-op: bump allocator doesn't free

-- | Generate memory growth helper function
-- ensure_memory(min_ptr: i32) -> i32 (0 = success, -1 = failure)
generateEnsureMemoryFunc :: AllocatorConfig -> WasmCodeEntry
generateEnsureMemoryFunc _config = wasmCode
  [(1, WasmI32)]  -- local for pages_needed
  [ -- Calculate pages needed
    WLocalGet 0                               -- min_ptr
  , WI32Const 16
  , WI32ShrU                                  -- min_ptr / PAGE_SIZE
  , WI32Const 1
  , WI32Add                                   -- + 1 for safety
  , WLocalSet 1                               -- pages_needed

    -- Check if we need to grow
  , WLocalGet 1                               -- pages_needed
  , WMemorySize                               -- current_pages
  , WI32GtU                                   -- need more pages?
  , WIf
      -- Then: grow memory
      [ WLocalGet 1                           -- pages_needed
      , WMemorySize                           -- current_pages
      , WI32Sub                               -- delta
      , WMemoryGrow                           -- grow and return result
      , WReturn
      ]
      -- Else: already have enough
      []

    -- Success
  , WI32Const 0
  ]

--------------------------------------------------------------------------------
-- Instruction Helpers
--------------------------------------------------------------------------------

-- | Generate instructions to allocate memory
-- Expects: size on stack
-- Returns: pointer on stack
allocInstrs :: [WasmInstr]
allocInstrs = [WCall allocFuncIndex]

-- | Generate instructions to allocate aligned memory
-- Expects: size, alignment on stack
-- Returns: pointer on stack
-- Note: Current implementation uses default alignment; the alignment
-- parameter is reserved for future use when a separate aligned alloc is added
allocAlignedInstrs :: Int -> [WasmInstr]
allocAlignedInstrs _alignment =
  -- For now, just use regular alloc (which aligns to default)
  -- A more sophisticated version would have a separate aligned alloc
  [ WCall allocFuncIndex
  ]

-- | Generate instructions to free memory (no-op for bump allocator)
-- Expects: pointer on stack
-- Returns: nothing
freeInstrs :: [WasmInstr]
freeInstrs =
  [ WDrop  -- Just drop the pointer, bump allocator doesn't free
  ]
