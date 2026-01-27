{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.IR.Layout
-- Description : Data layout computation for LLIR
--
-- Computes memory layouts for data types:
-- - Struct layouts with field offsets
-- - Tagged union layouts with tag size and max payload
-- - Closure representations
-- - Alignment and padding
--
-- Used for WebAssembly memory management and data access.

module Crisp.IR.Layout
  ( -- * Layout Types
    Layout(..)
  , TypeDef(..)
  , ConstrDef(..)
    -- * Layout Computation
  , computeLayout
  , computeTypeDefLayout
  , layoutFieldOffsets
    -- * Closure Representation
  , ClosureRep(..)
  , closureCaptureCount
  , closureSize
  , generateClosureAlloc
  , generateClosureCall
  ) where

import Crisp.IR.LLIR
import Crisp.Core.Term (Type(..), Kind(..))

import Data.Text (Text)
import Data.List (foldl')

--------------------------------------------------------------------------------
-- Layout Types
--------------------------------------------------------------------------------

-- | Memory layout for a type
data Layout = Layout
  { layoutSize         :: !Int           -- ^ Total size in bytes
  , layoutAlign        :: !Int           -- ^ Alignment requirement
  , layoutTag          :: !(Maybe Int)   -- ^ Tag size for unions (Nothing for structs)
  , layoutFieldOffsets :: ![Int]         -- ^ Offsets of each field
  }
  deriving stock (Eq, Show)

-- | Type definition for layout computation
data TypeDef = TypeDef
  { typeDefName    :: !Text
  , typeDefParams  :: ![(Text, Kind)]
  , typeDefConstrs :: ![ConstrDef]
  }
  deriving stock (Eq, Show)

-- | Constructor definition
data ConstrDef = ConstrDef
  { constrDefName   :: !Text
  , constrDefFields :: ![Type]
  , constrDefReturn :: !Type
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Layout Computation
--------------------------------------------------------------------------------

-- | Compute layout for a Crisp type
computeLayout :: Type -> Layout
computeLayout ty =
  let llirTy = toLlirType ty
      size = llirTypeSize llirTy
      align = llirTypeAlign llirTy
  in Layout
       { layoutSize = size
       , layoutAlign = align
       , layoutTag = Nothing
       , layoutFieldOffsets = []
       }

-- | Compute layout for a type definition
computeTypeDefLayout :: TypeDef -> Layout
computeTypeDefLayout def
  | null (typeDefConstrs def) =
      -- Empty type (no constructors)
      Layout { layoutSize = 0, layoutAlign = 1, layoutTag = Nothing, layoutFieldOffsets = [] }
  | length (typeDefConstrs def) == 1 =
      -- Single constructor - struct layout
      computeStructLayout (head (typeDefConstrs def))
  | otherwise =
      -- Multiple constructors - tagged union layout
      computeUnionLayout (typeDefConstrs def)

-- | Compute struct layout for a single constructor
computeStructLayout :: ConstrDef -> Layout
computeStructLayout constr =
  let fieldTypes = map toLlirType (constrDefFields constr)
      (offsets, totalSize, maxAlign) = computeFieldLayouts fieldTypes
  in Layout
       { layoutSize = totalSize
       , layoutAlign = max 1 maxAlign
       , layoutTag = Nothing
       , layoutFieldOffsets = offsets
       }

-- | Compute tagged union layout for multiple constructors
computeUnionLayout :: [ConstrDef] -> Layout
computeUnionLayout constrs =
  let tagSize = 4  -- 4-byte tag
      tagAlign = 4
      -- Compute layout for each constructor's fields
      fieldLayouts = map (computeFieldLayouts . map toLlirType . constrDefFields) constrs
      -- Find maximum payload size and alignment
      maxPayloadSize = maximum (0 : map (\(_, s, _) -> s) fieldLayouts)
      maxPayloadAlign = maximum (1 : map (\(_, _, a) -> a) fieldLayouts)
      -- Union alignment is max of tag and payload alignments
      unionAlign = max tagAlign maxPayloadAlign
      -- Payload starts after tag, aligned
      payloadOffset = alignTo tagSize unionAlign
      -- Total size is payload offset + max payload size, aligned
      totalSize = alignTo (payloadOffset + maxPayloadSize) unionAlign
  in Layout
       { layoutSize = totalSize
       , layoutAlign = unionAlign
       , layoutTag = Just tagSize
       , layoutFieldOffsets = [payloadOffset]  -- All fields start at payload offset
       }

-- | Compute field layouts for a list of field types
-- Returns (offsets, total size, max alignment)
computeFieldLayouts :: [LlirType] -> ([Int], Int, Int)
computeFieldLayouts [] = ([], 0, 1)
computeFieldLayouts fieldTypes =
  let sizes = map llirTypeSize fieldTypes
      aligns = map llirTypeAlign fieldTypes
      -- Compute offsets with alignment
      (offsets, finalOffset) = computeOffsets sizes aligns
      maxAlign = maximum (1 : aligns)
      -- Final size is aligned to max alignment
      totalSize = alignTo finalOffset maxAlign
  in (offsets, totalSize, maxAlign)

-- | Compute offsets for fields with alignment
computeOffsets :: [Int] -> [Int] -> ([Int], Int)
computeOffsets sizes aligns =
  let pairs = zip sizes aligns
      go (offset, offsets) (size, align) =
        let alignedOffset = alignTo offset align
        in (alignedOffset + size, alignedOffset : offsets)
      (finalOffset, revOffsets) = foldl' go (0, []) pairs
  in (reverse revOffsets, finalOffset)

-- | Align a value to the next multiple of alignment
alignTo :: Int -> Int -> Int
alignTo value alignment
  | alignment <= 0 = value
  | remainder == 0 = value
  | otherwise = value + (alignment - remainder)
  where
    remainder = value `mod` alignment

--------------------------------------------------------------------------------
-- Closure Representation
--------------------------------------------------------------------------------

-- | Closure representation
data ClosureRep = ClosureRep
  { closureFuncPtr  :: !Text              -- ^ Function pointer name
  , closureCaptures :: ![(Text, LlirType)] -- ^ Captured variables
  }
  deriving stock (Eq, Show)

-- | Get the number of captured variables
closureCaptureCount :: ClosureRep -> Int
closureCaptureCount = length . closureCaptures

-- | Compute the size of a closure in bytes
closureSize :: ClosureRep -> Int
closureSize closure =
  let ptrSize = 4  -- Function pointer
      captureSize = sum (map (llirTypeSize . snd) (closureCaptures closure))
  in ptrSize + captureSize

-- | Generate instructions to allocate a closure
-- Uses the runtime allocator (function at allocFuncIndex)
generateClosureAlloc :: ClosureRep -> [LlirInstr]
generateClosureAlloc closure =
  let size = closureSize closure
      -- Allocate memory using runtime allocator
      allocateInstrs =
        [ LlirConst (LlirValI32 size)
        , LlirCall "_crisp_alloc" 1  -- Call runtime allocator
        ]
      -- Store function pointer at offset 0
      storeFuncPtr =
        [ LlirLocalTee 0  -- Keep pointer on stack and store in local
        , LlirConst (LlirValI32 0)  -- Offset 0
        , LlirBinOp LlirAdd
        -- Would need to get function address here
        , LlirStore LlirPtr 0
        ]
      -- Store captured variables
      storeCaptures = concatMap (storeCapture 4) (zip [0..] (closureCaptures closure))
  in allocateInstrs ++ storeFuncPtr ++ storeCaptures
  where
    storeCapture baseOffset (idx, (_name, ty)) =
      let offset = baseOffset + idx * 4  -- Simplified: all 4 bytes
      in [ LlirLocalGet 0  -- Get closure pointer
         , LlirConst (LlirValI32 offset)
         , LlirBinOp LlirAdd
         -- Would load the captured variable here
         , LlirStore ty 0
         ]

-- | Generate instructions to call a closure
generateClosureCall :: ClosureRep -> [LlirInstr]
generateClosureCall closure =
  let -- Load function pointer from closure
      loadFuncPtr =
        [ LlirLocalGet 0  -- Assume closure pointer is in local 0
        , LlirLoad LlirPtr 0  -- Load function pointer at offset 0
        ]
      -- Load captured variables
      loadCaptures = concatMap loadCapture (zip [0..] (closureCaptures closure))
      -- Indirect call
      funcType = LlirFuncType (map snd (closureCaptures closure)) LlirI32  -- Simplified
      callInstr = [LlirCallIndirect funcType]
  in loadFuncPtr ++ loadCaptures ++ callInstr
  where
    loadCapture (idx, (_, ty)) =
      let offset = 4 + idx * 4  -- After function pointer
      in [ LlirLocalGet 0
         , LlirConst (LlirValI32 offset)
         , LlirBinOp LlirAdd
         , LlirLoad ty 0
         ]
