{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Prelude.Effects
-- Description : Standard effect definitions for the Crisp prelude
--
-- Defines the common effects available in the Crisp standard library:
-- State, Reader, Writer, Exception, IO, and NonDet.
--
-- These effects are defined as Haskell data structures that can be
-- used to generate Crisp effect definitions and for type checking.

module Crisp.Prelude.Effects
  ( -- * Effect Definitions
    PreludeEffect(..)
  , EffectOperation(..)
    -- * Standard Effects
  , stateEffect
  , readerEffect
  , writerEffect
  , exceptionEffect
  , ioEffect
  , nondetEffect
  , asyncEffect
    -- * All Effects
  , preludeEffects
    -- * Effect Lookup
  , lookupPreludeEffect
  , lookupEffectOperation
  , isPreludeEffect
  , isEffectOperation
    -- * Core Representations
  , effectOperationType
  ) where

import Crisp.Core.Term (Type(..), Kind(..), EffectRow(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Effect Definitions
--------------------------------------------------------------------------------

-- | A prelude effect definition
data PreludeEffect = PreludeEffect
  { effectName       :: !Text                -- ^ Effect name (e.g., "State")
  , effectTypeParams :: ![(Text, Kind)]      -- ^ Type parameters (e.g., [("S", KiType 0)])
  , effectOperations :: ![EffectOperation]   -- ^ Operations provided by the effect
  } deriving stock (Eq, Show)

-- | An operation within an effect
data EffectOperation = EffectOperation
  { opName       :: !Text        -- ^ Operation name (e.g., "get")
  , opParamType  :: !Type        -- ^ Parameter type (Unit for no params)
  , opReturnType :: !Type        -- ^ Return type
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Type variable shorthand
tyVar :: Text -> Int -> Type
tyVar = TyVar

-- | Type constructor shorthand
tyCon :: Text -> [Type] -> Type
tyCon = TyCon

-- | Unit type shorthand
unitTy :: Type
unitTy = tyCon "Unit" []

--------------------------------------------------------------------------------
-- Standard Effect Definitions
--------------------------------------------------------------------------------

-- | State effect: State S { get() -> S, put(S) -> Unit }
stateEffect :: PreludeEffect
stateEffect = PreludeEffect
  { effectName = "State"
  , effectTypeParams = [("S", KiType 0)]
  , effectOperations =
      [ EffectOperation "get" unitTy (tyVar "S" 0)
      , EffectOperation "put" (tyVar "S" 0) unitTy
      , EffectOperation "modify" (TyPi "_" (tyVar "S" 0) EffEmpty (tyVar "S" 0)) unitTy
      ]
  }

-- | Reader effect: Reader R { ask() -> R, local((R -> R), A) -> A }
readerEffect :: PreludeEffect
readerEffect = PreludeEffect
  { effectName = "Reader"
  , effectTypeParams = [("R", KiType 0)]
  , effectOperations =
      [ EffectOperation "ask" unitTy (tyVar "R" 0)
      , EffectOperation "local"
          (tyCon "Pair"
            [ TyPi "_" (tyVar "R" 0) EffEmpty (tyVar "R" 0)  -- R -> R
            , TyForall "A" (KiType 0) (tyVar "A" 0)          -- A (the computation)
            ])
          (TyForall "A" (KiType 0) (tyVar "A" 0))
      ]
  }

-- | Writer effect: Writer W { tell(W) -> Unit, listen(A) -> Pair A W }
writerEffect :: PreludeEffect
writerEffect = PreludeEffect
  { effectName = "Writer"
  , effectTypeParams = [("W", KiType 0)]
  , effectOperations =
      [ EffectOperation "tell" (tyVar "W" 0) unitTy
      ]
  }

-- | Exception effect: Exception E { raise(E) -> A }
exceptionEffect :: PreludeEffect
exceptionEffect = PreludeEffect
  { effectName = "Exception"
  , effectTypeParams = [("E", KiType 0)]
  , effectOperations =
      [ EffectOperation "raise" (tyVar "E" 0) (TyForall "A" (KiType 0) (tyVar "A" 0))
      ]
  }

-- | IO effect: IO { print(String) -> Unit, read_line() -> String }
ioEffect :: PreludeEffect
ioEffect = PreludeEffect
  { effectName = "IO"
  , effectTypeParams = []
  , effectOperations =
      [ EffectOperation "print" (tyCon "String" []) unitTy
      , EffectOperation "read_line" unitTy (tyCon "String" [])
      , EffectOperation "read_file" (tyCon "String" []) (tyCon "Result" [tyCon "String" [], tyCon "String" []])
      , EffectOperation "write_file"
          (tyCon "Pair" [tyCon "String" [], tyCon "String" []])
          (tyCon "Result" [unitTy, tyCon "String" []])
      ]
  }

-- | NonDet effect: NonDet { choose(List A) -> A, fail() -> A }
nondetEffect :: PreludeEffect
nondetEffect = PreludeEffect
  { effectName = "NonDet"
  , effectTypeParams = []
  , effectOperations =
      [ EffectOperation "choose"
          (TyForall "A" (KiType 0) (tyCon "List" [tyVar "A" 0]))
          (TyForall "A" (KiType 0) (tyVar "A" 0))
      , EffectOperation "fail" unitTy (TyForall "A" (KiType 0) (tyVar "A" 0))
      ]
  }

-- | Async effect: Async { fork(A) -> Task A, await(Task A) -> A }
asyncEffect :: PreludeEffect
asyncEffect = PreludeEffect
  { effectName = "Async"
  , effectTypeParams = []
  , effectOperations =
      [ EffectOperation "fork"
          (TyForall "A" (KiType 0) (TyLazy (tyVar "A" 0)))
          (TyForall "A" (KiType 0) (tyCon "Task" [tyVar "A" 0]))
      , EffectOperation "await"
          (TyForall "A" (KiType 0) (tyCon "Task" [tyVar "A" 0]))
          (TyForall "A" (KiType 0) (tyVar "A" 0))
      , EffectOperation "yield" unitTy unitTy
      ]
  }

--------------------------------------------------------------------------------
-- All Effects
--------------------------------------------------------------------------------

-- | All standard prelude effects
preludeEffects :: [PreludeEffect]
preludeEffects =
  [ stateEffect
  , readerEffect
  , writerEffect
  , exceptionEffect
  , ioEffect
  , nondetEffect
  , asyncEffect
  ]

--------------------------------------------------------------------------------
-- Effect Lookup
--------------------------------------------------------------------------------

-- | Map of effect names to definitions
preludeEffectMap :: Map Text PreludeEffect
preludeEffectMap = Map.fromList [(effectName e, e) | e <- preludeEffects]

-- | Map of operation names to their definitions and parent effects
preludeOperationMap :: Map (Text, Text) (EffectOperation, PreludeEffect)
preludeOperationMap = Map.fromList
  [ ((effectName e, opName op), (op, e))
  | e <- preludeEffects
  , op <- effectOperations e
  ]

-- | Look up a prelude effect by name
lookupPreludeEffect :: Text -> Maybe PreludeEffect
lookupPreludeEffect name = Map.lookup name preludeEffectMap

-- | Look up an effect operation by effect and operation name
lookupEffectOperation :: Text -> Text -> Maybe (EffectOperation, PreludeEffect)
lookupEffectOperation effName opNm = Map.lookup (effName, opNm) preludeOperationMap

-- | Check if a name is a prelude effect
isPreludeEffect :: Text -> Bool
isPreludeEffect name = Map.member name preludeEffectMap

-- | Check if an operation exists in a prelude effect
isEffectOperation :: Text -> Text -> Bool
isEffectOperation effName opNm = Map.member (effName, opNm) preludeOperationMap

--------------------------------------------------------------------------------
-- Core Representations
--------------------------------------------------------------------------------

-- | Get the type of an effect operation (as a function from param to return)
effectOperationType :: EffectOperation -> Type
effectOperationType op = TyPi "_" (opParamType op) EffEmpty (opReturnType op)
