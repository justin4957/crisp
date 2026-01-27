{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.IR.ENIR
-- Description : Effect-Normalized Intermediate Representation
--
-- After proof erasure and CPS transformation of effects.
-- This IR has explicit continuation passing for effect operations.

module Crisp.IR.ENIR
  ( -- * Terms
    ENIRTerm(..)
  , ENIRType(..)
  , ENIRValue(..)
    -- * Handlers
  , ENIRHandler(..)
  , ENIROpHandler(..)
  , ENIRReturnHandler(..)
    -- * Continuation
  , Continuation(..)
    -- * Transformation
  , toENIR
  , transformHandler
  , transformOpHandler
  ) where

import Crisp.Core.Term (Term(..), Type(..), Kind(..), EffectRow(..), Effect(..), Pattern(..), Case(..), Handler(..), OpHandler(..), ReturnHandler(..))
import qualified Crisp.Core.Term as Core

import Data.Text (Text)

-- | ENIR terms (continuation-passing style)
data ENIRTerm
  = ENIRVar !Text !Int
  | ENIRLam !Text !ENIRType !ENIRTerm
  | ENIRApp !ENIRTerm !ENIRValue
  | ENIRLet !Text !ENIRType !ENIRTerm !ENIRTerm
  | ENIRCon !Text ![ENIRValue]
  | ENIRMatch !ENIRValue ![(Pattern, ENIRTerm)]
  | ENIRCall !Text !Text !ENIRValue !Continuation  -- ^ Effect call with continuation
  | ENIRHandle !ENIRHandler !ENIRTerm              -- ^ Effect handler around a body
  | ENIRReturn !ENIRValue
  deriving stock (Eq, Show)

-- | ENIR values (fully evaluated)
data ENIRValue
  = ENIRVVar !Text !Int
  | ENIRVCon !Text ![ENIRValue]
  | ENIRVLam !Text !ENIRType !ENIRTerm
  deriving stock (Eq, Show)

-- | ENIR types (no Prop types, no dependent types at runtime)
data ENIRType
  = ENIRTyCon !Text ![ENIRType]
  | ENIRTyFn !ENIRType !ENIRType
  | ENIRTyLazy !ENIRType
  deriving stock (Eq, Show)

-- | A continuation for effect handling
data Continuation = Continuation
  { contParam :: !Text
  , contBody  :: !ENIRTerm
  } deriving stock (Eq, Show)

-- | An effect handler in ENIR
-- Handlers intercept effect operations and provide implementations
data ENIRHandler = ENIRHandler
  { enirHandlerEffect     :: !Text                -- ^ Effect being handled (e.g., "State")
  , enirHandlerOps        :: ![ENIROpHandler]     -- ^ Operation handlers
  , enirHandlerReturn     :: !ENIRReturnHandler   -- ^ Return clause
  } deriving stock (Eq, Show)

-- | Handler for a single effect operation
-- The resumption is represented as a continuation parameter
data ENIROpHandler = ENIROpHandler
  { enirOpName      :: !Text        -- ^ Operation name (e.g., "get", "put")
  , enirOpParam     :: !Text        -- ^ Parameter name for operation argument
  , enirOpParamType :: !ENIRType    -- ^ Type of the operation argument
  , enirOpResume    :: !Text        -- ^ Name of the resumption continuation
  , enirOpBody      :: !ENIRTerm    -- ^ Handler body (can call resume)
  } deriving stock (Eq, Show)

-- | Handler for the return value
-- Transforms the final result of the handled computation
data ENIRReturnHandler = ENIRReturnHandler
  { enirReturnParam     :: !Text      -- ^ Parameter name for return value
  , enirReturnParamType :: !ENIRType  -- ^ Type of the return value
  , enirReturnBody      :: !ENIRTerm  -- ^ Transformation of the return value
  } deriving stock (Eq, Show)

-- | Transform Core terms to ENIR (placeholder implementation)
toENIR :: Core.Term -> ENIRTerm
toENIR = transformTerm

transformTerm :: Core.Term -> ENIRTerm
transformTerm term = case term of
  TmVar name idx ->
    ENIRReturn (ENIRVVar name idx)

  TmLam name ty body ->
    ENIRReturn (ENIRVLam name (transformType ty) (transformTerm body))

  TmApp func arg ->
    -- ANF transformation would be more complex
    let func' = transformTerm func
        arg' = transformToValue arg
    in ENIRApp (extractLam func') arg'

  TmLet name ty val body ->
    ENIRLet name (transformType ty) (transformTerm val) (transformTerm body)

  TmTyAbs _ _ body ->
    -- Type abstraction is erased
    transformTerm body

  TmTyApp tm _ ->
    -- Type application is erased
    transformTerm tm

  TmCon name _ args ->
    ENIRReturn (ENIRVCon name (map transformToValue args))

  TmMatch subj _ cases ->
    ENIRMatch (transformToValue subj) (map transformCase cases)

  TmPerform effect op arg ->
    -- Transform to CPS-style effect call
    ENIRCall effect op (transformToValue arg) (Continuation "result" (ENIRReturn (ENIRVVar "result" 0)))

  TmHandle handler body ->
    -- Transform handler and wrap the body
    ENIRHandle (transformHandler handler) (transformTerm body)

  TmLazy body ->
    -- Lazy becomes a lambda
    ENIRReturn (ENIRVLam "_" (ENIRTyCon "Unit" []) (transformTerm body))

  TmForce tm ->
    -- Force becomes application to unit
    ENIRApp (extractLam (transformTerm tm)) (ENIRVCon "Unit" [])

  TmAnnot tm _ ->
    transformTerm tm

transformType :: Core.Type -> ENIRType
transformType ty = case ty of
  TyCon name args -> ENIRTyCon name (map transformType args)
  TyPi _ from _ to -> ENIRTyFn (transformType from) (transformType to)
  TyForall _ _ body -> transformType body
  TyForallDep _ _ body -> transformType body
  TyLazy inner -> ENIRTyLazy (transformType inner)
  TyLinear inner -> transformType inner
  TyRef inner -> transformType inner
  TyRefMut inner -> transformType inner
  TyUniverse _ -> ENIRTyCon "Type" []
  TyProp -> ENIRTyCon "Unit" []  -- Prop is erased to Unit
  TyVar name idx -> ENIRTyCon name []  -- Type variables become concrete

transformToValue :: Core.Term -> ENIRValue
transformToValue term = case term of
  TmVar name idx -> ENIRVVar name idx
  TmCon name _ args -> ENIRVCon name (map transformToValue args)
  TmLam name ty body -> ENIRVLam name (transformType ty) (transformTerm body)
  _ -> ENIRVVar "_complex" 0  -- Placeholder for complex terms

transformCase :: Core.Case -> (Pattern, ENIRTerm)
transformCase (Case pat body) = (pat, transformTerm body)

extractLam :: ENIRTerm -> ENIRTerm
extractLam (ENIRReturn (ENIRVLam name ty body)) = ENIRLam name ty body
extractLam other = other

-- | Transform a Core handler to ENIR representation
-- Converts operation handlers to CPS-style with explicit resumption continuations
transformHandler :: Handler -> ENIRHandler
transformHandler handler = ENIRHandler
  { enirHandlerEffect = handlerEffect handler
  , enirHandlerOps    = map transformOpHandler (handlerOperations handler)
  , enirHandlerReturn = transformReturnHandler (handlerReturn handler)
  }

-- | Transform an operation handler
-- The resumption becomes an explicit continuation parameter
transformOpHandler :: OpHandler -> ENIROpHandler
transformOpHandler opHandler = ENIROpHandler
  { enirOpName      = opHandlerOperation opHandler
  , enirOpParam     = patternToName (opHandlerPattern opHandler)
  , enirOpParamType = patternToType (opHandlerPattern opHandler)
  , enirOpResume    = opHandlerResume opHandler
  , enirOpBody      = transformTerm (opHandlerBody opHandler)
  }

-- | Transform a return handler
transformReturnHandler :: ReturnHandler -> ENIRReturnHandler
transformReturnHandler retHandler = ENIRReturnHandler
  { enirReturnParam     = patternToName (returnHandlerPattern retHandler)
  , enirReturnParamType = patternToType (returnHandlerPattern retHandler)
  , enirReturnBody      = transformTerm (returnHandlerBody retHandler)
  }

-- | Extract a name from a pattern (for simple variable patterns)
patternToName :: Pattern -> Text
patternToName (PatVar name) = name
patternToName PatWild = "_"
patternToName (PatCon _ _) = "_arg"  -- Constructor patterns get generic name

-- | Extract a type from a pattern (placeholder - patterns don't carry types)
patternToType :: Pattern -> ENIRType
patternToType _ = ENIRTyCon "Any" []  -- Placeholder type, would be inferred
