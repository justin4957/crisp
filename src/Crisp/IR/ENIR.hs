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
    -- * Continuation
  , Continuation(..)
    -- * Transformation
  , toENIR
  ) where

import Crisp.Core.Term (Term(..), Type(..), Kind(..), EffectRow(..), Effect(..), Pattern(..), Case(..), Handler(..))
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

  TmHandle _handler body ->
    -- Handler transformation is complex - placeholder
    transformTerm body

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
