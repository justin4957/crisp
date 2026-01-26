{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Types.Unify
-- Description : Type unification algorithm
--
-- Implements the type unification algorithm for Crisp's bidirectional type checker.
-- Unification finds the most general unifier (MGU) for two types, enabling
-- type inference through constraint solving.
--
-- The algorithm handles:
-- - Type variables: bind to concrete types
-- - Function types (TyPi): unify domains and codomains
-- - Type constructors (TyCon): match names and unify arguments
-- - Forall types (TyForall): unify under the binder
-- - Wrapper types (TyLazy, TyRef, etc.): unify inner types
-- - Effect rows: unify effect variables and concrete effects

module Crisp.Types.Unify
  ( -- * Unification
    unify
  , unifyEffects
  , unifyList
    -- * Errors
  , UnifyError(..)
    -- * Re-exports
  , module Crisp.Types.Substitution
  ) where

import Crisp.Core.Term
import Crisp.Types.Substitution

import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T

-- | Errors that can occur during unification
data UnifyError
  = TypeMismatch !Type !Type           -- ^ Types cannot be unified
  | OccursCheck !Int !Type             -- ^ Infinite type would result
  | KindMismatch !Kind !Kind           -- ^ Kind mismatch in forall
  | ArityMismatch !Text !Int !Int      -- ^ Constructor arity mismatch
  | EffectMismatch !EffectRow !EffectRow  -- ^ Effect rows cannot be unified
  | UnifyError !Text                   -- ^ Generic error
  deriving stock (Eq, Show)

-- | Unify two types, returning the most general unifier (MGU)
--
-- The unification algorithm:
-- 1. If types are identical, return empty substitution
-- 2. If either is a type variable, bind it (with occurs check)
-- 3. For composite types, recursively unify components
-- 4. For mismatched type constructors, fail
unify :: Type -> Type -> Either UnifyError Substitution
unify t1 t2 = case (t1, t2) of
  -- Identical types (structural equality)
  _ | t1 == t2 -> Right emptySubst

  -- Type variable on left: bind it
  (TyVar _ idx, _) -> bindVar idx t2

  -- Type variable on right: bind it
  (_, TyVar _ idx) -> bindVar idx t1

  -- Function types (Pi types): unify domain, effects, and codomain
  (TyPi _ p1 e1 r1, TyPi _ p2 e2 r2) -> do
    s1 <- unify p1 p2
    s2 <- unifyEffects (applySubstEffects s1 e1) (applySubstEffects s1 e2)
    let s12 = composeSubst s2 s1
    s3 <- unify (applySubst s12 r1) (applySubst s12 r2)
    Right $ composeSubst s3 s12

  -- Type constructors: must have same name, then unify arguments
  (TyCon n1 args1, TyCon n2 args2)
    | n1 == n2 -> do
        if length args1 /= length args2
          then Left $ ArityMismatch n1 (length args1) (length args2)
          else unifyList args1 args2
    | otherwise -> Left $ TypeMismatch t1 t2

  -- Forall types: unify kinds and bodies
  (TyForall _ k1 b1, TyForall _ k2 b2) -> do
    if k1 /= k2
      then Left $ KindMismatch k1 k2
      else unify b1 b2

  -- Dependent forall types: unify param types and bodies
  (TyForallDep _ pt1 b1, TyForallDep _ pt2 b2) -> do
    s1 <- unify pt1 pt2
    s2 <- unify (applySubst s1 b1) (applySubst s1 b2)
    Right $ composeSubst s2 s1

  -- Lazy types: unify inner types
  (TyLazy i1, TyLazy i2) -> unify i1 i2

  -- Linear types: unify inner types
  (TyLinear i1, TyLinear i2) -> unify i1 i2

  -- Ref types: unify inner types
  (TyRef i1, TyRef i2) -> unify i1 i2

  -- RefMut types: unify inner types
  (TyRefMut i1, TyRefMut i2) -> unify i1 i2

  -- Universe types: must have same level
  (TyUniverse l1, TyUniverse l2)
    | l1 == l2 -> Right emptySubst
    | otherwise -> Left $ TypeMismatch t1 t2

  -- Prop: already handled by equality check above

  -- Anything else: cannot unify
  _ -> Left $ TypeMismatch t1 t2

-- | Bind a type variable to a type, with occurs check
bindVar :: Int -> Type -> Either UnifyError Substitution
bindVar idx ty
  -- Variable binding to itself: empty substitution
  | ty == TyVar "_" idx = Right emptySubst
  | otherwise = case ty of
      TyVar _ idx'
        | idx == idx' -> Right emptySubst  -- Same variable
        | otherwise   -> Right $ singleSubst idx ty
      _ ->
        -- Occurs check: the variable must not appear in the type
        if occursIn idx ty
          then Left $ OccursCheck idx ty
          else Right $ singleSubst idx ty

-- | Check if a type variable index occurs in a type
occursIn :: Int -> Type -> Bool
occursIn idx = go
  where
    go ty = case ty of
      TyVar _ idx' -> idx == idx'
      TyCon _ args -> any go args
      TyPi _ paramTy _ retTy -> go paramTy || go retTy
      TyForall _ _ bodyTy -> go bodyTy
      TyForallDep _ paramTy bodyTy -> go paramTy || go bodyTy
      TyLazy inner -> go inner
      TyLinear inner -> go inner
      TyRef inner -> go inner
      TyRefMut inner -> go inner
      TyUniverse _ -> False
      TyProp -> False

-- | Unify a list of type pairs, threading the substitution through
unifyList :: [Type] -> [Type] -> Either UnifyError Substitution
unifyList [] [] = Right emptySubst
unifyList (t1:ts1) (t2:ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyList (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  Right $ composeSubst s2 s1
unifyList _ _ = Left $ UnifyError "Type list length mismatch"

-- | Unify two effect rows
--
-- Effect row unification is more complex than type unification because
-- effect rows can contain multiple effects and row variables.
unifyEffects :: EffectRow -> EffectRow -> Either UnifyError Substitution
unifyEffects e1 e2 = case (e1, e2) of
  -- Both empty: success
  (EffEmpty, EffEmpty) -> Right emptySubst

  -- Effect variable can unify with anything
  (EffVar _ _, _) -> Right emptySubst  -- Simplified: just succeed
  (_, EffVar _ _) -> Right emptySubst

  -- Both concrete effect sets
  (EffSet effs1, EffSet effs2)
    | effectsCompatible effs1 effs2 -> Right emptySubst
    | otherwise -> Left $ EffectMismatch e1 e2

  -- Effect union: try to match components
  (EffUnion a1 b1, EffUnion a2 b2) -> do
    s1 <- unifyEffects a1 a2
    s2 <- unifyEffects b1 b2
    Right $ composeSubst s2 s1

  -- Union with concrete: flatten and compare
  (EffUnion a b, EffSet effs) -> do
    let flattened = flattenEffects (EffUnion a b)
    if effectsCompatible flattened effs
      then Right emptySubst
      else Left $ EffectMismatch e1 e2

  (EffSet effs, EffUnion a b) ->
    unifyEffects (EffUnion a b) (EffSet effs)

  -- Empty with variable
  (EffEmpty, EffVar _ _) -> Right emptySubst
  (EffVar _ _, EffEmpty) -> Right emptySubst

  -- Empty with non-empty concrete: fail
  (EffEmpty, EffSet effs)
    | null effs -> Right emptySubst
    | otherwise -> Left $ EffectMismatch e1 e2
  (EffSet effs, EffEmpty)
    | null effs -> Right emptySubst
    | otherwise -> Left $ EffectMismatch e1 e2

  -- Anything else
  _ -> Left $ EffectMismatch e1 e2

-- | Check if two effect lists are compatible (contain same effects)
effectsCompatible :: [Effect] -> [Effect] -> Bool
effectsCompatible effs1 effs2 =
  let names1 = map effectName effs1
      names2 = map effectName effs2
  in all (`elem` names2) names1 && all (`elem` names1) names2

-- | Flatten an effect row into a list of effects
flattenEffects :: EffectRow -> [Effect]
flattenEffects = \case
  EffEmpty -> []
  EffSet effs -> effs
  EffVar _ _ -> []  -- Variables don't contribute concrete effects
  EffUnion a b -> flattenEffects a ++ flattenEffects b
