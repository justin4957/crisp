{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.Substitution
-- Description : Type substitution operations
--
-- Provides substitution data structure and operations for type unification.
-- A substitution maps type variable indices to types.

module Crisp.Types.Substitution
  ( -- * Substitution type
    Substitution
    -- * Construction
  , emptySubst
  , singleSubst
    -- * Operations
  , lookupSubst
  , insertSubst
  , composeSubst
  , domainSubst
    -- * Application
  , applySubst
  , applySubstEffects
  ) where

import Crisp.Core.Term

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)

-- | A substitution mapping type variable indices to types
newtype Substitution = Substitution { unSubstitution :: IntMap Type }
  deriving stock (Eq, Show)

-- | The empty substitution
emptySubst :: Substitution
emptySubst = Substitution IntMap.empty

-- | Create a singleton substitution
singleSubst :: Int -> Type -> Substitution
singleSubst idx ty = Substitution $ IntMap.singleton idx ty

-- | Look up a type variable in the substitution
lookupSubst :: Int -> Substitution -> Maybe Type
lookupSubst idx (Substitution m) = IntMap.lookup idx m

-- | Insert a binding into the substitution
insertSubst :: Int -> Type -> Substitution -> Substitution
insertSubst idx ty (Substitution m) = Substitution $ IntMap.insert idx ty m

-- | Get the domain of the substitution (bound variable indices)
domainSubst :: Substitution -> [Int]
domainSubst (Substitution m) = IntMap.keys m

-- | Compose two substitutions: (s2 `composeSubst` s1) applies s1 first, then s2
--
-- The result is a substitution that when applied is equivalent to
-- applying s1 then s2: applySubst (composeSubst s2 s1) t = applySubst s2 (applySubst s1 t)
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s2 s1 =
  -- Apply s2 to all types in s1, then union with s2
  let Substitution m1 = s1
      Substitution m2 = s2
      -- Apply s2 to each type in s1
      m1' = IntMap.map (applySubst s2) m1
      -- Union, with s2 taking precedence for overlapping keys
  in Substitution $ IntMap.union m1' m2

-- | Apply a substitution to a type
applySubst :: Substitution -> Type -> Type
applySubst subst = go
  where
    go ty = case ty of
      TyVar name idx ->
        case lookupSubst idx subst of
          Just replacement -> replacement
          Nothing -> TyVar name idx

      TyCon name args ->
        TyCon name (map go args)

      TyPi paramName paramTy effs retTy ->
        TyPi paramName (go paramTy) (applySubstEffects subst effs) (go retTy)

      TyForall typeVar kind bodyTy ->
        TyForall typeVar kind (go bodyTy)

      TyForallDep paramName paramTy bodyTy ->
        TyForallDep paramName (go paramTy) (go bodyTy)

      TyLazy inner ->
        TyLazy (go inner)

      TyLinear inner ->
        TyLinear (go inner)

      TyRef inner ->
        TyRef (go inner)

      TyRefMut inner ->
        TyRefMut (go inner)

      TyUniverse level ->
        TyUniverse level

      TyProp ->
        TyProp

-- | Apply a substitution to an effect row
applySubstEffects :: Substitution -> EffectRow -> EffectRow
applySubstEffects subst = go
  where
    go effs = case effs of
      EffEmpty -> EffEmpty

      EffSet effects -> EffSet effects

      EffVar name idx ->
        -- Effect variables could potentially be unified too
        -- For now, we don't substitute effect variables with types
        EffVar name idx

      EffUnion a b ->
        EffUnion (go a) (go b)
