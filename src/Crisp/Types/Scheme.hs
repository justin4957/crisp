{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.Scheme
-- Description : Type schemes for let-polymorphism
--
-- Type schemes represent polymorphic types with quantified type variables.
-- They are used for let-polymorphism where local bindings can have
-- polymorphic types that are instantiated at each use site.
--
-- A type scheme `forall a b. T` represents a type T with universally
-- quantified variables a and b.

module Crisp.Types.Scheme
  ( -- * Type schemes
    Scheme(..)
  , monoScheme
  , schemeVars
  , schemeType
    -- * Free type variables
  , freeTypeVars
  , freeTypeVarsScheme
  , freeTypeVarsEnv
  ) where

import Crisp.Core.Term
import Crisp.Types.Substitution

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | A type scheme: forall vars. type
--
-- The set contains the indices of quantified type variables.
-- For example, `forall a. a -> a` would be represented as:
--   Scheme {0} (TyPi "_" (TyVar "a" 0) EffEmpty (TyVar "a" 0))
data Scheme = Scheme
  { schemeQuantified :: !(Set Int)  -- ^ Quantified variable indices
  , schemeBody       :: !Type       -- ^ The type body
  } deriving stock (Eq, Show)

-- | Create a monomorphic scheme (no quantified variables)
monoScheme :: Type -> Scheme
monoScheme ty = Scheme Set.empty ty

-- | Get the quantified variables of a scheme
schemeVars :: Scheme -> Set Int
schemeVars = schemeQuantified

-- | Get the type body of a scheme
schemeType :: Scheme -> Type
schemeType = schemeBody

-- | Compute the free type variables in a type
freeTypeVars :: Type -> Set Int
freeTypeVars = go
  where
    go ty = case ty of
      TyVar _ idx -> Set.singleton idx

      TyCon _ args -> Set.unions (map go args)

      TyPi _ paramTy _effs retTy ->
        Set.union (go paramTy) (go retTy)

      TyForall _ _ bodyTy ->
        -- The bound variable is not free in the body
        -- In our representation, forall doesn't change indices,
        -- so we just recurse
        go bodyTy

      TyForallDep _ paramTy bodyTy ->
        Set.union (go paramTy) (go bodyTy)

      TyLazy inner -> go inner

      TyLinear inner -> go inner

      TyRef inner -> go inner

      TyRefMut inner -> go inner

      TyUniverse _ -> Set.empty

      TyProp -> Set.empty

-- | Compute free type variables in a scheme
--
-- The quantified variables are bound, so they're not free.
freeTypeVarsScheme :: Scheme -> Set Int
freeTypeVarsScheme (Scheme quantified body) =
  freeTypeVars body `Set.difference` quantified

-- | Apply a substitution to a scheme
--
-- Only substitutes for free variables (not the quantified ones).
-- Note: Currently unused but kept for potential future use
_applySubstScheme :: Substitution -> Scheme -> Scheme
_applySubstScheme subst (Scheme quantified body) =
  -- Remove quantified variables from substitution before applying
  let restrictedSubst = _restrictSubst subst quantified
  in Scheme quantified (applySubst restrictedSubst body)

-- | Restrict a substitution by removing certain variable bindings
-- Note: Currently unused but kept for potential future use
_restrictSubst :: Substitution -> Set Int -> Substitution
_restrictSubst subst excluded =
  -- Rebuild the substitution keeping only non-excluded bindings
  let allBindings = domainSubst subst
      keptBindings = filter (`Set.notMember` excluded) allBindings
  in foldr addBinding emptySubst keptBindings
  where
    addBinding idx acc = case lookupSubst idx subst of
      Just ty -> insertSubst idx ty acc
      Nothing -> acc

-- | Compute free type variables in an environment (list of schemes)
freeTypeVarsEnv :: [(Text, Scheme)] -> Set Int
freeTypeVarsEnv = Set.unions . map (freeTypeVarsScheme . snd)
