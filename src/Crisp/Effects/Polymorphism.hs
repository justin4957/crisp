{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Crisp.Effects.Polymorphism
-- Description : Effect polymorphism with row variables
--
-- Implements effect polymorphism for Crisp's algebraic effect system.
-- Effect polymorphism allows functions to be generic over effect rows,
-- enabling code reuse across different effect contexts.
--
-- Key concepts:
-- - Row variables: Abstract over unknown effects (e.g., ε, ρ)
-- - Effect schemes: Quantified effect types (∀ε. τ -[ε]-> τ')
-- - Effect unification: Finding most general unifier for effect rows
-- - Effect constraints: Subset relationships between effect rows
--
-- Example:
-- @
-- fn map(f: (A) -[ε]-> B, xs: List(A)) -[ε]-> List(B) = ...
-- @
-- Here, ε is a row variable that can be instantiated to any effect row.

module Crisp.Effects.Polymorphism
  ( -- * Effect Row Operations
    hasEffectVar
  , effectRowVars
  , containsConcreteEffect
  , rowUnion
  , rowRemove
  , rowDifference
  , isEffectRowSubset
  , normalizeEffectRow
    -- * Effect Row Unification
  , unifyEffectRow
  , EffectUnifyError(..)
    -- * Effect Substitution
  , EffectSubstitution
  , emptyEffectSubst
  , singleEffectSubst
  , lookupEffectVar
  , applyEffectSubst
  , composeEffectSubst
    -- * Effect Polymorphism
  , isEffectPolymorphic
  , freeEffectVars
    -- * Effect Schemes
  , EffectScheme(..)
  , effectSchemeQuantified
  , generalizeEffects
  , instantiateEffects
    -- * Effect Environment
  , EffectPolyEnv
  , emptyEffectPolyEnv
  , extendEffectPolyEnv
  , lookupEffectPolyEnv
    -- * Effect Constraints
  , EffectConstraint
  , makeSubsetConstraint
  , constraintLhs
  , constraintRhs
  , solveEffectConstraint
    -- * Effect Monad
  , EffectPoly
  , runEffectPoly
  , EffectPolyError(..)
  ) where

import Crisp.Core.Term

import Control.Monad.Except
import Control.Monad.State
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Effect Row Operations
--------------------------------------------------------------------------------

-- | Check if an effect row contains any effect variables
hasEffectVar :: EffectRow -> Bool
hasEffectVar = \case
  EffEmpty -> False
  EffSet _ -> False
  EffVar _ _ -> True
  EffUnion a b -> hasEffectVar a || hasEffectVar b

-- | Extract all effect variable indices from an effect row
effectRowVars :: EffectRow -> [Int]
effectRowVars = nub . go
  where
    go = \case
      EffEmpty -> []
      EffSet _ -> []
      EffVar _ idx -> [idx]
      EffUnion a b -> go a ++ go b

-- | Check if an effect row contains a specific concrete effect
containsConcreteEffect :: Text -> EffectRow -> Bool
containsConcreteEffect name = \case
  EffEmpty -> False
  EffSet effs -> any (\e -> effectName e == name) effs
  EffVar _ _ -> False
  EffUnion a b -> containsConcreteEffect name a || containsConcreteEffect name b

-- | Union two effect rows
rowUnion :: EffectRow -> EffectRow -> EffectRow
rowUnion EffEmpty other = other
rowUnion other EffEmpty = other
rowUnion (EffSet a) (EffSet b) = EffSet (mergeEffects a b)
rowUnion a b = EffUnion a b

-- | Merge two effect lists, deduplicating by name
mergeEffects :: [Effect] -> [Effect] -> [Effect]
mergeEffects a b = nub (a ++ b)

-- | Remove an effect from an effect row
rowRemove :: Text -> EffectRow -> EffectRow
rowRemove _ EffEmpty = EffEmpty
rowRemove name (EffSet effs) =
  case filter (\e -> effectName e /= name) effs of
    [] -> EffEmpty
    filtered -> EffSet filtered
rowRemove name (EffVar n i) = EffVar n i  -- Cannot remove from variable
rowRemove name (EffUnion a b) =
  let a' = rowRemove name a
      b' = rowRemove name b
  in rowUnion a' b'

-- | Compute the difference of two effect rows (row1 - row2)
rowDifference :: EffectRow -> EffectRow -> EffectRow
rowDifference row1 row2 =
  let names2 = concreteEffectNames row2
  in foldr rowRemove row1 names2

-- | Get concrete effect names from a row
concreteEffectNames :: EffectRow -> [Text]
concreteEffectNames = \case
  EffEmpty -> []
  EffSet effs -> map effectName effs
  EffVar _ _ -> []
  EffUnion a b -> concreteEffectNames a ++ concreteEffectNames b

-- | Check if one effect row is a subset of another
--
-- With row variables, subset checking is more permissive:
-- - A row variable can be a subset of anything (it might be empty)
-- - Anything can be a subset of a row variable (it might contain everything)
isEffectRowSubset :: EffectRow -> EffectRow -> Bool
isEffectRowSubset lhs rhs
  -- Empty is always a subset
  | isEmptyRow lhs = True
  -- Anything is subset of a row variable
  | hasEffectVar rhs = True
  -- A row variable can be a subset (might be empty)
  | hasEffectVar lhs = True
  -- Concrete subset check
  | otherwise =
      let namesL = concreteEffectNames lhs
          namesR = concreteEffectNames rhs
      in all (`elem` namesR) namesL

-- | Check if an effect row is empty
isEmptyRow :: EffectRow -> Bool
isEmptyRow EffEmpty = True
isEmptyRow (EffSet []) = True
isEmptyRow _ = False

-- | Normalize an effect row by flattening unions and deduplicating
normalizeEffectRow :: EffectRow -> EffectRow
normalizeEffectRow row =
  let (concretes, vars) = collectEffects row
      uniqueConcretes = nub concretes
      uniqueVars = nub vars
  in case (uniqueConcretes, uniqueVars) of
       ([], []) -> EffEmpty
       (effs, []) -> EffSet effs
       ([], [v]) -> uncurry EffVar v
       (effs, [v]) -> EffUnion (EffSet effs) (uncurry EffVar v)
       (effs, vs) ->
         let base = if null effs then EffEmpty else EffSet effs
         in foldr (\(n, i) acc -> EffUnion acc (EffVar n i)) base vs

-- | Collect all effects and variables from a row
collectEffects :: EffectRow -> ([Effect], [(Text, Int)])
collectEffects = \case
  EffEmpty -> ([], [])
  EffSet effs -> (effs, [])
  EffVar name idx -> ([], [(name, idx)])
  EffUnion a b ->
    let (effsA, varsA) = collectEffects a
        (effsB, varsB) = collectEffects b
    in (effsA ++ effsB, varsA ++ varsB)

--------------------------------------------------------------------------------
-- Effect Substitution
--------------------------------------------------------------------------------

-- | Substitution for effect variables
newtype EffectSubstitution = EffectSubstitution
  { unEffectSubst :: IntMap EffectRow
  } deriving stock (Eq, Show)

-- | Empty effect substitution
emptyEffectSubst :: EffectSubstitution
emptyEffectSubst = EffectSubstitution IntMap.empty

-- | Create a singleton effect substitution
singleEffectSubst :: Int -> EffectRow -> EffectSubstitution
singleEffectSubst idx row = EffectSubstitution $ IntMap.singleton idx row

-- | Look up an effect variable in the substitution
lookupEffectVar :: Int -> EffectSubstitution -> Maybe EffectRow
lookupEffectVar idx (EffectSubstitution m) = IntMap.lookup idx m

-- | Insert into effect substitution
insertEffectSubst :: Int -> EffectRow -> EffectSubstitution -> EffectSubstitution
insertEffectSubst idx row (EffectSubstitution m) =
  EffectSubstitution $ IntMap.insert idx row m

-- | Compose two effect substitutions
composeEffectSubst :: EffectSubstitution -> EffectSubstitution -> EffectSubstitution
composeEffectSubst s2 s1 =
  let EffectSubstitution m1 = s1
      EffectSubstitution m2 = s2
      m1' = IntMap.map (applyEffectSubstToRow s2) m1
  in EffectSubstitution $ IntMap.union m1' m2

-- | Apply effect substitution to an effect row
applyEffectSubstToRow :: EffectSubstitution -> EffectRow -> EffectRow
applyEffectSubstToRow subst = \case
  EffEmpty -> EffEmpty
  EffSet effs -> EffSet effs
  EffVar name idx ->
    case lookupEffectVar idx subst of
      Just replacement -> replacement
      Nothing -> EffVar name idx
  EffUnion a b ->
    rowUnion (applyEffectSubstToRow subst a) (applyEffectSubstToRow subst b)

-- | Apply effect substitution to a type
applyEffectSubst :: EffectSubstitution -> Type -> Type
applyEffectSubst subst = go
  where
    go = \case
      TyVar name idx -> TyVar name idx
      TyCon name args -> TyCon name (map go args)
      TyPi paramName paramTy effs retTy ->
        TyPi paramName (go paramTy) (applyEffectSubstToRow subst effs) (go retTy)
      TyForall typeVar kind bodyTy ->
        TyForall typeVar kind (go bodyTy)
      TyForallDep paramName paramTy bodyTy ->
        TyForallDep paramName (go paramTy) (go bodyTy)
      TyLazy inner -> TyLazy (go inner)
      TyLinear inner -> TyLinear (go inner)
      TyRef inner -> TyRef (go inner)
      TyRefMut inner -> TyRefMut (go inner)
      TyUniverse level -> TyUniverse level
      TyProp -> TyProp

--------------------------------------------------------------------------------
-- Effect Row Unification
--------------------------------------------------------------------------------

-- | Errors during effect row unification
data EffectUnifyError
  = EffectRowMismatch !EffectRow !EffectRow
    -- ^ Effect rows cannot be unified
  | EffectOccursCheck !Int !EffectRow
    -- ^ Infinite effect row would result
  | MissingEffect !Text !EffectRow
    -- ^ Required effect not present in row
  deriving stock (Eq, Show)

-- | Unify two effect rows
unifyEffectRow :: EffectRow -> EffectRow -> Either EffectUnifyError EffectSubstitution
unifyEffectRow row1 row2 = case (row1, row2) of
  -- Both empty: success
  (EffEmpty, EffEmpty) -> Right emptyEffectSubst
  (EffSet [], EffEmpty) -> Right emptyEffectSubst
  (EffEmpty, EffSet []) -> Right emptyEffectSubst
  (EffSet [], EffSet []) -> Right emptyEffectSubst

  -- Effect variable on left
  (EffVar _ idx, _) -> bindEffectVar idx row2

  -- Effect variable on right
  (_, EffVar _ idx) -> bindEffectVar idx row1

  -- Both concrete sets
  (EffSet effs1, EffSet effs2)
    | effectsEquivalent effs1 effs2 -> Right emptyEffectSubst
    | otherwise -> Left $ EffectRowMismatch row1 row2

  -- Union on left
  (EffUnion a b, _) -> do
    let (concretes1, vars1) = collectEffects (EffUnion a b)
        (concretes2, vars2) = collectEffects row2
    -- Unify concrete parts
    if effectsCompatible concretes1 concretes2
      then case (vars1, vars2) of
        ([], []) -> Right emptyEffectSubst
        ((_, idx):_, []) ->
          -- Bind variable to remaining effects
          let remaining = filter (\e -> effectName e `notElem` map effectName concretes1) concretes2
          in Right $ singleEffectSubst idx (EffSet remaining)
        ([], (_, idx):_) ->
          let remaining = filter (\e -> effectName e `notElem` map effectName concretes2) concretes1
          in Right $ singleEffectSubst idx (EffSet remaining)
        ((_, idx1):_, (_, idx2):_) ->
          -- Both have variables - unify them
          Right $ singleEffectSubst idx1 (EffVar "ρ" idx2)
      else Left $ EffectRowMismatch row1 row2

  -- Union on right
  (_, EffUnion _ _) -> unifyEffectRow row2 row1

  -- Empty with non-empty concrete
  (EffEmpty, EffSet effs)
    | null effs -> Right emptyEffectSubst
    | otherwise -> Left $ EffectRowMismatch row1 row2
  (EffSet effs, EffEmpty)
    | null effs -> Right emptyEffectSubst
    | otherwise -> Left $ EffectRowMismatch row1 row2

-- | Bind an effect variable to a row
bindEffectVar :: Int -> EffectRow -> Either EffectUnifyError EffectSubstitution
bindEffectVar idx row
  | row == EffVar "_" idx = Right emptyEffectSubst
  | otherwise = case row of
      EffVar _ idx'
        | idx == idx' -> Right emptyEffectSubst
        | otherwise -> Right $ singleEffectSubst idx row
      _ ->
        -- Occurs check: variable should not appear in row
        -- But for effect rows, this is often OK (the variable becomes the "rest")
        Right $ singleEffectSubst idx row

-- | Check if two effect lists contain the same effects
effectsEquivalent :: [Effect] -> [Effect] -> Bool
effectsEquivalent effs1 effs2 =
  let names1 = map effectName effs1
      names2 = map effectName effs2
  in length names1 == length names2 &&
     all (`elem` names2) names1 &&
     all (`elem` names1) names2

-- | Check if effect lists are compatible (subset relation)
effectsCompatible :: [Effect] -> [Effect] -> Bool
effectsCompatible effs1 effs2 =
  let names1 = map effectName effs1
      names2 = map effectName effs2
  in all (`elem` names2) names1 || all (`elem` names1) names2

--------------------------------------------------------------------------------
-- Effect Polymorphism
--------------------------------------------------------------------------------

-- | Check if a type is effect-polymorphic
isEffectPolymorphic :: Type -> Bool
isEffectPolymorphic = not . Set.null . freeEffectVars

-- | Compute free effect variables in a type
freeEffectVars :: Type -> Set Int
freeEffectVars = \case
  TyVar _ _ -> Set.empty
  TyCon _ args -> Set.unions (map freeEffectVars args)
  TyPi _ paramTy effs retTy ->
    Set.unions
      [ freeEffectVars paramTy
      , Set.fromList (effectRowVars effs)
      , freeEffectVars retTy
      ]
  TyForall _ _ bodyTy -> freeEffectVars bodyTy
  TyForallDep _ paramTy bodyTy ->
    Set.union (freeEffectVars paramTy) (freeEffectVars bodyTy)
  TyLazy inner -> freeEffectVars inner
  TyLinear inner -> freeEffectVars inner
  TyRef inner -> freeEffectVars inner
  TyRefMut inner -> freeEffectVars inner
  TyUniverse _ -> Set.empty
  TyProp -> Set.empty

--------------------------------------------------------------------------------
-- Effect Schemes
--------------------------------------------------------------------------------

-- | An effect scheme: quantified effect type
data EffectScheme = EffectScheme
  { effectSchemeQuantified :: !(Set Int)
    -- ^ Quantified effect variable indices
  , effectSchemeBody :: !Type
    -- ^ The type body
  } deriving stock (Eq, Show)

-- | Create a monomorphic effect scheme
monoEffectScheme :: Type -> EffectScheme
monoEffectScheme ty = EffectScheme Set.empty ty

-- | Effect polymorphism environment
newtype EffectPolyEnv = EffectPolyEnv
  { unEffectPolyEnv :: Map Text EffectScheme
  } deriving stock (Eq, Show)

-- | Empty effect polymorphism environment
emptyEffectPolyEnv :: EffectPolyEnv
emptyEffectPolyEnv = EffectPolyEnv Map.empty

-- | Extend environment with a binding
extendEffectPolyEnv :: Text -> EffectScheme -> EffectPolyEnv -> EffectPolyEnv
extendEffectPolyEnv name scheme (EffectPolyEnv env) =
  EffectPolyEnv $ Map.insert name scheme env

-- | Look up a binding
lookupEffectPolyEnv :: Text -> EffectPolyEnv -> Maybe EffectScheme
lookupEffectPolyEnv name (EffectPolyEnv env) = Map.lookup name env

-- | Generalize a type by quantifying free effect variables not in environment
generalizeEffects :: EffectPolyEnv -> Type -> EffectScheme
generalizeEffects env ty =
  let envFreeVars = freeEffectVarsInEnv env
      tyFreeVars = freeEffectVars ty
      quantified = tyFreeVars `Set.difference` envFreeVars
  in EffectScheme quantified ty

-- | Compute free effect variables in environment
freeEffectVarsInEnv :: EffectPolyEnv -> Set Int
freeEffectVarsInEnv (EffectPolyEnv env) =
  Set.unions $ map freeEffectVarsInScheme $ Map.elems env

-- | Free effect variables in a scheme
freeEffectVarsInScheme :: EffectScheme -> Set Int
freeEffectVarsInScheme (EffectScheme quantified body) =
  freeEffectVars body `Set.difference` quantified

--------------------------------------------------------------------------------
-- Effect Monad
--------------------------------------------------------------------------------

-- | Errors in effect polymorphism
data EffectPolyError
  = UnifyError !EffectUnifyError
  | UnboundEffectVar !Int
  | OtherEffectError !Text
  deriving stock (Eq, Show)

-- | Effect polymorphism state
data EffectPolyState = EffectPolyState
  { effectFreshCounter :: !Int
  } deriving stock (Eq, Show)

-- | Initial state
initialEffectState :: EffectPolyState
initialEffectState = EffectPolyState 1000  -- Start at 1000 to avoid conflicts

-- | Effect polymorphism monad
newtype EffectPoly a = EffectPoly
  { unEffectPoly :: StateT EffectPolyState (Except EffectPolyError) a
  } deriving newtype (Functor, Applicative, Monad, MonadState EffectPolyState, MonadError EffectPolyError)

-- | Run effect polymorphism computation
runEffectPoly :: EffectPoly a -> Either EffectPolyError a
runEffectPoly = runExcept . flip evalStateT initialEffectState . unEffectPoly

-- | Generate a fresh effect variable
freshEffectVar :: Text -> EffectPoly EffectRow
freshEffectVar prefix = do
  n <- gets effectFreshCounter
  modify $ \s -> s { effectFreshCounter = n + 1 }
  pure $ EffVar (prefix <> T.pack (show n)) n

-- | Instantiate an effect scheme with fresh effect variables
instantiateEffects :: EffectScheme -> EffectPoly Type
instantiateEffects (EffectScheme quantified body) = do
  -- Create fresh effect variables for each quantified variable
  freshVars <- mapM (\idx -> do
    fresh <- freshEffectVar "ε"
    case fresh of
      EffVar _ freshIdx -> pure (idx, freshIdx)
      _ -> error "freshEffectVar returned non-variable") (Set.toList quantified)
  -- Build substitution mapping old indices to fresh variables
  let subst = foldr (\(oldIdx, newIdx) s ->
        insertEffectSubst oldIdx (EffVar ("ε" <> T.pack (show newIdx)) newIdx) s)
        emptyEffectSubst freshVars
  pure $ applyEffectSubst subst body

--------------------------------------------------------------------------------
-- Effect Constraints
--------------------------------------------------------------------------------

-- | An effect constraint (subset relationship)
data EffectConstraint = EffectConstraint
  { constraintLhs :: !EffectRow
    -- ^ Left-hand side (must be subset)
  , constraintRhs :: !EffectRow
    -- ^ Right-hand side (superset)
  } deriving stock (Eq, Show)

-- | Create a subset constraint
makeSubsetConstraint :: EffectRow -> EffectRow -> EffectConstraint
makeSubsetConstraint = EffectConstraint

-- | Solve an effect constraint
solveEffectConstraint :: EffectConstraint -> Either EffectUnifyError EffectSubstitution
solveEffectConstraint (EffectConstraint lhs rhs) =
  case (lhs, rhs) of
    -- Variable can be anything
    (EffVar _ idx, _) -> Right $ singleEffectSubst idx rhs

    -- Anything is subset of variable
    (_, EffVar _ _) -> Right emptyEffectSubst

    -- Check concrete subset
    (EffSet effsL, EffSet effsR)
      | all (\e -> effectName e `elem` map effectName effsR) effsL ->
          Right emptyEffectSubst
      | otherwise ->
          Left $ EffectRowMismatch lhs rhs

    -- Empty is always subset
    (EffEmpty, _) -> Right emptyEffectSubst
    (EffSet [], _) -> Right emptyEffectSubst

    -- Union cases
    (EffUnion a b, _) -> do
      s1 <- solveEffectConstraint (EffectConstraint a rhs)
      let b' = applyEffectSubstToRow s1 b
          rhs' = applyEffectSubstToRow s1 rhs
      s2 <- solveEffectConstraint (EffectConstraint b' rhs')
      Right $ composeEffectSubst s2 s1

    -- Other cases
    _ -> Left $ EffectRowMismatch lhs rhs
