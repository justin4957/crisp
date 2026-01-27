{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Prelude.Core
-- Description : Core prelude module for Crisp
--
-- The main entry point for the Crisp standard prelude. This module
-- re-exports all prelude types, functions, and effects, and provides
-- utilities for loading and working with the prelude.
--
-- == Design Philosophy
--
-- Crisp follows an explicit-import philosophy: there is no implicit prelude.
-- All types, functions, and effects must be explicitly imported:
--
-- @
-- import Core.Prelude (Option, Some, None, map_option)
-- import Core.Prelude.Effects (State, get, put)
-- @
--
-- This design supports:
--
-- * Auditability: Every dependency is visible in the import list
-- * Clarity: No hidden magic or implicit behavior
-- * Modularity: Only import what you need
--
-- == Module Structure
--
-- @
-- Core/
--   Prelude        -- All types and basic functions
--   Prelude/
--     Types        -- Type definitions only
--     Functions    -- Function definitions only
--     Effects      -- Effect definitions only
-- @

module Crisp.Prelude.Core
  ( -- * Re-exports
    module Crisp.Prelude.Types
  , module Crisp.Prelude.Functions
  , module Crisp.Prelude.Effects
    -- * Prelude Context
  , PreludeContext(..)
  , emptyPreludeContext
  , fullPreludeContext
  , minimalPreludeContext
    -- * Name Resolution
  , resolvePreludeName
  , ResolvedName(..)
    -- * Type Checking Support
  , preludeTypeEnv
  , preludeFunctionEnv
  , preludeEffectEnv
    -- * Code Generation
  , generatePreludeModule
  , generateTypeDefinition
  , generateFunctionDefinition
  , generateEffectDefinition
  ) where

import Crisp.Prelude.Types
import Crisp.Prelude.Functions
import Crisp.Prelude.Effects

import Crisp.Core.Term (Type(..), Kind(..), EffectRow(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Prelude Context
--------------------------------------------------------------------------------

-- | A context specifying which prelude items are available
data PreludeContext = PreludeContext
  { ctxTypes     :: !(Set Text)  -- ^ Available type names
  , ctxConstrs   :: !(Set Text)  -- ^ Available constructor names
  , ctxFunctions :: !(Set Text)  -- ^ Available function names
  , ctxEffects   :: !(Set Text)  -- ^ Available effect names
  , ctxOps       :: !(Set (Text, Text))  -- ^ Available operations (effect, op)
  } deriving stock (Eq, Show)

-- | Empty prelude context (no prelude items available)
emptyPreludeContext :: PreludeContext
emptyPreludeContext = PreludeContext
  { ctxTypes = Set.empty
  , ctxConstrs = Set.empty
  , ctxFunctions = Set.empty
  , ctxEffects = Set.empty
  , ctxOps = Set.empty
  }

-- | Full prelude context (all prelude items available)
fullPreludeContext :: PreludeContext
fullPreludeContext = PreludeContext
  { ctxTypes = Set.fromList (map preludeTypeName preludeTypes)
  , ctxConstrs = Set.fromList [constrName c | t <- preludeTypes, c <- preludeTypeConstrs t]
  , ctxFunctions = Set.fromList (map fnName preludeFunctions)
  , ctxEffects = Set.fromList (map effectName preludeEffects)
  , ctxOps = Set.fromList [(effectName e, opName op) | e <- preludeEffects, op <- effectOperations e]
  }

-- | Minimal prelude context (basic types only)
minimalPreludeContext :: PreludeContext
minimalPreludeContext = PreludeContext
  { ctxTypes = Set.fromList ["Unit", "Bool", "Option", "List"]
  , ctxConstrs = Set.fromList ["Unit", "True", "False", "None", "Some", "Nil", "Cons"]
  , ctxFunctions = Set.fromList ["id", "not"]
  , ctxEffects = Set.empty
  , ctxOps = Set.empty
  }

--------------------------------------------------------------------------------
-- Name Resolution
--------------------------------------------------------------------------------

-- | A resolved prelude name
data ResolvedName
  = ResolvedType !PreludeType
  | ResolvedConstructor !PreludeConstructor !PreludeType
  | ResolvedFunction !PreludeFunction
  | ResolvedEffect !PreludeEffect
  | ResolvedOperation !EffectOperation !PreludeEffect
  | NotFound
  deriving stock (Eq, Show)

-- | Resolve a name in the prelude
resolvePreludeName :: Text -> ResolvedName
resolvePreludeName name
  -- Try types first
  | Just t <- lookupPreludeType name = ResolvedType t
  -- Then constructors
  | Just (c, t) <- lookupPreludeConstructor name = ResolvedConstructor c t
  -- Then functions
  | Just f <- lookupPreludeFunction name = ResolvedFunction f
  -- Then effects
  | Just e <- lookupPreludeEffect name = ResolvedEffect e
  -- Not found
  | otherwise = NotFound

--------------------------------------------------------------------------------
-- Type Checking Support
--------------------------------------------------------------------------------

-- | Type environment for prelude types (name -> kind)
preludeTypeEnv :: Map Text Kind
preludeTypeEnv = Map.fromList
  [ (preludeTypeName t, preludeTypeKind t) | t <- preludeTypes ]

-- | Function environment for prelude functions (name -> type)
preludeFunctionEnv :: Map Text Type
preludeFunctionEnv = Map.fromList
  [ (fnName f, functionType f) | f <- preludeFunctions ]

-- | Effect environment for prelude effects (name -> operations)
preludeEffectEnv :: Map Text [(Text, Type)]
preludeEffectEnv = Map.fromList
  [ (effectName e, [(opName op, effectOperationType op) | op <- effectOperations e])
  | e <- preludeEffects
  ]

-- | Compute the type of a function from its definition
functionType :: PreludeFunction -> Type
functionType fn =
  let -- Build function type from params and return
      paramTypes = map snd (fnParams fn)
      fnType = foldr (\p r -> TyPi "_" p (fnEffects fn) r) (fnReturnType fn) paramTypes
      -- Wrap in foralls for type parameters
      withForalls = foldr (\(name, kind) t -> TyForall name kind t) fnType (fnTypeParams fn)
  in withForalls

--------------------------------------------------------------------------------
-- Code Generation
--------------------------------------------------------------------------------

-- | Generate the complete prelude module as Crisp source
generatePreludeModule :: Text
generatePreludeModule = T.unlines
  [ "module Core.Prelude"
  , ""
  , "-- Standard Types"
  , T.unlines (map generateTypeDefinition preludeTypes)
  , ""
  , "-- Standard Functions"
  , T.unlines (map generateFunctionDefinition preludeFunctions)
  , ""
  , "-- Standard Effects"
  , T.unlines (map generateEffectDefinition preludeEffects)
  ]

-- | Generate a type definition as Crisp source
generateTypeDefinition :: PreludeType -> Text
generateTypeDefinition pt =
  let params = if null (preludeTypeParams pt)
               then ""
               else " " <> T.unwords (map fst (preludeTypeParams pt))
      constrs = T.intercalate "\n" (map genConstr (preludeTypeConstrs pt))
  in "type " <> preludeTypeName pt <> params <> ":\n" <> constrs
  where
    genConstr c =
      let args = if null (constrParams c)
                 then ""
                 else " " <> T.unwords (map genType (constrParams c))
      in "  " <> constrName c <> args

-- | Generate a function definition as Crisp source
generateFunctionDefinition :: PreludeFunction -> Text
generateFunctionDefinition fn =
  let typeParams = if null (fnTypeParams fn)
                   then ""
                   else "[" <> T.intercalate ", " (map fst (fnTypeParams fn)) <> "]"
      params = "(" <> T.intercalate ", " (map genParam (fnParams fn)) <> ")"
      retTy = " -> " <> genType (fnReturnType fn)
      effects = case fnEffects fn of
        EffEmpty -> ""
        _ -> " ! Effects"
      body = case fnBody fn of
        BodyBuiltin name -> "  -- builtin: " <> name
        BodyTerm _ -> "  -- implementation"
        BodyMatch var _ -> "  match " <> var <> " ..."
  in "fn " <> fnName fn <> typeParams <> params <> retTy <> effects <> ":\n" <> body
  where
    genParam (name, ty) = name <> ": " <> genType ty

-- | Generate an effect definition as Crisp source
generateEffectDefinition :: PreludeEffect -> Text
generateEffectDefinition eff =
  let params = if null (effectTypeParams eff)
               then ""
               else "(" <> T.intercalate ", " (map fst (effectTypeParams eff)) <> ")"
      ops = T.intercalate "\n" (map genOp (effectOperations eff))
  in "effect " <> effectName eff <> params <> ":\n" <> ops
  where
    genOp op = "  " <> opName op <> ": " <> genType (opParamType op) <> " -> " <> genType (opReturnType op)

-- | Generate a type as Crisp source (simplified)
genType :: Type -> Text
genType (TyVar name _) = name
genType (TyCon name []) = name
genType (TyCon name args) = name <> " " <> T.unwords (map genTypeAtom args)
genType (TyPi "_" from EffEmpty to) = genTypeAtom from <> " -> " <> genType to
genType (TyPi name from _ to) = "(" <> name <> ": " <> genType from <> ") -> " <> genType to
genType (TyForall name _ body) = "forall " <> name <> ". " <> genType body
genType (TyLazy inner) = "Lazy " <> genTypeAtom inner
genType (TyUniverse 0) = "Type"
genType (TyUniverse n) = "Type_" <> T.pack (show n)
genType other = T.pack (show other)

-- | Generate an atomic type (with parens if needed)
genTypeAtom :: Type -> Text
genTypeAtom t@(TyVar _ _) = genType t
genTypeAtom t@(TyCon _ []) = genType t
genTypeAtom t = "(" <> genType t <> ")"
