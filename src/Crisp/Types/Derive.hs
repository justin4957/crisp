{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.Derive
-- Description : Automatic trait derivation for sum types
--
-- This module generates trait implementations from deriving clauses.
-- Supports deriving Eq and Ord for enum and sum types.
--
-- Example:
-- @
-- type JudicialAction deriving (Eq, Ord):
--   HearCase
--   IssueInjunction
--   HoldInContempt
-- @
--
-- Generates implementations equivalent to:
-- @
-- impl Eq for JudicialAction:
--   fn eq(a, b) = match (a, b)
--     (HearCase, HearCase) -> true
--     (IssueInjunction, IssueInjunction) -> true
--     (HoldInContempt, HoldInContempt) -> true
--     _ -> false
-- @

module Crisp.Types.Derive
  ( -- * Derivation errors
    DeriveError(..)
  , formatDeriveError
    -- * Deriving functions
  , deriveImpl
  , deriveEq
  , deriveOrd
    -- * Batch processing
  , processDerivingClauses
  ) where

import Crisp.Core.Term (Type(..), Kind(..), EffectRow(..), simpleType)
import Crisp.Types.Context

import Data.Text (Text)
import qualified Data.Text as T

-- | Errors that can occur during derivation
data DeriveError
  = UnsupportedTrait !Text !Text
    -- ^ Trait cannot be derived for this type (trait name, type name)
  | NoConstructors !Text !Text
    -- ^ Type has no constructors (trait name, type name)
  | MissingSuperTrait !Text !Text !Text
    -- ^ Missing supertrait (trait name, type name, missing supertrait)
  | ParameterizedConstructor !Text !Text !Text
    -- ^ Cannot derive for constructors with parameters (trait, type, constructor)
  deriving stock (Eq, Show)

-- | Format a derivation error for display
formatDeriveError :: DeriveError -> Text
formatDeriveError (UnsupportedTrait trait typeName) =
  "Cannot derive " <> trait <> " for " <> typeName <> ": trait is not derivable"
formatDeriveError (NoConstructors trait typeName) =
  "Cannot derive " <> trait <> " for " <> typeName <> ": type has no constructors"
formatDeriveError (MissingSuperTrait trait typeName super) =
  "Cannot derive " <> trait <> " for " <> typeName <>
  ": missing required supertrait " <> super
formatDeriveError (ParameterizedConstructor trait typeName conName) =
  "Cannot derive " <> trait <> " for " <> typeName <>
  ": constructor " <> conName <> " has parameters (only simple enums are supported)"

-- | Derive an implementation for a trait
-- Returns either an error or the generated ImplInfo
deriveImpl :: Text        -- ^ Trait name
           -> TypeInfo    -- ^ Type to derive for
           -> Context     -- ^ Current context (for supertrait checking)
           -> Either DeriveError ImplInfo
deriveImpl "Eq" typeInfo _ctx = deriveEq typeInfo
deriveImpl "Ord" typeInfo ctx = deriveOrd typeInfo ctx
deriveImpl traitName typeInfo _ctx =
  Left $ UnsupportedTrait traitName (typeInfoName typeInfo)

-- | Derive Eq implementation for a type
-- For enum types (constructors without parameters), generates equality by constructor matching
deriveEq :: TypeInfo -> Either DeriveError ImplInfo
deriveEq typeInfo
  | null constructors = Left $ NoConstructors "Eq" typeName
  | not (all isSimpleConstructor constructors) =
      case filter (not . isSimpleConstructor) constructors of
        (c:_) -> Left $ ParameterizedConstructor "Eq" typeName (constructorName c)
        []    -> Left $ NoConstructors "Eq" typeName  -- Should not happen
  | otherwise = Right $ ImplInfo "Eq" (simpleType typeName) eqMethods
  where
    typeName = typeInfoName typeInfo
    constructors = typeInfoConstructors typeInfo

    -- Check if constructor has no parameters (simple enum variant)
    isSimpleConstructor :: ConstructorInfo -> Bool
    isSimpleConstructor con = null (constructorParamTypes con)

    -- Generate method types for Eq
    eqMethods :: [(Text, Type)]
    eqMethods =
      [ ("eq", eqMethodType typeName)
      , ("ne", eqMethodType typeName)
      ]

-- | Derive Ord implementation for a type
-- Requires Eq to be implemented (checks context)
-- Orders constructors by their declaration order (first = smallest)
deriveOrd :: TypeInfo -> Context -> Either DeriveError ImplInfo
deriveOrd typeInfo ctx
  | null constructors = Left $ NoConstructors "Ord" typeName
  | not (all isSimpleConstructor constructors) =
      case filter (not . isSimpleConstructor) constructors of
        (c:_) -> Left $ ParameterizedConstructor "Ord" typeName (constructorName c)
        []    -> Left $ NoConstructors "Ord" typeName
  | not (satisfiesConstraint (simpleType typeName) "Eq" ctx) =
      Left $ MissingSuperTrait "Ord" typeName "Eq"
  | otherwise = Right $ ImplInfo "Ord" (simpleType typeName) ordMethods
  where
    typeName = typeInfoName typeInfo
    constructors = typeInfoConstructors typeInfo

    isSimpleConstructor :: ConstructorInfo -> Bool
    isSimpleConstructor con = null (constructorParamTypes con)

    -- Generate method types for Ord
    ordMethods :: [(Text, Type)]
    ordMethods =
      [ ("compare", compareMethodType typeName)
      , ("lt", boolMethodType typeName)
      , ("le", boolMethodType typeName)
      , ("gt", boolMethodType typeName)
      , ("ge", boolMethodType typeName)
      ]

-- | Generate type for eq/ne method: T -> T -> Bool
eqMethodType :: Text -> Type
eqMethodType typeName =
  TyPi "a" (simpleType typeName) EffEmpty
    (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Bool"))

-- | Generate type for compare method: T -> T -> Ordering
compareMethodType :: Text -> Type
compareMethodType typeName =
  TyPi "a" (simpleType typeName) EffEmpty
    (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Ordering"))

-- | Generate type for lt/le/gt/ge methods: T -> T -> Bool
boolMethodType :: Text -> Type
boolMethodType typeName =
  TyPi "a" (simpleType typeName) EffEmpty
    (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Bool"))

-- | Process deriving clauses for a type and register implementations
-- Takes trait names from a deriving clause and a TypeInfo, returns updated context
-- Processes traits in order, so Eq is derived before Ord (which requires Eq)
processDerivingClauses :: [Text]     -- ^ Trait names from deriving clause
                       -> TypeInfo   -- ^ Type information
                       -> Context    -- ^ Current context
                       -> Either [DeriveError] Context
processDerivingClauses traitNames typeInfo ctx =
  processTraits traitNames ctx []
  where
    processTraits :: [Text] -> Context -> [DeriveError] -> Either [DeriveError] Context
    processTraits [] ctx' errors
      | null errors = Right ctx'
      | otherwise   = Left (reverse errors)
    processTraits (trait:rest) ctx' errors =
      case deriveImpl trait typeInfo ctx' of
        Right impl ->
          -- Register the implementation and continue
          processTraits rest (registerImpl impl ctx') errors
        Left err ->
          -- Accumulate error and continue (to report all errors)
          processTraits rest ctx' (err : errors)
