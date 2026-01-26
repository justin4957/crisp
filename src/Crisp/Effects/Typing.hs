{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Effects.Typing
-- Description : Effect declaration typing
--
-- Implements type checking for effect declarations, enabling the compiler
-- to verify effect signatures and build the effect environment.
--
-- Effect declarations define sets of operations with their types:
--
-- @
-- effect State(S):
--   get() -> S
--   put(s: S) -> Unit
-- @
--
-- Each operation can perform the effect and potentially resume with a value.
-- Effects are first-class in Crisp's type system.

module Crisp.Effects.Typing
  ( -- * Effect Declarations
    EffectDecl(..)
  , OperationDecl(..)
    -- * Effect Information
  , EffectInfo(..)
  , OperationInfo(..)
    -- * Effect Environment
  , EffectEnv
  , emptyEffectEnv
  , addEffectDecl
  , lookupEffect
  , lookupOperation
    -- * Checking
  , checkEffectDecl
  , checkEffectDecls
    -- * Type inference
  , inferOperationType
  , instantiateOperation
    -- * Errors
  , EffectTypingError(..)
  ) where

import Crisp.Core.Term

import Data.List (find, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | An effect declaration
data EffectDecl = EffectDecl
  { effectDeclName   :: !Text
    -- ^ Name of the effect
  , effectDeclParams :: ![(Text, Kind)]
    -- ^ Type parameters with their kinds
  , effectDeclOps    :: ![OperationDecl]
    -- ^ Operations defined by this effect
  } deriving stock (Eq, Show)

-- | An operation declaration
data OperationDecl = OperationDecl
  { operationDeclName       :: !Text
    -- ^ Name of the operation
  , operationDeclReturnType :: !Type
    -- ^ Return type of the operation
  , operationDeclParams     :: ![(Text, Type)]
    -- ^ Parameters (name, type)
  } deriving stock (Eq, Show)

-- | Information about a typed effect
data EffectInfo = EffectInfo
  { effectInfoName   :: !Text
    -- ^ Name of the effect
  , effectInfoParams :: ![(Text, Kind)]
    -- ^ Type parameters with their kinds
  , effectInfoOps    :: ![OperationInfo]
    -- ^ Typed operations
  } deriving stock (Eq, Show)

-- | Information about a typed operation
data OperationInfo = OperationInfo
  { operationName       :: !Text
    -- ^ Name of the operation
  , operationEffect     :: !Text
    -- ^ Effect this operation belongs to
  , operationReturnType :: !Type
    -- ^ Return type
  , operationParams     :: ![(Text, Type)]
    -- ^ Parameters
  } deriving stock (Eq, Show)

-- | Effect environment mapping effect names to their info
newtype EffectEnv = EffectEnv
  { unEffectEnv :: Map Text EffectInfo
  } deriving stock (Eq, Show)

-- | Empty effect environment
emptyEffectEnv :: EffectEnv
emptyEffectEnv = EffectEnv Map.empty

-- | Add an effect declaration to the environment
addEffectDecl :: EffectDecl -> EffectEnv -> EffectEnv
addEffectDecl decl (EffectEnv env) =
  let info = effectDeclToInfo decl
  in EffectEnv $ Map.insert (effectInfoName info) info env

-- | Convert a declaration to effect info
effectDeclToInfo :: EffectDecl -> EffectInfo
effectDeclToInfo decl = EffectInfo
  { effectInfoName = effectDeclName decl
  , effectInfoParams = effectDeclParams decl
  , effectInfoOps = map (opDeclToInfo (effectDeclName decl)) (effectDeclOps decl)
  }

-- | Convert an operation declaration to info
opDeclToInfo :: Text -> OperationDecl -> OperationInfo
opDeclToInfo effName decl = OperationInfo
  { operationName = operationDeclName decl
  , operationEffect = effName
  , operationReturnType = operationDeclReturnType decl
  , operationParams = operationDeclParams decl
  }

-- | Look up an effect by name
lookupEffect :: Text -> EffectEnv -> Maybe EffectInfo
lookupEffect name (EffectEnv env) = Map.lookup name env

-- | Look up an operation by effect name and operation name
lookupOperation :: Text -> Text -> EffectEnv -> Maybe OperationInfo
lookupOperation effName opName env = do
  effInfo <- lookupEffect effName env
  find (\op -> operationName op == opName) (effectInfoOps effInfo)

-- | Errors that can occur during effect typing
data EffectTypingError
  = DuplicateEffectName !Text
    -- ^ Effect with this name already exists
  | DuplicateOperationName !Text !Text
    -- ^ Operation with this name already exists in effect
  | UnboundTypeVariable !Text !Text
    -- ^ Type variable not in scope (var name, context)
  | UnknownEffect !Text
    -- ^ Reference to unknown effect
  | UnknownOperation !Text !Text
    -- ^ Reference to unknown operation (effect, op)
  | KindMismatch !Kind !Kind
    -- ^ Expected kind, got kind
  | WrongNumberOfTypeArgs !Text !Int !Int
    -- ^ Effect name, expected, actual
  | OtherEffectError !Text
    -- ^ Other error
  deriving stock (Eq, Show)

-- | Check an effect declaration
checkEffectDecl :: EffectEnv -> EffectDecl -> Either EffectTypingError ()
checkEffectDecl env decl = do
  -- Check for duplicate effect name
  case lookupEffect (effectDeclName decl) env of
    Just _ -> Left $ DuplicateEffectName (effectDeclName decl)
    Nothing -> pure ()

  -- Check for duplicate operation names within the effect
  let opNames = map operationDeclName (effectDeclOps decl)
      duplicates = opNames /= nub opNames
  if duplicates
    then Left $ DuplicateOperationName (effectDeclName decl) (findDuplicate opNames)
    else pure ()

  -- All checks passed
  pure ()

-- | Find first duplicate in a list
findDuplicate :: Eq a => [a] -> a
findDuplicate [] = error "No duplicates"
findDuplicate (x:xs)
  | x `elem` xs = x
  | otherwise = findDuplicate xs

-- | Check multiple effect declarations
checkEffectDecls :: EffectEnv -> [EffectDecl] -> Either EffectTypingError EffectEnv
checkEffectDecls env [] = Right env
checkEffectDecls env (decl:rest) = do
  checkEffectDecl env decl
  let env' = addEffectDecl decl env
  checkEffectDecls env' rest

-- | Infer the full type of an operation
--
-- Given an effect name, operation name, and type arguments,
-- produces the function type for calling that operation.
--
-- For example, for State(Int).get:
--   () -[State(Int)]-> Int
inferOperationType :: EffectEnv -> Text -> Text -> [Type] -> Either EffectTypingError Type
inferOperationType env effName opName typeArgs = do
  -- Look up the effect
  effInfo <- case lookupEffect effName env of
    Just info -> Right info
    Nothing -> Left $ UnknownEffect effName

  -- Look up the operation
  opInfo <- case find (\op -> operationName op == opName) (effectInfoOps effInfo) of
    Just info -> Right info
    Nothing -> Left $ UnknownOperation effName opName

  -- Build type argument substitution
  let paramNames = map fst (effectInfoParams effInfo)
      expectedArgs = length paramNames
      actualArgs = length typeArgs

  -- Check arity (allow fewer for partial application)
  subst <- if actualArgs > expectedArgs
    then Left $ WrongNumberOfTypeArgs effName expectedArgs actualArgs
    else Right $ zip [0..] typeArgs

  -- Build the parameter type
  let paramTypes = operationParams opInfo
      paramType = case paramTypes of
        [] -> simpleType "Unit"
        [(_, ty)] -> applyTypeSubst subst ty
        _ -> error "Multiple parameters not yet supported"

  -- Build the return type with substitution
  let returnType = applyTypeSubst subst (operationReturnType opInfo)

  -- Build the effect row
  let effectRow = EffSet [Effect effName Nothing]

  -- Construct the function type
  pure $ TyPi "_" paramType effectRow returnType

-- | Apply a type substitution (index -> type) to a type
applyTypeSubst :: [(Int, Type)] -> Type -> Type
applyTypeSubst subst = go
  where
    go ty = case ty of
      TyVar _name idx ->
        case lookup idx subst of
          Just replacement -> replacement
          Nothing -> ty

      TyCon name args ->
        TyCon name (map go args)

      TyPi paramName paramTy effs retTy ->
        TyPi paramName (go paramTy) effs (go retTy)

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

-- | Instantiate an operation with type arguments
instantiateOperation :: EffectEnv -> Text -> Text -> [Type] -> Either EffectTypingError OperationInfo
instantiateOperation env effName opName typeArgs = do
  -- Look up the operation
  opInfo <- case lookupOperation effName opName env of
    Just info -> Right info
    Nothing -> Left $ UnknownOperation effName opName

  -- Look up effect for parameters
  _effInfo <- case lookupEffect effName env of
    Just info -> Right info
    Nothing -> Left $ UnknownEffect effName

  -- Build substitution
  let subst = zip [0..] typeArgs

  -- Apply substitution to operation
  pure $ OperationInfo
    { operationName = operationName opInfo
    , operationEffect = effName
    , operationReturnType = applyTypeSubst subst (operationReturnType opInfo)
    , operationParams = map (\(n, t) -> (n, applyTypeSubst subst t)) (operationParams opInfo)
    }
