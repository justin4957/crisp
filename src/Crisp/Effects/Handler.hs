{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Effects.Handler
-- Description : Handler typing rules for algebraic effects
--
-- Implements type checking for effect handlers, ensuring operation clauses
-- have correct types and handlers correctly transform effect rows.
--
-- A handler intercepts effect operations and provides implementations:
--
-- @
-- handler state_handler(init: S) -> (A) -[State(S) | ε]-> A to (A) -[ε]-> A:
--   return(x) -> x
--   get() -> resume(init)
--   put(s) -> resume(())
-- @
--
-- Key responsibilities:
-- - Validate operation clauses match effect signatures
-- - Verify resume continuation has correct type
-- - Check return clause transforms values correctly
-- - Compute output effect row (handled effect removed)

module Crisp.Effects.Handler
  ( -- * Handler Declarations
    HandlerDecl(..)
  , OpClause(..)
  , ReturnClause(..)
    -- * Handler Information
  , HandlerInfo(..)
    -- * Checking
  , checkHandlerDecl
  , checkHandlerDeclWithArgs
  , checkOperationClause
  , checkReturnClause
  , HandlerError(..)
    -- * Type Computation
  , getOperationParamType
  , getResumeType
  , computeHandlerType
  , buildHandlerInfo
    -- * Effect Row Operations
  , removeHandledEffect
  , containsEffect
  , hasEffectVariable
  , computeHandlerInputEffects
  , computeHandlerOutputEffects
  ) where

import Crisp.Core.Term
import Crisp.Effects.Typing

import Data.List (nub, (\\))
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Handler Declarations
--------------------------------------------------------------------------------

-- | A handler declaration
data HandlerDecl = HandlerDecl
  { handlerDeclName :: !Text
    -- ^ Name of the handler
  , handlerDeclEffect :: !Text
    -- ^ Effect being handled
  , handlerDeclTypeParams :: ![(Text, Kind)]
    -- ^ Type parameters
  , handlerDeclInputType :: !Type
    -- ^ Input computation result type
  , handlerDeclOutputType :: !Type
    -- ^ Output result type
  , handlerDeclOperations :: ![OpClause]
    -- ^ Operation clauses
  , handlerDeclReturn :: !ReturnClause
    -- ^ Return clause
  } deriving stock (Eq, Show)

-- | An operation clause in a handler
data OpClause = OpClause
  { opClauseName :: !Text
    -- ^ Operation name
  , opClausePattern :: !Pattern
    -- ^ Pattern for operation argument
  , opClauseResume :: !Text
    -- ^ Name for resume continuation
  , opClauseBody :: !Term
    -- ^ Clause body
  } deriving stock (Eq, Show)

-- | Return clause in a handler
data ReturnClause = ReturnClause
  { returnClausePattern :: !Pattern
    -- ^ Pattern for return value
  , returnClauseBody :: !Term
    -- ^ Clause body
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Handler Information
--------------------------------------------------------------------------------

-- | Information about a typed handler
data HandlerInfo = HandlerInfo
  { handlerInfoName :: !Text
    -- ^ Handler name
  , handlerInfoEffect :: !Text
    -- ^ Handled effect
  , handlerInfoTypeParams :: ![(Text, Kind)]
    -- ^ Type parameters
  , handlerInfoInputType :: !Type
    -- ^ Input type
  , handlerInfoOutputType :: !Type
    -- ^ Output type
  , handlerInfoInputEffects :: !EffectRow
    -- ^ Input effect row (includes handled effect)
  , handlerInfoOutputEffects :: !EffectRow
    -- ^ Output effect row (handled effect removed)
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

-- | Errors during handler type checking
data HandlerError
  = UnknownHandledEffect !Text
    -- ^ Unknown effect being handled
  | MissingOperations !Text ![Text]
    -- ^ Handler missing operations (effect name, missing ops)
  | UnknownOperationInHandler !Text !Text
    -- ^ Unknown operation in handler (effect, op)
  | DuplicateOperationClause !Text !Text
    -- ^ Duplicate operation clause (effect, op)
  | OperationTypeMismatch !Text !Text !Type !Type
    -- ^ Operation type mismatch (effect, op, expected, actual)
  | ResumeTypeMismatch !Text !Type !Type
    -- ^ Resume type mismatch (op, expected, actual)
  | ReturnTypeMismatch !Type !Type
    -- ^ Return clause type mismatch (expected, actual)
  | InvalidPattern !Pattern !Type
    -- ^ Pattern doesn't match type
  | HandlerEffectMismatch !EffectRow !EffectRow
    -- ^ Effect row mismatch
  | OtherHandlerError !Text
    -- ^ Other error
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Checking
--------------------------------------------------------------------------------

-- | Check a handler declaration
checkHandlerDecl :: EffectEnv -> HandlerDecl -> Either HandlerError ()
checkHandlerDecl env decl = checkHandlerDeclWithArgs env decl []

-- | Check a handler declaration with effect type arguments
checkHandlerDeclWithArgs :: EffectEnv -> HandlerDecl -> [Type] -> Either HandlerError ()
checkHandlerDeclWithArgs env decl typeArgs = do
  let effName = handlerDeclEffect decl

  -- Look up the effect
  effInfo <- case lookupEffect effName env of
    Just info -> Right info
    Nothing -> Left $ UnknownHandledEffect effName

  -- Get expected operations
  let expectedOps = map operationName (effectInfoOps effInfo)
      providedOps = map opClauseName (handlerDeclOperations decl)

  -- Check for duplicate operation clauses
  let dupOps = findDuplicates providedOps
  case dupOps of
    (dup:_) -> Left $ DuplicateOperationClause effName dup
    [] -> pure ()

  -- Check for unknown operations
  let unknownOps = providedOps \\ expectedOps
  case unknownOps of
    (unknown:_) -> Left $ UnknownOperationInHandler effName unknown
    [] -> pure ()

  -- Check for missing operations
  let missingOps = expectedOps \\ providedOps
  case missingOps of
    [] -> pure ()
    missing -> Left $ MissingOperations effName missing

  -- Check each operation clause
  mapM_ (checkOperationClause env effName typeArgs) (handlerDeclOperations decl)

  -- Check return clause
  checkReturnClause (handlerDeclInputType decl) (handlerDeclOutputType decl)
                    (handlerDeclReturn decl)

  pure ()

-- | Find duplicates in a list
findDuplicates :: Eq a => [a] -> [a]
findDuplicates xs = xs \\ nub xs

-- | Check an operation clause
checkOperationClause :: EffectEnv -> Text -> [Type] -> OpClause -> Either HandlerError ()
checkOperationClause env effName typeArgs clause = do
  let opName = opClauseName clause

  -- Look up operation
  opInfo <- case lookupOperation effName opName env of
    Just info -> Right info
    Nothing -> Left $ UnknownOperationInHandler effName opName

  -- Get instantiated parameter types
  let subst = zip [0..] typeArgs
      paramTypes = map (applyTypeSubst' subst . snd) (operationParams opInfo)

  -- Pattern should match the parameter type
  -- For now, just check that pattern exists if there are params
  case (paramTypes, opClausePattern clause) of
    ([], PatWild) -> pure ()
    ([], PatVar _) -> pure ()
    ([], PatCon _ _) -> pure ()  -- Constructor pattern for unit-like ops
    (_:_, _) -> pure ()  -- Pattern exists, basic check passes

  pure ()

-- | Check a return clause
checkReturnClause :: Type -> Type -> ReturnClause -> Either HandlerError ()
checkReturnClause _inputType _outputType _clause = do
  -- For now, basic structural check
  -- A full implementation would type-check the clause body
  pure ()

--------------------------------------------------------------------------------
-- Type Computation
--------------------------------------------------------------------------------

-- | Get the parameter types of an operation
getOperationParamType :: EffectEnv -> Text -> Text -> [Type] -> Either HandlerError [Type]
getOperationParamType env effName opName typeArgs = do
  opInfo <- case lookupOperation effName opName env of
    Just info -> Right info
    Nothing -> Left $ UnknownOperationInHandler effName opName

  let subst = zip [0..] typeArgs
      paramTypes = map (applyTypeSubst' subst . snd) (operationParams opInfo)

  pure paramTypes

-- | Get the type of the resume continuation for an operation
--
-- For an operation with signature: op(params) -> ReturnType
-- The resume continuation has type: ReturnType -> HandlerOutputType
getResumeType :: EffectEnv -> Text -> Text -> [Type] -> Type -> Either HandlerError Type
getResumeType env effName opName typeArgs resultType = do
  opInfo <- case lookupOperation effName opName env of
    Just info -> Right info
    Nothing -> Left $ UnknownOperationInHandler effName opName

  let subst = zip [0..] typeArgs
      retTy = applyTypeSubst' subst (operationReturnType opInfo)

  -- Resume takes the operation's return type and produces the handler's result
  pure $ TyPi "_" retTy EffEmpty resultType

-- | Compute the full handler type
computeHandlerType :: EffectEnv -> HandlerDecl -> EffectRow -> Either HandlerError HandlerInfo
computeHandlerType env decl restEffects = do
  let effName = handlerDeclEffect decl

  -- Verify effect exists
  _ <- case lookupEffect effName env of
    Just info -> Right info
    Nothing -> Left $ UnknownHandledEffect effName

  let inputEffects = computeHandlerInputEffects effName restEffects
      outputEffects = computeHandlerOutputEffects effName inputEffects

  pure $ HandlerInfo
    { handlerInfoName = handlerDeclName decl
    , handlerInfoEffect = effName
    , handlerInfoTypeParams = handlerDeclTypeParams decl
    , handlerInfoInputType = handlerDeclInputType decl
    , handlerInfoOutputType = handlerDeclOutputType decl
    , handlerInfoInputEffects = inputEffects
    , handlerInfoOutputEffects = outputEffects
    }

-- | Build handler info from a declaration
buildHandlerInfo :: EffectEnv -> HandlerDecl -> Either HandlerError HandlerInfo
buildHandlerInfo env decl = computeHandlerType env decl (EffVar "ε" 0)

--------------------------------------------------------------------------------
-- Effect Row Operations
--------------------------------------------------------------------------------

-- | Remove a handled effect from an effect row
removeHandledEffect :: Text -> EffectRow -> EffectRow
removeHandledEffect name = \case
  EffEmpty -> EffEmpty
  EffSet effs ->
    case filter (\e -> effectName e /= name) effs of
      [] -> EffEmpty
      filtered -> EffSet filtered
  EffVar n i -> EffVar n i  -- Can't remove from variable
  EffUnion a b ->
    let a' = removeHandledEffect name a
        b' = removeHandledEffect name b
    in unionEffects a' b'

-- | Union two effect rows
unionEffects :: EffectRow -> EffectRow -> EffectRow
unionEffects EffEmpty other = other
unionEffects other EffEmpty = other
unionEffects (EffSet a) (EffSet b) = EffSet (nub (a ++ b))
unionEffects a b = EffUnion a b

-- | Check if an effect row contains a specific effect
containsEffect :: Text -> EffectRow -> Bool
containsEffect name = \case
  EffEmpty -> False
  EffSet effs -> any (\e -> effectName e == name) effs
  EffVar _ _ -> False  -- Variable might contain it, but we don't know
  EffUnion a b -> containsEffect name a || containsEffect name b

-- | Check if an effect row contains any effect variables
hasEffectVariable :: EffectRow -> Bool
hasEffectVariable = \case
  EffEmpty -> False
  EffSet _ -> False
  EffVar _ _ -> True
  EffUnion a b -> hasEffectVariable a || hasEffectVariable b

-- | Compute input effect row for handler (adds handled effect)
computeHandlerInputEffects :: Text -> EffectRow -> EffectRow
computeHandlerInputEffects effName restEffects =
  EffUnion (EffSet [Effect effName Nothing]) restEffects

-- | Compute output effect row for handler (removes handled effect)
computeHandlerOutputEffects :: Text -> EffectRow -> EffectRow
computeHandlerOutputEffects = removeHandledEffect

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Apply type substitution (index -> type)
applyTypeSubst' :: [(Int, Type)] -> Type -> Type
applyTypeSubst' subst = go
  where
    go ty = case ty of
      TyVar name idx ->
        case lookup idx subst of
          Just replacement -> replacement
          Nothing -> TyVar name idx

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
