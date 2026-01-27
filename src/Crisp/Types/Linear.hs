{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.Linear
-- Description : Linear type checking
--
-- Implements linear type checking for Crisp, ensuring that unique values
-- are used exactly once, borrowed references don't consume ownership,
-- and shared references allow multiple readers.

module Crisp.Types.Linear
  ( -- * Linear Environment
    LinearEnv(..)
  , emptyLinearEnv
  , extendLinearEnv
  , lookupUsage
  , markUsed
    -- * Linear Errors
  , LinearError(..)
    -- * Linear Checking
  , checkLinear
  , checkLinearWithPatternBindings
  , checkLinearWithLetBindings
  ) where

import Crisp.Types.Usage
import Crisp.Core.Term

import Control.Monad (foldM, forM, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- =============================================================================
-- Linear Environment
-- =============================================================================

-- | Linear environment tracking variable linearity and usage
newtype LinearEnv = LinearEnv
  { linearEnvBindings :: Map Text (LinearityMode, Usage)
  } deriving stock (Eq, Show, Generic)

-- | Empty linear environment
emptyLinearEnv :: LinearEnv
emptyLinearEnv = LinearEnv Map.empty

-- | Extend environment with a new binding
extendLinearEnv :: Text -> (LinearityMode, Usage) -> LinearEnv -> LinearEnv
extendLinearEnv name binding (LinearEnv bindings) =
  LinearEnv $ Map.insert name binding bindings

-- | Look up a variable's linearity mode and usage
lookupUsage :: Text -> LinearEnv -> Maybe (LinearityMode, Usage)
lookupUsage name (LinearEnv bindings) = Map.lookup name bindings

-- | Mark a variable as used (increments usage count)
markUsed :: Text -> LinearEnv -> LinearEnv
markUsed name (LinearEnv bindings) =
  LinearEnv $ Map.adjust incrementUsage name bindings
  where
    incrementUsage (mode, usage) = (mode, addUsage usage One)

-- | Get the usage of a variable
getUsage :: Text -> LinearEnv -> Usage
getUsage name env = case lookupUsage name env of
  Just (_, usage) -> usage
  Nothing -> Zero

-- | Get the linearity mode of a variable
getMode :: Text -> LinearEnv -> Maybe LinearityMode
getMode name env = fst <$> lookupUsage name env

-- | Update usage for a variable
updateUsage :: Text -> Usage -> LinearEnv -> LinearEnv
updateUsage name usage (LinearEnv bindings) =
  LinearEnv $ Map.adjust (\(mode, _) -> (mode, usage)) name bindings

-- | Get all unique variables in environment
uniqueVariables :: LinearEnv -> [Text]
uniqueVariables (LinearEnv bindings) =
  [name | (name, (Unique, _)) <- Map.toList bindings]

-- | Merge environments after alternative branches (like if-then-else)
mergeEnvs :: LinearEnv -> LinearEnv -> LinearEnv
mergeEnvs (LinearEnv bindings1) (LinearEnv bindings2) =
  LinearEnv $ Map.unionWith mergeBinding bindings1 bindings2
  where
    mergeBinding (mode, usage1) (_, usage2) = (mode, altUsage usage1 usage2)

-- =============================================================================
-- Linear Errors
-- =============================================================================

-- | Errors from linear type checking
data LinearError
  = UsedMoreThanOnce !Text           -- ^ Unique value used more than once
  | NotUsed !Text                    -- ^ Unique value not used
  | NotUsedInAllBranches !Text       -- ^ Unique value not used in all branches
  | BorrowEscapes !Text              -- ^ Borrowed reference escapes scope
  | InvalidBorrow !Text              -- ^ Invalid borrow operation
  deriving stock (Eq, Show, Generic)

-- =============================================================================
-- Linear Checking
-- =============================================================================

-- | Check linearity of a term, returning the updated environment or an error
checkLinear :: LinearEnv -> Term -> Either LinearError LinearEnv
checkLinear env term = do
  env' <- checkTerm env term
  -- After checking, verify all unique variables were used exactly once
  checkFinalUsage env'

-- | Check linearity with additional pattern bindings
-- Pattern bindings are added with their linearity modes, and the term is
-- assumed to use those bindings (e.g., a match branch with pattern variables)
checkLinearWithPatternBindings :: LinearEnv -> Term -> [(Text, LinearityMode)] -> Either LinearError LinearEnv
checkLinearWithPatternBindings env term bindings = do
  let env' = foldr (\(name, mode) e -> extendLinearEnv name (mode, Zero) e) env bindings
  env'' <- checkTerm env' term
  -- Check the pattern bindings were used correctly
  checkBindingsUsage env'' bindings

-- | Check linearity with let bindings
-- This uses a custom checker that injects linearity modes for let-bound variables
checkLinearWithLetBindings :: LinearEnv -> Term -> [(Text, LinearityMode)] -> Either LinearError LinearEnv
checkLinearWithLetBindings env term bindings = do
  env' <- checkTermWithBindings env term bindings
  -- Check final usage for injected bindings
  checkBindingsUsage env' bindings

-- | Check a term with injected binding modes
checkTermWithBindings :: LinearEnv -> Term -> [(Text, LinearityMode)] -> Either LinearError LinearEnv
checkTermWithBindings env term bindings = case term of
  TmLet name ty val body -> do
    -- Check the value expression
    env' <- checkTermWithBindings env val bindings
    -- Get the mode for this binding from the provided list
    let mode = lookupBindingMode name bindings
    -- Check if we're shadowing a unique variable that hasn't been used
    case lookupUsage name env' of
      Just (Unique, Zero) -> Left $ NotUsed name
      _ -> do
        -- Add binding with the specified mode
        let env'' = extendLinearEnv name (mode, Zero) env'
        -- Check the body
        checkTermWithBindings env'' body bindings

  -- For other terms, defer to regular checking
  _ -> checkTerm env term

-- | Look up binding mode from a list, default to Unrestricted
lookupBindingMode :: Text -> [(Text, LinearityMode)] -> LinearityMode
lookupBindingMode name bindings =
  case lookup name bindings of
    Just mode -> mode
    Nothing -> Unrestricted

-- | Check that bindings were used correctly according to their modes
checkBindingsUsage :: LinearEnv -> [(Text, LinearityMode)] -> Either LinearError LinearEnv
checkBindingsUsage env bindings = do
  mapM_ checkBinding bindings
  pure env
  where
    checkBinding (name, Unique) = case lookupUsage name env of
      Just (_, Zero) -> Left $ NotUsed name
      Just (_, One) -> Right ()
      Just (_, Many) -> Left $ UsedMoreThanOnce name
      Just (_, Inconsistent) -> Left $ NotUsedInAllBranches name
      Nothing -> Right ()  -- Not in scope (possibly in a branch), skip
    checkBinding _ = Right ()  -- Non-unique bindings don't need checking

-- | Check that all unique variables have been used exactly once
checkFinalUsage :: LinearEnv -> Either LinearError LinearEnv
checkFinalUsage env = do
  -- Check each unique variable
  mapM_ checkVar (uniqueVariables env)
  pure env
  where
    checkVar name = case lookupUsage name env of
      Just (Unique, Zero) -> Left $ NotUsed name
      Just (Unique, One) -> Right ()
      Just (Unique, Many) -> Left $ UsedMoreThanOnce name
      Just (Unique, Inconsistent) -> Left $ NotUsedInAllBranches name
      _ -> Right ()

-- | Check a term and update the environment
checkTerm :: LinearEnv -> Term -> Either LinearError LinearEnv
checkTerm env term = case term of
  -- Variable reference
  TmVar name _ -> do
    case getMode name env of
      Just Unique -> pure $ markUsed name env
      Just Borrowed -> pure $ markUsed name env
      Just Shared -> pure $ markUsed name env
      Just Unrestricted -> pure env
      Nothing -> pure env  -- Free variable (e.g., from outer scope)

  -- Lambda abstraction
  TmLam paramName _paramType body -> do
    -- Add parameter as unrestricted by default
    -- (linearity would come from type annotation)
    let env' = extendLinearEnv paramName (Unrestricted, Zero) env
    checkTerm env' body

  -- Application
  TmApp func arg -> do
    env' <- checkTerm env func
    checkTerm env' arg

  -- Let binding
  TmLet name _ty value body -> do
    env' <- checkTerm env value
    -- Check if we're shadowing a unique variable that hasn't been used
    case lookupUsage name env' of
      Just (Unique, Zero) -> Left $ NotUsed name
      _ -> do
        -- Add binding (default to unrestricted if not specified)
        let env'' = extendLinearEnv name (Unrestricted, Zero) env'
        checkTerm env'' body

  -- Type abstraction
  TmTyAbs _name _kind body ->
    checkTerm env body

  -- Type application
  TmTyApp term' _ty ->
    checkTerm env term'

  -- Constructor application
  TmCon _name _types args ->
    foldM checkTerm env args

  -- Pattern match
  TmMatch scrutinee _ty cases -> do
    env' <- checkTerm env scrutinee
    -- Check each case and merge environments
    checkCases env' cases

  -- Effect operation
  TmPerform _effect _op arg ->
    checkTerm env arg

  -- Effect handler
  TmHandle handler body -> do
    -- Check the handler clauses
    env' <- checkHandler env handler
    checkTerm env' body

  -- Lazy
  TmLazy body ->
    -- The body is delayed, but captures happen now
    checkTerm env body

  -- Force
  TmForce thunk ->
    checkTerm env thunk

  -- Type annotation
  TmAnnot term' _ty ->
    checkTerm env term'

-- | Check pattern match cases
checkCases :: LinearEnv -> [Case] -> Either LinearError LinearEnv
checkCases env cases = do
  -- Check each case independently and merge results
  envs <- forM cases $ \(Case pat body) -> do
    let patBindings = patternBindings pat
        env' = foldr (\name e -> extendLinearEnv name (Unrestricted, Zero) e) env patBindings
    checkTerm env' body

  -- Merge all case environments
  case envs of
    [] -> pure env
    (e:es) -> do
      let merged = foldl mergeEnvs e es
      -- Check for inconsistent usage
      checkInconsistentUsage merged

-- | Check for inconsistent usage and report error
checkInconsistentUsage :: LinearEnv -> Either LinearError LinearEnv
checkInconsistentUsage env@(LinearEnv bindings) = do
  mapM_ checkBinding (Map.toList bindings)
  pure env
  where
    checkBinding (name, (Unique, Inconsistent)) =
      Left $ NotUsedInAllBranches name
    checkBinding _ = Right ()

-- | Get variable names bound by a pattern
patternBindings :: Pattern -> [Text]
patternBindings pat = case pat of
  PatVar name -> [name]
  PatWild -> []
  PatCon _ pats -> concatMap patternBindings pats

-- | Check handler clauses
checkHandler :: LinearEnv -> Handler -> Either LinearError LinearEnv
checkHandler env (Handler _effect _intros ops ret) = do
  -- Check each operation handler
  env' <- foldM checkOpHandler env ops
  -- Check return handler
  checkReturnHandler env' ret

-- | Check an operation handler clause
checkOpHandler :: LinearEnv -> OpHandler -> Either LinearError LinearEnv
checkOpHandler env (OpHandler _op pat _resume body) = do
  let patBindings = patternBindings pat
      env' = foldr (\name e -> extendLinearEnv name (Unrestricted, Zero) e) env patBindings
  checkTerm env' body

-- | Check a return handler clause
checkReturnHandler :: LinearEnv -> ReturnHandler -> Either LinearError LinearEnv
checkReturnHandler env (ReturnHandler pat body) = do
  let patBindings = patternBindings pat
      env' = foldr (\name e -> extendLinearEnv name (Unrestricted, Zero) e) env patBindings
  checkTerm env' body
