{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Crisp.Types.Infer
-- Description : Type inference for let-bindings
--
-- Implements Hindley-Milner style type inference with let-polymorphism.
-- Key features:
-- - Inference: Infer types of expressions
-- - Generalization: Quantify free type variables not in environment
-- - Instantiation: Create fresh type variables for polymorphic uses
--
-- The algorithm follows Algorithm W with extensions for Crisp's type system.

module Crisp.Types.Infer
  ( -- * Inference
    infer
  , inferScheme
    -- * Inference environment
  , InferEnv
  , emptyInferEnv
  , extendInferEnv
  , lookupInferEnv
    -- * Generalization and instantiation
  , generalize
  , instantiate
    -- * Running inference
  , runInfer
  , runInferWith
  , Infer
    -- * Errors
  , InferError(..)
    -- * Re-exports
  , module Crisp.Types.Scheme
  ) where

import Crisp.Core.Term
import Crisp.Types.Scheme
import Crisp.Types.Substitution
import Crisp.Types.Unify (unify, UnifyError(..))

import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Errors that can occur during type inference
data InferError
  = UnboundVariable !Text
  | UnificationFailed !UnifyError
  | NotAFunction !Type
  | OtherInferError !Text
  deriving stock (Eq, Show)

-- | Inference state for fresh variable generation
data InferState = InferState
  { inferFreshCounter :: !Int
  } deriving stock (Eq, Show)

-- | Initial inference state
initialState :: InferState
initialState = InferState 0

-- | Inference monad
newtype Infer a = Infer { unInfer :: StateT InferState (Except InferError) a }
  deriving newtype (Functor, Applicative, Monad, MonadState InferState, MonadError InferError)

-- | Run inference computation
runInfer :: Infer a -> Either InferError a
runInfer = runExcept . flip evalStateT initialState . unInfer

-- | Run inference computation with an environment
runInferWith :: InferEnv -> Infer a -> Either InferError a
runInferWith _env = runInfer

-- | Inference environment mapping names to type schemes
newtype InferEnv = InferEnv { unInferEnv :: Map Text Scheme }
  deriving stock (Eq, Show)

-- | Empty inference environment
emptyInferEnv :: InferEnv
emptyInferEnv = InferEnv $ Map.fromList
  [ ("Unit", monoScheme (simpleType "Unit"))
  , ("True", monoScheme (simpleType "Bool"))
  , ("False", monoScheme (simpleType "Bool"))
  , ("Int", monoScheme (simpleType "Int"))
  , ("Bool", monoScheme (simpleType "Bool"))
  , ("String", monoScheme (simpleType "String"))
  -- Nil is polymorphic: forall a. List a
  , ("Nil", Scheme (Set.singleton 0) (TyCon "List" [TyVar "a" 0]))
  ]

-- | Extend environment with a binding
extendInferEnv :: Text -> Scheme -> InferEnv -> InferEnv
extendInferEnv name scheme (InferEnv env) =
  InferEnv $ Map.insert name scheme env

-- | Look up a binding in the environment
lookupInferEnv :: Text -> InferEnv -> Maybe Scheme
lookupInferEnv name (InferEnv env) = Map.lookup name env

-- | Generate a fresh type variable
freshTypeVar :: Text -> Infer Type
freshTypeVar prefix = do
  n <- gets inferFreshCounter
  modify $ \s -> s { inferFreshCounter = n + 1 }
  pure $ TyVar (prefix <> T.pack (show n)) n

-- | Infer the type of a term
--
-- Returns the inferred type and a substitution recording type variable bindings.
infer :: InferEnv -> Term -> Either InferError (Type, Substitution)
infer env term = runInfer (inferTerm env term)

-- | Infer a type scheme for a term
inferScheme :: InferEnv -> Term -> Either InferError Scheme
inferScheme env term = do
  (ty, subst) <- infer env term
  let finalTy = applySubst subst ty
  pure $ generalize env finalTy

-- | Internal inference function
inferTerm :: InferEnv -> Term -> Infer (Type, Substitution)
inferTerm env = \case
  TmVar name _idx -> do
    case lookupInferEnv name env of
      Just scheme -> do
        ty <- instantiate scheme
        pure (ty, emptySubst)
      Nothing -> throwError $ UnboundVariable name

  TmCon conName _tyArgs _args -> do
    case lookupInferEnv conName env of
      Just scheme -> do
        ty <- instantiate scheme
        pure (ty, emptySubst)
      Nothing ->
        -- Default to returning the constructor name as a simple type
        pure (simpleType conName, emptySubst)

  TmLam paramName paramTy body -> do
    -- Extend environment with parameter
    let paramScheme = monoScheme paramTy
        env' = extendInferEnv paramName paramScheme env
    -- Infer body type
    (bodyTy, subst) <- inferTerm env' body
    -- Construct function type
    let resultTy = TyPi paramName (applySubst subst paramTy) EffEmpty bodyTy
    pure (resultTy, subst)

  TmApp func arg -> do
    -- Infer function type
    (funcTy, s1) <- inferTerm env func
    -- Apply substitution to environment
    let env1 = applySubstEnv s1 env
    -- Infer argument type
    (argTy, s2) <- inferTerm env1 arg
    -- Create fresh type variable for result
    resultTy <- freshTypeVar "r"
    -- Unify function type with arg -> result
    let funcTy' = applySubst s2 funcTy
        expectedFuncTy = TyPi "_" argTy EffEmpty resultTy
    case unify funcTy' expectedFuncTy of
      Right s3 -> do
        let finalSubst = composeSubst s3 (composeSubst s2 s1)
            finalTy = applySubst s3 resultTy
        pure (finalTy, finalSubst)
      Left err -> throwError $ UnificationFailed err

  TmLet name boundTy value body -> do
    -- Infer value type
    (valueTy, s1) <- inferTerm env value
    -- Unify with annotation
    case unify valueTy (applySubst s1 boundTy) of
      Right s2 -> do
        let s12 = composeSubst s2 s1
            generalizedTy = applySubst s12 boundTy
            -- Generalize the type
            env' = applySubstEnv s12 env
            scheme = generalize env' generalizedTy
            -- Extend environment with generalized scheme
            env'' = extendInferEnv name scheme env'
        -- Infer body
        (bodyTy, s3) <- inferTerm env'' body
        pure (bodyTy, composeSubst s3 s12)
      Left err -> throwError $ UnificationFailed err

  TmTyAbs typeVar kind body -> do
    -- Type abstraction - just infer the body
    (bodyTy, subst) <- inferTerm env body
    pure (TyForall typeVar kind bodyTy, subst)

  TmTyApp tm _tyArg -> do
    -- Type application
    (tmTy, subst) <- inferTerm env tm
    case tmTy of
      TyForall _ _ bodyTy -> do
        -- Substitute type argument
        -- For now, simple substitution
        pure (bodyTy, subst)
      _ -> throwError $ OtherInferError "Expected forall type in type application"

  TmMatch subject _retTy cases -> do
    (subjTy, s1) <- inferTerm env subject
    resultTy <- freshTypeVar "match"
    (finalTy, subst) <- inferCases (applySubstEnv s1 env) cases subjTy resultTy s1
    pure (finalTy, subst)

  TmPerform _effect _op arg -> do
    (_argTy, subst) <- inferTerm env arg
    -- Effect operations return a result type that we can't know statically
    resultTy <- freshTypeVar "eff"
    pure (resultTy, subst)

  TmHandle _handler body -> do
    inferTerm env body

  TmLazy inner -> do
    (innerTy, subst) <- inferTerm env inner
    pure (TyLazy innerTy, subst)

  TmForce inner -> do
    (innerTy, subst) <- inferTerm env inner
    case innerTy of
      TyLazy resultTy -> pure (resultTy, subst)
      TyVar _name _idx -> do
        -- Create fresh result type and unify
        resultTy <- freshTypeVar "force"
        case unify innerTy (TyLazy resultTy) of
          Right s2 -> pure (applySubst s2 resultTy, composeSubst s2 subst)
          Left err -> throwError $ UnificationFailed err
      _ -> throwError $ OtherInferError "Cannot force non-lazy value"

  TmAnnot inner ty -> do
    (innerTy, subst) <- inferTerm env inner
    case unify innerTy ty of
      Right s2 -> pure (applySubst s2 ty, composeSubst s2 subst)
      Left err -> throwError $ UnificationFailed err

-- | Infer types for match cases
inferCases :: InferEnv -> [Case] -> Type -> Type -> Substitution -> Infer (Type, Substitution)
inferCases _env [] _subjTy resultTy subst = pure (resultTy, subst)
inferCases env (Case pat body : rest) subjTy resultTy subst = do
  -- Extend environment with pattern bindings
  let env' = extendWithPattern pat subjTy env
  -- Infer body type
  (bodyTy, s1) <- inferTerm env' body
  -- Unify with expected result type
  case unify bodyTy (applySubst s1 resultTy) of
    Right s2 -> do
      let s12 = composeSubst s2 (composeSubst s1 subst)
      inferCases (applySubstEnv s12 env) rest subjTy (applySubst s2 resultTy) s12
    Left err -> throwError $ UnificationFailed err

-- | Extend environment with pattern bindings
extendWithPattern :: Pattern -> Type -> InferEnv -> InferEnv
extendWithPattern pat ty env = case pat of
  PatVar name -> extendInferEnv name (monoScheme ty) env
  PatWild -> env
  PatCon _ pats ->
    -- For constructor patterns, we'd need constructor info
    -- For now, just recurse with unknown types
    foldr (\p e -> extendWithPattern p (TyVar "_" 0) e) env pats

-- | Generalize a type to a scheme by quantifying over free variables not in env
generalize :: InferEnv -> Type -> Scheme
generalize env ty =
  let envFreeVars = freeTypeVarsInEnv env
      tyFreeVars = freeTypeVars ty
      quantified = tyFreeVars `Set.difference` envFreeVars
  in Scheme quantified ty

-- | Instantiate a scheme with fresh type variables
instantiate :: Scheme -> Infer Type
instantiate (Scheme quantified body) = do
  -- Create fresh variables for each quantified variable
  freshVars <- mapM (\idx -> do
    fresh <- freshTypeVar "inst"
    pure (idx, fresh)) (Set.toList quantified)
  -- Build substitution
  let subst = foldr (\(idx, ty) s -> insertSubst idx ty s) emptySubst freshVars
  pure $ applySubst subst body

-- | Compute free type variables in an inference environment
freeTypeVarsInEnv :: InferEnv -> Set Int
freeTypeVarsInEnv (InferEnv env) =
  Set.unions $ map freeTypeVarsScheme $ Map.elems env

-- | Apply substitution to environment
applySubstEnv :: Substitution -> InferEnv -> InferEnv
applySubstEnv subst (InferEnv env) =
  InferEnv $ Map.map (applySubstToScheme subst) env

-- | Apply substitution to a scheme
applySubstToScheme :: Substitution -> Scheme -> Scheme
applySubstToScheme subst (Scheme quantified body) =
  Scheme quantified (applySubst subst body)
