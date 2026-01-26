{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Types.GADT
-- Description : GADT typing with index refinement
--
-- Implements GADT (Generalized Algebraic Data Types) support including:
-- - GADT declaration validation
-- - Type index refinement during pattern matching
-- - Impossible branch detection
-- - Existential types in constructors
-- - Type equality witness handling
--
-- GADTs allow constructors to have refined return types, enabling
-- type-level evidence to flow from pattern matching into branches.
--
-- Example:
-- @
-- type Expr(A: Type) =
--   | IntLit(value: Int) : Expr(Int)
--   | BoolLit(value: Bool) : Expr(Bool)
--   | If(cond: Expr(Bool), then_: Expr(A), else_: Expr(A)) : Expr(A)
--
-- fn eval(e: Expr(A)) -> A =
--   match e:
--     IntLit(n) -> n        -- Here A = Int
--     BoolLit(b) -> b       -- Here A = Bool
--     If(c, t, e) -> if eval(c) then eval(t) else eval(e)
-- @

module Crisp.Types.GADT
  ( -- * GADT Environment
    GadtEnv
  , emptyGadtEnv
  , addGadtDecl
  , getGadtConstructorInfo
    -- * GADT Constructor Info
  , GadtConstructorInfo(..)
  , gadtReturnType
    -- * Declaration Checking
  , checkGadtDecl
    -- * Pattern Refinement
  , GadtRefinement(..)
  , refineGadtPattern
    -- * Impossible Branch Detection
  , isImpossibleBranch
    -- * GADT Unification
  , unifyGadtTypes
    -- * Errors
  , GadtError(..)
  ) where

import Crisp.Core.Term
import Crisp.Types.Constructor
import Crisp.Types.Unify
import Crisp.Types.Dependent (evalType)

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- GADT Environment
--------------------------------------------------------------------------------

-- | Information about a GADT constructor
data GadtConstructorInfo = GadtConstructorInfo
  { gadtConName :: !Text
    -- ^ Constructor name
  , gadtConTypeName :: !Text
    -- ^ Parent type name
  , gadtConParamTypes :: ![Type]
    -- ^ Parameter types (may contain type variables)
  , gadtConReturnType :: !Type
    -- ^ Refined return type (the GADT index)
  , gadtConTypeParams :: ![(Text, Kind)]
    -- ^ Type parameters from parent type
  , gadtConExistentials :: ![Text]
    -- ^ Existentially quantified type variables in this constructor
  } deriving stock (Eq, Show)

-- | Get the GADT return type
gadtReturnType :: GadtConstructorInfo -> Type
gadtReturnType = gadtConReturnType

-- | GADT environment
data GadtEnv = GadtEnv
  { gadtEnvTypes :: !(Map Text TypeDecl)
  , gadtEnvConstructors :: !(Map Text GadtConstructorInfo)
  } deriving stock (Eq, Show)

-- | Empty GADT environment
emptyGadtEnv :: GadtEnv
emptyGadtEnv = GadtEnv Map.empty Map.empty

-- | Alias for ConstructorEnv for backwards compatibility
type GadtConstructorEnv = GadtEnv

emptyConstructorEnv :: GadtEnv
emptyConstructorEnv = emptyGadtEnv

-- | Add a GADT declaration to the environment
addGadtDecl :: TypeDecl -> GadtEnv -> GadtEnv
addGadtDecl decl env =
  let conInfos = mkGadtConstructorInfos decl
      env' = env { gadtEnvTypes = Map.insert (typeDeclName decl) decl (gadtEnvTypes env) }
  in foldr addGadtCon env' conInfos

-- | Add a constructor to the environment
addGadtCon :: GadtConstructorInfo -> GadtEnv -> GadtEnv
addGadtCon info env = env
  { gadtEnvConstructors = Map.insert (gadtConName info) info (gadtEnvConstructors env) }

-- | Create GADT constructor info from type declaration
mkGadtConstructorInfos :: TypeDecl -> [GadtConstructorInfo]
mkGadtConstructorInfos decl = map mkConInfo (typeDeclConstructors decl)
  where
    typeName = typeDeclName decl
    typeParams = typeDeclParams decl
    defaultReturnType = mkDefaultReturnType typeName typeParams

    mkConInfo conDecl =
      let explicitRetTy = case conDeclReturnType conDecl of
            Just ty -> ty
            Nothing -> defaultReturnType
          existentials = findExistentials typeParams (conDeclParams conDecl) explicitRetTy
      in GadtConstructorInfo
           { gadtConName = conDeclName conDecl
           , gadtConTypeName = typeName
           , gadtConParamTypes = conDeclParams conDecl
           , gadtConReturnType = explicitRetTy
           , gadtConTypeParams = typeParams
           , gadtConExistentials = existentials
           }

-- | Create default return type T(A1, A2, ...) from type parameters
mkDefaultReturnType :: Text -> [(Text, Kind)] -> Type
mkDefaultReturnType name params =
  TyCon name [TyVar n i | ((n, _), i) <- zip params [0..]]

-- | Find existentially quantified type variables
-- These are type variables that appear in params but not in the declared type params
findExistentials :: [(Text, Kind)] -> [Type] -> Type -> [Text]
findExistentials typeParams paramTys retTy =
  let declaredVars = map fst typeParams
      paramVars = concatMap freeTypeVars paramTys
      retVars = freeTypeVars retTy
      allUsedVars = paramVars ++ retVars
      -- Variables in params that aren't declared type params
      existentialVars = filter (`notElem` declaredVars) allUsedVars
  in existentialVars

-- | Get free type variable names from a type
freeTypeVars :: Type -> [Text]
freeTypeVars = \case
  TyVar name _ -> [name]
  TyCon _ args -> concatMap freeTypeVars args
  TyPi _ paramTy _ retTy -> freeTypeVars paramTy ++ freeTypeVars retTy
  TyForall _ _ body -> freeTypeVars body
  TyForallDep _ paramTy body -> freeTypeVars paramTy ++ freeTypeVars body
  TyLazy inner -> freeTypeVars inner
  TyLinear inner -> freeTypeVars inner
  TyRef inner -> freeTypeVars inner
  TyRefMut inner -> freeTypeVars inner
  TySigma _ fstTy sndTy -> freeTypeVars fstTy ++ freeTypeVars sndTy
  TyAdd t1 t2 -> freeTypeVars t1 ++ freeTypeVars t2
  TyNatLit _ -> []
  TyUniverse _ -> []
  TyProp -> []
  TyEffect _ inner -> freeTypeVars inner

-- | Look up GADT constructor info
getGadtConstructorInfo :: Text -> GadtEnv -> Maybe GadtConstructorInfo
getGadtConstructorInfo name env = Map.lookup name (gadtEnvConstructors env)

--------------------------------------------------------------------------------
-- GADT Errors
--------------------------------------------------------------------------------

-- | Errors in GADT operations
data GadtError
  = InvalidGadtReturnType !Text !Type !Text
    -- ^ Constructor return type doesn't match parent type
  | GadtUnificationFailed !Text
    -- ^ Failed to unify GADT indices
  | UnknownGadtConstructor !Text
    -- ^ Constructor not found
  | ImpossiblePattern !Pattern !Type
    -- ^ Pattern is impossible for scrutinee type
  | GadtOtherError !Text
    -- ^ Generic error
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Declaration Checking
--------------------------------------------------------------------------------

-- | Check a GADT declaration
checkGadtDecl :: GadtEnv -> TypeDecl -> Either GadtError ()
checkGadtDecl _env decl = do
  -- Check each constructor's return type
  mapM_ (checkConstructorReturnType (typeDeclName decl)) (typeDeclConstructors decl)
  pure ()

-- | Check that constructor return type matches parent type
checkConstructorReturnType :: Text -> ConDecl -> Either GadtError ()
checkConstructorReturnType parentName conDecl =
  case conDeclReturnType conDecl of
    Nothing -> pure ()  -- Default return type is always valid
    Just retTy ->
      case retTy of
        TyCon name _ | name == parentName -> pure ()
        _ -> Left $ InvalidGadtReturnType (conDeclName conDecl) retTy parentName

--------------------------------------------------------------------------------
-- Pattern Refinement
--------------------------------------------------------------------------------

-- | Result of GADT pattern refinement
data GadtRefinement = GadtRefinement
  { gadtRefinedBindings :: ![(Text, Type)]
    -- ^ Variable bindings with their refined types
  , gadtTypeConstraints :: ![(Type, Type)]
    -- ^ Type equality constraints discovered (e.g., A = Int)
  , gadtExistentials :: ![(Text, Type)]
    -- ^ Existentially quantified variables introduced
  } deriving stock (Eq, Show)

-- | Refine types based on GADT pattern matching
--
-- When matching a GADT pattern, we:
-- 1. Look up the constructor's refined return type
-- 2. Unify it with the scrutinee type to get constraints
-- 3. Apply constraints to parameter types to get refined bindings
refineGadtPattern :: GadtEnv -> Type -> Pattern -> Either GadtError GadtRefinement
refineGadtPattern env scrutineeTy pat = case pat of
  PatWild -> Right $ GadtRefinement [] [] []

  PatVar name -> Right $ GadtRefinement [(name, scrutineeTy)] [] []

  PatCon conName subPats -> do
    info <- case getGadtConstructorInfo conName env of
      Just i -> Right i
      Nothing -> Left $ UnknownGadtConstructor conName

    -- Get the constructor's return type
    let conRetTy = gadtConReturnType info

    -- Unify constructor return type with scrutinee to get constraints
    case unifyGadtTypes conRetTy scrutineeTy of
      Left err -> Left $ GadtUnificationFailed (T.pack $ show err)
      Right subst -> do
        -- Apply substitution to get refined parameter types
        let refinedParamTys = map (applySubst subst) (gadtConParamTypes info)

        -- Extract bindings from subpatterns
        subBindings <- extractSubpatternBindings env refinedParamTys subPats

        -- Collect type constraints from unification
        let constraints = extractConstraints subst scrutineeTy conRetTy

        -- Handle existential types
        let existentials = map (\name -> (name, TyVar name 0)) (gadtConExistentials info)

        Right $ GadtRefinement
          { gadtRefinedBindings = subBindings
          , gadtTypeConstraints = constraints
          , gadtExistentials = existentials
          }

-- | Extract bindings from subpatterns with their types
extractSubpatternBindings :: GadtEnv -> [Type] -> [Pattern] -> Either GadtError [(Text, Type)]
extractSubpatternBindings env tys pats = do
  bindings <- zipWithM (refineGadtPattern env) tys pats
  pure $ concatMap gadtRefinedBindings bindings

-- | zipWithM for Either
zipWithM :: (a -> b -> Either e c) -> [a] -> [b] -> Either e [c]
zipWithM f xs ys = sequence $ zipWith f xs ys

-- | Extract type constraints from unification substitution
extractConstraints :: Substitution -> Type -> Type -> [(Type, Type)]
extractConstraints subst scrutineeTy conRetTy =
  case (scrutineeTy, conRetTy) of
    (TyCon _ scrutArgs, TyCon _ conArgs) ->
      -- Constraints are between corresponding type arguments
      zipWith makeConstraint conArgs scrutArgs
    _ -> []
  where
    makeConstraint conArg scrutArg =
      (applySubst subst conArg, scrutArg)

--------------------------------------------------------------------------------
-- Impossible Branch Detection
--------------------------------------------------------------------------------

-- | Check if a pattern branch is impossible for the given scrutinee type
--
-- A branch is impossible when the constructor's return type cannot
-- unify with the scrutinee type.
isImpossibleBranch :: GadtEnv -> Type -> Pattern -> Bool
isImpossibleBranch env scrutineeTy pat = case pat of
  PatWild -> False
  PatVar _ -> False
  PatCon conName _ ->
    case getGadtConstructorInfo conName env of
      Nothing -> False  -- Unknown constructor, can't determine
      Just info ->
        let conRetTy = gadtConReturnType info
        in case unifyGadtTypesStrict conRetTy scrutineeTy of
             Left _ -> True   -- Unification failed, branch is impossible
             Right _ -> False -- Unification succeeded

-- | Strict GADT unification that fails on incompatible indices
--
-- Unlike regular unification which succeeds with type variables,
-- this fails when concrete indices don't match.
unifyGadtTypesStrict :: Type -> Type -> Either GadtError Substitution
unifyGadtTypesStrict t1 t2 = case (normalizeType t1, normalizeType t2) of
  -- Both concrete type constructors
  (TyCon n1 args1, TyCon n2 args2)
    | n1 == n2 -> do
        subst <- unifyArgsStrict args1 args2
        Right subst
    | otherwise -> Left $ GadtUnificationFailed $
        "Type mismatch: " <> n1 <> " vs " <> n2

  -- Type variable on left can be instantiated
  (TyVar _ _, _) -> Right emptySubst

  -- Type variable on right can be instantiated
  (_, TyVar _ _) -> Right emptySubst

  -- Nat literals must match exactly
  (TyNatLit n, TyNatLit m)
    | n == m -> Right emptySubst
    | otherwise -> Left $ GadtUnificationFailed $
        "Index mismatch: " <> T.pack (show n) <> " vs " <> T.pack (show m)

  -- Type-level addition: try to normalize and compare
  (TyAdd a b, TyNatLit n) ->
    case evalType (TyAdd a b) of
      TyNatLit m | m == n -> Right emptySubst
      _ -> Left $ GadtUnificationFailed "Type-level arithmetic mismatch"

  (TyNatLit n, TyAdd a b) ->
    unifyGadtTypesStrict (TyAdd a b) (TyNatLit n)

  -- Other cases
  _ -> case unify t1 t2 of
    Left _ -> Left $ GadtUnificationFailed "Unification failed"
    Right s -> Right s

-- | Unify argument lists strictly
unifyArgsStrict :: [Type] -> [Type] -> Either GadtError Substitution
unifyArgsStrict [] [] = Right emptySubst
unifyArgsStrict (a:as) (b:bs) = do
  s1 <- unifyGadtTypesStrict a b
  s2 <- unifyArgsStrict (map (applySubst s1) as) (map (applySubst s1) bs)
  Right $ composeSubst s2 s1
unifyArgsStrict _ _ = Left $ GadtUnificationFailed "Arity mismatch"

-- | Normalize a type (evaluate type-level computations)
normalizeType :: Type -> Type
normalizeType = evalType

--------------------------------------------------------------------------------
-- GADT Unification
--------------------------------------------------------------------------------

-- | Unify GADT types with index constraint solving
--
-- This is a variant of unification that handles:
-- - Type-level natural numbers
-- - Type-level arithmetic (TyAdd)
-- - GADT index constraints
unifyGadtTypes :: Type -> Type -> Either GadtError Substitution
unifyGadtTypes t1 t2 =
  let t1' = normalizeType t1
      t2' = normalizeType t2
  in case unifyWithIndices t1' t2' of
    Left err -> Left $ GadtUnificationFailed (T.pack $ show err)
    Right s -> Right s

-- | Unification with index type support
unifyWithIndices :: Type -> Type -> Either UnifyError Substitution
unifyWithIndices t1 t2 = case (t1, t2) of
  -- Identical after normalization
  _ | t1 == t2 -> Right emptySubst

  -- Type variables
  (TyVar _ idx, _) -> bindVarChecked idx t2
  (_, TyVar _ idx) -> bindVarChecked idx t1

  -- Type constructors with indices
  (TyCon n1 args1, TyCon n2 args2)
    | n1 == n2 && length args1 == length args2 -> do
        unifyIndexList args1 args2
    | otherwise -> Left $ TypeMismatch t1 t2

  -- Nat literals
  (TyNatLit n, TyNatLit m)
    | n == m -> Right emptySubst
    | otherwise -> Left $ TypeMismatch t1 t2

  -- Type-level addition
  (TyAdd a b, TyNatLit n) ->
    solveAddition a b n

  (TyNatLit n, TyAdd a b) ->
    solveAddition a b n

  -- Other types - delegate to regular unification
  _ -> unify t1 t2

-- | Bind a variable with occurs check
bindVarChecked :: Int -> Type -> Either UnifyError Substitution
bindVarChecked idx ty
  | occursCheck idx ty = Left $ OccursCheck idx ty
  | otherwise = Right $ singleSubst idx ty

-- | Check if a variable occurs in a type
occursCheck :: Int -> Type -> Bool
occursCheck idx = go
  where
    go = \case
      TyVar _ i -> i == idx
      TyCon _ args -> any go args
      TyPi _ p _ r -> go p || go r
      TyForall _ _ b -> go b
      TyForallDep _ p b -> go p || go b
      TyLazy i -> go i
      TyLinear i -> go i
      TyRef i -> go i
      TyRefMut i -> go i
      TySigma _ a b -> go a || go b
      TyAdd a b -> go a || go b
      TyNatLit _ -> False
      TyUniverse _ -> False
      TyProp -> False
      TyEffect _ i -> go i

-- | Unify lists of types including indices
unifyIndexList :: [Type] -> [Type] -> Either UnifyError Substitution
unifyIndexList [] [] = Right emptySubst
unifyIndexList (t1:ts1) (t2:ts2) = do
  s1 <- unifyWithIndices t1 t2
  s2 <- unifyIndexList (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  Right $ composeSubst s2 s1
unifyIndexList _ _ = Left $ UnifyError "Index list length mismatch"

-- | Solve type-level addition constraint: a + b = n
solveAddition :: Type -> Type -> Int -> Either UnifyError Substitution
solveAddition a b n = case (a, b) of
  -- Variable + constant = n -> variable = n - constant
  (TyVar _ idx, TyNatLit m) | n >= m ->
    Right $ singleSubst idx (TyNatLit (n - m))

  (TyNatLit m, TyVar _ idx) | n >= m ->
    Right $ singleSubst idx (TyNatLit (n - m))

  -- Both constants - just check
  (TyNatLit m1, TyNatLit m2) | m1 + m2 == n ->
    Right emptySubst

  -- Both variables - can't solve uniquely, but allow if possible
  (TyVar _ _, TyVar _ _) ->
    Right emptySubst  -- Simplified: allow, constraints propagate

  -- Nested addition
  (TyAdd a' b', _) ->
    case evalType (TyAdd a' b') of
      TyNatLit m -> solveAddition (TyNatLit m) b n
      _ -> Left $ UnifyError "Cannot solve nested addition"

  (_, TyAdd a' b') ->
    case evalType (TyAdd a' b') of
      TyNatLit m -> solveAddition a (TyNatLit m) n
      _ -> Left $ UnifyError "Cannot solve nested addition"

  _ -> Left $ UnifyError $ T.pack $
         "Cannot solve: " <> T.unpack (typeToText a) <>
         " + " <> T.unpack (typeToText b) <>
         " = " <> show n

-- | Convert type to text for error messages
typeToText :: Type -> Text
typeToText = \case
  TyVar name _ -> name
  TyCon name args ->
    if null args then name
    else name <> "(" <> T.intercalate ", " (map typeToText args) <> ")"
  TyNatLit n -> T.pack (show n)
  TyAdd a b -> typeToText a <> " + " <> typeToText b
  _ -> "<?>"
