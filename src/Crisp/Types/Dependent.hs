{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Types.Dependent
-- Description : Dependent type support (Pi and Sigma types)
--
-- Implements Pi types (dependent function types) and Sigma types
-- (dependent pairs) for the Crisp type system. Includes:
--
-- - Pi type formation and elimination
-- - Sigma type formation and elimination
-- - Type-level evaluation/normalization
-- - Purity checking for type-level expressions
-- - Dependent type unification
--
-- Pi types: @(x : A) -> B[x]@ where B can mention x
-- Sigma types: @Sigma x:A. B[x]@ (dependent pairs)

module Crisp.Types.Dependent
  ( -- * Pi Types
    isPiType
  , piParamName
  , piParamType
  , piReturnType
  , checkPiTypeWellFormed
  , getPiDomainKind
  , applyPiType
    -- * Sigma Types
  , isSigmaType
  , sigmaFstType
  , sigmaSndTypeVar
  , checkSigmaTypeWellFormed
  , sigmaFst
  , sigmaSnd
    -- * Type-Level Evaluation
  , evalType
  , whnf
  , normalize
    -- * Purity Checking
  , isPure
    -- * Unification
  , unifyPiTypes
  , unifySigmaTypes
    -- * Desugaring
  , desugarForall
    -- * Pattern Refinement
  , refineTypeInPattern
    -- * Type Equivalence
  , typeEquiv
    -- * Errors
  , DependentTypeError(..)
  ) where

import Crisp.Core.Term
import Crisp.Types.Context

import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

-- | Errors during dependent type operations
data DependentTypeError
  = UnboundTypeVariable !Text
    -- ^ Type variable not in scope
  | PiDomainKindError !Text
    -- ^ Domain type has wrong kind
  | SigmaFormationError !Text
    -- ^ Sigma type malformed
  | ApplicationError !Text
    -- ^ Pi type application error
  | UnificationError !Text
    -- ^ Unification failed
  | PurityError !Text
    -- ^ Effectful expression in type
  | RefinementError !Text
    -- ^ Pattern refinement error
  | OtherDependentError !Text
    -- ^ Other error
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Pi Types
--------------------------------------------------------------------------------

-- | Check if a type is a Pi type
isPiType :: Type -> Bool
isPiType (TyPi _ _ _ _) = True
isPiType _ = False

-- | Get the parameter name of a Pi type
piParamName :: Type -> Maybe Text
piParamName (TyPi name _ _ _) = Just name
piParamName _ = Nothing

-- | Get the parameter type (domain) of a Pi type
piParamType :: Type -> Maybe Type
piParamType (TyPi _ paramTy _ _) = Just paramTy
piParamType _ = Nothing

-- | Get the return type (codomain) of a Pi type
piReturnType :: Type -> Maybe Type
piReturnType (TyPi _ _ _ retTy) = Just retTy
piReturnType _ = Nothing

-- | Check if a Pi type is well-formed
--
-- A Pi type (x : A) -> B is well-formed if:
-- - A is a well-formed type
-- - B is well-formed in context extended with x : A
checkPiTypeWellFormed :: Context -> Type -> Either DependentTypeError ()
checkPiTypeWellFormed ctx (TyPi name paramTy _ retTy) = do
  -- Check domain is well-formed
  checkTypeWellFormed ctx paramTy
  -- Check codomain in extended context
  let extendedCtx = extendWithType name paramTy ctx
  checkTypeWellFormed extendedCtx retTy
checkPiTypeWellFormed _ _ = Left $ OtherDependentError "Not a Pi type"

-- | Get the kind of a Pi type's domain
getPiDomainKind :: Context -> Type -> Either DependentTypeError Kind
getPiDomainKind ctx (TyPi _ paramTy _ _) = getTypeKind ctx paramTy
getPiDomainKind _ _ = Left $ OtherDependentError "Not a Pi type"

-- | Apply a Pi type to an argument
--
-- @(x : A) -> B@ applied to @a : A@ gives @B[a/x]@
applyPiType :: Type -> Type -> Either DependentTypeError Type
applyPiType (TyPi _ _ _ retTy) argTy = Right $ substituteType retTy 0 argTy
applyPiType _ _ = Left $ ApplicationError "Not a Pi type"

--------------------------------------------------------------------------------
-- Sigma Types
--------------------------------------------------------------------------------

-- | Check if a type is a Sigma type
isSigmaType :: Type -> Bool
isSigmaType (TySigma _ _ _) = True
isSigmaType _ = False

-- | Get the first component type of a Sigma type
sigmaFstType :: Type -> Maybe Type
sigmaFstType (TySigma _ fstTy _) = Just fstTy
sigmaFstType _ = Nothing

-- | Get the variable name for the second component
sigmaSndTypeVar :: Type -> Maybe Text
sigmaSndTypeVar (TySigma name _ _) = Just name
sigmaSndTypeVar _ = Nothing

-- | Check if a Sigma type is well-formed
checkSigmaTypeWellFormed :: Context -> Type -> Either DependentTypeError ()
checkSigmaTypeWellFormed ctx (TySigma name fstTy sndTy) = do
  -- Check first type is well-formed
  checkTypeWellFormed ctx fstTy
  -- Check second type in extended context
  let extendedCtx = extendWithType name fstTy ctx
  checkTypeWellFormed extendedCtx sndTy
checkSigmaTypeWellFormed _ _ = Left $ SigmaFormationError "Not a Sigma type"

-- | Get the first projection type
sigmaFst :: Type -> Either DependentTypeError Type
sigmaFst (TySigma _ fstTy _) = Right fstTy
sigmaFst _ = Left $ OtherDependentError "Not a Sigma type"

-- | Get the second projection type with the first value substituted
sigmaSnd :: Type -> Type -> Either DependentTypeError Type
sigmaSnd (TySigma _ _ sndTy) fstVal = Right $ substituteType sndTy 0 fstVal
sigmaSnd _ _ = Left $ OtherDependentError "Not a Sigma type"

--------------------------------------------------------------------------------
-- Type-Level Evaluation
--------------------------------------------------------------------------------

-- | Evaluate type-level expressions
--
-- Reduces type-level computations like Nat addition
evalType :: Type -> Type
evalType = \case
  TyAdd t1 t2 ->
    case (evalType t1, evalType t2) of
      (TyNatLit n, TyNatLit m) -> TyNatLit (n + m)
      (TyNatLit 0, t) -> t
      (t, TyNatLit 0) -> t
      (t1', t2') -> TyAdd t1' t2'

  TyCon name args -> TyCon name (map evalType args)

  TyPi name paramTy effs retTy ->
    TyPi name (evalType paramTy) effs (evalType retTy)

  TySigma name fstTy sndTy ->
    TySigma name (evalType fstTy) (evalType sndTy)

  TyForall name kind body ->
    TyForall name kind (evalType body)

  TyForallDep name paramTy body ->
    TyForallDep name (evalType paramTy) (evalType body)

  TyLazy inner -> TyLazy (evalType inner)
  TyLinear inner -> TyLinear (evalType inner)
  TyRef inner -> TyRef (evalType inner)
  TyRefMut inner -> TyRefMut (evalType inner)

  -- Atomic types
  t@(TyVar _ _) -> t
  t@(TyNatLit _) -> t
  t@(TyUniverse _) -> t
  t@TyProp -> t

-- | Weak head normal form
--
-- Evaluate until the outermost constructor is exposed
whnf :: Type -> Type
whnf = evalType  -- For now, full evaluation is sufficient

-- | Full normalization
--
-- Recursively normalize all subterms
normalize :: Type -> Type
normalize = evalType  -- evalType already recurses

--------------------------------------------------------------------------------
-- Purity Checking
--------------------------------------------------------------------------------

-- | Check if a type expression is pure (no effects)
--
-- Type-level expressions must be pure because they are evaluated
-- during type checking, not at runtime.
isPure :: Type -> Bool
isPure = \case
  TyVar _ _ -> True
  TyNatLit _ -> True
  TyUniverse _ -> True
  TyProp -> True
  TyCon _ args -> all isPure args
  TyPi _ paramTy _ retTy -> isPure paramTy && isPure retTy
  TySigma _ fstTy sndTy -> isPure fstTy && isPure sndTy
  TyForall _ _ body -> isPure body
  TyForallDep _ paramTy body -> isPure paramTy && isPure body
  TyLazy inner -> isPure inner
  TyLinear inner -> isPure inner
  TyRef inner -> isPure inner
  TyRefMut inner -> isPure inner
  TyAdd t1 t2 -> isPure t1 && isPure t2
  TyEffect _ _ -> False  -- Effectful expressions are impure

--------------------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------------------

-- | Unify two Pi types
--
-- Pi types unify if their domains and codomains unify
-- (modulo alpha-equivalence of the bound variable)
unifyPiTypes :: Type -> Type -> Either DependentTypeError ()
unifyPiTypes (TyPi _ dom1 eff1 cod1) (TyPi _ dom2 eff2 cod2) = do
  unifyTypes dom1 dom2
  unifyEffects eff1 eff2
  unifyTypes cod1 cod2
unifyPiTypes _ _ = Left $ UnificationError "Not Pi types"

-- | Unify two Sigma types
unifySigmaTypes :: Type -> Type -> Either DependentTypeError ()
unifySigmaTypes (TySigma _ fst1 snd1) (TySigma _ fst2 snd2) = do
  unifyTypes fst1 fst2
  unifyTypes snd1 snd2
unifySigmaTypes _ _ = Left $ UnificationError "Not Sigma types"

-- | General type unification
unifyTypes :: Type -> Type -> Either DependentTypeError ()
unifyTypes t1 t2 =
  let n1 = normalize t1
      n2 = normalize t2
  in if typeStructuralEq n1 n2
     then Right ()
     else Left $ UnificationError $ "Cannot unify " <> T.pack (show n1) <> " with " <> T.pack (show n2)

-- | Unify effect rows
unifyEffects :: EffectRow -> EffectRow -> Either DependentTypeError ()
unifyEffects EffEmpty EffEmpty = Right ()
unifyEffects (EffVar _ i1) (EffVar _ i2) | i1 == i2 = Right ()
unifyEffects (EffSet e1) (EffSet e2) =
  if length e1 == length e2 && all (`elemEffect` e2) e1
  then Right ()
  else Left $ UnificationError "Effect mismatch"
unifyEffects _ _ = Left $ UnificationError "Effect unification failed"

elemEffect :: Effect -> [Effect] -> Bool
elemEffect e = any (\e' -> effectName e == effectName e')

--------------------------------------------------------------------------------
-- Desugaring
--------------------------------------------------------------------------------

-- | Desugar forall to Pi type
--
-- @forall a:K. T@ becomes @(a: Type) -> T@ with the kind encoded
desugarForall :: Type -> Type
desugarForall (TyForall name kind body) =
  let domainTy = kindToType kind
  in TyPi name domainTy EffEmpty body
desugarForall t = t

-- | Convert kind to type (for forall desugaring)
kindToType :: Kind -> Type
kindToType (KiType level) = TyUniverse level
kindToType KiProp = TyProp
kindToType KiLinear = TyCon "Linear" []
kindToType (KiArrow k1 k2) = TyPi "_" (kindToType k1) EffEmpty (kindToType k2)

--------------------------------------------------------------------------------
-- Pattern Refinement
--------------------------------------------------------------------------------

-- | Refine types based on pattern matching
--
-- When matching against a constructor pattern, we learn information
-- about the types of subpatterns.
refineTypeInPattern :: Context -> Type -> Pattern -> IO Context
refineTypeInPattern ctx scrTy pat = case pat of
  PatWild -> pure ctx
  PatVar name -> pure $ extendTerm name scrTy ctx
  PatCon conName subPats -> do
    -- Look up constructor and refine based on it
    case lookupConstructor conName ctx of
      Just (_, conInfo) -> do
        -- Bind subpatterns to their types
        let refinedTys = refineConstructorTypes scrTy conInfo
            bindings = zip subPats refinedTys
        pure $ foldr (\(p, t) c -> extendTerm (patternVar p) t c) ctx bindings
      Nothing -> pure ctx

-- | Get variable name from pattern (or generate one)
patternVar :: Pattern -> Text
patternVar (PatVar name) = name
patternVar PatWild = "_"
patternVar (PatCon _ _) = "_"

-- | Refine constructor argument types based on scrutinee type
refineConstructorTypes :: Type -> ConstructorInfo -> [Type]
refineConstructorTypes scrTy conInfo =
  -- For Vec(A, S(n)), Cons constructor gives head: A, tail: Vec(A, n)
  case scrTy of
    TyCon "Vec" [elemTy, TyNatLit n] | n > 0 ->
      [elemTy, TyCon "Vec" [elemTy, TyNatLit (n - 1)]]
    _ -> constructorParamTypes conInfo

--------------------------------------------------------------------------------
-- Type Equivalence
--------------------------------------------------------------------------------

-- | Check type equivalence (modulo evaluation)
typeEquiv :: Type -> Type -> Bool
typeEquiv t1 t2 = typeStructuralEq (normalize t1) (normalize t2)

-- | Structural equality of types
typeStructuralEq :: Type -> Type -> Bool
typeStructuralEq t1 t2 = case (t1, t2) of
  (TyVar _ i1, TyVar _ i2) -> i1 == i2
  (TyNatLit n, TyNatLit m) -> n == m
  (TyUniverse l1, TyUniverse l2) -> l1 == l2
  (TyProp, TyProp) -> True
  (TyCon n1 args1, TyCon n2 args2) ->
    n1 == n2 && length args1 == length args2 && and (zipWith typeStructuralEq args1 args2)
  (TyPi _ dom1 eff1 cod1, TyPi _ dom2 eff2 cod2) ->
    typeStructuralEq dom1 dom2 && effectsEq eff1 eff2 && typeStructuralEq cod1 cod2
  (TySigma _ fst1 snd1, TySigma _ fst2 snd2) ->
    typeStructuralEq fst1 fst2 && typeStructuralEq snd1 snd2
  (TyForall _ k1 b1, TyForall _ k2 b2) ->
    kindsEq k1 k2 && typeStructuralEq b1 b2
  (TyForallDep _ p1 b1, TyForallDep _ p2 b2) ->
    typeStructuralEq p1 p2 && typeStructuralEq b1 b2
  (TyLazy i1, TyLazy i2) -> typeStructuralEq i1 i2
  (TyLinear i1, TyLinear i2) -> typeStructuralEq i1 i2
  (TyRef i1, TyRef i2) -> typeStructuralEq i1 i2
  (TyRefMut i1, TyRefMut i2) -> typeStructuralEq i1 i2
  (TyAdd a1 b1, TyAdd a2 b2) -> typeStructuralEq a1 a2 && typeStructuralEq b1 b2
  _ -> False

-- | Effect row equality
effectsEq :: EffectRow -> EffectRow -> Bool
effectsEq EffEmpty EffEmpty = True
effectsEq (EffVar _ i1) (EffVar _ i2) = i1 == i2
effectsEq (EffSet e1) (EffSet e2) =
  length e1 == length e2 && all (`elemEffect` e2) e1
effectsEq _ _ = False

-- | Kind equality
kindsEq :: Kind -> Kind -> Bool
kindsEq (KiType l1) (KiType l2) = l1 == l2
kindsEq KiProp KiProp = True
kindsEq KiLinear KiLinear = True
kindsEq (KiArrow f1 t1) (KiArrow f2 t2) = kindsEq f1 f2 && kindsEq t1 t2
kindsEq _ _ = False

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Check type well-formedness
checkTypeWellFormed :: Context -> Type -> Either DependentTypeError ()
checkTypeWellFormed ctx = \case
  TyVar name _ ->
    case lookupTypeVar name ctx of
      Just _ -> Right ()
      Nothing -> Left $ UnboundTypeVariable name
  TyCon _ args -> mapM_ (checkTypeWellFormed ctx) args
  TyPi name paramTy _ retTy -> do
    checkTypeWellFormed ctx paramTy
    checkTypeWellFormed (extendWithType name paramTy ctx) retTy
  TySigma name fstTy sndTy -> do
    checkTypeWellFormed ctx fstTy
    checkTypeWellFormed (extendWithType name fstTy ctx) sndTy
  TyForall name kind body ->
    checkTypeWellFormed (extendType name kind ctx) body
  TyForallDep name paramTy body -> do
    checkTypeWellFormed ctx paramTy
    checkTypeWellFormed (extendWithType name paramTy ctx) body
  TyLazy inner -> checkTypeWellFormed ctx inner
  TyLinear inner -> checkTypeWellFormed ctx inner
  TyRef inner -> checkTypeWellFormed ctx inner
  TyRefMut inner -> checkTypeWellFormed ctx inner
  TyAdd t1 t2 -> checkTypeWellFormed ctx t1 >> checkTypeWellFormed ctx t2
  TyNatLit _ -> Right ()
  TyUniverse _ -> Right ()
  TyProp -> Right ()

-- | Get the kind of a type
getTypeKind :: Context -> Type -> Either DependentTypeError Kind
getTypeKind ctx = \case
  TyVar name _ ->
    case lookupTypeVar name ctx of
      Just (kind, _) -> Right kind
      Nothing -> Left $ UnboundTypeVariable name
  TyCon name _ ->
    case lookupType name ctx of
      Just info -> Right (typeInfoKind info)
      Nothing -> Right (KiType 0)  -- Default for unknown types
  TyPi {} -> Right (KiType 0)
  TySigma {} -> Right (KiType 0)
  TyForall {} -> Right (KiType 0)
  TyForallDep {} -> Right (KiType 0)
  TyLazy _ -> Right (KiType 0)
  TyLinear _ -> Right KiLinear
  TyRef _ -> Right (KiType 0)
  TyRefMut _ -> Right (KiType 0)
  TyNatLit _ -> Right (KiType 0)
  TyAdd _ _ -> Right (KiType 0)
  TyUniverse n -> Right (KiType (n + 1))
  TyProp -> Right KiProp

-- | Extend context with a type binding (for dependent types)
extendWithType :: Text -> Type -> Context -> Context
extendWithType name ty ctx =
  let kind = case getTypeKind ctx ty of
               Right k -> k
               Left _ -> KiType 0
  in extendType name kind ctx

-- | Type substitution
substituteType :: Type -> Int -> Type -> Type
substituteType ty idx replacement = case ty of
  TyVar name i
    | i == idx  -> replacement
    | i > idx   -> TyVar name (i - 1)  -- Adjust for removed binding
    | otherwise -> TyVar name i
  TyCon name args -> TyCon name (map (\t -> substituteType t idx replacement) args)
  TyPi pn pt eff rt -> TyPi pn
    (substituteType pt idx replacement)
    eff
    (substituteType rt (idx + 1) (shiftType replacement 1))
  TySigma name fstTy sndTy -> TySigma name
    (substituteType fstTy idx replacement)
    (substituteType sndTy (idx + 1) (shiftType replacement 1))
  TyForall tv k body -> TyForall tv k
    (substituteType body (idx + 1) (shiftType replacement 1))
  TyForallDep pn pt body -> TyForallDep pn
    (substituteType pt idx replacement)
    (substituteType body (idx + 1) (shiftType replacement 1))
  TyLazy inner -> TyLazy (substituteType inner idx replacement)
  TyLinear inner -> TyLinear (substituteType inner idx replacement)
  TyRef inner -> TyRef (substituteType inner idx replacement)
  TyRefMut inner -> TyRefMut (substituteType inner idx replacement)
  TyAdd t1 t2 -> TyAdd (substituteType t1 idx replacement) (substituteType t2 idx replacement)
  TyNatLit n -> TyNatLit n
  TyUniverse l -> TyUniverse l
  TyProp -> TyProp

-- | Shift type indices
shiftType :: Type -> Int -> Type
shiftType ty amount = shiftTypeAbove ty 0 amount

shiftTypeAbove :: Type -> Int -> Int -> Type
shiftTypeAbove ty cutoff amount = case ty of
  TyVar name i
    | i >= cutoff -> TyVar name (i + amount)
    | otherwise   -> TyVar name i
  TyCon name args -> TyCon name (map (\t -> shiftTypeAbove t cutoff amount) args)
  TyPi pn pt eff rt -> TyPi pn
    (shiftTypeAbove pt cutoff amount)
    eff
    (shiftTypeAbove rt (cutoff + 1) amount)
  TySigma name fstTy sndTy -> TySigma name
    (shiftTypeAbove fstTy cutoff amount)
    (shiftTypeAbove sndTy (cutoff + 1) amount)
  TyForall tv k body -> TyForall tv k
    (shiftTypeAbove body (cutoff + 1) amount)
  TyForallDep pn pt body -> TyForallDep pn
    (shiftTypeAbove pt cutoff amount)
    (shiftTypeAbove body (cutoff + 1) amount)
  TyLazy inner -> TyLazy (shiftTypeAbove inner cutoff amount)
  TyLinear inner -> TyLinear (shiftTypeAbove inner cutoff amount)
  TyRef inner -> TyRef (shiftTypeAbove inner cutoff amount)
  TyRefMut inner -> TyRefMut (shiftTypeAbove inner cutoff amount)
  TyAdd t1 t2 -> TyAdd (shiftTypeAbove t1 cutoff amount) (shiftTypeAbove t2 cutoff amount)
  TyNatLit n -> TyNatLit n
  TyUniverse l -> TyUniverse l
  TyProp -> TyProp
