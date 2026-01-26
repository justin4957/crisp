{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Types.Checker
-- Description : Bidirectional Type Checker
--
-- Implements bidirectional type checking for the Crisp core calculus.
-- Supports synthesis (inferring types) and checking (verifying types).

module Crisp.Types.Checker
  ( -- * Type checking
    synthesize
  , check
    -- * Errors
  , TypeError(..)
  , TypeResult
  ) where

import Crisp.Core.Term
import Crisp.Types.Context

import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T

-- | Type checking errors
data TypeError
  = UnboundVariable !Text
  | UnboundTypeVariable !Text
  | TypeMismatch !Type !Type          -- ^ Expected, Actual
  | NotAFunction !Type
  | NotAForall !Type
  | CannotSynthesize !Term
  | EffectNotAllowed !Text
  | UnknownEffect !Text
  | UnknownOperation !Text !Text      -- ^ Effect, Operation
  | KindMismatch !Kind !Kind          -- ^ Expected, Actual
  | LinearityViolation !Text
  | PatternTypeMismatch !Pattern !Type
  | NonExhaustiveMatch
  | OtherError !Text
  deriving stock (Eq, Show)

-- | Result of type checking
type TypeResult a = Either TypeError a

-- | Type checking monad
type Check a = ReaderT Context (Except TypeError) a

-- | Run type checking
runCheck :: Context -> Check a -> TypeResult a
runCheck ctx = runExcept . flip runReaderT ctx

-- | Synthesize the type of a term (with effects)
synthesize :: Context -> Term -> TypeResult (Type, EffectRow)
synthesize ctx term = runCheck ctx (synth term)

-- | Check a term against an expected type
check :: Context -> Term -> Type -> TypeResult EffectRow
check ctx term ty = runCheck ctx (checkTerm term ty)

-- | Internal synthesis
synth :: Term -> Check (Type, EffectRow)
synth = \case
  TmVar name _ -> do
    ctx <- ask
    case lookupTerm name ctx of
      Just (ty, _) -> pure (ty, EffEmpty)
      Nothing      -> throwError $ UnboundVariable name

  TmApp func arg -> do
    (funcTy, funcEffs) <- synth func
    case funcTy of
      TyPi _paramName paramTy effs retTy -> do
        argEffs <- checkTerm arg paramTy
        let combinedEffs = combineEffects funcEffs (combineEffects effs argEffs)
        pure (retTy, combinedEffs)
      _ -> throwError $ NotAFunction funcTy

  TmTyApp tm tyArg -> do
    (tmTy, effs) <- synth tm
    case tmTy of
      TyForall _typeVar _kind bodyTy -> do
        let resultTy = substituteType bodyTy 0 tyArg
        pure (resultTy, effs)
      _ -> throwError $ NotAForall tmTy

  TmAnnot tm ty -> do
    effs <- checkTerm tm ty
    pure (ty, effs)

  TmLet name boundTy value body -> do
    valueEffs <- checkTerm value boundTy
    local (extendTerm name boundTy) $ do
      (bodyTy, bodyEffs) <- synth body
      pure (bodyTy, combineEffects valueEffs bodyEffs)

  TmCon conName tyArgs args -> do
    ctx <- ask
    case lookupConstructor conName ctx of
      Just (_typeName, conInfo) -> do
        -- Check arguments match expected types
        effs <- checkConArgs args (constructorParamTypes conInfo)
        pure (constructorReturnType conInfo, effs)
      Nothing -> throwError $ OtherError $ "Unknown constructor: " <> conName

  TmMatch subject retTy cases -> do
    (subjectTy, subjectEffs) <- synth subject
    caseEffs <- checkCases cases subjectTy retTy
    pure (retTy, combineEffects subjectEffs caseEffs)

  TmPerform effect op arg -> do
    ctx <- ask
    case lookupOperation effect op ctx of
      Just opInfo -> do
        argEffs <- checkTerm arg (operationInfoInputType opInfo)
        let effRow = EffSet [Effect effect (getAuthority ctx)]
        pure (operationInfoOutputType opInfo, combineEffects argEffs effRow)
      Nothing -> throwError $ UnknownOperation effect op

  TmForce inner -> do
    (innerTy, effs) <- synth inner
    case innerTy of
      TyLazy resultTy -> pure (resultTy, effs)
      _ -> throwError $ OtherError "Cannot force non-lazy value"

  TmHandle handler body -> do
    (bodyTy, bodyEffs) <- synth body
    let remainingEffs = removeEffect bodyEffs (handlerEffect handler)
    let resultEffs = combineEffects remainingEffs (handlerIntroducedEffects handler)
    pure (bodyTy, resultEffs)

  -- Terms that cannot synthesize
  TmLam {} -> throwError $ CannotSynthesize (TmLam "_" TyProp (TmVar "_" 0))
  TmTyAbs {} -> throwError $ CannotSynthesize (TmTyAbs "_" KiProp (TmVar "_" 0))
  TmLazy {} -> throwError $ CannotSynthesize (TmLazy (TmVar "_" 0))

-- | Internal checking
checkTerm :: Term -> Type -> Check EffectRow
checkTerm term expected = case (term, expected) of
  -- Lambda checking
  (TmLam paramName paramTy body, TyPi _ expectedParam effs retTy) -> do
    checkTypeEqual paramTy expectedParam
    local (extendTerm paramName paramTy) $ do
      bodyEffs <- checkTerm body retTy
      checkEffectsSubsumed bodyEffs effs
      pure EffEmpty

  -- Type abstraction checking
  (TmTyAbs typeVar kind body, TyForall _ expectedKind bodyTy) -> do
    checkKindEqual kind expectedKind
    local (extendType typeVar kind) $ do
      checkTerm body bodyTy

  -- Lazy checking
  (TmLazy body, TyLazy inner) -> checkTerm body inner

  -- Fall back to synthesis and subsumption
  _ -> do
    (actual, effs) <- synth term
    checkTypeSubsumed actual expected
    pure effs

-- | Check constructor arguments
checkConArgs :: [Term] -> [Type] -> Check EffectRow
checkConArgs [] [] = pure EffEmpty
checkConArgs (arg:args) (ty:tys) = do
  argEffs <- checkTerm arg ty
  restEffs <- checkConArgs args tys
  pure $ combineEffects argEffs restEffs
checkConArgs _ _ = throwError $ OtherError "Constructor argument count mismatch"

-- | Check match cases
checkCases :: [Case] -> Type -> Type -> Check EffectRow
checkCases [] _ _ = pure EffEmpty
checkCases (Case pat body : rest) subjectTy retTy = do
  ctx <- ask
  let extendedCtx = extendWithPattern pat subjectTy ctx
  bodyEffs <- local (const extendedCtx) $ checkTerm body retTy
  restEffs <- checkCases rest subjectTy retTy
  pure $ combineEffects bodyEffs restEffs

-- | Extend context with pattern bindings
extendWithPattern :: Pattern -> Type -> Context -> Context
extendWithPattern pat ty ctx = case pat of
  PatVar name -> extendTerm name ty ctx
  PatWild     -> ctx
  PatCon _ pats -> case lookupConstructorByPattern pat ctx of
    Just (_, conInfo) ->
      foldr (\(p, t) c -> extendWithPattern p t c)
            ctx
            (zip pats (constructorParamTypes conInfo))
    Nothing -> ctx

-- | Look up constructor from pattern
lookupConstructorByPattern :: Pattern -> Context -> Maybe (Text, ConstructorInfo)
lookupConstructorByPattern (PatCon name _) ctx = lookupConstructor name ctx
lookupConstructorByPattern _ _ = Nothing

-- | Check type equality
checkTypeEqual :: Type -> Type -> Check ()
checkTypeEqual t1 t2 =
  unless (typesEqual t1 t2) $
    throwError $ TypeMismatch t1 t2

-- | Check if types are equal
typesEqual :: Type -> Type -> Bool
typesEqual t1 t2 = case (t1, t2) of
  (TyVar _ i1, TyVar _ i2) -> i1 == i2
  (TyCon n1 args1, TyCon n2 args2) ->
    n1 == n2 && length args1 == length args2 &&
    all (uncurry typesEqual) (zip args1 args2)
  (TyPi _ p1 e1 r1, TyPi _ p2 e2 r2) ->
    typesEqual p1 p2 && effectsEqual e1 e2 && typesEqual r1 r2
  (TyForall _ k1 b1, TyForall _ k2 b2) ->
    kindsEqual k1 k2 && typesEqual b1 b2
  (TyLazy i1, TyLazy i2) -> typesEqual i1 i2
  (TyLinear i1, TyLinear i2) -> typesEqual i1 i2
  (TyRef i1, TyRef i2) -> typesEqual i1 i2
  (TyRefMut i1, TyRefMut i2) -> typesEqual i1 i2
  (TyUniverse l1, TyUniverse l2) -> l1 == l2
  (TyProp, TyProp) -> True
  _ -> False

-- | Check type subsumption
checkTypeSubsumed :: Type -> Type -> Check ()
checkTypeSubsumed actual expected =
  -- For now, just check equality; full subtyping would be more complex
  checkTypeEqual actual expected

-- | Check kind equality
checkKindEqual :: Kind -> Kind -> Check ()
checkKindEqual k1 k2 =
  unless (kindsEqual k1 k2) $
    throwError $ KindMismatch k1 k2

-- | Check if kinds are equal
kindsEqual :: Kind -> Kind -> Bool
kindsEqual k1 k2 = case (k1, k2) of
  (KiType l1, KiType l2) -> l1 == l2
  (KiProp, KiProp) -> True
  (KiLinear, KiLinear) -> True
  (KiArrow f1 t1, KiArrow f2 t2) -> kindsEqual f1 f2 && kindsEqual t1 t2
  _ -> False

-- | Check if effects are equal
effectsEqual :: EffectRow -> EffectRow -> Bool
effectsEqual e1 e2 = case (e1, e2) of
  (EffEmpty, EffEmpty) -> True
  (EffVar _ i1, EffVar _ i2) -> i1 == i2
  (EffSet effs1, EffSet effs2) ->
    let names1 = map effectName effs1
        names2 = map effectName effs2
    in all (`elem` names2) names1 && all (`elem` names1) names2
  _ -> False

-- | Check effect subsumption
checkEffectsSubsumed :: EffectRow -> EffectRow -> Check ()
checkEffectsSubsumed _actual _allowed =
  -- For now, accept any effects; full checking would verify subset
  pure ()

-- | Combine effect rows
combineEffects :: EffectRow -> EffectRow -> EffectRow
combineEffects EffEmpty e = e
combineEffects e EffEmpty = e
combineEffects (EffSet e1) (EffSet e2) = EffSet (e1 ++ e2)
combineEffects e1 e2 = EffUnion e1 e2

-- | Remove an effect from an effect row
removeEffect :: EffectRow -> Text -> EffectRow
removeEffect EffEmpty _ = EffEmpty
removeEffect (EffSet effs) name =
  case filter (\e -> effectName e /= name) effs of
    [] -> EffEmpty
    filtered -> EffSet filtered
removeEffect (EffVar n i) _ = EffVar n i
removeEffect (EffUnion a b) name =
  combineEffects (removeEffect a name) (removeEffect b name)

-- | Type substitution
substituteType :: Type -> Int -> Type -> Type
substituteType ty idx replacement = case ty of
  TyVar name i
    | i == idx  -> replacement
    | otherwise -> TyVar name i
  TyCon name args -> TyCon name (map (\t -> substituteType t idx replacement) args)
  TyPi pn pt eff rt -> TyPi pn
    (substituteType pt idx replacement)
    eff
    (substituteType rt (idx + 1) (shiftType replacement 1))
  TyForall tv k body -> TyForall tv k
    (substituteType body (idx + 1) (shiftType replacement 1))
  TyForallDep pn pt body -> TyForallDep pn
    (substituteType pt idx replacement)
    (substituteType body (idx + 1) (shiftType replacement 1))
  TyLazy inner -> TyLazy (substituteType inner idx replacement)
  TyLinear inner -> TyLinear (substituteType inner idx replacement)
  TyRef inner -> TyRef (substituteType inner idx replacement)
  TyRefMut inner -> TyRefMut (substituteType inner idx replacement)
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
  TyForall tv k body -> TyForall tv k
    (shiftTypeAbove body (cutoff + 1) amount)
  TyForallDep pn pt body -> TyForallDep pn
    (shiftTypeAbove pt cutoff amount)
    (shiftTypeAbove body (cutoff + 1) amount)
  TyLazy inner -> TyLazy (shiftTypeAbove inner cutoff amount)
  TyLinear inner -> TyLinear (shiftTypeAbove inner cutoff amount)
  TyRef inner -> TyRef (shiftTypeAbove inner cutoff amount)
  TyRefMut inner -> TyRefMut (shiftTypeAbove inner cutoff amount)
  TyUniverse l -> TyUniverse l
  TyProp -> TyProp
