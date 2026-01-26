{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Types.Prop
-- Description : Prop universe and proof erasure
--
-- Implements the Prop universe for computationally irrelevant proofs:
-- - Proof irrelevance detection
-- - Proof term type checking
-- - Proof erasure before code generation
--
-- Crisp includes a Prop universe for proofs that:
-- 1. Are checked at compile time
-- 2. Do not exist at runtime (erased)
-- 3. Cannot influence computation (proof irrelevance)
--
-- This enables dependent types with proofs that don't incur runtime overhead.
--
-- Example:
-- @
-- -- Prop types (erased at runtime)
-- type Eq(A: Type, x: A, y: A) : Prop = Refl : Eq(A, x, x)
--
-- -- Proofs in types but not in values
-- fn safe_head(xs: Vec(A, n), proof: n > 0) -> A = ...
-- @

module Crisp.Types.Prop
  ( -- * Kind Operations
    kindOfProp
  , kindsCompatible
  , isPropKind
    -- * Type Operations
  , isPropType
  , isPropTypeName
  , hasProofInTypeArgs
    -- * Proof Irrelevance
  , isProofIrrelevant
  , filterRelevantArgs
  , filterRelevantParams
  , proofPositions
  , hasProofArguments
    -- * Proof Erasure
  , eraseProofTerm
  , eraseProofLet
  , eraseProofArgs
  , eraseAllProofs
  , eraseProofFromCase
    -- * Relevance Checking
  , checkRelevanceBoundary
  , canMatchOnProof
  , canInspectProofValue
  , isProofUsedForRefinement
    -- * Dependent Type Operations
  , hasPropParameter
  , countProofParameters
  , hasPropInSigma
  , forallHasPropBody
  , hasNestedProof
    -- * Errors
  , PropError(..)
  ) where

import Crisp.Core.Term

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (findIndices)

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

-- | Errors related to Prop universe operations
data PropError
  = ProofEscapesToComputation !Text
    -- ^ A proof value is used in computation
  | TypeDependsOnProof !Text
    -- ^ A Type depends on a Prop value
  | InvalidProofMatch !Text
    -- ^ Invalid pattern match on proof
  | ProofIrrelevanceViolation !Text
    -- ^ Proof irrelevance violated
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Kind Operations
--------------------------------------------------------------------------------

-- | Get the kind of a type, handling Prop specially
kindOfProp :: Type -> Either PropError Kind
kindOfProp = \case
  TyProp -> Right KiProp
  TyUniverse n -> Right (KiType (n + 1))
  TyVar _ _ -> Right (KiType 0)  -- Simplified
  TyCon name _ ->
    if isPropTypeName name
    then Right KiProp
    else Right (KiType 0)
  TyPi _ _ _ _ -> Right (KiType 0)
  TyForall _ _ _ -> Right (KiType 0)
  TyForallDep _ _ _ -> Right (KiType 0)
  TySigma _ _ _ -> Right (KiType 0)
  TyLazy t -> kindOfProp t
  TyLinear t -> kindOfProp t
  TyRef t -> kindOfProp t
  TyRefMut t -> kindOfProp t
  TyNatLit _ -> Right (KiType 0)
  TyAdd _ _ -> Right (KiType 0)
  TyEffect _ _ -> Right (KiType 0)

-- | Check if two kinds are compatible
kindsCompatible :: Kind -> Kind -> Bool
kindsCompatible k1 k2 = case (k1, k2) of
  (KiProp, KiProp) -> True
  (KiType n1, KiType n2) -> n1 == n2
  (KiLinear, KiLinear) -> True
  (KiArrow a1 b1, KiArrow a2 b2) -> kindsCompatible a1 a2 && kindsCompatible b1 b2
  _ -> False

-- | Check if a kind is Prop
isPropKind :: Kind -> Bool
isPropKind KiProp = True
isPropKind _ = False

--------------------------------------------------------------------------------
-- Type Operations
--------------------------------------------------------------------------------

-- | Check if a type is TyProp
isPropType :: Type -> Bool
isPropType TyProp = True
isPropType (TyCon name _) = isPropTypeName name
isPropType _ = False

-- | Check if a type constructor name is a known proof type
--
-- Standard proof types in Crisp include equality, ordering, and logical connectives.
isPropTypeName :: Text -> Bool
isPropTypeName name = name `elem` propTypeNames
  where
    propTypeNames =
      [ "Eq"      -- Equality proof
      , "Lt"      -- Less than
      , "Le"      -- Less than or equal
      , "Gt"      -- Greater than
      , "Ge"      -- Greater than or equal
      , "And"     -- Logical and
      , "Or"      -- Logical or
      , "Not"     -- Logical not
      , "True"    -- True proposition
      , "False"   -- False proposition
      , "Void"    -- Empty type (no inhabitants)
      , "Prop"    -- Prop itself
      ]

-- | Check if a type has Prop in its type arguments
hasProofInTypeArgs :: Type -> Bool
hasProofInTypeArgs = \case
  TyCon _ args -> any isPropType args || any hasProofInTypeArgs args
  TyPi _ paramTy _ retTy -> isPropType paramTy || hasProofInTypeArgs paramTy || hasProofInTypeArgs retTy
  TyForall _ _ body -> hasProofInTypeArgs body
  TyForallDep _ paramTy body -> hasProofInTypeArgs paramTy || hasProofInTypeArgs body
  TySigma _ fstTy sndTy -> isPropType fstTy || isPropType sndTy || hasProofInTypeArgs fstTy || hasProofInTypeArgs sndTy
  TyLazy t -> hasProofInTypeArgs t
  TyLinear t -> hasProofInTypeArgs t
  TyRef t -> hasProofInTypeArgs t
  TyRefMut t -> hasProofInTypeArgs t
  TyAdd t1 t2 -> hasProofInTypeArgs t1 || hasProofInTypeArgs t2
  _ -> False

--------------------------------------------------------------------------------
-- Proof Irrelevance
--------------------------------------------------------------------------------

-- | Check if a type is proof-irrelevant (i.e., is a Prop type)
isProofIrrelevant :: Type -> Bool
isProofIrrelevant = isPropType

-- | Filter out proof-irrelevant arguments from a list of types
filterRelevantArgs :: [Type] -> [Type]
filterRelevantArgs = filter (not . isProofIrrelevant)

-- | Filter out proof-irrelevant parameters from a list of (name, type) pairs
filterRelevantParams :: [(Text, Type)] -> [(Text, Type)]
filterRelevantParams = filter (not . isProofIrrelevant . snd)

-- | Get the indices of proof arguments in a parameter list
proofPositions :: [Type] -> [Int]
proofPositions = findIndices isProofIrrelevant

-- | Check if a list of types contains any proof arguments
hasProofArguments :: [Type] -> Bool
hasProofArguments = any isProofIrrelevant

--------------------------------------------------------------------------------
-- Proof Erasure
--------------------------------------------------------------------------------

-- | Erase a proof term to unit
--
-- If the term has type Prop, it is erased to the unit value.
-- Otherwise, the term is returned unchanged.
eraseProofTerm :: Term -> Type -> Term
eraseProofTerm term ty
  | isProofIrrelevant ty = TmCon "Unit" [] []
  | otherwise = term

-- | Erase proof let bindings
--
-- If a let binding has type Prop, the binding is removed and
-- the body is returned (with adjusted indices).
eraseProofLet :: Term -> Term
eraseProofLet term = case term of
  TmLet _ ty _ body | isProofIrrelevant ty ->
    -- Remove the let binding, shift indices in body
    shiftTerm (-1) 0 body
  _ -> term

-- | Erase proof arguments from a list of arguments
--
-- Given the parameter types and argument terms, removes arguments
-- whose types are Prop.
eraseProofArgs :: [Type] -> [Term] -> [Term]
eraseProofArgs types args =
  [ arg | (ty, arg) <- zip types args, not (isProofIrrelevant ty) ]

-- | Erase all proof bindings from a term
--
-- This performs a full traversal, removing all proof let bindings
-- and adjusting de Bruijn indices accordingly.
eraseAllProofs :: Term -> Term
eraseAllProofs = go 0
  where
    go depth term = case term of
      TmVar name idx
        | idx >= depth -> TmVar name idx
        | otherwise -> TmVar name idx

      TmLam name ty body ->
        if isProofIrrelevant ty
        then go depth (shiftTerm (-1) 0 body)  -- Skip proof parameter
        else TmLam name ty (go (depth + 1) body)

      TmApp fn arg ->
        TmApp (go depth fn) (go depth arg)

      TmLet name ty val body ->
        if isProofIrrelevant ty
        then go depth (shiftTerm (-1) 0 body)  -- Skip proof binding
        else TmLet name ty (go depth val) (go (depth + 1) body)

      TmTyAbs name kind body ->
        TmTyAbs name kind (go depth body)

      TmTyApp fn tyArg ->
        TmTyApp (go depth fn) tyArg

      TmCon name tyArgs args ->
        TmCon name tyArgs (map (go depth) args)

      TmMatch scrutinee retTy cases ->
        TmMatch (go depth scrutinee) retTy (map (goCase depth) cases)

      TmPerform eff op arg ->
        TmPerform eff op (go depth arg)

      TmHandle handler body ->
        TmHandle (goHandler depth handler) (go depth body)

      TmLazy inner ->
        TmLazy (go depth inner)

      TmForce inner ->
        TmForce (go depth inner)

      TmAnnot inner ty ->
        TmAnnot (go depth inner) ty

    goCase depth (Case pat body) =
      let patDepth = patternBindingCount pat
      in Case pat (go (depth + patDepth) body)

    goHandler depth (Handler eff introduced ops ret) =
      Handler eff introduced (map (goOpHandler depth) ops) (goRetHandler depth ret)

    goOpHandler depth (OpHandler op pat resume body) =
      let patDepth = patternBindingCount pat + 1  -- +1 for resume
      in OpHandler op pat resume (go (depth + patDepth) body)

    goRetHandler depth (ReturnHandler pat body) =
      let patDepth = patternBindingCount pat
      in ReturnHandler pat (go (depth + patDepth) body)

-- | Erase proof from a case branch
--
-- Removes proof pattern variables from constructor patterns.
eraseProofFromCase :: [Type] -> Case -> Case
eraseProofFromCase paramTypes (Case pat body) =
  Case (eraseProofFromPattern paramTypes pat) body

-- | Erase proof from a pattern
eraseProofFromPattern :: [Type] -> Pattern -> Pattern
eraseProofFromPattern paramTypes pat = case pat of
  PatCon name subPats ->
    let relevantSubPats = [ p | (ty, p) <- zip paramTypes subPats, not (isProofIrrelevant ty) ]
    in PatCon name relevantSubPats
  _ -> pat

-- | Count bindings introduced by a pattern
patternBindingCount :: Pattern -> Int
patternBindingCount = \case
  PatVar _ -> 1
  PatWild -> 0
  PatCon _ pats -> sum (map patternBindingCount pats)

-- | Shift term indices
shiftTerm :: Int -> Int -> Term -> Term
shiftTerm amount cutoff = go
  where
    go term = case term of
      TmVar name idx
        | idx >= cutoff -> TmVar name (idx + amount)
        | otherwise -> TmVar name idx

      TmLam name ty body ->
        TmLam name ty (shiftTerm amount (cutoff + 1) body)

      TmApp fn arg ->
        TmApp (go fn) (go arg)

      TmLet name ty val body ->
        TmLet name ty (go val) (shiftTerm amount (cutoff + 1) body)

      TmTyAbs name kind body ->
        TmTyAbs name kind (go body)

      TmTyApp fn tyArg ->
        TmTyApp (go fn) tyArg

      TmCon name tyArgs args ->
        TmCon name tyArgs (map go args)

      TmMatch scrutinee retTy cases ->
        TmMatch (go scrutinee) retTy (map goCase cases)

      TmPerform eff op arg ->
        TmPerform eff op (go arg)

      TmHandle handler body ->
        TmHandle (goHandler handler) (go body)

      TmLazy inner ->
        TmLazy (go inner)

      TmForce inner ->
        TmForce (go inner)

      TmAnnot inner ty ->
        TmAnnot (go inner) ty

    goCase (Case pat body) =
      let patDepth = patternBindingCount pat
      in Case pat (shiftTerm amount (cutoff + patDepth) body)

    goHandler (Handler eff introduced ops ret) =
      Handler eff introduced (map goOpHandler ops) (goRetHandler ret)

    goOpHandler (OpHandler op pat resume body) =
      let patDepth = patternBindingCount pat + 1
      in OpHandler op pat resume (shiftTerm amount (cutoff + patDepth) body)

    goRetHandler (ReturnHandler pat body) =
      let patDepth = patternBindingCount pat
      in ReturnHandler pat (shiftTerm amount (cutoff + patDepth) body)

--------------------------------------------------------------------------------
-- Relevance Checking
--------------------------------------------------------------------------------

-- | Check relevance boundary: Type cannot depend on Prop values
--
-- Returns an error if a Type depends on a Prop value.
checkRelevanceBoundary :: Type -> Bool -> Either PropError ()
checkRelevanceBoundary ty typeDepOnProof
  | typeDepOnProof = Left $ TypeDependsOnProof "Type cannot depend on Prop value"
  | otherwise = Right ()

-- | Check if we can pattern match on a proof
--
-- Pattern matching on proofs is only allowed for wildcard patterns
-- (which don't inspect the proof value).
canMatchOnProof :: Type -> Pattern -> Bool
canMatchOnProof ty pat
  | not (isProofIrrelevant ty) = True  -- Non-proof, can always match
  | otherwise = case pat of
      PatWild -> True   -- Wildcard doesn't inspect
      PatVar _ -> True  -- Variable binding doesn't inspect
      PatCon _ _ -> False  -- Constructor matching inspects the proof!

-- | Check if we can inspect a proof value
--
-- Proof values cannot be inspected at runtime because they are erased.
canInspectProofValue :: Type -> Bool
canInspectProofValue ty = not (isProofIrrelevant ty)

-- | Check if a proof type is used for type refinement
--
-- Equality proofs (Eq) are used to refine types, not for computation.
isProofUsedForRefinement :: Type -> Bool
isProofUsedForRefinement = \case
  TyCon "Eq" _ -> True
  TyCon "Lt" _ -> True
  TyCon "Le" _ -> True
  TyCon "Gt" _ -> True
  TyCon "Ge" _ -> True
  _ -> False

--------------------------------------------------------------------------------
-- Dependent Type Operations
--------------------------------------------------------------------------------

-- | Check if a Pi type has a Prop parameter
hasPropParameter :: Type -> Bool
hasPropParameter = \case
  TyPi _ paramTy _ _ -> isProofIrrelevant paramTy
  _ -> False

-- | Count proof parameters in a (possibly nested) Pi type
countProofParameters :: Type -> Int
countProofParameters = \case
  TyPi _ paramTy _ retTy ->
    (if isProofIrrelevant paramTy then 1 else 0) + countProofParameters retTy
  _ -> 0

-- | Check if a Sigma type has Prop in second component
hasPropInSigma :: Type -> Bool
hasPropInSigma = \case
  TySigma _ _ sndTy -> isProofIrrelevant sndTy
  _ -> False

-- | Check if a forall has Prop as body
forallHasPropBody :: Type -> Bool
forallHasPropBody = \case
  TyForall _ _ body -> isProofIrrelevant body
  TyForallDep _ _ body -> isProofIrrelevant body
  _ -> False

-- | Check if type has nested proofs
hasNestedProof :: Type -> Bool
hasNestedProof = \case
  TyPi _ paramTy _ retTy ->
    isProofIrrelevant paramTy || hasProofInTypeArgs paramTy || hasNestedProof retTy
  TyForall _ _ body -> hasNestedProof body
  TyForallDep _ paramTy body -> hasProofInTypeArgs paramTy || hasNestedProof body
  TySigma _ fstTy sndTy -> hasProofInTypeArgs fstTy || hasProofInTypeArgs sndTy
  TyCon _ args -> any hasProofInTypeArgs args
  _ -> False
