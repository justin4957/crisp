{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Types.KindChecker
-- Description : Kind checking for Crisp's type system
--
-- Implements kind checking ensuring that types are well-formed and type
-- constructors are applied with the correct number and kinds of arguments.
--
-- Kinds classify types:
-- - Type (KiType 0) is the kind of ordinary types (Int, Bool, List(Int))
-- - Type -> Type is the kind of type constructors (List, Option)
-- - Prop (KiProp) is the kind of proof types
-- - Linear (KiLinear) is the kind of linear types

module Crisp.Types.KindChecker
  ( -- * Kind checking
    kindOf
  , checkKind
  , checkEffectRow
    -- * Kind environment
  , KindEnv
  , defaultKindEnv
  , emptyKindEnv
  , extendKindEnv
  , extendEffectVar
  , lookupKindEnv
    -- * Errors
  , KindError(..)
  ) where

import Crisp.Core.Term

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Errors that can occur during kind checking
data KindError
  = UnboundTypeVar !Text
    -- ^ Reference to unbound type variable
  | UnboundTypeCon !Text
    -- ^ Reference to unbound type constructor
  | KindMismatch !Kind !Kind
    -- ^ Expected kind, got kind
  | KindArityMismatch !Text !Int !Int
    -- ^ Type constructor, expected args, actual args
  | NotATypeConstructor !Text
    -- ^ Tried to apply a non-constructor type
  | IllFormedEffectRow !Text
    -- ^ Effect row is malformed
  | OtherKindError !Text
    -- ^ Other kind error
  deriving stock (Eq, Show)

-- | Kind environment mapping type names to kinds
data KindEnv = KindEnv
  { kindEnvTypes   :: !(Map Text Kind)
    -- ^ Kinds of type constructors and type variables
  , kindEnvEffects :: !(Set Text)
    -- ^ Known effect variables
  } deriving stock (Eq, Show)

-- | Empty kind environment
emptyKindEnv :: KindEnv
emptyKindEnv = KindEnv Map.empty Set.empty

-- | Default kind environment with built-in types
defaultKindEnv :: KindEnv
defaultKindEnv = KindEnv
  { kindEnvTypes = Map.fromList
      -- Primitive types: Kind Type
      [ ("Int", KiType 0)
      , ("Bool", KiType 0)
      , ("String", KiType 0)
      , ("Char", KiType 0)
      , ("Unit", KiType 0)
      , ("Float", KiType 0)
      -- Type constructors: Type -> Type
      , ("List", KiArrow (KiType 0) (KiType 0))
      , ("Option", KiArrow (KiType 0) (KiType 0))
      , ("Maybe", KiArrow (KiType 0) (KiType 0))
      , ("Array", KiArrow (KiType 0) (KiType 0))
      , ("Set", KiArrow (KiType 0) (KiType 0))
      -- Two-argument type constructors: Type -> Type -> Type
      , ("Either", KiArrow (KiType 0) (KiArrow (KiType 0) (KiType 0)))
      , ("Map", KiArrow (KiType 0) (KiArrow (KiType 0) (KiType 0)))
      , ("Pair", KiArrow (KiType 0) (KiArrow (KiType 0) (KiType 0)))
      , ("Result", KiArrow (KiType 0) (KiArrow (KiType 0) (KiType 0)))
      ]
  , kindEnvEffects = Set.empty
  }

-- | Extend the kind environment with a type binding
extendKindEnv :: Text -> Kind -> KindEnv -> KindEnv
extendKindEnv name kind env = env
  { kindEnvTypes = Map.insert name kind (kindEnvTypes env)
  }

-- | Extend the kind environment with an effect variable
extendEffectVar :: Text -> KindEnv -> KindEnv
extendEffectVar name env = env
  { kindEnvEffects = Set.insert name (kindEnvEffects env)
  }

-- | Look up the kind of a type name
lookupKindEnv :: Text -> KindEnv -> Maybe Kind
lookupKindEnv name = Map.lookup name . kindEnvTypes

-- | Check if an effect variable is in scope
isEffectVarInScope :: Text -> KindEnv -> Bool
isEffectVarInScope name = Set.member name . kindEnvEffects

-- | Infer the kind of a type
kindOf :: KindEnv -> Type -> Either KindError Kind
kindOf env = \case
  TyVar name _idx ->
    case lookupKindEnv name env of
      Just kind -> Right kind
      Nothing -> Left $ UnboundTypeVar name

  TyCon name args -> do
    -- Look up the kind of the constructor
    conKind <- case lookupKindEnv name env of
      Just k -> Right k
      Nothing -> Left $ UnboundTypeCon name
    -- Apply the constructor to its arguments
    applyKind env name conKind args

  TyPi _paramName paramTy _effs retTy -> do
    -- Check both sides have kind Type
    paramKind <- kindOf env paramTy
    checkKindIs (KiType 0) paramKind
    retKind <- kindOf env retTy
    checkKindIs (KiType 0) retKind
    Right (KiType 0)

  TyForall varName varKind bodyTy -> do
    -- Extend environment with the quantified variable
    let env' = extendKindEnv varName varKind env
    bodyKind <- kindOf env' bodyTy
    checkKindIs (KiType 0) bodyKind
    Right (KiType 0)

  TyForallDep varName varTy bodyTy -> do
    -- Check the variable type has kind Type
    varKind <- kindOf env varTy
    checkKindIs (KiType 0) varKind
    -- Extend environment and check body
    let env' = extendKindEnv varName (KiType 0) env
    bodyKind <- kindOf env' bodyTy
    checkKindIs (KiType 0) bodyKind
    Right (KiType 0)

  TyLazy innerTy -> do
    innerKind <- kindOf env innerTy
    checkKindIs (KiType 0) innerKind
    Right (KiType 0)

  TyLinear innerTy -> do
    innerKind <- kindOf env innerTy
    checkKindIs (KiType 0) innerKind
    Right KiLinear

  TyRef innerTy -> do
    innerKind <- kindOf env innerTy
    checkKindIs (KiType 0) innerKind
    Right (KiType 0)

  TyRefMut innerTy -> do
    innerKind <- kindOf env innerTy
    checkKindIs (KiType 0) innerKind
    Right (KiType 0)

  TyUniverse level ->
    -- Type_n has kind Type_(n+1)
    Right (KiType (level + 1))

  TyProp ->
    Right KiProp

-- | Apply a kind to type arguments
applyKind :: KindEnv -> Text -> Kind -> [Type] -> Either KindError Kind
applyKind env _name kind args = go kind args
  where
    go k [] = Right k
    go (KiArrow paramKind resultKind) (arg:rest) = do
      -- Check the argument has the expected kind
      argKind <- kindOf env arg
      checkKindIs paramKind argKind
      -- Continue with remaining arguments
      go resultKind rest
    go k (_:_) =
      -- Trying to apply a non-arrow kind
      Left $ KindMismatch (KiArrow (KiType 0) (KiType 0)) k

-- | Check that two kinds are equal
checkKindIs :: Kind -> Kind -> Either KindError ()
checkKindIs expected actual
  | kindsEqual expected actual = Right ()
  | otherwise = Left $ KindMismatch expected actual

-- | Check if two kinds are equal
kindsEqual :: Kind -> Kind -> Bool
kindsEqual (KiType n1) (KiType n2) = n1 == n2
kindsEqual KiProp KiProp = True
kindsEqual KiLinear KiLinear = True
kindsEqual (KiArrow a1 b1) (KiArrow a2 b2) =
  kindsEqual a1 a2 && kindsEqual b1 b2
kindsEqual _ _ = False

-- | Check that a type has a specific kind
checkKind :: KindEnv -> Type -> Kind -> Either KindError ()
checkKind env ty expectedKind = do
  actualKind <- kindOf env ty
  checkKindIs expectedKind actualKind

-- | Check that an effect row is well-formed
checkEffectRow :: KindEnv -> EffectRow -> Either KindError ()
checkEffectRow env = \case
  EffEmpty -> Right ()

  EffSet effects -> mapM_ checkEffect effects
    where
      checkEffect (Effect name _auth) =
        -- Effects should be known types or declared effects
        case lookupKindEnv name env of
          Just _ -> Right ()
          Nothing -> Right ()  -- Allow unknown effects for now

  EffVar name _idx ->
    if isEffectVarInScope name env
      then Right ()
      else Left $ IllFormedEffectRow name

  EffUnion row1 row2 -> do
    checkEffectRow env row1
    checkEffectRow env row2
