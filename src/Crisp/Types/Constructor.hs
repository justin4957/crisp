{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Types.Constructor
-- Description : Constructor typing for algebraic data types
--
-- Implements type checking for algebraic data type constructors, including:
-- - Constructor declarations within type definitions
-- - Constructor type synthesis
-- - Constructor application checking
-- - Pattern matching with type extraction
-- - GADT-style refined return types
-- - Type parameter instantiation
--
-- Example type declarations:
-- @
-- type Bool = True | False
-- type Option(A) = None | Some(A)
-- type List(A) = Nil | Cons(A, List(A))
-- @

module Crisp.Types.Constructor
  ( -- * Type Declarations
    TypeDecl(..)
  , ConDecl(..)
    -- * Constructor Environment
  , ConstructorEnv
  , emptyConstructorEnv
  , addTypeDecl
  , getTypeInfo
  , getConstructorInfo
  , lookupConstructorType
    -- * Checking
  , checkTypeDecl
  , checkTypeDeclGroup
  , ConstructorError(..)
    -- * Type Synthesis
  , synthesizeConstructor
  , checkConstructorApp
  , partialApplyConstructor
    -- * Pattern Matching
  , extractPatternBindings
    -- * Instantiation
  , instantiateConstructor
  , instantiateConstructorReturnType
    -- * Type Info (for tests)
  , TypeInfo'(..)
  , ConstructorInfo'(..)
  , typeInfoConstructors'
  , constructorInfoName
  , constructorInfoReturnType
  ) where

import Crisp.Core.Term

import Data.List (find, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------

-- | A type declaration
data TypeDecl = TypeDecl
  { typeDeclName :: !Text
    -- ^ Name of the type
  , typeDeclParams :: ![(Text, Kind)]
    -- ^ Type parameters with their kinds
  , typeDeclConstructors :: ![ConDecl]
    -- ^ Constructor declarations
  } deriving stock (Eq, Show)

-- | A constructor declaration
data ConDecl = ConDecl
  { conDeclName :: !Text
    -- ^ Name of the constructor
  , conDeclParams :: ![Type]
    -- ^ Parameter types
  , conDeclReturnType :: !(Maybe Type)
    -- ^ Optional refined return type (for GADTs)
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Constructor Environment
--------------------------------------------------------------------------------

-- | Information about a registered type
data TypeInfo' = TypeInfo'
  { typeInfoName' :: !Text
  , typeInfoParams' :: ![(Text, Kind)]
  , typeInfoConstructors' :: ![ConstructorInfo']
  } deriving stock (Eq, Show)

-- | Information about a constructor
data ConstructorInfo' = ConstructorInfo'
  { constructorInfoName :: !Text
  , constructorInfoTypeName :: !Text
  , constructorInfoParamTypes :: ![Type]
  , constructorInfoReturnType :: !Type
  , constructorInfoTypeParams :: ![(Text, Kind)]
  } deriving stock (Eq, Show)

-- | Constructor environment
data ConstructorEnv = ConstructorEnv
  { envTypes :: !(Map Text TypeInfo')
  , envConstructors :: !(Map Text ConstructorInfo')
  } deriving stock (Eq, Show)

-- | Empty constructor environment
emptyConstructorEnv :: ConstructorEnv
emptyConstructorEnv = ConstructorEnv Map.empty Map.empty

-- | Add a type declaration to the environment
addTypeDecl :: TypeDecl -> ConstructorEnv -> ConstructorEnv
addTypeDecl decl env =
  let typeInfo = mkTypeInfo decl
      conInfos = mkConstructorInfos decl
      env' = env { envTypes = Map.insert (typeDeclName decl) typeInfo (envTypes env) }
  in foldr addConstructor env' conInfos

-- | Add a constructor to the environment
addConstructor :: ConstructorInfo' -> ConstructorEnv -> ConstructorEnv
addConstructor info env = env
  { envConstructors = Map.insert (constructorInfoName info) info (envConstructors env) }

-- | Create type info from declaration
mkTypeInfo :: TypeDecl -> TypeInfo'
mkTypeInfo decl = TypeInfo'
  { typeInfoName' = typeDeclName decl
  , typeInfoParams' = typeDeclParams decl
  , typeInfoConstructors' = mkConstructorInfos decl
  }

-- | Create constructor infos from type declaration
mkConstructorInfos :: TypeDecl -> [ConstructorInfo']
mkConstructorInfos decl = map mkConInfo (typeDeclConstructors decl)
  where
    typeName = typeDeclName decl
    typeParams = typeDeclParams decl
    defaultReturnType = mkTypeWithParams typeName typeParams

    mkConInfo conDecl = ConstructorInfo'
      { constructorInfoName = conDeclName conDecl
      , constructorInfoTypeName = typeName
      , constructorInfoParamTypes = conDeclParams conDecl
      , constructorInfoReturnType = case conDeclReturnType conDecl of
          Just ty -> ty
          Nothing -> defaultReturnType
      , constructorInfoTypeParams = typeParams
      }

-- | Create a type with its parameters as arguments
mkTypeWithParams :: Text -> [(Text, Kind)] -> Type
mkTypeWithParams name params =
  TyCon name [TyVar n i | ((n, _), i) <- zip params [0..]]

-- | Look up type info
getTypeInfo :: Text -> ConstructorEnv -> Maybe TypeInfo'
getTypeInfo name env = Map.lookup name (envTypes env)

-- | Look up constructor info
getConstructorInfo :: Text -> ConstructorEnv -> Maybe ConstructorInfo'
getConstructorInfo name env = Map.lookup name (envConstructors env)

-- | Look up the full type of a constructor
lookupConstructorType :: Text -> ConstructorEnv -> Maybe Type
lookupConstructorType name env = do
  info <- getConstructorInfo name env
  pure $ buildConstructorType info

-- | Build the full type of a constructor
buildConstructorType :: ConstructorInfo' -> Type
buildConstructorType info =
  let params = constructorInfoParamTypes info
      retTy = constructorInfoReturnType info
      typeParams = constructorInfoTypeParams info
      -- Build function type from params to return type
      fnTy = foldr (\p t -> TyPi "_" p EffEmpty t) retTy params
      -- Wrap in foralls for type parameters
  in wrapInForalls typeParams fnTy

-- | Wrap a type in forall quantifiers
wrapInForalls :: [(Text, Kind)] -> Type -> Type
wrapInForalls [] ty = ty
wrapInForalls ((name, kind):rest) ty =
  TyForall name kind (wrapInForalls rest ty)

--------------------------------------------------------------------------------
-- Checking
--------------------------------------------------------------------------------

-- | Errors in constructor typing
data ConstructorError
  = DuplicateTypeName !Text
  | DuplicateConstructorName !Text
  | UnknownType !Text
  | UnknownConstructor !Text
  | ConstructorArityMismatch !Text !Int !Int
  | TypeMismatch' !Type !Type
  | PatternTypeMismatch' !Pattern !Type
  | InvalidGADTReturnType !Text !Type
  | OtherConstructorError !Text
  deriving stock (Eq, Show)

-- | Check a type declaration
checkTypeDecl :: ConstructorEnv -> TypeDecl -> Either ConstructorError ()
checkTypeDecl env decl = do
  -- Check for duplicate type name
  case getTypeInfo (typeDeclName decl) env of
    Just _ -> Left $ DuplicateTypeName (typeDeclName decl)
    Nothing -> pure ()

  -- Check for duplicate constructor names
  let conNames = map conDeclName (typeDeclConstructors decl)
  if conNames /= nub conNames
    then Left $ DuplicateConstructorName (findDuplicate conNames)
    else pure ()

  -- Check constructor names don't conflict with existing constructors
  mapM_ (checkConNotExists env) conNames

  -- All checks passed
  pure ()

-- | Check constructor doesn't already exist
checkConNotExists :: ConstructorEnv -> Text -> Either ConstructorError ()
checkConNotExists env name =
  case getConstructorInfo name env of
    Just _ -> Left $ DuplicateConstructorName name
    Nothing -> pure ()

-- | Find first duplicate in a list
findDuplicate :: Eq a => [a] -> a
findDuplicate [] = error "No duplicates"
findDuplicate (x:xs)
  | x `elem` xs = x
  | otherwise = findDuplicate xs

-- | Check a group of mutually recursive type declarations
checkTypeDeclGroup :: ConstructorEnv -> [TypeDecl] -> Either ConstructorError ConstructorEnv
checkTypeDeclGroup env decls = do
  -- First, add all types to environment (for mutual recursion)
  let env' = foldr addTypeDecl env decls
  -- Then check each declaration
  mapM_ (checkTypeDecl env) decls
  pure env'

--------------------------------------------------------------------------------
-- Type Synthesis
--------------------------------------------------------------------------------

-- | Synthesize the type of a constructor (without type arguments)
synthesizeConstructor :: ConstructorEnv -> Text -> [Type] -> Either ConstructorError Type
synthesizeConstructor env name _tyArgs = do
  info <- case getConstructorInfo name env of
    Just i -> Right i
    Nothing -> Left $ UnknownConstructor name
  pure $ buildConstructorType info

-- | Check a constructor application against an expected type
checkConstructorApp :: ConstructorEnv -> Text -> [Type] -> Type -> Either ConstructorError ()
checkConstructorApp env name argTys expectedTy = do
  info <- case getConstructorInfo name env of
    Just i -> Right i
    Nothing -> Left $ UnknownConstructor name

  -- Get the type parameters from the expected return type
  let typeArgs = extractTypeArgs expectedTy (constructorInfoTypeName info)

  -- Instantiate the constructor parameter types
  let expectedParamTys = map (substituteTypeParams (constructorInfoTypeParams info) typeArgs)
                             (constructorInfoParamTypes info)

  -- Check arity
  let expectedArity = length expectedParamTys
      actualArity = length argTys
  if expectedArity /= actualArity
    then Left $ ConstructorArityMismatch name expectedArity actualArity
    else pure ()

  -- Check each argument type matches
  mapM_ (uncurry checkTypesMatch) (zip expectedParamTys argTys)

  pure ()

-- | Check two types match
checkTypesMatch :: Type -> Type -> Either ConstructorError ()
checkTypesMatch expected actual =
  if typesEqual expected actual
    then pure ()
    else Left $ TypeMismatch' expected actual

-- | Simple type equality check
typesEqual :: Type -> Type -> Bool
typesEqual t1 t2 = case (t1, t2) of
  (TyVar _ i1, TyVar _ i2) -> i1 == i2
  (TyCon n1 args1, TyCon n2 args2) ->
    n1 == n2 && length args1 == length args2 &&
    all (uncurry typesEqual) (zip args1 args2)
  (TyPi _ p1 _ r1, TyPi _ p2 _ r2) ->
    typesEqual p1 p2 && typesEqual r1 r2
  (TyForall _ k1 b1, TyForall _ k2 b2) ->
    k1 == k2 && typesEqual b1 b2
  (TyLazy i1, TyLazy i2) -> typesEqual i1 i2
  (TyUniverse l1, TyUniverse l2) -> l1 == l2
  (TyProp, TyProp) -> True
  _ -> False

-- | Extract type arguments from an instantiated type
extractTypeArgs :: Type -> Text -> [Type]
extractTypeArgs (TyCon name args) expectedName
  | name == expectedName = args
extractTypeArgs _ _ = []

-- | Partially apply a constructor to some type arguments
partialApplyConstructor :: ConstructorEnv -> Text -> [Type] -> Either ConstructorError Type
partialApplyConstructor env name tyArgs = do
  info <- case getConstructorInfo name env of
    Just i -> Right i
    Nothing -> Left $ UnknownConstructor name

  let typeParams = constructorInfoTypeParams info
      numTypeParams = length typeParams
      numProvided = length tyArgs

  if numProvided > numTypeParams
    then Left $ OtherConstructorError "Too many type arguments"
    else do
      -- Get remaining type parameters
      let remainingParams = drop numProvided typeParams

      -- Substitute provided type args
      let subst = zip [0..] tyArgs
          paramTys = map (applyIndexSubst subst) (constructorInfoParamTypes info)
          retTy = applyIndexSubst subst (constructorInfoReturnType info)

      -- Build remaining function type
      let fnTy = foldr (\p t -> TyPi "_" p EffEmpty t) retTy paramTys

      -- Wrap in remaining foralls
      pure $ wrapInForalls remainingParams fnTy

--------------------------------------------------------------------------------
-- Pattern Matching
--------------------------------------------------------------------------------

-- | Extract bindings from a pattern given the subject type
extractPatternBindings :: ConstructorEnv -> Pattern -> Type -> Either ConstructorError [(Text, Type)]
extractPatternBindings _env (PatVar name) ty = Right [(name, ty)]
extractPatternBindings _env PatWild _ty = Right []
extractPatternBindings env (PatCon conName subPats) ty = do
  info <- case getConstructorInfo conName env of
    Just i -> Right i
    Nothing -> Left $ UnknownConstructor conName

  -- Get type arguments from subject type
  let tyArgs = extractTypeArgs ty (constructorInfoTypeName info)

  -- Check constructor belongs to the right type
  let expectedTypeName = constructorInfoTypeName info
  case ty of
    TyCon actualTypeName _
      | actualTypeName == expectedTypeName -> pure ()
      | otherwise -> Left $ PatternTypeMismatch' (PatCon conName subPats) ty
    _ -> Left $ PatternTypeMismatch' (PatCon conName subPats) ty

  -- Instantiate parameter types
  let instantiatedParamTys = map (substituteTypeParams (constructorInfoTypeParams info) tyArgs)
                                 (constructorInfoParamTypes info)

  -- Check pattern arity
  let expectedArity = length instantiatedParamTys
      actualArity = length subPats
  if expectedArity /= actualArity
    then Left $ ConstructorArityMismatch conName expectedArity actualArity
    else pure ()

  -- Recursively extract bindings from subpatterns
  bindings <- mapM (\(pat, paramTy) -> extractPatternBindings env pat paramTy)
                   (zip subPats instantiatedParamTys)

  pure $ concat bindings

--------------------------------------------------------------------------------
-- Instantiation
--------------------------------------------------------------------------------

-- | Instantiate a constructor's parameter types with type arguments
instantiateConstructor :: ConstructorEnv -> Text -> [Type] -> Either ConstructorError [Type]
instantiateConstructor env name tyArgs = do
  info <- case getConstructorInfo name env of
    Just i -> Right i
    Nothing -> Left $ UnknownConstructor name

  let typeParams = constructorInfoTypeParams info
      instantiatedParams = map (substituteTypeParams typeParams tyArgs)
                               (constructorInfoParamTypes info)

  pure instantiatedParams

-- | Instantiate a constructor's return type with type arguments
instantiateConstructorReturnType :: ConstructorEnv -> Text -> [Type] -> Either ConstructorError Type
instantiateConstructorReturnType env name tyArgs = do
  info <- case getConstructorInfo name env of
    Just i -> Right i
    Nothing -> Left $ UnknownConstructor name

  let typeParams = constructorInfoTypeParams info
      instantiatedRetTy = substituteTypeParams typeParams tyArgs (constructorInfoReturnType info)

  pure instantiatedRetTy

-- | Substitute type parameters with type arguments
substituteTypeParams :: [(Text, Kind)] -> [Type] -> Type -> Type
substituteTypeParams params args = applyIndexSubst subst
  where
    subst = zip [0..] args

-- | Apply an index-based substitution to a type
applyIndexSubst :: [(Int, Type)] -> Type -> Type
applyIndexSubst subst = go
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
        -- Shift the substitution indices when going under a binder
        TyForall typeVar kind (applyIndexSubst (shiftSubst subst) bodyTy)

      TyForallDep paramName paramTy bodyTy ->
        TyForallDep paramName (go paramTy) (applyIndexSubst (shiftSubst subst) bodyTy)

      TyLazy inner -> TyLazy (go inner)
      TyLinear inner -> TyLinear (go inner)
      TyRef inner -> TyRef (go inner)
      TyRefMut inner -> TyRefMut (go inner)
      TyUniverse level -> TyUniverse level
      TyProp -> TyProp

-- | Shift substitution indices by 1
shiftSubst :: [(Int, Type)] -> [(Int, Type)]
shiftSubst = map (\(i, t) -> (i + 1, shiftType 1 t))

-- | Shift free type variable indices in a type
shiftType :: Int -> Type -> Type
shiftType amount = shiftTypeFrom 0 amount

-- | Shift type variables at or above cutoff
shiftTypeFrom :: Int -> Int -> Type -> Type
shiftTypeFrom cutoff amount = go
  where
    go ty = case ty of
      TyVar name idx
        | idx >= cutoff -> TyVar name (idx + amount)
        | otherwise -> TyVar name idx

      TyCon name args ->
        TyCon name (map go args)

      TyPi paramName paramTy effs retTy ->
        TyPi paramName (go paramTy) effs (shiftTypeFrom (cutoff + 1) amount retTy)

      TyForall typeVar kind bodyTy ->
        TyForall typeVar kind (shiftTypeFrom (cutoff + 1) amount bodyTy)

      TyForallDep paramName paramTy bodyTy ->
        TyForallDep paramName (go paramTy) (shiftTypeFrom (cutoff + 1) amount bodyTy)

      TyLazy inner -> TyLazy (go inner)
      TyLinear inner -> TyLinear (go inner)
      TyRef inner -> TyRef (go inner)
      TyRefMut inner -> TyRefMut (go inner)
      TyUniverse level -> TyUniverse level
      TyProp -> TyProp
