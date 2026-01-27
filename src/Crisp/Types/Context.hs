{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Types.Context
-- Description : Typing Context
--
-- Manages the typing environment during type checking, including
-- variable bindings, type definitions, and effect signatures.

module Crisp.Types.Context
  ( -- * Context
    Context(..)
  , emptyContext
  , withPrelude
    -- * Bindings
  , Binding(..)
  , extendTerm
  , extendType
  , extendEffect
  , lookupTerm
  , lookupTypeVar
    -- * Type definitions
  , TypeInfo(..)
  , ConstructorInfo(..)
  , registerType
  , lookupType
  , lookupConstructor
    -- * Effect definitions
  , EffectInfo(..)
  , OperationInfo(..)
  , registerEffect
  , lookupEffect
  , lookupOperation
    -- * Trait definitions
  , TraitInfo(..)
  , TraitMethodInfo(..)
  , ImplInfo(..)
  , registerTrait
  , lookupTrait
  , registerImpl
  , lookupImpl
  , lookupImplsForTrait
  , lookupImplsForType
    -- * External functions (FFI)
  , ExternalInfo(..)
  , registerExternal
  , lookupExternal
  , allExternals
    -- * Refinement types
  , RefinementInfo(..)
  , registerRefinement
  , lookupRefinement
  , lookupRefinementsForType
    -- * Type aliases
  , TypeAliasInfo(..)
  , FieldConstraintInfo(..)
  , registerTypeAlias
  , lookupTypeAlias
  , expandTypeAlias
    -- * Bounded polymorphism (trait constraints)
  , satisfiesConstraint
  , satisfiesAllConstraints
  , checkTypeParamConstraints
    -- * Authority
  , setAuthority
  , getAuthority
    -- * Utilities
  , termDepth
  , typeDepth
  ) where

import Crisp.Core.Term

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | A binding in the typing context
data Binding
  = TermBinding !Text !Type          -- ^ Term variable: x : T
  | TypeBinding !Text !Kind          -- ^ Type variable: a : K
  | EffectBinding !Text              -- ^ Effect variable
  deriving stock (Eq, Show)

-- | Information about a type definition
data TypeInfo = TypeInfo
  { typeInfoName         :: !Text
  , typeInfoParams       :: ![(Text, Kind)]
  , typeInfoKind         :: !Kind
  , typeInfoConstructors :: ![ConstructorInfo]
  , typeInfoIsProp       :: !Bool
  , typeInfoIsLinear     :: !Bool
  } deriving stock (Eq, Show)

-- | Information about a constructor
data ConstructorInfo = ConstructorInfo
  { constructorName       :: !Text
  , constructorParamTypes :: ![Type]
  , constructorReturnType :: !Type
  } deriving stock (Eq, Show)

-- | Information about an effect
data EffectInfo = EffectInfo
  { effectInfoName       :: !Text
  , effectInfoOperations :: ![OperationInfo]
  } deriving stock (Eq, Show)

-- | Information about an effect operation
data OperationInfo = OperationInfo
  { operationInfoName       :: !Text
  , operationInfoInputType  :: !Type
  , operationInfoOutputType :: !Type
  } deriving stock (Eq, Show)

-- | Information about a trait definition
data TraitInfo = TraitInfo
  { traitInfoName      :: !Text                   -- ^ Trait name (e.g., "Ord")
  , traitInfoParam     :: !(Text, Kind)           -- ^ Type parameter and its kind
  , traitInfoSupers    :: ![Text]                 -- ^ Supertrait names
  , traitInfoMethods   :: ![TraitMethodInfo]      -- ^ Method signatures
  } deriving stock (Eq, Show)

-- | Information about a trait method
data TraitMethodInfo = TraitMethodInfo
  { traitMethodInfoName    :: !Text               -- ^ Method name (e.g., "compare")
  , traitMethodInfoType    :: !Type               -- ^ Method type
  , traitMethodInfoDefault :: !(Maybe Type)       -- ^ Default implementation type (placeholder)
  } deriving stock (Eq, Show)

-- | Information about a trait implementation
data ImplInfo = ImplInfo
  { implInfoTrait   :: !Text                      -- ^ Trait being implemented
  , implInfoType    :: !Type                      -- ^ Type implementing the trait
  , implInfoMethods :: ![(Text, Type)]            -- ^ Method implementations (name, type)
  } deriving stock (Eq, Show)

-- | Information about an external (FFI) function
data ExternalInfo = ExternalInfo
  { externalInfoName     :: !Text                 -- ^ Crisp function name
  , externalInfoModule   :: !Text                 -- ^ External module (e.g., "postgres", "console")
  , externalInfoFunction :: !Text                 -- ^ External function name
  , externalInfoType     :: !Type                 -- ^ Function type signature
  } deriving stock (Eq, Show)

-- | Information about a refinement type
-- Refinement types express compile-time constraints on values
-- Example: type Month = Int { 1 <= self <= 12 }
data RefinementInfo = RefinementInfo
  { refinementInfoName       :: !Text             -- ^ Name of the refinement type (if aliased)
  , refinementInfoBaseType   :: !Type             -- ^ The underlying type being refined
  , refinementInfoPredicates :: ![Term]           -- ^ Predicate terms that must hold for values
  } deriving stock (Eq, Show)

-- | Information about a type alias with field constraints
-- Example: type JudicialAuthority = Authority { action: Judicial(_) }
data TypeAliasInfo = TypeAliasInfo
  { typeAliasInfoName        :: !Text             -- ^ Alias name (e.g., "JudicialAuthority")
  , typeAliasInfoParams      :: ![(Text, Kind)]   -- ^ Type parameters with their kinds
  , typeAliasInfoBaseType    :: !Type             -- ^ Base type (e.g., "Authority")
  , typeAliasInfoConstraints :: ![FieldConstraintInfo]  -- ^ Field constraints
  } deriving stock (Eq, Show)

-- | Information about a field constraint in a type alias
data FieldConstraintInfo = FieldConstraintInfo
  { fieldConstraintInfoName    :: !Text           -- ^ Field name
  , fieldConstraintInfoPattern :: !Text           -- ^ Pattern description (for display/error messages)
  } deriving stock (Eq, Show)

-- | The typing context
data Context = Context
  { contextBindings    :: ![Binding]              -- ^ Stack of bindings (innermost first)
  , contextTypes       :: !(Map Text TypeInfo)    -- ^ Registered type definitions
  , contextEffects     :: !(Map Text EffectInfo)  -- ^ Registered effect definitions
  , contextTraits      :: !(Map Text TraitInfo)   -- ^ Registered trait definitions
  , contextImpls       :: ![ImplInfo]             -- ^ Registered trait implementations
  , contextExternals   :: !(Map Text ExternalInfo) -- ^ Registered external (FFI) functions
  , contextRefinements :: !(Map Text RefinementInfo) -- ^ Registered refinement type aliases
  , contextTypeAliases :: !(Map Text TypeAliasInfo) -- ^ Registered type aliases with constraints
  , contextAuthority   :: !(Maybe Text)           -- ^ Current module authority
  } deriving stock (Eq, Show)

-- | Create an empty context
emptyContext :: Context
emptyContext = Context
  { contextBindings = []
  , contextTypes = Map.empty
  , contextEffects = Map.empty
  , contextTraits = Map.empty
  , contextImpls = []
  , contextExternals = Map.empty
  , contextRefinements = Map.empty
  , contextTypeAliases = Map.empty
  , contextAuthority = Nothing
  }

-- | Create a context with standard prelude types
withPrelude :: Context
withPrelude = registerPreludeImpls $ registerPreludeTraits preludeTypesContext
  where
    -- Register prelude types
    preludeTypesContext = foldr registerType emptyContext preludeTypes

    preludeTypes =
      [ TypeInfo "Unit" [] (KiType 0)
          [ConstructorInfo "Unit" [] (simpleType "Unit")]
          False False
      , TypeInfo "Bool" [] (KiType 0)
          [ ConstructorInfo "True" [] (simpleType "Bool")
          , ConstructorInfo "False" [] (simpleType "Bool")
          ]
          False False
      , TypeInfo "Nat" [] (KiType 0) [] False False
      , TypeInfo "Int" [] (KiType 0) [] False False
      , TypeInfo "Float" [] (KiType 0) [] False False
      , TypeInfo "String" [] (KiType 0) [] False False
      , TypeInfo "Char" [] (KiType 0) [] False False
        -- Ordering type for comparison results
      , TypeInfo "Ordering" [] (KiType 0)
          [ ConstructorInfo "Less" [] (simpleType "Ordering")
          , ConstructorInfo "Equal" [] (simpleType "Ordering")
          , ConstructorInfo "Greater" [] (simpleType "Ordering")
          ]
          False False
        -- Vec type for dependent type testing
        -- Vec(A, n) is a length-indexed vector
      , TypeInfo "Vec" [("A", KiType 0), ("n", KiType 0)] (KiType 0)
          [ ConstructorInfo "Nil" [] (TyCon "Vec" [TyVar "A" 0, TyNatLit 0])
          , ConstructorInfo "Cons"
              [TyVar "A" 0, TyCon "Vec" [TyVar "A" 0, TyVar "n" 0]]
              (TyCon "Vec" [TyVar "A" 0, TyAdd (TyVar "n" 0) (TyNatLit 1)])
          ]
          False False
      ]

    -- Register prelude traits (Eq and Ord)
    registerPreludeTraits ctx = foldr registerTrait ctx preludeTraits

    preludeTraits =
      [ -- Eq trait: equality comparison
        TraitInfo "Eq" ("A", KiType 0) []
          [ TraitMethodInfo "eq"
              (TyPi "a" (TyVar "A" 0) EffEmpty
                (TyPi "b" (TyVar "A" 0) EffEmpty (simpleType "Bool")))
              Nothing
          , TraitMethodInfo "ne"
              (TyPi "a" (TyVar "A" 0) EffEmpty
                (TyPi "b" (TyVar "A" 0) EffEmpty (simpleType "Bool")))
              Nothing  -- Default: not (eq a b)
          ]
        -- Ord trait: ordering comparison (requires Eq)
      , TraitInfo "Ord" ("A", KiType 0) ["Eq"]
          [ TraitMethodInfo "compare"
              (TyPi "a" (TyVar "A" 0) EffEmpty
                (TyPi "b" (TyVar "A" 0) EffEmpty (simpleType "Ordering")))
              Nothing
          , TraitMethodInfo "lt"
              (TyPi "a" (TyVar "A" 0) EffEmpty
                (TyPi "b" (TyVar "A" 0) EffEmpty (simpleType "Bool")))
              Nothing  -- Default: compare a b == Less
          , TraitMethodInfo "le"
              (TyPi "a" (TyVar "A" 0) EffEmpty
                (TyPi "b" (TyVar "A" 0) EffEmpty (simpleType "Bool")))
              Nothing  -- Default: compare a b /= Greater
          , TraitMethodInfo "gt"
              (TyPi "a" (TyVar "A" 0) EffEmpty
                (TyPi "b" (TyVar "A" 0) EffEmpty (simpleType "Bool")))
              Nothing  -- Default: compare a b == Greater
          , TraitMethodInfo "ge"
              (TyPi "a" (TyVar "A" 0) EffEmpty
                (TyPi "b" (TyVar "A" 0) EffEmpty (simpleType "Bool")))
              Nothing  -- Default: compare a b /= Less
          ]
      ]

    -- Register prelude trait implementations for primitive types
    registerPreludeImpls ctx = foldr registerImpl ctx preludeImpls

    -- Helper to create Eq impl for a type
    eqImplFor :: Text -> ImplInfo
    eqImplFor typeName = ImplInfo "Eq" (simpleType typeName)
      [ ("eq", TyPi "a" (simpleType typeName) EffEmpty
                (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Bool")))
      , ("ne", TyPi "a" (simpleType typeName) EffEmpty
                (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Bool")))
      ]

    -- Helper to create Ord impl for a type
    ordImplFor :: Text -> ImplInfo
    ordImplFor typeName = ImplInfo "Ord" (simpleType typeName)
      [ ("compare", TyPi "a" (simpleType typeName) EffEmpty
                     (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Ordering")))
      , ("lt", TyPi "a" (simpleType typeName) EffEmpty
                (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Bool")))
      , ("le", TyPi "a" (simpleType typeName) EffEmpty
                (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Bool")))
      , ("gt", TyPi "a" (simpleType typeName) EffEmpty
                (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Bool")))
      , ("ge", TyPi "a" (simpleType typeName) EffEmpty
                (TyPi "b" (simpleType typeName) EffEmpty (simpleType "Bool")))
      ]

    preludeImpls =
      -- Eq implementations for primitives
      [ eqImplFor "Int"
      , eqImplFor "Float"
      , eqImplFor "String"
      , eqImplFor "Char"
      , eqImplFor "Bool"
      , eqImplFor "Nat"
      , eqImplFor "Unit"
      , eqImplFor "Ordering"
        -- Ord implementations for primitives
      , ordImplFor "Int"
      , ordImplFor "Float"
      , ordImplFor "String"
      , ordImplFor "Char"
      , ordImplFor "Bool"
      , ordImplFor "Nat"
      , ordImplFor "Ordering"
      ]

-- | Extend the context with a term binding
extendTerm :: Text -> Type -> Context -> Context
extendTerm name ty ctx = ctx
  { contextBindings = TermBinding name ty : contextBindings ctx }

-- | Extend the context with a type binding
extendType :: Text -> Kind -> Context -> Context
extendType name kind ctx = ctx
  { contextBindings = TypeBinding name kind : contextBindings ctx }

-- | Extend the context with an effect binding
extendEffect :: Text -> Context -> Context
extendEffect name ctx = ctx
  { contextBindings = EffectBinding name : contextBindings ctx }

-- | Look up a term variable
lookupTerm :: Text -> Context -> Maybe (Type, Int)
lookupTerm name ctx = go (contextBindings ctx) 0
  where
    go [] _ = Nothing
    go (TermBinding n t : rest) idx
      | n == name = Just (t, idx)
      | otherwise = go rest (idx + 1)
    go (_ : rest) idx = go rest idx

-- | Look up a type variable
lookupTypeVar :: Text -> Context -> Maybe (Kind, Int)
lookupTypeVar name ctx = go (contextBindings ctx) 0
  where
    go [] _ = Nothing
    go (TypeBinding n k : rest) idx
      | n == name = Just (k, idx)
      | otherwise = go rest (idx + 1)
    go (_ : rest) idx = go rest idx

-- | Register a type definition
registerType :: TypeInfo -> Context -> Context
registerType info ctx = ctx
  { contextTypes = Map.insert (typeInfoName info) info (contextTypes ctx) }

-- | Look up a type definition
lookupType :: Text -> Context -> Maybe TypeInfo
lookupType name ctx = Map.lookup name (contextTypes ctx)

-- | Look up a constructor
lookupConstructor :: Text -> Context -> Maybe (Text, ConstructorInfo)
lookupConstructor name ctx = go $ Map.toList (contextTypes ctx)
  where
    go [] = Nothing
    go ((typeName, info) : rest) =
      case findCon (typeInfoConstructors info) of
        Just con -> Just (typeName, con)
        Nothing  -> go rest

    findCon [] = Nothing
    findCon (c : cs)
      | constructorName c == name = Just c
      | otherwise = findCon cs

-- | Register an effect definition
registerEffect :: EffectInfo -> Context -> Context
registerEffect info ctx = ctx
  { contextEffects = Map.insert (effectInfoName info) info (contextEffects ctx) }

-- | Look up an effect definition
lookupEffect :: Text -> Context -> Maybe EffectInfo
lookupEffect name ctx = Map.lookup name (contextEffects ctx)

-- | Look up an operation in an effect
lookupOperation :: Text -> Text -> Context -> Maybe OperationInfo
lookupOperation effectName opName ctx = do
  eff <- lookupEffect effectName ctx
  findOp (effectInfoOperations eff)
  where
    findOp [] = Nothing
    findOp (op : ops)
      | operationInfoName op == opName = Just op
      | otherwise = findOp ops

-- | Set the current authority
setAuthority :: Text -> Context -> Context
setAuthority auth ctx = ctx { contextAuthority = Just auth }

-- | Get the current authority
getAuthority :: Context -> Maybe Text
getAuthority = contextAuthority

-- | Count term bindings
termDepth :: Context -> Int
termDepth ctx = length $ filter isTerm (contextBindings ctx)
  where
    isTerm (TermBinding _ _) = True
    isTerm _ = False

-- | Count type bindings
typeDepth :: Context -> Int
typeDepth ctx = length $ filter isType (contextBindings ctx)
  where
    isType (TypeBinding _ _) = True
    isType _ = False

-- | Register a trait definition
registerTrait :: TraitInfo -> Context -> Context
registerTrait info ctx = ctx
  { contextTraits = Map.insert (traitInfoName info) info (contextTraits ctx) }

-- | Look up a trait definition
lookupTrait :: Text -> Context -> Maybe TraitInfo
lookupTrait name ctx = Map.lookup name (contextTraits ctx)

-- | Register a trait implementation
registerImpl :: ImplInfo -> Context -> Context
registerImpl info ctx = ctx
  { contextImpls = info : contextImpls ctx }

-- | Look up an implementation for a specific trait and type
lookupImpl :: Text -> Type -> Context -> Maybe ImplInfo
lookupImpl traitName ty ctx = go (contextImpls ctx)
  where
    go [] = Nothing
    go (impl : rest)
      | implInfoTrait impl == traitName && typesMatch (implInfoType impl) ty = Just impl
      | otherwise = go rest

    -- Simple type matching (can be extended for more complex matching)
    typesMatch (TyCon n1 args1) (TyCon n2 args2) =
      n1 == n2 && length args1 == length args2 && all (uncurry typesMatch) (zip args1 args2)
    typesMatch t1 t2 = t1 == t2

-- | Look up all implementations for a trait
lookupImplsForTrait :: Text -> Context -> [ImplInfo]
lookupImplsForTrait traitName ctx =
  filter (\impl -> implInfoTrait impl == traitName) (contextImpls ctx)

-- | Look up all implementations for a type
lookupImplsForType :: Type -> Context -> [ImplInfo]
lookupImplsForType ty ctx =
  filter (\impl -> typesMatch (implInfoType impl) ty) (contextImpls ctx)
  where
    typesMatch (TyCon n1 args1) (TyCon n2 args2) =
      n1 == n2 && length args1 == length args2 && all (uncurry typesMatch) (zip args1 args2)
    typesMatch t1 t2 = t1 == t2

-- | Register an external (FFI) function
registerExternal :: ExternalInfo -> Context -> Context
registerExternal info ctx = ctx
  { contextExternals = Map.insert (externalInfoName info) info (contextExternals ctx) }

-- | Look up an external function by name
lookupExternal :: Text -> Context -> Maybe ExternalInfo
lookupExternal name ctx = Map.lookup name (contextExternals ctx)

-- | Get all registered external functions
allExternals :: Context -> [ExternalInfo]
allExternals ctx = Map.elems (contextExternals ctx)

-- | Register a refinement type alias
registerRefinement :: RefinementInfo -> Context -> Context
registerRefinement info ctx = ctx
  { contextRefinements = Map.insert (refinementInfoName info) info (contextRefinements ctx) }

-- | Look up a refinement type by name
lookupRefinement :: Text -> Context -> Maybe RefinementInfo
lookupRefinement name ctx = Map.lookup name (contextRefinements ctx)

-- | Look up all refinements that refine a specific base type
lookupRefinementsForType :: Type -> Context -> [RefinementInfo]
lookupRefinementsForType baseType ctx =
  filter (\r -> typesMatch (refinementInfoBaseType r) baseType) (Map.elems (contextRefinements ctx))
  where
    typesMatch (TyCon n1 args1) (TyCon n2 args2) =
      n1 == n2 && length args1 == length args2 && all (uncurry typesMatch) (zip args1 args2)
    typesMatch t1 t2 = t1 == t2

-- | Check if a type satisfies a single trait constraint
-- Returns True if there's an implementation of the trait for the type
satisfiesConstraint :: Type -> Text -> Context -> Bool
satisfiesConstraint ty traitName ctx =
  case lookupImpl traitName ty ctx of
    Just _  -> True
    Nothing -> False

-- | Check if a type satisfies all trait constraints
-- Returns list of unsatisfied constraints (empty list means all satisfied)
satisfiesAllConstraints :: Type -> [Text] -> Context -> [Text]
satisfiesAllConstraints ty traits ctx =
  filter (\t -> not (satisfiesConstraint ty t ctx)) traits

-- | Check that type arguments satisfy the constraints of their type parameters
-- Takes a list of (type argument, constraint trait names) pairs
-- Returns list of (type, unsatisfied trait) pairs for violations
checkTypeParamConstraints :: [(Type, [Text])] -> Context -> [(Type, Text)]
checkTypeParamConstraints typeArgConstraints ctx =
  concatMap checkOne typeArgConstraints
  where
    checkOne (ty, traits) =
      map (\t -> (ty, t)) (satisfiesAllConstraints ty traits ctx)

-- | Register a type alias with field constraints
registerTypeAlias :: TypeAliasInfo -> Context -> Context
registerTypeAlias info ctx = ctx
  { contextTypeAliases = Map.insert (typeAliasInfoName info) info (contextTypeAliases ctx) }

-- | Look up a type alias by name
lookupTypeAlias :: Text -> Context -> Maybe TypeAliasInfo
lookupTypeAlias name ctx = Map.lookup name (contextTypeAliases ctx)

-- | Expand a type alias to its base type
-- Returns Nothing if the name is not a type alias
expandTypeAlias :: Text -> Context -> Maybe Type
expandTypeAlias name ctx = typeAliasInfoBaseType <$> lookupTypeAlias name ctx
