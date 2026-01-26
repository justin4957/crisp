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

-- | The typing context
data Context = Context
  { contextBindings  :: ![Binding]              -- ^ Stack of bindings (innermost first)
  , contextTypes     :: !(Map Text TypeInfo)    -- ^ Registered type definitions
  , contextEffects   :: !(Map Text EffectInfo)  -- ^ Registered effect definitions
  , contextAuthority :: !(Maybe Text)           -- ^ Current module authority
  } deriving stock (Eq, Show)

-- | Create an empty context
emptyContext :: Context
emptyContext = Context
  { contextBindings = []
  , contextTypes = Map.empty
  , contextEffects = Map.empty
  , contextAuthority = Nothing
  }

-- | Create a context with standard prelude types
withPrelude :: Context
withPrelude = foldr registerType emptyContext preludeTypes
  where
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
