{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Crisp.Core.Term
-- Description : Core Calculus Terms
--
-- The internal representation of Crisp programs after desugaring.
-- This is the Dependent Effect Calculus (DEC) at the heart of Crisp.

module Crisp.Core.Term
  ( -- * Terms
    Term(..)
    -- * Types
  , Type(..)
    -- * Kinds
  , Kind(..)
    -- * Effects
  , EffectRow(..)
  , Effect(..)
    -- * Patterns
  , Pattern(..)
  , Case(..)
    -- * Handlers
  , Handler(..)
  , OpHandler(..)
  , ReturnHandler(..)
    -- * Smart constructors
  , simpleFnType
  , pureFnType
  , typeApp
  , simpleType
  , typeZero
  , effectsFromNames
  , emptyEffects
  , singletonEffect
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Core terms in the Dependent Effect Calculus
data Term
  = TmVar !Text !Int                          -- ^ Variable reference (name for debugging, de Bruijn index)
  | TmLam !Text !Type !Term                   -- ^ Lambda abstraction
  | TmApp !Term !Term                         -- ^ Application
  | TmLet !Text !Type !Term !Term             -- ^ Let binding
  | TmTyAbs !Text !Kind !Term                 -- ^ Type abstraction (polymorphism)
  | TmTyApp !Term !Type                       -- ^ Type application
  | TmCon !Text ![Type] ![Term]               -- ^ Constructor application
  | TmMatch !Term !Type ![Case]               -- ^ Pattern match
  | TmPerform !Text !Text !Term               -- ^ Effect operation
  | TmHandle !Handler !Term                   -- ^ Effect handler
  | TmLazy !Term                              -- ^ Lazy (deferred computation)
  | TmForce !Term                             -- ^ Force (evaluate deferred computation)
  | TmAnnot !Term !Type                       -- ^ Type annotation
  deriving stock (Eq, Show, Generic)

-- | Core types
data Type
  = TyVar !Text !Int                          -- ^ Type variable
  | TyCon !Text ![Type]                       -- ^ Type constructor application
  | TyPi !Text !Type !EffectRow !Type         -- ^ Dependent function type: (x : A) ->[E] B
  | TyForall !Text !Kind !Type                -- ^ Universal quantification: forall a. T
  | TyForallDep !Text !Type !Type             -- ^ Dependent quantification: forall (x : A). T
  | TyLazy !Type                              -- ^ Lazy type
  | TyLinear !Type                            -- ^ Linear type marker
  | TyRef !Type                               -- ^ Reference type (immutable borrow)
  | TyRefMut !Type                            -- ^ Mutable reference type
  | TyUniverse !Int                           -- ^ Universe (Type_i)
  | TyProp                                    -- ^ Prop universe (for erased proofs)
  deriving stock (Eq, Show, Generic)

-- | Kind expressions
data Kind
  = KiType !Int                               -- ^ Type universe at level i
  | KiProp                                    -- ^ Prop universe
  | KiLinear                                  -- ^ Linear kind
  | KiArrow !Kind !Kind                       -- ^ Arrow kind: k1 -> k2
  deriving stock (Eq, Show, Generic)

-- | Effect rows
data EffectRow
  = EffEmpty                                  -- ^ Empty effect row (pure)
  | EffSet ![Effect]                          -- ^ Concrete effect set
  | EffVar !Text !Int                         -- ^ Effect variable (for polymorphism)
  | EffUnion !EffectRow !EffectRow            -- ^ Union of effect rows
  deriving stock (Eq, Show, Generic)

-- | A single effect reference
data Effect = Effect
  { effectName      :: !Text
  , effectAuthority :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

-- | A pattern match case
data Case = Case
  { casePattern :: !Pattern
  , caseBody    :: !Term
  } deriving stock (Eq, Show, Generic)

-- | Core patterns
data Pattern
  = PatVar !Text                              -- ^ Variable binding
  | PatWild                                   -- ^ Wildcard
  | PatCon !Text ![Pattern]                   -- ^ Constructor pattern
  deriving stock (Eq, Show, Generic)

-- | An effect handler
data Handler = Handler
  { handlerEffect            :: !Text
  , handlerIntroducedEffects :: !EffectRow
  , handlerOperations        :: ![OpHandler]
  , handlerReturn            :: !ReturnHandler
  } deriving stock (Eq, Show, Generic)

-- | Handler for a single operation
data OpHandler = OpHandler
  { opHandlerOperation :: !Text
  , opHandlerPattern   :: !Pattern
  , opHandlerResume    :: !Text
  , opHandlerBody      :: !Term
  } deriving stock (Eq, Show, Generic)

-- | Handler for the return value
data ReturnHandler = ReturnHandler
  { returnHandlerPattern :: !Pattern
  , returnHandlerBody    :: !Term
  } deriving stock (Eq, Show, Generic)

-- * Smart constructors

-- | Create a simple (non-dependent) function type
simpleFnType :: Type -> Type -> EffectRow -> Type
simpleFnType from to effects = TyPi "_" from effects to

-- | Create a pure function type
pureFnType :: Type -> Type -> Type
pureFnType from to = simpleFnType from to EffEmpty

-- | Create a type application
typeApp :: Text -> [Type] -> Type
typeApp = TyCon

-- | Create a simple type (no arguments)
simpleType :: Text -> Type
simpleType name = TyCon name []

-- | Type₀ (the base universe)
typeZero :: Type
typeZero = TyUniverse 0

-- | Create an effect set from a list of effect names
effectsFromNames :: [Text] -> EffectRow
effectsFromNames [] = EffEmpty
effectsFromNames names = EffSet $ map (`Effect` Nothing) names

-- | Empty effect row
emptyEffects :: EffectRow
emptyEffects = EffEmpty

-- | Single effect
singletonEffect :: Text -> Maybe Text -> EffectRow
singletonEffect name auth = EffSet [Effect name auth]
