{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Prelude.Types
-- Description : Standard type definitions for the Crisp prelude
--
-- Defines the core types available in the Crisp standard library:
-- Unit, Bool, Option, Result, List, Pair, and Ordering.
--
-- These types are defined as Haskell data structures that can be
-- used to generate Crisp type definitions and for type checking.

module Crisp.Prelude.Types
  ( -- * Type Definitions
    PreludeType(..)
  , PreludeConstructor(..)
    -- * Standard Types
  , unitType
  , boolType
  , optionType
  , resultType
  , listType
  , pairType
  , orderingType
  , eitherType
    -- * All Types
  , preludeTypes
    -- * Type Lookup
  , lookupPreludeType
  , lookupPreludeConstructor
  , isPreludeType
  , isPreludeConstructor
    -- * Core Type Representations
  , toCoreTyCon
  , constructorType
  ) where

import Crisp.Core.Term (Type(..), Kind(..), EffectRow(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Type Definitions
--------------------------------------------------------------------------------

-- | A prelude type definition
data PreludeType = PreludeType
  { preludeTypeName    :: !Text              -- ^ Type name (e.g., "Option")
  , preludeTypeParams  :: ![(Text, Kind)]    -- ^ Type parameters with kinds
  , preludeTypeKind    :: !Kind              -- ^ Kind of the type
  , preludeTypeConstrs :: ![PreludeConstructor]  -- ^ Constructors
  } deriving stock (Eq, Show)

-- | A constructor for a prelude type
data PreludeConstructor = PreludeConstructor
  { constrName   :: !Text        -- ^ Constructor name (e.g., "Some")
  , constrParams :: ![Type]      -- ^ Parameter types
  , constrResult :: !Type        -- ^ Result type
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Standard Type Definitions
--------------------------------------------------------------------------------

-- | Unit type: Unit = ()
unitType :: PreludeType
unitType = PreludeType
  { preludeTypeName = "Unit"
  , preludeTypeParams = []
  , preludeTypeKind = KiType 0
  , preludeTypeConstrs =
      [ PreludeConstructor "Unit" [] (TyCon "Unit" [])
      ]
  }

-- | Bool type: Bool = True | False
boolType :: PreludeType
boolType = PreludeType
  { preludeTypeName = "Bool"
  , preludeTypeParams = []
  , preludeTypeKind = KiType 0
  , preludeTypeConstrs =
      [ PreludeConstructor "True" [] (TyCon "Bool" [])
      , PreludeConstructor "False" [] (TyCon "Bool" [])
      ]
  }

-- | Option type: Option A = None | Some A
optionType :: PreludeType
optionType = PreludeType
  { preludeTypeName = "Option"
  , preludeTypeParams = [("A", KiType 0)]
  , preludeTypeKind = KiArrow (KiType 0) (KiType 0)
  , preludeTypeConstrs =
      [ PreludeConstructor "None" [] (TyCon "Option" [TyVar "A" 0])
      , PreludeConstructor "Some" [TyVar "A" 0] (TyCon "Option" [TyVar "A" 0])
      ]
  }

-- | Result type: Result A E = Ok A | Err E
resultType :: PreludeType
resultType = PreludeType
  { preludeTypeName = "Result"
  , preludeTypeParams = [("A", KiType 0), ("E", KiType 0)]
  , preludeTypeKind = KiArrow (KiType 0) (KiArrow (KiType 0) (KiType 0))
  , preludeTypeConstrs =
      [ PreludeConstructor "Ok" [TyVar "A" 0] (TyCon "Result" [TyVar "A" 0, TyVar "E" 1])
      , PreludeConstructor "Err" [TyVar "E" 1] (TyCon "Result" [TyVar "A" 0, TyVar "E" 1])
      ]
  }

-- | List type: List A = Nil | Cons A (List A)
listType :: PreludeType
listType = PreludeType
  { preludeTypeName = "List"
  , preludeTypeParams = [("A", KiType 0)]
  , preludeTypeKind = KiArrow (KiType 0) (KiType 0)
  , preludeTypeConstrs =
      [ PreludeConstructor "Nil" [] (TyCon "List" [TyVar "A" 0])
      , PreludeConstructor "Cons" [TyVar "A" 0, TyCon "List" [TyVar "A" 0]] (TyCon "List" [TyVar "A" 0])
      ]
  }

-- | Pair type: Pair A B = Pair A B
pairType :: PreludeType
pairType = PreludeType
  { preludeTypeName = "Pair"
  , preludeTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , preludeTypeKind = KiArrow (KiType 0) (KiArrow (KiType 0) (KiType 0))
  , preludeTypeConstrs =
      [ PreludeConstructor "Pair" [TyVar "A" 0, TyVar "B" 1] (TyCon "Pair" [TyVar "A" 0, TyVar "B" 1])
      ]
  }

-- | Ordering type: Ordering = LT | EQ | GT
orderingType :: PreludeType
orderingType = PreludeType
  { preludeTypeName = "Ordering"
  , preludeTypeParams = []
  , preludeTypeKind = KiType 0
  , preludeTypeConstrs =
      [ PreludeConstructor "LT" [] (TyCon "Ordering" [])
      , PreludeConstructor "EQ" [] (TyCon "Ordering" [])
      , PreludeConstructor "GT" [] (TyCon "Ordering" [])
      ]
  }

-- | Either type: Either A B = Left A | Right B
eitherType :: PreludeType
eitherType = PreludeType
  { preludeTypeName = "Either"
  , preludeTypeParams = [("A", KiType 0), ("B", KiType 0)]
  , preludeTypeKind = KiArrow (KiType 0) (KiArrow (KiType 0) (KiType 0))
  , preludeTypeConstrs =
      [ PreludeConstructor "Left" [TyVar "A" 0] (TyCon "Either" [TyVar "A" 0, TyVar "B" 1])
      , PreludeConstructor "Right" [TyVar "B" 1] (TyCon "Either" [TyVar "A" 0, TyVar "B" 1])
      ]
  }

--------------------------------------------------------------------------------
-- All Prelude Types
--------------------------------------------------------------------------------

-- | All standard prelude types
preludeTypes :: [PreludeType]
preludeTypes =
  [ unitType
  , boolType
  , optionType
  , resultType
  , listType
  , pairType
  , orderingType
  , eitherType
  ]

--------------------------------------------------------------------------------
-- Type Lookup
--------------------------------------------------------------------------------

-- | Map of type names to definitions
preludeTypeMap :: Map Text PreludeType
preludeTypeMap = Map.fromList [(preludeTypeName t, t) | t <- preludeTypes]

-- | Map of constructor names to their definitions and parent types
preludeConstructorMap :: Map Text (PreludeConstructor, PreludeType)
preludeConstructorMap = Map.fromList
  [ (constrName c, (c, t))
  | t <- preludeTypes
  , c <- preludeTypeConstrs t
  ]

-- | Look up a prelude type by name
lookupPreludeType :: Text -> Maybe PreludeType
lookupPreludeType name = Map.lookup name preludeTypeMap

-- | Look up a prelude constructor by name
lookupPreludeConstructor :: Text -> Maybe (PreludeConstructor, PreludeType)
lookupPreludeConstructor name = Map.lookup name preludeConstructorMap

-- | Check if a name is a prelude type
isPreludeType :: Text -> Bool
isPreludeType name = Map.member name preludeTypeMap

-- | Check if a name is a prelude constructor
isPreludeConstructor :: Text -> Bool
isPreludeConstructor name = Map.member name preludeConstructorMap

--------------------------------------------------------------------------------
-- Core Type Representations
--------------------------------------------------------------------------------

-- | Convert a prelude type to a Core TyCon
toCoreTyCon :: PreludeType -> [Type] -> Type
toCoreTyCon pt args = TyCon (preludeTypeName pt) args

-- | Get the type of a constructor (as a function type)
constructorType :: PreludeConstructor -> Type
constructorType constr =
  foldr mkArrow (constrResult constr) (constrParams constr)
  where
    mkArrow param result = TyPi "_" param EffEmpty result
