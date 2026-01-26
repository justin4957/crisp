{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.IR.TypedIR
-- Description : Typed Intermediate Representation
--
-- The auditable artifact that preserves all type, effect, and authority
-- information from the source program.

module Crisp.IR.TypedIR
  ( -- * Module
    TypedModule(..)
  , ModuleRequirements(..)
  , EffectRequirement(..)
  , ImportRequirement(..)
  , ModuleHashes(..)
    -- * Definitions
  , TypedDefinition(..)
  , TypedFunction(..)
  , TypedTypeParam(..)
  , TypedParam(..)
  , TypedTypeDef(..)
  , TypedConstructor(..)
  , TypedEffectDef(..)
  , TypedOperation(..)
  , TypedHandlerDef(..)
  , TypedHandlerClause(..)
    -- * Serialization
  , toJSON
  , encodeModule
    -- * Construction
  , newModule
  ) where

import Crisp.Core.Term

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | The complete Typed IR for a module
data TypedModule = TypedModule
  { tirFormat      :: !Text
  , tirVersion     :: !Text
  , tirModuleName  :: !Text
  , tirAuthority   :: !(Maybe Text)
  , tirRequires    :: !ModuleRequirements
  , tirDefinitions :: ![TypedDefinition]
  , tirHashes      :: !ModuleHashes
  } deriving stock (Eq, Show, Generic)

-- | Module requirements
data ModuleRequirements = ModuleRequirements
  { reqEffects :: ![EffectRequirement]
  , reqImports :: ![ImportRequirement]
  } deriving stock (Eq, Show, Generic)

-- | A required effect
data EffectRequirement = EffectRequirement
  { effReqEffect    :: !Text
  , effReqAuthority :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

-- | A required import
data ImportRequirement = ImportRequirement
  { impReqModule :: !Text
  , impReqItems  :: ![Text]
  } deriving stock (Eq, Show, Generic)

-- | Module content hashes
data ModuleHashes = ModuleHashes
  { hashSource     :: !Text
  , hashNormalized :: !Text
  } deriving stock (Eq, Show, Generic)

-- | A typed definition
data TypedDefinition
  = TirFn !TypedFunction
  | TirType !TypedTypeDef
  | TirEffect !TypedEffectDef
  | TirHandler !TypedHandlerDef
  deriving stock (Eq, Show, Generic)

-- | A typed function
data TypedFunction = TypedFunction
  { tfName        :: !Text
  , tfTypeParams  :: ![TypedTypeParam]
  , tfParams      :: ![TypedParam]
  , tfReturnType  :: !Type
  , tfEffects     :: !EffectRow
  , tfBody        :: !Term
  , tfAuthorities :: ![(Text, Maybe Text)]
  } deriving stock (Eq, Show, Generic)

-- | A typed type parameter
data TypedTypeParam
  = TirTypeParam !Text !Kind
  | TirDepParam !Text !Type
  deriving stock (Eq, Show, Generic)

-- | A typed value parameter
data TypedParam = TypedParam
  { tpName :: !Text
  , tpType :: !Type
  } deriving stock (Eq, Show, Generic)

-- | A typed type definition
data TypedTypeDef = TypedTypeDef
  { ttName         :: !Text
  , ttParams       :: ![TypedTypeParam]
  , ttKind         :: !Kind
  , ttConstructors :: ![TypedConstructor]
  , ttIsProp       :: !Bool
  , ttIsLinear     :: !Bool
  } deriving stock (Eq, Show, Generic)

-- | A typed constructor
data TypedConstructor = TypedConstructor
  { tcName       :: !Text
  , tcParamTypes :: ![Type]
  , tcReturnType :: !Type
  } deriving stock (Eq, Show, Generic)

-- | A typed effect definition
data TypedEffectDef = TypedEffectDef
  { teName       :: !Text
  , teOperations :: ![TypedOperation]
  } deriving stock (Eq, Show, Generic)

-- | A typed operation
data TypedOperation = TypedOperation
  { toName       :: !Text
  , toInputType  :: !Type
  , toOutputType :: !Type
  } deriving stock (Eq, Show, Generic)

-- | A typed handler definition
data TypedHandlerDef = TypedHandlerDef
  { thName              :: !Text
  , thParams            :: ![TypedParam]
  , thHandledEffect     :: !Text
  , thIntroducedEffects :: !EffectRow
  , thClauses           :: ![TypedHandlerClause]
  } deriving stock (Eq, Show, Generic)

-- | A typed handler clause
data TypedHandlerClause
  = TirOpClause !Text !Type !Type !Term
  | TirReturnClause !Type !Term
  deriving stock (Eq, Show, Generic)

-- * JSON Serialization

instance ToJSON TypedModule where
  toJSON m = object
    [ "format" .= tirFormat m
    , "version" .= tirVersion m
    , "module" .= tirModuleName m
    , "authority" .= tirAuthority m
    , "requires" .= tirRequires m
    , "definitions" .= tirDefinitions m
    , "hashes" .= tirHashes m
    ]

instance ToJSON ModuleRequirements where
  toJSON r = object
    [ "effects" .= reqEffects r
    , "imports" .= reqImports r
    ]

instance ToJSON EffectRequirement where
  toJSON e = object
    [ "effect" .= effReqEffect e
    , "authority" .= effReqAuthority e
    ]

instance ToJSON ImportRequirement where
  toJSON i = object
    [ "module" .= impReqModule i
    , "items" .= impReqItems i
    ]

instance ToJSON ModuleHashes where
  toJSON h = object
    [ "source" .= hashSource h
    , "normalized" .= hashNormalized h
    ]

instance ToJSON TypedDefinition where
  toJSON (TirFn f) = toJSON f
  toJSON (TirType t) = toJSON t
  toJSON (TirEffect e) = toJSON e
  toJSON (TirHandler h) = toJSON h

instance ToJSON TypedFunction where
  toJSON f = object
    [ "kind" .= ("function" :: Text)
    , "name" .= tfName f
    , "type_params" .= tfTypeParams f
    , "params" .= tfParams f
    , "return_type" .= typeToJSON (tfReturnType f)
    , "effects" .= effectRowToJSON (tfEffects f)
    , "body" .= termToJSON (tfBody f)
    , "authority_annotations" .= tfAuthorities f
    ]

instance ToJSON TypedTypeParam where
  toJSON (TirTypeParam name kind) = object
    [ "name" .= name
    , "kind" .= kindToJSON kind
    ]
  toJSON (TirDepParam name ty) = object
    [ "name" .= name
    , "type" .= typeToJSON ty
    ]

instance ToJSON TypedParam where
  toJSON p = object
    [ "name" .= tpName p
    , "type" .= typeToJSON (tpType p)
    ]

instance ToJSON TypedTypeDef where
  toJSON t = object
    [ "kind" .= ("type" :: Text)
    , "name" .= ttName t
    , "params" .= ttParams t
    , "type_kind" .= kindToJSON (ttKind t)
    , "constructors" .= ttConstructors t
    , "is_prop" .= ttIsProp t
    , "is_linear" .= ttIsLinear t
    ]

instance ToJSON TypedConstructor where
  toJSON c = object
    [ "name" .= tcName c
    , "param_types" .= map typeToJSON (tcParamTypes c)
    , "return_type" .= typeToJSON (tcReturnType c)
    ]

instance ToJSON TypedEffectDef where
  toJSON e = object
    [ "kind" .= ("effect" :: Text)
    , "name" .= teName e
    , "operations" .= teOperations e
    ]

instance ToJSON TypedOperation where
  toJSON o = object
    [ "name" .= toName o
    , "input_type" .= typeToJSON (toInputType o)
    , "output_type" .= typeToJSON (toOutputType o)
    ]

instance ToJSON TypedHandlerDef where
  toJSON h = object
    [ "kind" .= ("handler" :: Text)
    , "name" .= thName h
    , "params" .= thParams h
    , "handled_effect" .= thHandledEffect h
    , "introduced_effects" .= effectRowToJSON (thIntroducedEffects h)
    , "clauses" .= thClauses h
    ]

instance ToJSON TypedHandlerClause where
  toJSON (TirOpClause op pt rt body) = object
    [ "kind" .= ("operation" :: Text)
    , "operation" .= op
    , "param_type" .= typeToJSON pt
    , "resume_type" .= typeToJSON rt
    , "body" .= termToJSON body
    ]
  toJSON (TirReturnClause pt body) = object
    [ "kind" .= ("return" :: Text)
    , "param_type" .= typeToJSON pt
    , "body" .= termToJSON body
    ]

-- Helper functions for JSON encoding

typeToJSON :: Type -> Aeson.Value
typeToJSON = Aeson.String . typeToText

typeToText :: Type -> Text
typeToText (TyVar name _) = name
typeToText (TyCon name []) = name
typeToText (TyCon name args) = name <> " " <> unwords' (map typeToText args)
typeToText (TyPi _ from eff to) =
  "(" <> typeToText from <> ") ->[" <> effectRowToText eff <> "] " <> typeToText to
typeToText (TyForall name _ body) = "forall " <> name <> ". " <> typeToText body
typeToText (TyForallDep name ty body) =
  "forall (" <> name <> " : " <> typeToText ty <> "). " <> typeToText body
typeToText (TyLazy inner) = "Lazy " <> typeToText inner
typeToText (TyLinear inner) = "Linear " <> typeToText inner
typeToText (TyRef inner) = "ref " <> typeToText inner
typeToText (TyRefMut inner) = "ref mut " <> typeToText inner
typeToText (TyUniverse n) = "Type" <> if n == 0 then "" else showT n
typeToText TyProp = "Prop"

kindToJSON :: Kind -> Aeson.Value
kindToJSON = Aeson.String . kindToText

kindToText :: Kind -> Text
kindToText (KiType 0) = "Type"
kindToText (KiType n) = "Type" <> showT n
kindToText KiProp = "Prop"
kindToText KiLinear = "Linear"
kindToText (KiArrow k1 k2) = kindToText k1 <> " -> " <> kindToText k2

effectRowToJSON :: EffectRow -> Aeson.Value
effectRowToJSON = Aeson.String . effectRowToText

effectRowToText :: EffectRow -> Text
effectRowToText EffEmpty = "Pure"
effectRowToText (EffSet []) = "Pure"
effectRowToText (EffSet effs) = unwords' $ map effectToText' effs
effectRowToText (EffVar name _) = name
effectRowToText (EffUnion a b) = effectRowToText a <> ", " <> effectRowToText b

effectToText' :: Effect -> Text
effectToText' (Effect name Nothing) = name
effectToText' (Effect name (Just auth)) = name <> " @ " <> auth

termToJSON :: Term -> Aeson.Value
termToJSON = Aeson.String . termToText

termToText :: Term -> Text
termToText (TmVar name _) = name
termToText (TmLam name _ body) = "\\" <> name <> ". " <> termToText body
termToText (TmApp f x) = "(" <> termToText f <> " " <> termToText x <> ")"
termToText (TmLet name _ val body) =
  "let " <> name <> " = " <> termToText val <> " in " <> termToText body
termToText (TmTyAbs name _ body) = "/\\" <> name <> ". " <> termToText body
termToText (TmTyApp tm ty) = termToText tm <> " [" <> typeToText ty <> "]"
termToText (TmCon name _ args) = name <> " " <> unwords' (map termToText args)
termToText (TmMatch subj _ cases) =
  "match " <> termToText subj <> " { ... }"
termToText (TmPerform eff op arg) =
  "perform " <> eff <> "." <> op <> " " <> termToText arg
termToText (TmHandle _ body) = "handle { ... } " <> termToText body
termToText (TmLazy body) = "lazy " <> termToText body
termToText (TmForce tm) = "force " <> termToText tm
termToText (TmAnnot tm ty) = termToText tm <> " : " <> typeToText ty

unwords' :: [Text] -> Text
unwords' [] = ""
unwords' xs = foldr1 (\a b -> a <> " " <> b) xs

showT :: Show a => a -> Text
showT = Aeson.toJSON

-- | Encode a module to JSON
encodeModule :: TypedModule -> ByteString
encodeModule = Aeson.encodePretty

-- | Create a new typed module
newModule :: Text -> Maybe Text -> [TypedDefinition] -> TypedModule
newModule name auth defs = TypedModule
  { tirFormat = "crisp-tir"
  , tirVersion = "0.2.0"
  , tirModuleName = name
  , tirAuthority = auth
  , tirRequires = ModuleRequirements [] []
  , tirDefinitions = defs
  , tirHashes = ModuleHashes "" ""
  }
