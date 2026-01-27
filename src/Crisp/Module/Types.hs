{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Module.Types
-- Description : Module system types
--
-- Core data types for the Crisp module system including import declarations,
-- export lists, module paths, and visibility.

module Crisp.Module.Types
  ( -- * Module Paths
    ModPath(..)
  , renderModulePath
  , modulePathToFile
  , modulePathToInterfaceFile
  , fileToModulePath
    -- * Import Declarations
  , ImportDecl(..)
  , ImportForm(..)
  , importForm
  , importsName
  , hidesName
  , requiresQualifier
    -- * Export Declarations
  , ExportList(..)
  , Visibility(..)
  , visibility
  , isExported
    -- * Re-exports
  , ReExport(..)
  , ReExportForm(..)
    -- * Module Info
  , ModuleInfo(..)
  , TypeInfo(..)
  , KindInfo(..)
    -- * Interfaces
  , Interface(..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject, withText)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- =============================================================================
-- Module Paths
-- =============================================================================

-- | A module path (e.g., Core.Prelude or Treasury.Audit)
newtype ModPath = ModPath { modPathSegments :: [Text] }
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON ModPath where
  toJSON (ModPath segs) = toJSON segs

instance FromJSON ModPath where
  parseJSON v = ModPath <$> parseJSON v

-- | Render a module path as dotted text
renderModulePath :: ModPath -> Text
renderModulePath (ModPath segs) = T.intercalate "." segs

-- | Convert a module path to a file path
modulePathToFile :: ModPath -> FilePath
modulePathToFile (ModPath segs) =
  T.unpack (T.intercalate "/" segs) <> ".crisp"

-- | Convert a module path to an interface file path
modulePathToInterfaceFile :: ModPath -> FilePath
modulePathToInterfaceFile (ModPath segs) =
  T.unpack (T.intercalate "/" segs) <> ".crispi"

-- | Convert a file path to a module path
fileToModulePath :: FilePath -> Either Text ModPath
fileToModulePath path
  | ".crisp" `T.isSuffixOf` pathText =
      let base = T.dropEnd 6 pathText  -- Remove ".crisp"
          segs = T.splitOn "/" base
      in if all isValidSegment segs
         then Right (ModPath segs)
         else Left "Invalid module path segment"
  | otherwise = Left "File must have .crisp extension"
  where
    pathText = T.pack path
    isValidSegment seg = not (T.null seg) && isUpperHead seg
    isUpperHead t = case T.uncons t of
      Just (c, _) -> c >= 'A' && c <= 'Z'
      Nothing -> False

-- =============================================================================
-- Import Declarations
-- =============================================================================

-- | An import declaration
data ImportDecl = ImportDecl
  { importPath :: !ModPath
  , importFormData :: !ImportForm
  } deriving stock (Eq, Show, Generic)

-- | The form of an import
data ImportForm
  = Unqualified                      -- ^ import M (brings all names into scope)
  | Qualified !Text                  -- ^ import qualified M as X
  | Selective !(Set Text)            -- ^ import M (foo, bar)
  | Hiding !(Set Text)               -- ^ import M hiding (foo, bar)
  deriving stock (Eq, Show, Generic)

-- | Get a description of an import form
importForm :: ImportForm -> Text
importForm Unqualified = "unqualified"
importForm (Qualified alias) = "qualified as " <> alias
importForm (Selective names) = "selective (" <> T.intercalate ", " (Set.toList names) <> ")"
importForm (Hiding names) = "hiding (" <> T.intercalate ", " (Set.toList names) <> ")"

-- | Check if an import form imports a specific name
importsName :: ImportForm -> Text -> Bool
importsName Unqualified _ = True
importsName (Qualified _) _ = True  -- qualified imports still import the name
importsName (Selective names) name = Set.member name names
importsName (Hiding hidden) name = not (Set.member name hidden)

-- | Check if an import form hides a specific name
hidesName :: ImportForm -> Text -> Bool
hidesName (Hiding hidden) name = Set.member name hidden
hidesName _ _ = False

-- | Check if an import form requires qualified access
requiresQualifier :: ImportForm -> Bool
requiresQualifier (Qualified _) = True
requiresQualifier _ = False

-- =============================================================================
-- Export Declarations
-- =============================================================================

-- | An export list
data ExportList
  = ExportAll                        -- ^ Export everything (no export list)
  | ExportList !(Set Text)           -- ^ Export only specified names
  deriving stock (Eq, Show, Generic)

instance ToJSON ExportList where
  toJSON ExportAll = toJSON ("*" :: Text)
  toJSON (ExportList names) = toJSON names

instance FromJSON ExportList where
  parseJSON v = do
    result <- parseJSON v
    case result of
      ["*"] -> pure ExportAll
      names -> pure (ExportList (Set.fromList names))

-- | Visibility of a name
data Visibility = Public | Private
  deriving stock (Eq, Show, Generic)

-- | Check the visibility of a name given an export list
visibility :: ExportList -> Text -> Visibility
visibility ExportAll _ = Public
visibility (ExportList names) name
  | Set.member name names = Public
  | otherwise = Private

-- | Check if a name is exported
isExported :: ExportList -> Text -> Bool
isExported exports name = visibility exports name == Public

-- =============================================================================
-- Re-exports
-- =============================================================================

-- | A re-export declaration
data ReExport = ReExport
  { reExportPath :: !ModPath
  , reExportFormData :: !ReExportForm
  } deriving stock (Eq, Show, Generic)

-- | The form of a re-export
data ReExportForm
  = ReExportAll                      -- ^ export module M
  | ReExportSome !(Set Text)         -- ^ export M (foo, bar)
  deriving stock (Eq, Show, Generic)

-- =============================================================================
-- Module Info
-- =============================================================================

-- | Information about a compiled module
data ModuleInfo = ModuleInfo
  { modInfoPath      :: !ModPath
  , modInfoExports   :: !ExportList
  , modInfoTypes     :: !(Map Text TypeInfo)
  , modInfoFunctions :: !(Map Text Text)  -- name -> type signature
  } deriving stock (Eq, Show, Generic)

-- | Information about a type
data TypeInfo = TypeInfo
  { typeInfoName :: !Text
  , typeInfoKind :: !KindInfo
  } deriving stock (Eq, Show, Generic)

instance ToJSON TypeInfo where
  toJSON (TypeInfo name kind) = object
    [ "name" .= name
    , "kind" .= kind
    ]

instance FromJSON TypeInfo where
  parseJSON = withObject "TypeInfo" $ \o ->
    TypeInfo <$> o .: "name" <*> o .: "kind"

-- | Kind information for types
data KindInfo
  = KindType                         -- ^ Type (or Type_i)
  | KindProp                         -- ^ Prop
  | KindLinear                       -- ^ Linear
  | KindArrow !KindInfo !KindInfo    -- ^ k1 -> k2
  deriving stock (Eq, Show, Generic)

instance ToJSON KindInfo where
  toJSON KindType = toJSON ("Type" :: Text)
  toJSON KindProp = toJSON ("Prop" :: Text)
  toJSON KindLinear = toJSON ("Linear" :: Text)
  toJSON (KindArrow k1 k2) = object
    [ "arrow" .= object ["from" .= k1, "to" .= k2]
    ]

instance FromJSON KindInfo where
  parseJSON v = parseKindArrow v <|> parseSimpleKind v
    where
      parseSimpleKind = withText "KindInfo" $ \t ->
        case t of
          "Type" -> pure KindType
          "Prop" -> pure KindProp
          "Linear" -> pure KindLinear
          _ -> fail "Unknown kind"

      parseKindArrow = withObject "KindInfo" $ \o -> do
        arrow <- o .: "arrow"
        KindArrow <$> arrow .: "from" <*> arrow .: "to"

-- =============================================================================
-- Interfaces
-- =============================================================================

-- | A module interface (for separate compilation)
data Interface = Interface
  { interfacePath      :: !ModPath
  , interfaceExports   :: !(Set Text)
  , interfaceTypes     :: !(Map Text TypeInfo)
  , interfaceFunctions :: !(Map Text Text)
  } deriving stock (Eq, Show, Generic)

instance ToJSON Interface where
  toJSON iface = object
    [ "path" .= interfacePath iface
    , "exports" .= Set.toList (interfaceExports iface)
    , "types" .= interfaceTypes iface
    , "functions" .= interfaceFunctions iface
    ]

instance FromJSON Interface where
  parseJSON = withObject "Interface" $ \o -> do
    path <- o .: "path"
    exports <- o .: "exports"
    types <- o .: "types"
    functions <- o .: "functions"
    pure Interface
      { interfacePath = path
      , interfaceExports = Set.fromList exports
      , interfaceTypes = types
      , interfaceFunctions = functions
      }
