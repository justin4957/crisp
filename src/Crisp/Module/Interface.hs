{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Module.Interface
-- Description : Interface file handling
--
-- Functions for generating, serializing, and deserializing module
-- interface files (.crispi) for separate compilation.

module Crisp.Module.Interface
  ( -- * Interface Generation
    generateInterface
    -- * Serialization
  , serializeInterface
  , deserializeInterface
    -- * Interface Operations
  , mergeInterfaces
  , lookupInInterface
  , interfaceProvides
  ) where

import Crisp.Module.Types

import Data.Aeson (encode, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- =============================================================================
-- Interface Generation
-- =============================================================================

-- | Generate an interface from module info
generateInterface :: ModuleInfo -> Interface
generateInterface modInfo = Interface
  { interfacePath = modInfoPath modInfo
  , interfaceExports = exportedNames
  , interfaceTypes = filteredTypes
  , interfaceFunctions = filteredFunctions
  }
  where
    -- Get the set of exported names
    exportedNames = case modInfoExports modInfo of
      ExportAll -> allNames
      ExportList names -> names

    -- All defined names
    allNames = Set.fromList $
      Map.keys (modInfoTypes modInfo) ++
      Map.keys (modInfoFunctions modInfo)

    -- Filter types and functions by exports
    filteredTypes = Map.filterWithKey (\k _ -> Set.member k exportedNames) (modInfoTypes modInfo)
    filteredFunctions = Map.filterWithKey (\k _ -> Set.member k exportedNames) (modInfoFunctions modInfo)

-- =============================================================================
-- Serialization
-- =============================================================================

-- | Serialize an interface to JSON text
serializeInterface :: Interface -> Either Text Text
serializeInterface iface =
  Right $ TE.decodeUtf8 $ LBS.toStrict $ encodePretty iface

-- | Deserialize an interface from JSON text
deserializeInterface :: Text -> Either Text Interface
deserializeInterface txt =
  case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 txt) of
    Left err -> Left (T.pack err)
    Right iface -> Right iface

-- =============================================================================
-- Interface Operations
-- =============================================================================

-- | Merge multiple interfaces into a single namespace
mergeInterfaces :: [Interface] -> Map Text (ModPath, Text)
mergeInterfaces ifaces =
  Map.unions $ map interfaceToMap ifaces
  where
    interfaceToMap iface =
      let path = interfacePath iface
      in Map.fromList
           [(name, (path, sig)) | (name, sig) <- Map.toList (interfaceFunctions iface)]

-- | Look up a name in an interface
lookupInInterface :: Interface -> Text -> Maybe Text
lookupInInterface iface name
  | Set.member name (interfaceExports iface) =
      Map.lookup name (interfaceFunctions iface)
  | otherwise = Nothing

-- | Check if an interface provides a specific name
interfaceProvides :: Interface -> Text -> Bool
interfaceProvides iface name = Set.member name (interfaceExports iface)
