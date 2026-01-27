{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Crisp.Manifest.Types
-- Description : Types for Crisp compilation manifest
--
-- Defines the data types for manifest generation:
-- - Manifest structure
-- - Compiler info
-- - Dependencies
-- - Effect info for capability extraction

module Crisp.Manifest.Types
  ( -- * Manifest Types
    Manifest(..)
  , CompilerInfo(..)
  , Dependency(..)
    -- * Effect Types
  , EffectInfo(..)
    -- * Manifest Construction
  , emptyManifest
  , manifestWithHashes
  , manifestWithCapabilities
  , manifestWithAuthorities
  , manifestWithDependencies
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Manifest Type
--------------------------------------------------------------------------------

-- | A Crisp compilation manifest
data Manifest = Manifest
  { manifestVersion      :: !Text                -- ^ Manifest format version
  , manifestModule       :: !Text                -- ^ Module name
  , manifestWasmHash     :: !Text                -- ^ SHA-256 hash of .wasm file
  , manifestTirHash      :: !Text                -- ^ SHA-256 hash of .tir.json file
  , manifestCompiler     :: !CompilerInfo        -- ^ Compiler information
  , manifestCapabilities :: ![Text]              -- ^ List of capabilities (effect:op)
  , manifestAuthorities  :: !(Map Text Text)     -- ^ Effect to authority namespace mapping
  , manifestDependencies :: ![Dependency]        -- ^ Module dependencies
  , manifestBuildTime    :: !(Maybe Text)        -- ^ Build timestamp (ISO 8601)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Manifest where
  toJSON m = object
    [ "version"      .= manifestVersion m
    , "module"       .= manifestModule m
    , "wasmHash"     .= manifestWasmHash m
    , "tirHash"      .= manifestTirHash m
    , "compiler"     .= manifestCompiler m
    , "capabilities" .= manifestCapabilities m
    , "authorities"  .= manifestAuthorities m
    , "dependencies" .= manifestDependencies m
    , "buildTime"    .= manifestBuildTime m
    ]

instance FromJSON Manifest where
  parseJSON = withObject "Manifest" $ \v -> Manifest
    <$> v .: "version"
    <*> v .: "module"
    <*> v .: "wasmHash"
    <*> v .: "tirHash"
    <*> v .: "compiler"
    <*> v .: "capabilities"
    <*> v .: "authorities"
    <*> v .: "dependencies"
    <*> v .:? "buildTime"

--------------------------------------------------------------------------------
-- Compiler Info
--------------------------------------------------------------------------------

-- | Compiler information
data CompilerInfo = CompilerInfo
  { compilerName    :: !Text    -- ^ Compiler name
  , compilerVersion :: !Text    -- ^ Compiler version
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CompilerInfo where
  toJSON c = object
    [ "name"    .= compilerName c
    , "version" .= compilerVersion c
    ]

instance FromJSON CompilerInfo where
  parseJSON = withObject "CompilerInfo" $ \v -> CompilerInfo
    <$> v .: "name"
    <*> v .: "version"

--------------------------------------------------------------------------------
-- Dependency
--------------------------------------------------------------------------------

-- | A module dependency
data Dependency = Dependency
  { dependencyName :: !Text    -- ^ Dependency module name
  , dependencyHash :: !Text    -- ^ Content hash of dependency
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Dependency where
  toJSON d = object
    [ "name" .= dependencyName d
    , "hash" .= dependencyHash d
    ]

instance FromJSON Dependency where
  parseJSON = withObject "Dependency" $ \v -> Dependency
    <$> v .: "name"
    <*> v .: "hash"

--------------------------------------------------------------------------------
-- Effect Info
--------------------------------------------------------------------------------

-- | Information about an effect declaration
data EffectInfo = EffectInfo
  { effectInfoName       :: !Text      -- ^ Effect name
  , effectInfoOperations :: ![Text]    -- ^ Operation names
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Manifest Construction
--------------------------------------------------------------------------------

-- | Create an empty manifest with default values
emptyManifest :: Text -> Manifest
emptyManifest modName = Manifest
  { manifestVersion = "1.0"
  , manifestModule = modName
  , manifestWasmHash = ""
  , manifestTirHash = ""
  , manifestCompiler = CompilerInfo "crisp" "0.1.0"
  , manifestCapabilities = []
  , manifestAuthorities = Map.empty
  , manifestDependencies = []
  , manifestBuildTime = Nothing
  }

-- | Create a manifest with wasm and tir hashes
manifestWithHashes :: Text -> Text -> Text -> Manifest
manifestWithHashes modName wasmHash tirHash =
  (emptyManifest modName)
    { manifestWasmHash = "sha256:" <> wasmHash
    , manifestTirHash = "sha256:" <> tirHash
    }

-- | Create a manifest with capabilities
manifestWithCapabilities :: Text -> [Text] -> Manifest
manifestWithCapabilities modName caps =
  (emptyManifest modName)
    { manifestCapabilities = caps
    }

-- | Create a manifest with authorities
manifestWithAuthorities :: Text -> Map Text Text -> Manifest
manifestWithAuthorities modName auths =
  (emptyManifest modName)
    { manifestAuthorities = auths
    }

-- | Create a manifest with dependencies
manifestWithDependencies :: Text -> [Dependency] -> Manifest
manifestWithDependencies modName deps =
  (emptyManifest modName)
    { manifestDependencies = deps
    }
