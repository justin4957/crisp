{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Manifest.Generate
-- Description : Manifest generation for Crisp compilation artifacts
--
-- Generates manifests for Crisp compilation:
-- - Content hashing of .wasm and .tir.json files
-- - Capability extraction from effects
-- - Authority namespace mapping
-- - Dependency tracking
-- - Build metadata

module Crisp.Manifest.Generate
  ( -- * Manifest Generation
    generateManifest
  , generateManifestWithTime
  , generateManifestFromFiles
    -- * Capability Extraction
  , extractCapabilities
    -- * Authority Mapping
  , effectToAuthority
  , buildAuthorityMap
    -- * Dependency Creation
  , createDependency
    -- * Re-exports
  , module Crisp.Manifest.Types
  , module Crisp.Manifest.Hash
  ) where

import Crisp.Manifest.Types
import Crisp.Manifest.Hash

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.ByteString as BS

--------------------------------------------------------------------------------
-- Manifest Generation
--------------------------------------------------------------------------------

-- | Generate a manifest for a module
generateManifest :: Text         -- ^ Module name
                 -> Text         -- ^ Wasm content hash
                 -> Text         -- ^ TIR content hash
                 -> [Text]       -- ^ Capabilities
                 -> Map Text Text -- ^ Authorities
                 -> [Dependency] -- ^ Dependencies
                 -> Manifest
generateManifest modName wasmHash tirHash caps auths deps = Manifest
  { manifestVersion = "1.0"
  , manifestModule = modName
  , manifestWasmHash = formatHash wasmHash
  , manifestTirHash = formatHash tirHash
  , manifestCompiler = CompilerInfo "crisp" "0.1.0"
  , manifestCapabilities = caps
  , manifestAuthorities = auths
  , manifestDependencies = deps
  , manifestBuildTime = Nothing
  }

-- | Generate a manifest with current time as build time
generateManifestWithTime :: Text -> IO Manifest
generateManifestWithTime modName = do
  now <- getCurrentTime
  let timeStr = T.pack (iso8601Show now)
  pure $ (emptyManifest modName)
    { manifestBuildTime = Just timeStr
    }

-- | Generate a manifest from actual files
generateManifestFromFiles :: Text         -- ^ Module name
                          -> FilePath     -- ^ Path to .wasm file
                          -> FilePath     -- ^ Path to .tir.json file
                          -> [EffectInfo] -- ^ Effects used
                          -> [(EffectInfo, Text)] -- ^ Effect to authority mappings
                          -> [Dependency] -- ^ Dependencies
                          -> IO Manifest
generateManifestFromFiles modName wasmPath tirPath effects authMappings deps = do
  -- Hash the files
  wasmHash <- hashFile wasmPath
  tirHash <- hashFile tirPath

  -- Get current time
  now <- getCurrentTime
  let timeStr = T.pack (iso8601Show now)

  -- Extract capabilities and authorities
  let caps = extractCapabilities effects
  let auths = buildAuthorityMap authMappings

  pure $ Manifest
    { manifestVersion = "1.0"
    , manifestModule = modName
    , manifestWasmHash = formatHash wasmHash
    , manifestTirHash = formatHash tirHash
    , manifestCompiler = CompilerInfo "crisp" "0.1.0"
    , manifestCapabilities = caps
    , manifestAuthorities = auths
    , manifestDependencies = deps
    , manifestBuildTime = Just timeStr
    }

--------------------------------------------------------------------------------
-- Capability Extraction
--------------------------------------------------------------------------------

-- | Extract capabilities from a list of effects
-- Each capability is formatted as "effect:operation" (lowercase)
extractCapabilities :: [EffectInfo] -> [Text]
extractCapabilities = concatMap effectToCaps
  where
    effectToCaps (EffectInfo name ops) =
      let effectName = T.toLower name
      in map (\op -> effectName <> ":" <> T.toLower op) ops

--------------------------------------------------------------------------------
-- Authority Mapping
--------------------------------------------------------------------------------

-- | Create an authority mapping from an effect
effectToAuthority :: EffectInfo -> Text -> (Text, Text)
effectToAuthority effect authority = (effectInfoName effect, authority)

-- | Build a map from effects to authority namespaces
buildAuthorityMap :: [(EffectInfo, Text)] -> Map Text Text
buildAuthorityMap = Map.fromList . map (uncurry effectToAuthority)

--------------------------------------------------------------------------------
-- Dependency Creation
--------------------------------------------------------------------------------

-- | Create a dependency with a hash of its content
createDependency :: Text -> Text -> Dependency
createDependency name content = Dependency
  { dependencyName = name
  , dependencyHash = formatHash (hashContent content)
  }
