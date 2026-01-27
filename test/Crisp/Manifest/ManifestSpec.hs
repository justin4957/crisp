{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Manifest.ManifestSpec
-- Description : Tests for manifest generation and content hashing
--
-- TDD tests for Crisp compilation artifact manifests:
-- - SHA-256 content hashing
-- - Manifest structure and required fields
-- - Capability listing from effects
-- - Authority namespace mapping
-- - JSON serialization/deserialization
-- - Build metadata

module Crisp.Manifest.ManifestSpec (spec) where

import Test.Hspec

import Crisp.Manifest.Generate
import Crisp.Manifest.Hash
import Crisp.Manifest.Types

import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)

spec :: Spec
spec = do
  describe "Crisp Manifest Generation" $ do
    hashingTests
    manifestStructureTests
    capabilityTests
    authorityMappingTests
    dependencyTests
    jsonSerializationTests
    roundTripTests
    buildMetadataTests
    edgeCaseTests

--------------------------------------------------------------------------------
-- Content Hashing Tests
--------------------------------------------------------------------------------

hashingTests :: Spec
hashingTests = describe "content hashing" $ do
  it "computes SHA-256 hash of empty string" $ do
    hashContent "" `shouldBe`
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

  it "computes SHA-256 hash of 'hello'" $ do
    hashContent "hello" `shouldBe`
      "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"

  it "computes SHA-256 hash of 'hello world'" $ do
    hashContent "hello world" `shouldBe`
      "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"

  it "produces 64-character hex string" $ do
    let hash = hashContent "any content"
    T.length hash `shouldBe` 64

  it "produces only lowercase hex characters" $ do
    let hash = hashContent "test"
    T.all (`elem` ("0123456789abcdef" :: String)) hash `shouldBe` True

  it "hashes ByteString content" $ do
    let content = BS.pack [0x00, 0x61, 0x73, 0x6D]  -- Wasm magic
    hashByteString content `shouldBe`
      "cd5d4935a48c0672cb06407bb443bc0087aff947c6b864bac886982c73b3027f"

  it "same content produces same hash" $ do
    let hash1 = hashContent "deterministic"
    let hash2 = hashContent "deterministic"
    hash1 `shouldBe` hash2

  it "different content produces different hash" $ do
    let hash1 = hashContent "content1"
    let hash2 = hashContent "content2"
    hash1 `shouldNotBe` hash2

  it "formats hash with sha256 prefix" $ do
    formatHash (hashContent "test") `shouldBe`
      "sha256:9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"

  it "parses hash with sha256 prefix" $ do
    let formatted = "sha256:9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
    parseHash formatted `shouldBe` Just "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"

  it "returns Nothing for invalid hash format" $ do
    parseHash "invalid" `shouldBe` Nothing
    parseHash "md5:abc123" `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Manifest Structure Tests
--------------------------------------------------------------------------------

manifestStructureTests :: Spec
manifestStructureTests = describe "manifest structure" $ do
  it "includes version field" $ do
    let manifest = emptyManifest "test_module"
    manifestVersion manifest `shouldBe` "1.0"

  it "includes module name" $ do
    let manifest = emptyManifest "my_module"
    manifestModule manifest `shouldBe` "my_module"

  it "includes wasm hash field" $ do
    let manifest = manifestWithHashes "test" "wasm_hash" "tir_hash"
    manifestWasmHash manifest `shouldBe` "sha256:wasm_hash"

  it "includes tir hash field" $ do
    let manifest = manifestWithHashes "test" "wasm_hash" "tir_hash"
    manifestTirHash manifest `shouldBe` "sha256:tir_hash"

  it "includes compiler info" $ do
    let manifest = emptyManifest "test"
    compilerName (manifestCompiler manifest) `shouldBe` "crisp"

  it "includes compiler version" $ do
    let manifest = emptyManifest "test"
    compilerVersion (manifestCompiler manifest) `shouldBe` "0.1.0"

  it "includes empty capabilities by default" $ do
    let manifest = emptyManifest "test"
    manifestCapabilities manifest `shouldBe` []

  it "includes empty authorities by default" $ do
    let manifest = emptyManifest "test"
    manifestAuthorities manifest `shouldBe` Map.empty

  it "includes empty dependencies by default" $ do
    let manifest = emptyManifest "test"
    manifestDependencies manifest `shouldBe` []

  it "includes build time when generated with time" $ do
    manifest <- generateManifestWithTime "test"
    manifestBuildTime manifest `shouldSatisfy` isJust

--------------------------------------------------------------------------------
-- Capability Tests
--------------------------------------------------------------------------------

capabilityTests :: Spec
capabilityTests = describe "capabilities" $ do
  it "lists single capability" $ do
    let manifest = manifestWithCapabilities "test" ["console:print"]
    manifestCapabilities manifest `shouldBe` ["console:print"]

  it "lists multiple capabilities" $ do
    let caps = ["console:print", "console:read", "fs:read"]
    let manifest = manifestWithCapabilities "test" caps
    manifestCapabilities manifest `shouldBe` caps

  it "preserves capability order" $ do
    let caps = ["z:last", "a:first", "m:middle"]
    let manifest = manifestWithCapabilities "test" caps
    manifestCapabilities manifest `shouldBe` caps

  it "allows duplicate capabilities" $ do
    let caps = ["console:print", "console:print"]
    let manifest = manifestWithCapabilities "test" caps
    length (manifestCapabilities manifest) `shouldBe` 2

  it "extracts capabilities from effects" $ do
    let effects = [EffectInfo "Console" ["print", "read"], EffectInfo "FileSystem" ["read"]]
    extractCapabilities effects `shouldBe` ["console:print", "console:read", "filesystem:read"]

  it "normalizes effect names to lowercase" $ do
    let effects = [EffectInfo "CONSOLE" ["PRINT"]]
    extractCapabilities effects `shouldBe` ["console:print"]

  it "handles effect with no operations" $ do
    let effects = [EffectInfo "Empty" []]
    extractCapabilities effects `shouldBe` []

--------------------------------------------------------------------------------
-- Authority Mapping Tests
--------------------------------------------------------------------------------

authorityMappingTests :: Spec
authorityMappingTests = describe "authority mapping" $ do
  it "maps single effect to authority" $ do
    let auths = Map.fromList [("Console", "system:console")]
    let manifest = manifestWithAuthorities "test" auths
    Map.lookup "Console" (manifestAuthorities manifest) `shouldBe` Just "system:console"

  it "maps multiple effects to authorities" $ do
    let auths = Map.fromList
          [ ("Console", "system:console")
          , ("FileSystem", "system:fs")
          , ("Network", "system:net")
          ]
    let manifest = manifestWithAuthorities "test" auths
    Map.size (manifestAuthorities manifest) `shouldBe` 3

  it "creates authority from effect declaration" $ do
    let effect = EffectInfo "Console" ["print"]
    let authority = effectToAuthority effect "system:console"
    authority `shouldBe` ("Console", "system:console")

  it "builds authority map from effect list" $ do
    let effects =
          [ (EffectInfo "Console" ["print"], "system:console")
          , (EffectInfo "FileSystem" ["read"], "system:fs")
          ]
    let auths = buildAuthorityMap effects
    Map.lookup "Console" auths `shouldBe` Just "system:console"
    Map.lookup "FileSystem" auths `shouldBe` Just "system:fs"

  it "handles missing authority gracefully" $ do
    let manifest = emptyManifest "test"
    Map.lookup "Unknown" (manifestAuthorities manifest) `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Dependency Tests
--------------------------------------------------------------------------------

dependencyTests :: Spec
dependencyTests = describe "dependencies" $ do
  it "records single dependency" $ do
    let dep = Dependency "core" "sha256:abc123"
    let manifest = manifestWithDependencies "test" [dep]
    length (manifestDependencies manifest) `shouldBe` 1

  it "records multiple dependencies" $ do
    let deps =
          [ Dependency "core" "sha256:abc123"
          , Dependency "std" "sha256:def456"
          ]
    let manifest = manifestWithDependencies "test" deps
    length (manifestDependencies manifest) `shouldBe` 2

  it "preserves dependency order" $ do
    let deps =
          [ Dependency "z_last" "sha256:111"
          , Dependency "a_first" "sha256:222"
          ]
    let manifest = manifestWithDependencies "test" deps
    map dependencyName (manifestDependencies manifest) `shouldBe` ["z_last", "a_first"]

  it "includes dependency name" $ do
    let dep = Dependency "mylib" "sha256:abc"
    dependencyName dep `shouldBe` "mylib"

  it "includes dependency hash" $ do
    let dep = Dependency "mylib" "sha256:abc123"
    dependencyHash dep `shouldBe` "sha256:abc123"

  it "creates dependency from module info" $ do
    let dep = createDependency "stdlib" "content_to_hash"
    dependencyName dep `shouldBe` "stdlib"
    T.isPrefixOf "sha256:" (dependencyHash dep) `shouldBe` True

--------------------------------------------------------------------------------
-- JSON Serialization Tests
--------------------------------------------------------------------------------

jsonSerializationTests :: Spec
jsonSerializationTests = describe "JSON serialization" $ do
  it "encodes manifest to JSON" $ do
    let manifest = emptyManifest "test"
    let json = encode manifest
    LBS.length json `shouldSatisfy` (> 0)

  it "decodes manifest from JSON" $ do
    let manifest = emptyManifest "test"
    let json = encode manifest
    let decoded = decode json :: Maybe Manifest
    decoded `shouldSatisfy` isJust

  it "includes version in JSON" $ do
    let manifest = emptyManifest "test"
    let json = encode manifest
    LBS.toStrict json `shouldSatisfy` BS.isInfixOf "\"version\":\"1.0\""

  it "includes module in JSON" $ do
    let manifest = emptyManifest "my_module"
    let json = encode manifest
    LBS.toStrict json `shouldSatisfy` BS.isInfixOf "\"module\":\"my_module\""

  it "includes capabilities array in JSON" $ do
    let manifest = manifestWithCapabilities "test" ["cap1", "cap2"]
    let json = encode manifest
    LBS.toStrict json `shouldSatisfy` BS.isInfixOf "\"capabilities\""

  it "includes authorities object in JSON" $ do
    let auths = Map.fromList [("Effect", "auth:ns")]
    let manifest = manifestWithAuthorities "test" auths
    let json = encode manifest
    LBS.toStrict json `shouldSatisfy` BS.isInfixOf "\"authorities\""

  it "includes compiler object in JSON" $ do
    let manifest = emptyManifest "test"
    let json = encode manifest
    LBS.toStrict json `shouldSatisfy` BS.isInfixOf "\"compiler\""

  it "includes dependencies array in JSON" $ do
    let deps = [Dependency "dep1" "sha256:abc"]
    let manifest = manifestWithDependencies "test" deps
    let json = encode manifest
    LBS.toStrict json `shouldSatisfy` BS.isInfixOf "\"dependencies\""

  it "includes buildTime in JSON" $ do
    let manifest = emptyManifest "test"
    let json = encode manifest
    LBS.toStrict json `shouldSatisfy` BS.isInfixOf "\"buildTime\""

  it "produces valid JSON" $ do
    let manifest = emptyManifest "test"
    let json = encode manifest
    let result = eitherDecode json :: Either String Manifest
    result `shouldSatisfy` isRight
    where
      isRight (Right _) = True
      isRight (Left _) = False

--------------------------------------------------------------------------------
-- Round-Trip Tests
--------------------------------------------------------------------------------

roundTripTests :: Spec
roundTripTests = describe "round-trip serialization" $ do
  it "round-trips empty manifest" $ do
    let manifest = emptyManifest "test"
    let json = encode manifest
    let decoded = decode json :: Maybe Manifest
    decoded `shouldBe` Just manifest

  it "round-trips manifest with hashes" $ do
    let manifest = manifestWithHashes "test" "abc123" "def456"
    let json = encode manifest
    let decoded = decode json :: Maybe Manifest
    fmap manifestWasmHash decoded `shouldBe` Just "sha256:abc123"

  it "round-trips manifest with capabilities" $ do
    let caps = ["cap1", "cap2", "cap3"]
    let manifest = manifestWithCapabilities "test" caps
    let json = encode manifest
    let decoded = decode json :: Maybe Manifest
    fmap manifestCapabilities decoded `shouldBe` Just caps

  it "round-trips manifest with authorities" $ do
    let auths = Map.fromList [("E1", "a:1"), ("E2", "a:2")]
    let manifest = manifestWithAuthorities "test" auths
    let json = encode manifest
    let decoded = decode json :: Maybe Manifest
    fmap manifestAuthorities decoded `shouldBe` Just auths

  it "round-trips manifest with dependencies" $ do
    let deps = [Dependency "d1" "sha256:h1", Dependency "d2" "sha256:h2"]
    let manifest = manifestWithDependencies "test" deps
    let json = encode manifest
    let decoded = decode json :: Maybe Manifest
    fmap manifestDependencies decoded `shouldBe` Just deps

  it "round-trips full manifest" $ do
    let manifest = Manifest
          { manifestVersion = "1.0"
          , manifestModule = "full_test"
          , manifestWasmHash = "sha256:wasm_hash"
          , manifestTirHash = "sha256:tir_hash"
          , manifestCompiler = CompilerInfo "crisp" "0.1.0"
          , manifestCapabilities = ["cap1", "cap2"]
          , manifestAuthorities = Map.fromList [("E1", "a:1")]
          , manifestDependencies = [Dependency "dep" "sha256:dep_hash"]
          , manifestBuildTime = Nothing
          }
    let json = encode manifest
    let decoded = decode json :: Maybe Manifest
    decoded `shouldBe` Just manifest

--------------------------------------------------------------------------------
-- Build Metadata Tests
--------------------------------------------------------------------------------

buildMetadataTests :: Spec
buildMetadataTests = describe "build metadata" $ do
  it "sets current time as build time" $ do
    manifest <- generateManifestWithTime "test"
    manifestBuildTime manifest `shouldSatisfy` isJust

  it "formats build time as ISO 8601" $ do
    manifest <- generateManifestWithTime "test"
    let timeStr = fromJust (manifestBuildTime manifest)
    -- Should be parseable as ISO 8601
    let parsed = iso8601ParseM (T.unpack timeStr) :: Maybe UTCTime
    parsed `shouldSatisfy` isJust

  it "includes compiler name" $ do
    let manifest = emptyManifest "test"
    compilerName (manifestCompiler manifest) `shouldBe` "crisp"

  it "includes compiler version" $ do
    let manifest = emptyManifest "test"
    compilerVersion (manifestCompiler manifest) `shouldBe` "0.1.0"

--------------------------------------------------------------------------------
-- Edge Cases
--------------------------------------------------------------------------------

edgeCaseTests :: Spec
edgeCaseTests = describe "edge cases" $ do
  it "handles empty module name" $ do
    let manifest = emptyManifest ""
    manifestModule manifest `shouldBe` ""

  it "handles special characters in module name" $ do
    let manifest = emptyManifest "my-module_v2.0"
    manifestModule manifest `shouldBe` "my-module_v2.0"

  it "handles unicode in module name" $ do
    let manifest = emptyManifest "模块"
    manifestModule manifest `shouldBe` "模块"

  it "handles very long capability list" $ do
    let caps = ["cap" <> T.pack (show i) | i <- [1..100]]
    let manifest = manifestWithCapabilities "test" caps
    length (manifestCapabilities manifest) `shouldBe` 100

  it "handles empty hash" $ do
    let manifest = manifestWithHashes "test" "" ""
    manifestWasmHash manifest `shouldBe` "sha256:"

  it "handles hash with special characters" $ do
    -- Valid hex hash
    let hash = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    let manifest = manifestWithHashes "test" hash hash
    manifestWasmHash manifest `shouldBe` ("sha256:" <> hash)

  it "generates consistent manifest for same input" $ do
    let m1 = emptyManifest "test"
    let m2 = emptyManifest "test"
    -- Only compare deterministic fields (not build time)
    manifestVersion m1 `shouldBe` manifestVersion m2
    manifestModule m1 `shouldBe` manifestModule m2
    manifestCompiler m1 `shouldBe` manifestCompiler m2

  it "handles null build time in JSON" $ do
    let manifest = Manifest
          { manifestVersion = "1.0"
          , manifestModule = "test"
          , manifestWasmHash = ""
          , manifestTirHash = ""
          , manifestCompiler = CompilerInfo "crisp" "0.1.0"
          , manifestCapabilities = []
          , manifestAuthorities = Map.empty
          , manifestDependencies = []
          , manifestBuildTime = Nothing
          }
    let json = encode manifest
    let decoded = decode json :: Maybe Manifest
    decoded `shouldBe` Just manifest
