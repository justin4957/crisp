{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.FFI.Http
-- Description : FFI bindings for HTTP client operations
--
-- Provides foreign function interface bindings for making HTTP requests
-- to external APIs. Designed for LexSim's legal data integrations
-- (CourtListener, legislation.gov.uk, EUR-Lex, etc.).
--
-- == Design Philosophy
--
-- HTTP operations are essential for integrating with external legal data
-- sources. This module provides the minimal set of primitives needed for:
--
-- * HTTP GET requests (fetching resources)
-- * HTTP POST requests (submitting data)
-- * HTTP PUT requests (updating resources)
-- * HTTP DELETE requests (removing resources)
--
-- Higher-level API clients should be built on top of these primitives
-- in user-space libraries.
--
-- == WebAssembly Integration
--
-- These functions map to host-provided imports:
--
-- @
-- (import "http" "get" (func $http_get (param i32 i32 i32 i32) (result i32)))
-- (import "http" "post" (func $http_post (param i32 i32 i32 i32 i32 i32) (result i32)))
-- (import "http" "put" (func $http_put (param i32 i32 i32 i32 i32 i32) (result i32)))
-- (import "http" "delete" (func $http_delete (param i32 i32 i32 i32) (result i32)))
-- @
--
-- == Response Format
--
-- All HTTP functions return a JSON string with the structure:
--
-- @
-- {
--   "status": 200,
--   "headers": {"Content-Type": "application/json"},
--   "body": "..."
-- }
-- @
--
-- Or on error:
--
-- @
-- {
--   "error": "NetworkError",
--   "message": "Connection refused"
-- }
-- @

module Crisp.FFI.Http
  ( -- * HTTP Types
    HttpMethod(..)
  , HttpResponse(..)
  , HttpError(..)
    -- * External Function Definitions
  , httpExternals
  , httpGetExternal
  , httpPostExternal
  , httpPutExternal
  , httpDeleteExternal
    -- * Lookup
  , lookupHttpExternal
  , isHttpExternal
  , allHttpExternals
    -- * Type Helpers
  , httpResponseType
  , urlType
  , headersType
  , bodyType
  ) where

import Crisp.Types.Context (ExternalInfo(..))
import Crisp.Core.Term (Type(..), EffectRow(..), simpleType, pureFnType)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

--------------------------------------------------------------------------------
-- HTTP Types
--------------------------------------------------------------------------------

-- | HTTP request methods
data HttpMethod
  = HttpGet
  | HttpPost
  | HttpPut
  | HttpDelete
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | HTTP response representation
data HttpResponse = HttpResponse
  { httpResponseStatus  :: !Int           -- ^ HTTP status code (e.g., 200, 404)
  , httpResponseHeaders :: ![(Text, Text)] -- ^ Response headers
  , httpResponseBody    :: !Text          -- ^ Response body content
  } deriving stock (Eq, Show)

-- | HTTP error types
data HttpError
  = NetworkError !Text         -- ^ Network connectivity error
  | TimeoutError               -- ^ Request timed out
  | TlsError !Text             -- ^ TLS/SSL error
  | InvalidUrl !Text           -- ^ Malformed URL
  | HttpStatusError !Int !Text -- ^ Non-success HTTP status
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Type Helpers
--------------------------------------------------------------------------------

-- | String type (used for URLs, headers, bodies)
stringType :: Type
stringType = simpleType "String"

-- | Unit type
unitType :: Type
unitType = simpleType "Unit"

-- | URL type alias (String representing a URL)
urlType :: Type
urlType = stringType

-- | Headers type alias (String, JSON-encoded headers)
headersType :: Type
headersType = stringType

-- | Body type alias (String, request/response body)
bodyType :: Type
bodyType = stringType

-- | HTTP response type alias (String, JSON-encoded response)
httpResponseType :: Type
httpResponseType = stringType

--------------------------------------------------------------------------------
-- External Function Definitions
--------------------------------------------------------------------------------

-- | HTTP GET request
--
-- @
-- external fn http_get(url: String, headers: String) -> String = ("http", "get")
-- @
--
-- Performs an HTTP GET request to the specified URL with optional headers.
-- Headers are passed as a JSON-encoded string: @{"Authorization": "Bearer token"}@
-- Returns a JSON-encoded response with status, headers, and body.
httpGetExternal :: ExternalInfo
httpGetExternal = ExternalInfo
  { externalInfoName     = "http_get"
  , externalInfoModule   = "http"
  , externalInfoFunction = "get"
  , externalInfoType     = TyPi "url" stringType EffEmpty
                             (TyPi "headers" stringType EffEmpty stringType)
  }

-- | HTTP POST request
--
-- @
-- external fn http_post(url: String, body: String, headers: String) -> String = ("http", "post")
-- @
--
-- Performs an HTTP POST request to the specified URL with body and headers.
-- Body is the raw request body (typically JSON).
-- Headers are passed as a JSON-encoded string.
-- Returns a JSON-encoded response with status, headers, and body.
httpPostExternal :: ExternalInfo
httpPostExternal = ExternalInfo
  { externalInfoName     = "http_post"
  , externalInfoModule   = "http"
  , externalInfoFunction = "post"
  , externalInfoType     = TyPi "url" stringType EffEmpty
                             (TyPi "body" stringType EffEmpty
                               (TyPi "headers" stringType EffEmpty stringType))
  }

-- | HTTP PUT request
--
-- @
-- external fn http_put(url: String, body: String, headers: String) -> String = ("http", "put")
-- @
--
-- Performs an HTTP PUT request to the specified URL with body and headers.
-- Used for updating existing resources.
-- Returns a JSON-encoded response with status, headers, and body.
httpPutExternal :: ExternalInfo
httpPutExternal = ExternalInfo
  { externalInfoName     = "http_put"
  , externalInfoModule   = "http"
  , externalInfoFunction = "put"
  , externalInfoType     = TyPi "url" stringType EffEmpty
                             (TyPi "body" stringType EffEmpty
                               (TyPi "headers" stringType EffEmpty stringType))
  }

-- | HTTP DELETE request
--
-- @
-- external fn http_delete(url: String, headers: String) -> String = ("http", "delete")
-- @
--
-- Performs an HTTP DELETE request to the specified URL with optional headers.
-- Used for deleting resources.
-- Returns a JSON-encoded response with status, headers, and body.
httpDeleteExternal :: ExternalInfo
httpDeleteExternal = ExternalInfo
  { externalInfoName     = "http_delete"
  , externalInfoModule   = "http"
  , externalInfoFunction = "delete"
  , externalInfoType     = TyPi "url" stringType EffEmpty
                             (TyPi "headers" stringType EffEmpty stringType)
  }

--------------------------------------------------------------------------------
-- All HTTP Externals
--------------------------------------------------------------------------------

-- | All HTTP-related external function definitions
httpExternals :: [ExternalInfo]
httpExternals =
  [ httpGetExternal
  , httpPostExternal
  , httpPutExternal
  , httpDeleteExternal
  ]

--------------------------------------------------------------------------------
-- Lookup Functions
--------------------------------------------------------------------------------

-- | Map of HTTP external names to their definitions
httpExternalMap :: Map Text ExternalInfo
httpExternalMap = Map.fromList
  [(externalInfoName ext, ext) | ext <- httpExternals]

-- | Look up an HTTP external by name
lookupHttpExternal :: Text -> Maybe ExternalInfo
lookupHttpExternal name = Map.lookup name httpExternalMap

-- | Check if a name is an HTTP external
isHttpExternal :: Text -> Bool
isHttpExternal name = Map.member name httpExternalMap

-- | Get all HTTP external definitions
allHttpExternals :: [ExternalInfo]
allHttpExternals = httpExternals
