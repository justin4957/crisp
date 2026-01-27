{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.FFI.Time
-- Description : FFI bindings for system time operations
--
-- Provides foreign function interface bindings for accessing system time,
-- enabling timestamp generation, duration calculations, and time formatting
-- in Crisp programs.
--
-- == Design Philosophy
--
-- Time operations are fundamental for practical applications. This module
-- provides the minimal set of primitives needed for:
--
-- * Getting current system time (milliseconds since Unix epoch)
-- * Getting system timezone
-- * Formatting timestamps to ISO 8601
-- * Computing durations
--
-- Higher-level time abstractions (calendars, time zones, etc.) should be
-- built on top of these primitives in user-space libraries.
--
-- == WebAssembly Integration
--
-- These functions map to WASI or host-provided imports:
--
-- @
-- (import "system" "time_ms" (func $time_ms (result i64)))
-- (import "system" "timezone" (func $timezone (result i32)))  -- returns string ptr
-- (import "system" "format_iso8601" (func $format_iso8601 (param i64) (result i32)))
-- @

module Crisp.FFI.Time
  ( -- * Time Types
    TimeInfo(..)
  , Duration(..)
    -- * External Function Definitions
  , timeExternals
  , timeMsExternal
  , timezoneExternal
  , formatIso8601External
  , monotonicMsExternal
    -- * Lookup
  , lookupTimeExternal
  , isTimeExternal
  , allTimeExternals
    -- * Type Helpers
  , timestampType
  , durationMsType
  ) where

import Crisp.Types.Context (ExternalInfo(..))
import Crisp.Core.Term (Type(..), EffectRow(..), simpleType, pureFnType)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Time Types
--------------------------------------------------------------------------------

-- | Information about a timestamp
data TimeInfo = TimeInfo
  { timeUnixMs   :: !Integer    -- ^ Milliseconds since Unix epoch
  , timeTimezone :: !Text       -- ^ Timezone identifier (e.g., "UTC", "America/New_York")
  , timeIso8601  :: !Text       -- ^ ISO 8601 formatted string
  } deriving stock (Eq, Show)

-- | A duration in milliseconds
newtype Duration = Duration { durationMs :: Integer }
  deriving stock (Eq, Show)
  deriving newtype (Ord, Num)

--------------------------------------------------------------------------------
-- Type Helpers
--------------------------------------------------------------------------------

-- | Int type (timestamps are represented as Int in Crisp)
intType :: Type
intType = simpleType "Int"

-- | String type
stringType :: Type
stringType = simpleType "String"

-- | Unit type
unitType :: Type
unitType = simpleType "Unit"

-- | Timestamp type alias (Int representing milliseconds since epoch)
timestampType :: Type
timestampType = intType

-- | Duration type alias (Int representing milliseconds)
durationMsType :: Type
durationMsType = intType

--------------------------------------------------------------------------------
-- External Function Definitions
--------------------------------------------------------------------------------

-- | Get current system time in milliseconds since Unix epoch
--
-- @
-- external fn time_ms() -> Int = ("system", "time_ms")
-- @
--
-- Returns the current wall-clock time as milliseconds since January 1, 1970 UTC.
-- This is suitable for timestamps but not for measuring durations (use monotonic_ms).
timeMsExternal :: ExternalInfo
timeMsExternal = ExternalInfo
  { externalInfoName     = "time_ms"
  , externalInfoModule   = "system"
  , externalInfoFunction = "time_ms"
  , externalInfoType     = pureFnType unitType intType
  }

-- | Get current system timezone as a string
--
-- @
-- external fn timezone() -> String = ("system", "timezone")
-- @
--
-- Returns the system's current timezone identifier (e.g., "UTC", "America/New_York").
timezoneExternal :: ExternalInfo
timezoneExternal = ExternalInfo
  { externalInfoName     = "timezone"
  , externalInfoModule   = "system"
  , externalInfoFunction = "timezone"
  , externalInfoType     = pureFnType unitType stringType
  }

-- | Format a Unix timestamp (ms) as ISO 8601 string
--
-- @
-- external fn format_iso8601(unix_ms: Int) -> String = ("system", "format_iso8601")
-- @
--
-- Converts milliseconds since epoch to an ISO 8601 formatted string.
-- Example: 1706400000000 -> "2024-01-28T00:00:00.000Z"
formatIso8601External :: ExternalInfo
formatIso8601External = ExternalInfo
  { externalInfoName     = "format_iso8601"
  , externalInfoModule   = "system"
  , externalInfoFunction = "format_iso8601"
  , externalInfoType     = pureFnType intType stringType
  }

-- | Get monotonic time in milliseconds (for duration measurement)
--
-- @
-- external fn monotonic_ms() -> Int = ("system", "monotonic_ms")
-- @
--
-- Returns a monotonically increasing time value suitable for measuring durations.
-- Unlike time_ms, this is not affected by system clock adjustments.
monotonicMsExternal :: ExternalInfo
monotonicMsExternal = ExternalInfo
  { externalInfoName     = "monotonic_ms"
  , externalInfoModule   = "system"
  , externalInfoFunction = "monotonic_ms"
  , externalInfoType     = pureFnType unitType intType
  }

--------------------------------------------------------------------------------
-- All Time Externals
--------------------------------------------------------------------------------

-- | All time-related external function definitions
timeExternals :: [ExternalInfo]
timeExternals =
  [ timeMsExternal
  , timezoneExternal
  , formatIso8601External
  , monotonicMsExternal
  ]

--------------------------------------------------------------------------------
-- Lookup Functions
--------------------------------------------------------------------------------

-- | Map of time external names to their definitions
timeExternalMap :: Map Text ExternalInfo
timeExternalMap = Map.fromList
  [(externalInfoName ext, ext) | ext <- timeExternals]

-- | Look up a time external by name
lookupTimeExternal :: Text -> Maybe ExternalInfo
lookupTimeExternal name = Map.lookup name timeExternalMap

-- | Check if a name is a time external
isTimeExternal :: Text -> Bool
isTimeExternal name = Map.member name timeExternalMap

-- | Get all time external definitions
allTimeExternals :: [ExternalInfo]
allTimeExternals = timeExternals
