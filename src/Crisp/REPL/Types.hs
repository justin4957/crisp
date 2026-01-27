{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.REPL.Types
-- Description : REPL data types
--
-- Core data types for the Crisp REPL including state, commands,
-- and result types.

module Crisp.REPL.Types
  ( -- * REPL State
    ReplState(..)
  , initialReplState
  , hasBinding
  , hasType
  , addToHistory
    -- * REPL Input
  , ReplInput(..)
    -- * REPL Commands
  , ReplCommand(..)
    -- * REPL Results
  , ReplResult(..)
    -- * Definition Info
  , DefInfo(..)
  , TypeInfo(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- =============================================================================
-- REPL State
-- =============================================================================

-- | The state of the REPL session
data ReplState = ReplState
  { replBindings   :: !(Map Text DefInfo)    -- ^ Value bindings (let, fn)
  , replTypes      :: !(Map Text TypeInfo)   -- ^ Type definitions
  , replLoadedFile :: !(Maybe FilePath)      -- ^ Last loaded file (for :reload)
  , replHistory    :: ![Text]                -- ^ Input history (newest first)
  , replPrompt     :: !Text                  -- ^ Current prompt string
  } deriving stock (Eq, Show, Generic)

-- | Create initial REPL state with prelude
initialReplState :: ReplState
initialReplState = ReplState
  { replBindings = preludeBindings
  , replTypes = preludeTypes
  , replLoadedFile = Nothing
  , replHistory = []
  , replPrompt = "crisp> "
  }

-- | Built-in bindings available in REPL
preludeBindings :: Map Text DefInfo
preludeBindings = Map.fromList
  [ ("true", DefInfo "true" "Bool" "true")
  , ("false", DefInfo "false" "Bool" "false")
  ]

-- | Built-in types available in REPL
preludeTypes :: Map Text TypeInfo
preludeTypes = Map.fromList
  [ ("Int", TypeInfo "Int" "Type" [])
  , ("Bool", TypeInfo "Bool" "Type" [])
  , ("String", TypeInfo "String" "Type" [])
  , ("Unit", TypeInfo "Unit" "Type" [])
  , ("List", TypeInfo "List" "Type -> Type" ["A"])
  , ("Option", TypeInfo "Option" "Type -> Type" ["A"])
  ]

-- | Check if a binding exists in the state
hasBinding :: Text -> ReplState -> Bool
hasBinding name state = Map.member name (replBindings state)

-- | Check if a type exists in the state
hasType :: Text -> ReplState -> Bool
hasType name state = Map.member name (replTypes state)

-- | Add an entry to history
addToHistory :: Text -> ReplState -> ReplState
addToHistory input state = state
  { replHistory = input : replHistory state }

-- =============================================================================
-- Definition Info
-- =============================================================================

-- | Information about a value binding
data DefInfo = DefInfo
  { defName  :: !Text
  , defType  :: !Text
  , defValue :: !Text
  } deriving stock (Eq, Show, Generic)

-- | Information about a type definition
data TypeInfo = TypeInfo
  { typeName   :: !Text
  , typeKind   :: !Text
  , typeParams :: ![Text]
  } deriving stock (Eq, Show, Generic)

-- =============================================================================
-- REPL Input
-- =============================================================================

-- | Parsed REPL input
data ReplInput
  = ReplExpr !Text           -- ^ Expression to evaluate
  | ReplDef !Text            -- ^ Definition to add
  | ReplCmd !ReplCommand     -- ^ Command to execute
  | ReplEmpty                -- ^ Empty input (blank line)
  deriving stock (Eq, Show, Generic)

-- =============================================================================
-- REPL Commands
-- =============================================================================

-- | REPL commands (prefixed with :)
data ReplCommand
  = CmdHelp                  -- ^ :help, :h, :?
  | CmdQuit                  -- ^ :quit, :q, :exit
  | CmdType !Text            -- ^ :type expr, :t expr
  | CmdKind !Text            -- ^ :kind Type, :k Type
  | CmdLoad !FilePath        -- ^ :load file, :l file
  | CmdReload                -- ^ :reload, :r
  | CmdReset                 -- ^ :reset
  | CmdBrowse !Text          -- ^ :browse Module, :b Module
  | CmdUnknown !Text         -- ^ Unknown command
  deriving stock (Eq, Show, Generic)

-- =============================================================================
-- REPL Results
-- =============================================================================

-- | Result of evaluating an expression
data ReplResult
  = ReplValue !Text !Text    -- ^ Value and its type
  | ReplUnit                 -- ^ Unit result (no output)
  deriving stock (Eq, Show, Generic)
