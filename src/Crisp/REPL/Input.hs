{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.REPL.Input
-- Description : REPL input parsing
--
-- Parses REPL input into commands, expressions, and definitions.

module Crisp.REPL.Input
  ( -- * Input Parsing
    parseReplInput
    -- * Multi-line Support
  , needsContinuation
  , combineLines
    -- * Completion
  , getCompletions
  ) where

import Crisp.REPL.Types

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- =============================================================================
-- Input Parsing
-- =============================================================================

-- | Parse a line of REPL input
parseReplInput :: Text -> Either Text ReplInput
parseReplInput input =
  let stripped = T.strip input
  in if T.null stripped
     then Right ReplEmpty
     else if isComment stripped
          then Right ReplEmpty
          else if ":" `T.isPrefixOf` stripped
               then Right $ ReplCmd (parseCommand stripped)
               else if isDefinition stripped
                    then Right $ ReplDef (stripTrailingComment stripped)
                    else Right $ ReplExpr (stripTrailingComment stripped)

-- | Check if input is just a comment
isComment :: Text -> Bool
isComment t = "--" `T.isPrefixOf` t

-- | Strip trailing comment from input
stripTrailingComment :: Text -> Text
stripTrailingComment t =
  case T.breakOn "--" t of
    (before, _) -> T.stripEnd before

-- | Check if input is a definition (let, fn, type, effect, handler)
isDefinition :: Text -> Bool
isDefinition t =
  let word = T.takeWhile (not . isSpace) t
  in word `elem` ["let", "fn", "type", "effect", "handler"]

-- | Parse a command (input starting with :)
parseCommand :: Text -> ReplCommand
parseCommand input =
  let withoutColon = T.drop 1 input
      (cmdName, rest) = T.break isSpace withoutColon
      arg = T.strip rest
  in case T.toLower cmdName of
    -- Help
    "help" -> CmdHelp
    "h" -> CmdHelp
    "?" -> CmdHelp

    -- Quit
    "quit" -> CmdQuit
    "q" -> CmdQuit
    "exit" -> CmdQuit

    -- Type
    "type" -> CmdType arg
    "t" -> CmdType arg

    -- Kind
    "kind" -> CmdKind arg
    "k" -> CmdKind arg

    -- Load
    "load" -> CmdLoad (T.unpack arg)
    "l" -> CmdLoad (T.unpack arg)

    -- Reload
    "reload" -> CmdReload
    "r" -> CmdReload

    -- Reset
    "reset" -> CmdReset

    -- Browse
    "browse" -> CmdBrowse arg
    "b" -> CmdBrowse arg

    -- Unknown
    _ -> CmdUnknown cmdName

-- =============================================================================
-- Multi-line Support
-- =============================================================================

-- | Check if input needs continuation (incomplete)
needsContinuation :: Text -> Bool
needsContinuation input =
  let stripped = T.strip input
  in -- Explicit continuation marker
     "\\" `T.isSuffixOf` stripped ||
     -- Unclosed parentheses
     unclosedParens stripped ||
     -- Unclosed braces
     unclosedBraces stripped ||
     -- Unclosed brackets
     unclosedBrackets stripped

-- | Check for unclosed parentheses
unclosedParens :: Text -> Bool
unclosedParens t = countChar '(' t > countChar ')' t

-- | Check for unclosed braces
unclosedBraces :: Text -> Bool
unclosedBraces t = countChar '{' t > countChar '}' t

-- | Check for unclosed brackets
unclosedBrackets :: Text -> Bool
unclosedBrackets t = countChar '[' t > countChar ']' t

-- | Count occurrences of a character (simple, doesn't handle strings)
countChar :: Char -> Text -> Int
countChar c = T.length . T.filter (== c)

-- | Combine multiple continuation lines
combineLines :: [Text] -> Text
combineLines [] = ""
combineLines lines' =
  let processed = map processLine lines'
  in T.concat processed
  where
    processLine t =
      let stripped = T.strip t
      in if "\\" `T.isSuffixOf` stripped
         then T.init stripped  -- Remove the backslash
         else stripped

-- =============================================================================
-- Completion
-- =============================================================================

-- | Get completions for partial input
getCompletions :: ReplState -> Text -> [Text]
getCompletions state partial
  | ":" `T.isPrefixOf` partial = commandCompletions partial
  | otherwise = identifierCompletions state partial

-- | Complete command names
commandCompletions :: Text -> [Text]
commandCompletions partial =
  let cmds = [ ":help", ":quit", ":type", ":kind", ":load"
             , ":reload", ":reset", ":browse" ]
  in filter (partial `T.isPrefixOf`) cmds

-- | Complete identifier names
identifierCompletions :: ReplState -> Text -> [Text]
identifierCompletions state partial =
  let bindings = Map.keys (replBindings state)
      types = Map.keys (replTypes state)
      allNames = bindings ++ types
  in filter (partial `T.isPrefixOf`) allNames
