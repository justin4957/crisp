{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.REPL.Commands
-- Description : REPL command execution
--
-- Executes REPL commands like :help, :type, :kind, etc.

module Crisp.REPL.Commands
  ( -- * Command Execution
    runReplCommand
  , executeCommand
    -- * Help Text
  , helpText
  ) where

import Crisp.REPL.Types
import Crisp.REPL.Eval (typeOfExpr)

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- =============================================================================
-- Command Execution
-- =============================================================================

-- | Run a REPL command
runReplCommand :: ReplState -> ReplCommand -> IO (Either Text Text)
runReplCommand state cmd = pure $ executeCommand state cmd

-- | Execute a REPL command
executeCommand :: ReplState -> ReplCommand -> Either Text Text
executeCommand state cmd = case cmd of
  CmdHelp -> Right helpText

  CmdQuit -> Right "Goodbye!"

  CmdType expr -> do
    ty <- typeOfExprWithState state expr
    Right $ expr <> " : " <> ty

  CmdKind typeName -> do
    kind <- kindOfType state typeName
    Right $ typeName <> " : " <> kind

  CmdLoad _path ->
    -- File loading would be implemented with IO
    Right "File loading not yet implemented in pure context"

  CmdReload ->
    case replLoadedFile state of
      Nothing -> Left "No file loaded to reload"
      Just _path -> Right "Reload not yet implemented in pure context"

  CmdReset -> Right "State reset"

  CmdBrowse modName -> do
    browseMod <- browseModule state modName
    Right browseMod

  CmdUnknown cmdName ->
    Left $ "Unknown command: :" <> cmdName <> "\nType :help for available commands"

-- | Get type of expression with state
typeOfExprWithState :: ReplState -> Text -> Either Text Text
typeOfExprWithState state expr =
  -- First try to evaluate and get type
  case typeOfExpr state (T.strip expr) of
    Right ty -> Right ty
    Left _ ->
      -- Try to infer from syntax
      inferTypeFromSyntax state expr

-- | Infer type from syntax (simplified)
inferTypeFromSyntax :: ReplState -> Text -> Either Text Text
inferTypeFromSyntax _state expr =
  let stripped = T.strip expr
  in -- Check for lambda syntax
     if "fn(" `T.isPrefixOf` stripped || "fn (" `T.isPrefixOf` stripped
     then inferLambdaType stripped
     else if isNumericExpr stripped
     then Right "Int"
     else if isBoolExpr stripped
     then Right "Bool"
     else Left $ "Cannot infer type of: " <> expr

-- | Check if expression is numeric
isNumericExpr :: Text -> Bool
isNumericExpr t =
  let stripped = T.strip t
  in T.all (\c -> c >= '0' && c <= '9') stripped ||
     any (`T.isInfixOf` stripped) [" + ", " - ", " * ", " / "]

-- | Check if expression is boolean
isBoolExpr :: Text -> Bool
isBoolExpr t =
  let stripped = T.strip t
  in stripped `elem` ["true", "false"] ||
     any (`T.isInfixOf` stripped) [" == ", " /= ", " < ", " > ", " && ", " || "]

-- | Infer lambda type
inferLambdaType :: Text -> Either Text Text
inferLambdaType expr =
  -- Extract type from fn(x: T) -> R syntax
  case T.breakOn "(" expr of
    (_, rest) ->
      case T.breakOn ")" rest of
        (params, afterParams) ->
          let paramTypes = extractParamTypes (T.drop 1 params)
              returnType = extractReturnType (T.drop 1 afterParams)
          in Right $ "(" <> paramTypes <> ") -> " <> returnType

-- | Extract parameter types
extractParamTypes :: Text -> Text
extractParamTypes params =
  let parts = T.splitOn "," params
      types = map getTypeFromParam parts
  in T.intercalate ", " types
  where
    getTypeFromParam p =
      case T.breakOn ":" p of
        (_, typePart) -> T.strip $ T.drop 1 typePart

-- | Extract return type
extractReturnType :: Text -> Text
extractReturnType afterParen =
  case T.breakOn "->" afterParen of
    (_, retPart) ->
      let ret = T.strip $ T.drop 2 retPart
      in if T.null ret then "?" else T.strip $ T.takeWhile (/= '=') ret

-- | Get kind of a type
kindOfType :: ReplState -> Text -> Either Text Text
kindOfType state typeName =
  case Map.lookup typeName (replTypes state) of
    Just typeInfo -> Right $ typeKind typeInfo
    Nothing -> Left $ "Unknown type: " <> typeName

-- | Browse a module (list its definitions)
browseModule :: ReplState -> Text -> Either Text Text
browseModule state modName =
  if modName == "Prelude" || T.null modName
  then Right $ formatBindings state
  else Left $ "Unknown module: " <> modName

-- | Format bindings for display
formatBindings :: ReplState -> Text
formatBindings state =
  let bindings = Map.toList (replBindings state)
      types = Map.toList (replTypes state)
      bindingLines = map formatBinding bindings
      typeLines = map formatType types
  in T.unlines $ ["-- Values --"] ++ bindingLines ++
                 ["", "-- Types --"] ++ typeLines

-- | Format a single binding
formatBinding :: (Text, DefInfo) -> Text
formatBinding (name, def) =
  name <> " : " <> defType def

-- | Format a single type
formatType :: (Text, TypeInfo) -> Text
formatType (name, typeInfo) =
  name <> " : " <> typeKind typeInfo

-- =============================================================================
-- Help Text
-- =============================================================================

-- | Help text for REPL commands
helpText :: Text
helpText = T.unlines
  [ "Crisp REPL Commands:"
  , ""
  , "  :help, :h, :?     Show this help"
  , "  :quit, :q, :exit  Exit the REPL"
  , "  :type <expr>      Show the type of an expression"
  , "  :kind <type>      Show the kind of a type"
  , "  :load <file>      Load definitions from a file"
  , "  :reload, :r       Reload the last loaded file"
  , "  :reset            Clear all definitions"
  , "  :browse [module]  List definitions in a module"
  , ""
  , "Entering expressions evaluates them and prints the result."
  , "Definitions (let, fn, type) are added to the environment."
  , ""
  , "Examples:"
  , "  crisp> 1 + 2"
  , "  3"
  , ""
  , "  crisp> let x = 42"
  , "  x : Int = 42"
  , ""
  , "  crisp> fn double(n: Int) -> Int = n * 2"
  , "  double : (Int) -> Int"
  , ""
  , "  crisp> double(21)"
  , "  42"
  , ""
  , "  crisp> :type fn(x: Int) -> x + 1"
  , "  fn(x: Int) -> x + 1 : (Int) -> Int"
  ]
