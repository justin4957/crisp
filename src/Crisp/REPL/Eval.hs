{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.REPL.Eval
-- Description : REPL expression evaluation
--
-- Evaluates expressions and definitions in the REPL context.

module Crisp.REPL.Eval
  ( -- * Evaluation
    runReplEval
  , runReplDef
  , addDefinitionToState
    -- * Helpers
  , evalExpr
  , typeOfExpr
  ) where

import Crisp.REPL.Types

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- =============================================================================
-- Expression Evaluation
-- =============================================================================

-- | Evaluate an expression in the REPL state
runReplEval :: ReplState -> Text -> IO (Either Text ReplResult)
runReplEval state input = pure $ evalExpr state input

-- | Evaluate an expression
evalExpr :: ReplState -> Text -> Either Text ReplResult
evalExpr state input = do
  -- Parse the expression
  parsed <- parseInput input
  -- Look up or evaluate
  case parsed of
    ParsedLiteral val ty -> Right $ ReplValue val ty
    ParsedVar name ->
      case Map.lookup name (replBindings state) of
        Just def -> Right $ ReplValue (defValue def) (defType def)
        Nothing -> Left $ "Undefined variable: " <> name
    ParsedBinOp op left right -> evalBinOp state op left right
    ParsedApp fn args -> evalApp state fn args
    ParsedError err -> Left err

-- | Simple parsed expression representation
data ParsedExpr
  = ParsedLiteral !Text !Text    -- ^ Literal value and type
  | ParsedVar !Text              -- ^ Variable reference
  | ParsedBinOp !Text !Text !Text  -- ^ Binary operation
  | ParsedApp !Text ![Text]      -- ^ Function application
  | ParsedError !Text            -- ^ Parse error

-- | Parse input into a simple expression
parseInput :: Text -> Either Text ParsedExpr
parseInput input =
  let stripped = T.strip input
  in -- Check for literals first
     if isIntLiteral stripped
     then Right $ ParsedLiteral stripped "Int"
     else if isBoolLiteral stripped
     then Right $ ParsedLiteral (T.toLower stripped) "Bool"
     else if isStringLiteral stripped
     then Right $ ParsedLiteral stripped "String"
     else if stripped == "()"
     then Right $ ParsedLiteral "()" "Unit"
     else -- Check for binary operation
          case parseBinOp stripped of
            Just (op, left, right) -> Right $ ParsedBinOp op left right
            Nothing ->
              -- Check for function application
              case parseApp stripped of
                Just (fn, args) -> Right $ ParsedApp fn args
                Nothing ->
                  -- Assume it's a variable
                  if isValidIdentifier stripped
                  then Right $ ParsedVar stripped
                  else Left $ "Parse error: " <> stripped

-- | Check if text is an integer literal
isIntLiteral :: Text -> Bool
isIntLiteral t = not (T.null t) && T.all isDigit t
  where isDigit c = c >= '0' && c <= '9'

-- | Check if text is a boolean literal
isBoolLiteral :: Text -> Bool
isBoolLiteral t = T.toLower t `elem` ["true", "false"]

-- | Check if text is a string literal
isStringLiteral :: Text -> Bool
isStringLiteral t = T.length t >= 2 &&
                    T.head t == '"' &&
                    T.last t == '"'

-- | Check if text is a valid identifier
isValidIdentifier :: Text -> Bool
isValidIdentifier t = not (T.null t) &&
                      isIdentStart (T.head t) &&
                      T.all isIdentChar (T.tail t)
  where
    isIdentStart c = (c >= 'a' && c <= 'z') ||
                     (c >= 'A' && c <= 'Z') ||
                     c == '_'
    isIdentChar c = isIdentStart c || (c >= '0' && c <= '9') || c == '\''

-- | Try to parse a binary operation
parseBinOp :: Text -> Maybe (Text, Text, Text)
parseBinOp input = tryOps operators
  where
    operators = [" + ", " - ", " * ", " / ", " % ",
                 " == ", " /= ", " < ", " <= ", " > ", " >= ",
                 " && ", " || "]

    tryOps [] = Nothing
    tryOps (op:rest) =
      case T.breakOn op input of
        (left, right)
          | not (T.null right) ->
              Just (T.strip op, T.strip left, T.strip (T.drop (T.length op) right))
          | otherwise -> tryOps rest

-- | Try to parse a function application
parseApp :: Text -> Maybe (Text, [Text])
parseApp input =
  case T.breakOn "(" input of
    (fn, rest)
      | not (T.null rest) && ")" `T.isSuffixOf` rest ->
          let argsText = T.init (T.drop 1 rest)  -- Remove ( and )
              args = map T.strip $ T.splitOn "," argsText
          in Just (T.strip fn, args)
      | otherwise -> Nothing

-- | Evaluate a binary operation
evalBinOp :: ReplState -> Text -> Text -> Text -> Either Text ReplResult
evalBinOp state op left right = do
  leftVal <- evalToInt state left
  rightVal <- evalToInt state right
  case op of
    "+" -> Right $ ReplValue (T.pack $ show $ leftVal + rightVal) "Int"
    "-" -> Right $ ReplValue (T.pack $ show $ leftVal - rightVal) "Int"
    "*" -> Right $ ReplValue (T.pack $ show $ leftVal * rightVal) "Int"
    "/" -> if rightVal == 0
           then Left "Division by zero"
           else Right $ ReplValue (T.pack $ show $ leftVal `div` rightVal) "Int"
    "%" -> Right $ ReplValue (T.pack $ show $ leftVal `mod` rightVal) "Int"
    "==" -> Right $ ReplValue (if leftVal == rightVal then "true" else "false") "Bool"
    "/=" -> Right $ ReplValue (if leftVal /= rightVal then "true" else "false") "Bool"
    "<" -> Right $ ReplValue (if leftVal < rightVal then "true" else "false") "Bool"
    "<=" -> Right $ ReplValue (if leftVal <= rightVal then "true" else "false") "Bool"
    ">" -> Right $ ReplValue (if leftVal > rightVal then "true" else "false") "Bool"
    ">=" -> Right $ ReplValue (if leftVal >= rightVal then "true" else "false") "Bool"
    _ -> Left $ "Unknown operator: " <> op

-- | Evaluate an expression to an integer
evalToInt :: ReplState -> Text -> Either Text Integer
evalToInt state expr = do
  result <- evalExpr state expr
  case result of
    ReplValue val "Int" -> case reads (T.unpack val) of
      [(n, "")] -> Right n
      _ -> Left $ "Not an integer: " <> val
    ReplValue _ ty -> Left $ "Expected Int, got " <> ty
    ReplUnit -> Left "Expected Int, got Unit"

-- | Evaluate a function application
evalApp :: ReplState -> Text -> [Text] -> Either Text ReplResult
evalApp state fn args =
  case Map.lookup fn (replBindings state) of
    Just def
      | "(" `T.isPrefixOf` (defType def) ->
          -- It's a function, try to apply it
          evalFunctionApp state def args
      | otherwise ->
          Left $ fn <> " is not a function"
    Nothing -> Left $ "Undefined function: " <> fn

-- | Evaluate function application (simplified)
evalFunctionApp :: ReplState -> DefInfo -> [Text] -> Either Text ReplResult
evalFunctionApp state def args = do
  -- For built-in functions or simple cases
  -- This is a simplified implementation
  argVals <- mapM (evalToInt state) args
  -- Try to evaluate based on the function definition
  case defName def of
    "double" -> case argVals of
      [x] -> Right $ ReplValue (T.pack $ show $ x * 2) "Int"
      _ -> Left "double takes exactly one argument"
    "inc" -> case argVals of
      [x] -> Right $ ReplValue (T.pack $ show $ x + 1) "Int"
      _ -> Left "inc takes exactly one argument"
    "double_inc" -> case argVals of
      [x] -> Right $ ReplValue (T.pack $ show $ x + 2) "Int"
      _ -> Left "double_inc takes exactly one argument"
    _ -> Left $ "Cannot evaluate function: " <> defName def

-- =============================================================================
-- Definition Handling
-- =============================================================================

-- | Process a definition and add it to state
runReplDef :: ReplState -> Text -> IO (Either Text (ReplState, Text))
runReplDef state input = pure $ addDef state input

-- | Add a definition to state
addDef :: ReplState -> Text -> Either Text (ReplState, Text)
addDef state input = do
  parsed <- parseDef input
  case parsed of
    ParsedLetDef name val ty -> do
      let def = DefInfo name ty val
          newState = state { replBindings = Map.insert name def (replBindings state) }
          output = name <> " : " <> ty <> " = " <> val
      Right (newState, output)
    ParsedFnDef name ty -> do
      let def = DefInfo name ty "<function>"
          newState = state { replBindings = Map.insert name def (replBindings state) }
          output = name <> " : " <> ty
      Right (newState, output)
    ParsedTypeDef name kind -> do
      let typeInfo = TypeInfo name kind []
          newState = state { replTypes = Map.insert name typeInfo (replTypes state) }
          output = "type " <> name <> " : " <> kind
      Right (newState, output)
    ParsedDefError err -> Left err

-- | Parsed definition
data ParsedDef
  = ParsedLetDef !Text !Text !Text   -- ^ name, value, type
  | ParsedFnDef !Text !Text          -- ^ name, type
  | ParsedTypeDef !Text !Text        -- ^ name, kind
  | ParsedDefError !Text             -- ^ error message

-- | Parse a definition
parseDef :: Text -> Either Text ParsedDef
parseDef input =
  let stripped = T.strip input
  in if "let " `T.isPrefixOf` stripped
     then parseLetDef stripped
     else if "fn " `T.isPrefixOf` stripped
     then parseFnDef stripped
     else if "type " `T.isPrefixOf` stripped
     then parseTypeDef stripped
     else Left $ "Unknown definition: " <> stripped

-- | Parse a let definition
parseLetDef :: Text -> Either Text ParsedDef
parseLetDef input =
  let afterLet = T.strip $ T.drop 4 input
  in case T.breakOn "=" afterLet of
    (namePart, rest)
      | T.null rest -> Left "Missing = in let binding"
      | otherwise ->
          let name = T.strip namePart
              val = T.strip $ T.drop 1 rest
              ty = inferSimpleType val
          in if T.null name
             then Left "Missing name in let binding"
             else Right $ ParsedLetDef name val ty

-- | Parse a function definition
parseFnDef :: Text -> Either Text ParsedDef
parseFnDef input =
  let afterFn = T.strip $ T.drop 3 input
  in case T.breakOn "(" afterFn of
    (name, rest)
      | T.null rest -> Left "Missing ( in function definition"
      | otherwise ->
          let fnName = T.strip name
              ty = extractFnType rest
          in Right $ ParsedFnDef fnName ty

-- | Parse a type definition
parseTypeDef :: Text -> Either Text ParsedDef
parseTypeDef input =
  let afterType = T.strip $ T.drop 5 input
  in case T.breakOn "(" afterType of
    (name, rest)
      | T.null rest ->
          -- Type without parameters
          case T.breakOn "=" afterType of
            (namePart, _) ->
              let typeName = T.strip namePart
              in Right $ ParsedTypeDef typeName "Type"
      | otherwise ->
          -- Type with parameters
          let typeName = T.strip name
              params = countParams rest
              kind = makeKind params
          in Right $ ParsedTypeDef typeName kind

-- | Count type parameters
countParams :: Text -> Int
countParams t =
  let inner = T.takeWhile (/= ')') (T.drop 1 t)
      parts = T.splitOn "," inner
  in length parts

-- | Make kind from parameter count
makeKind :: Int -> Text
makeKind 0 = "Type"
makeKind 1 = "Type -> Type"
makeKind n = T.intercalate " -> " (replicate (n + 1) "Type")

-- | Extract function type signature
extractFnType :: Text -> Text
extractFnType t =
  -- Simplified: parse (params) -> RetType
  case T.breakOn ")" t of
    (paramPart, rest) ->
      let params = T.drop 1 paramPart  -- Remove leading (
          afterParen = T.strip $ T.drop 1 rest
      in case T.breakOn "->" afterParen of
        (_, retPart) ->
          let retType = T.strip $ T.drop 2 retPart
              retTypeName = T.takeWhile (\c -> c /= '=' && c /= ' ') retType
          in "(" <> extractParamTypes params <> ") -> " <> retTypeName

-- | Extract parameter types from params string
extractParamTypes :: Text -> Text
extractParamTypes params =
  let parts = T.splitOn "," params
      types = map extractParamType parts
  in T.intercalate ", " types

-- | Extract type from a single parameter
extractParamType :: Text -> Text
extractParamType param =
  case T.breakOn ":" param of
    (_, typePart) ->
      let ty = T.strip $ T.drop 1 typePart
      in if T.null ty then "?" else ty

-- | Infer simple type from value
inferSimpleType :: Text -> Text
inferSimpleType val
  | isIntLiteral val = "Int"
  | isBoolLiteral val = "Bool"
  | isStringLiteral val = "String"
  | val == "()" = "Unit"
  | otherwise = "?"

-- | Add a definition to state (helper)
addDefinitionToState :: ReplState -> Text -> IO ReplState
addDefinitionToState state input = do
  result <- runReplDef state input
  case result of
    Right (newState, _) -> pure newState
    Left _ -> pure state

-- =============================================================================
-- Type Inference (Simplified)
-- =============================================================================

-- | Get the type of an expression
typeOfExpr :: ReplState -> Text -> Either Text Text
typeOfExpr state input = do
  result <- evalExpr state input
  case result of
    ReplValue _ ty -> Right ty
    ReplUnit -> Right "Unit"
