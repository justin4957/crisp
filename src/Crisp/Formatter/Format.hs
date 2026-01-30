{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Formatter.Format
-- Description : Code formatter for Crisp source files
--
-- Provides formatting for Crisp source code, producing consistent and
-- readable output. The formatter parses Crisp source and pretty-prints
-- it with consistent indentation and styling.
--
-- == Usage
--
-- @
-- import Crisp.Formatter.Format
--
-- -- Format a source file
-- result <- formatFile defaultFormatOptions "example.crisp"
--
-- -- Format source text directly
-- let formatted = formatSource defaultFormatOptions sourceText
-- @

module Crisp.Formatter.Format
  ( -- * Formatting
    formatFile
  , formatSource
  , formatExpr
  , formatModule
    -- * Options
  , FormatOptions(..)
  , defaultFormatOptions
    -- * Pretty Printing
  , prettyModule
  , prettyDefinition
  , prettyExpr
  , prettyType
  , prettyPattern
  ) where

import Crisp.Syntax.Surface
import Crisp.Parser.Parser (parseModule, parseExpr, ParseError)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

--------------------------------------------------------------------------------
-- Format Options
--------------------------------------------------------------------------------

-- | Configuration options for the formatter
data FormatOptions = FormatOptions
  { optIndentWidth     :: !Int    -- ^ Number of spaces for indentation (default: 2)
  , optMaxLineWidth    :: !Int    -- ^ Maximum line width before wrapping (default: 80)
  , optTrailingNewline :: !Bool   -- ^ Add trailing newline (default: True)
  , optAlignArrows     :: !Bool   -- ^ Align arrows in match expressions (default: True)
  , optSpaceAroundOps  :: !Bool   -- ^ Space around operators (default: True)
  } deriving stock (Eq, Show)

-- | Default formatting options
defaultFormatOptions :: FormatOptions
defaultFormatOptions = FormatOptions
  { optIndentWidth     = 2
  , optMaxLineWidth    = 80
  , optTrailingNewline = True
  , optAlignArrows     = True
  , optSpaceAroundOps  = True
  }

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | Format a source file
formatFile :: FormatOptions -> FilePath -> IO (Either Text Text)
formatFile opts path = do
  content <- TIO.readFile path
  pure $ formatSource opts content

-- | Format source text
formatSource :: FormatOptions -> Text -> Either Text Text
formatSource opts source =
  case parseModule "<formatter>" source of
    Left err -> Left $ T.pack $ show err
    Right mod' ->
      let formatted = prettyModule opts mod'
          withNewline = if optTrailingNewline opts && not (T.isSuffixOf "\n" formatted)
                        then formatted <> "\n"
                        else formatted
      in Right withNewline

-- | Format a single expression
formatExpr :: FormatOptions -> Text -> Either Text Text
formatExpr opts source =
  case parseExpr "<formatter>" source of
    Left err -> Left $ T.pack $ show err
    Right expr -> Right $ prettyExpr opts 0 expr

-- | Format a parsed module
formatModule :: FormatOptions -> Module -> Text
formatModule = prettyModule

--------------------------------------------------------------------------------
-- Pretty Printing - Module
--------------------------------------------------------------------------------

-- | Pretty print a module
prettyModule :: FormatOptions -> Module -> Text
prettyModule opts mod' = T.intercalate "\n\n" $ filter (not . T.null)
  [ prettyModuleHeader opts mod'
  , prettyRequires opts (moduleRequires mod')
  , prettyProvides opts (moduleProvides mod')
  , T.intercalate "\n\n" (map (prettyDefinition opts 0) (moduleDefinitions mod'))
  ]

-- | Pretty print module header
prettyModuleHeader :: FormatOptions -> Module -> Text
prettyModuleHeader _opts mod' =
  let docStr = prettyDocComment 0 (moduleDocComment mod')
      path = T.intercalate "." (modulePathSegments (moduleName mod'))
      auth = case moduleAuthority mod' of
        Nothing -> ""
        Just a -> " authority " <> a
  in docStr <> "module " <> path <> auth

-- | Pretty print requires declarations
prettyRequires :: FormatOptions -> [Require] -> Text
prettyRequires _opts [] = ""
prettyRequires _opts reqs = T.intercalate "\n" (map prettyReq reqs)
  where
    prettyReq (RequireModule modPath _) = "requires " <> prettyModulePath modPath
    prettyReq (RequireEffects effs _) = "requires effects: " <> T.intercalate ", " effs
    prettyReq (RequireTypes tys _) = "requires types: " <> T.intercalate ", " tys

-- | Pretty print a module path
prettyModulePath :: ModulePath -> Text
prettyModulePath (ModulePath segs _) = T.intercalate "." segs

-- | Pretty print provides declarations
prettyProvides :: FormatOptions -> [Provide] -> Text
prettyProvides _opts [] = ""
prettyProvides opts provs = T.intercalate "\n" (map prettyProv provs)
  where
    prettyProv (ProvideType name _) = "provides type " <> name
    prettyProv (ProvideTypeProp name _) = "provides type prop " <> name
    prettyProv (ProvideEffect name _) = "provides effect " <> name
    prettyProv (ProvideTrait name _) = "provides trait " <> name
    prettyProv (ProvideHandler name _) = "provides handler " <> name
    prettyProv (ProvideFn name mTy _) = case mTy of
      Just ty -> "provides fn " <> name <> ": " <> prettyType opts 0 ty
      Nothing -> "provides fn " <> name
    prettyProv (ProvideExternalFn name mTy _) = case mTy of
      Just ty -> "provides external fn " <> name <> ": " <> prettyType opts 0 ty
      Nothing -> "provides external fn " <> name

--------------------------------------------------------------------------------
-- Pretty Printing - Definitions
--------------------------------------------------------------------------------

-- | Pretty print a definition
prettyDefinition :: FormatOptions -> Int -> Definition -> Text
prettyDefinition opts ind = \case
  DefType td -> prettyDocComment ind (typeDefDocComment td) <> prettyTypeDef opts ind td
  DefEffect ed -> prettyDocComment ind (effectDefDocComment ed) <> prettyEffectDef opts ind ed
  DefHandler hd -> prettyDocComment ind (handlerDefDocComment hd) <> prettyHandlerDef opts ind hd
  DefFn fd -> prettyDocComment ind (fnDefDocComment fd) <> prettyFunctionDef opts ind fd
  DefTrait td -> prettyDocComment ind (traitDefDocComment td) <> prettyTraitDef opts ind td
  DefImpl id' -> prettyDocComment ind (implDefDocComment id') <> prettyImplDef opts ind id'
  DefExternal ed -> prettyDocComment ind (extFnDefDocComment ed) <> prettyExternalDef opts ind ed
  DefTypeAlias ad -> prettyDocComment ind (typeAliasDocComment ad) <> prettyTypeAlias opts ind ad

-- | Pretty print a doc comment, if present
prettyDocComment :: Int -> Maybe DocComment -> Text
prettyDocComment _ Nothing = ""
prettyDocComment ind (Just doc) =
  let docLines = T.lines doc
      prefixed = map (\l -> indent ind <> "--- | " <> l) docLines
  in T.unlines prefixed

-- | Pretty print a type definition
prettyTypeDef :: FormatOptions -> Int -> TypeDef -> Text
prettyTypeDef opts ind td =
  let prefix = indent ind <> "type " <> modifiers <> typeDefName td <> params <> constraints <> kind
      modifiers = if modifierIsProp (typeDefModifiers td) then "prop " else ""
                <> if modifierIsLinear (typeDefModifiers td) then "linear " else ""
      params = if null (typeDefParams td) then ""
               else " " <> T.unwords (map (prettyTypeParam opts) (typeDefParams td))
      constraints = prettyConstraints opts (typeDefConstraints td)
      kind = case typeDefKind td of
        Nothing -> ""
        Just k -> ": " <> prettyKind opts k
      deriv = case typeDefDeriving td of
        Nothing -> ""
        Just dc -> " deriving " <> prettyDerivingClause dc
      constructors = case typeDefConstructors td of
        [] -> ""
        cs -> ":\n" <> T.intercalate "\n" (map (prettyConstructor opts (ind + optIndentWidth opts)) cs)
  in prefix <> deriv <> constructors

-- | Pretty print deriving clause
prettyDerivingClause :: DerivingClause -> Text
prettyDerivingClause dc = case derivingTraits dc of
  [t] -> t
  ts -> "(" <> T.intercalate ", " ts <> ")"

-- | Pretty print type parameter
prettyTypeParam :: FormatOptions -> TypeParam -> Text
prettyTypeParam opts = \case
  TypeVar name Nothing _ -> name
  TypeVar name (Just k) _ -> "(" <> name <> ": " <> prettyKind opts k <> ")"
  DepParam name ty _ -> "(" <> name <> ": " <> prettyType opts 0 ty <> ")"
  BoundedTypeVar name mKind traits _ ->
    let kindPart = case mKind of
          Nothing -> ""
          Just k -> prettyKind opts k <> ": "
        traitPart = T.intercalate " + " traits
    in "(" <> name <> ": " <> kindPart <> traitPart <> ")"

-- | Pretty print kind
prettyKind :: FormatOptions -> Kind -> Text
prettyKind opts = \case
  KindType Nothing _ -> "Type"
  KindType (Just n) _ -> "Type_" <> T.pack (show n)
  KindProp _ -> "Prop"
  KindLinear _ -> "Linear"
  KindArrow k1 k2 _ -> prettyKind opts k1 <> " -> " <> prettyKind opts k2

-- | Pretty print constraints
prettyConstraints :: FormatOptions -> [TraitConstraint] -> Text
prettyConstraints _ [] = ""
prettyConstraints opts cs = " where " <> T.intercalate ", " (map prettyConstraint cs)
  where
    prettyConstraint tc = prettyType opts 0 (constraintType tc) <> ": " <> constraintTrait tc

-- | Pretty print a constructor
prettyConstructor :: FormatOptions -> Int -> Constructor -> Text
prettyConstructor opts ind = \case
  SimpleConstructor name args _ ->
    indent ind <> name <> if null args then "" else " " <> T.unwords (map (prettyType opts 10) args)
  GadtConstructor name ty _ ->
    indent ind <> name <> ": " <> prettyType opts 0 ty
  RecordConstructor name fields _ ->
    indent ind <> name <> " { " <> T.intercalate ", " (map prettyField fields) <> " }"
  where
    prettyField f = fieldName f <> ": " <> prettyType opts 0 (fieldType f)

-- | Pretty print effect definition
prettyEffectDef :: FormatOptions -> Int -> EffectDef -> Text
prettyEffectDef opts ind ed =
  let header = indent ind <> "effect " <> effectDefName ed <> ":"
      operations = map (prettyOperation opts (ind + optIndentWidth opts)) (effectDefOperations ed)
  in header <> "\n" <> T.intercalate "\n" operations

-- | Pretty print an operation
prettyOperation :: FormatOptions -> Int -> Operation -> Text
prettyOperation opts ind op =
  prettyDocComment ind (operationDocComment op)
  <> indent ind <> operationName op <> ": " <> prettyType opts 0 (operationSignature op)

-- | Pretty print handler definition
prettyHandlerDef :: FormatOptions -> Int -> HandlerDef -> Text
prettyHandlerDef opts ind hd =
  let params = T.unwords (map (prettyHandlerParam opts) (handlerDefParams hd))
      paramStr = if T.null params then "" else " " <> params
      intros = case handlerDefIntroducedEffects hd of
        [] -> ""
        es -> " ! " <> T.intercalate ", " (map prettyEffectRef es)
      header = indent ind <> "handler " <> handlerDefName hd <> paramStr
             <> " for " <> handlerDefEffect hd <> intros <> ":"
      clauses = map (prettyHandlerClause opts (ind + optIndentWidth opts)) (handlerDefClauses hd)
  in header <> "\n" <> T.intercalate "\n" clauses

-- | Pretty print handler parameter
prettyHandlerParam :: FormatOptions -> HandlerParam -> Text
prettyHandlerParam opts = \case
  HandlerTypeParam name Nothing _ -> name
  HandlerTypeParam name (Just k) _ -> "(" <> name <> ": " <> prettyKind opts k <> ")"
  HandlerValueParam name ty _ -> "(" <> name <> ": " <> prettyType opts 0 ty <> ")"

-- | Pretty print handler clause
prettyHandlerClause :: FormatOptions -> Int -> HandlerClause -> Text
prettyHandlerClause opts ind = \case
  ReturnClause pat body _ ->
    indent ind <> "return " <> prettyPattern opts pat <> " -> " <> prettyExpr opts 0 body
  OpClause name pats resume body _ ->
    indent ind <> name <> " " <> T.unwords (map (prettyPattern opts) pats)
    <> " -> " <> resume <> ": " <> prettyExpr opts 0 body

-- | Pretty print effect reference
prettyEffectRef :: EffectRef -> Text
prettyEffectRef er = effectRefName er <> case effectRefAuthority er of
  Nothing -> ""
  Just auth -> "@" <> auth

-- | Pretty print function definition
prettyFunctionDef :: FormatOptions -> Int -> FunctionDef -> Text
prettyFunctionDef opts ind fd =
  let tyParams = case fnDefTypeParams fd of
        [] -> ""
        ps -> "[" <> T.intercalate ", " (map (prettyTypeParam opts) ps) <> "]"
      params = case fnDefParams fd of
        [] -> "()"
        ps -> "(" <> T.intercalate ", " (map (prettyParam opts) ps) <> ")"
      retType = case fnDefReturnType fd of
        Nothing -> ""
        Just ty -> " -> " <> prettyType opts 0 ty
      effects = case fnDefEffects fd of
        [] -> ""
        es -> " ! " <> T.intercalate ", " (map prettyEffectRef es)
  in indent ind <> "fn " <> fnDefName fd <> tyParams <> params <> retType <> effects <> ":\n"
     <> indent (ind + optIndentWidth opts) <> prettyExpr opts (ind + optIndentWidth opts) (fnDefBody fd)

-- | Pretty print parameter
prettyParam :: FormatOptions -> Param -> Text
prettyParam opts p = paramName p <> ": " <> prettyType opts 0 (paramType p)

-- | Pretty print trait definition
prettyTraitDef :: FormatOptions -> Int -> TraitDef -> Text
prettyTraitDef opts ind td =
  let paramKind = case traitDefParamKind td of
        Nothing -> traitDefParam td
        Just k -> "(" <> traitDefParam td <> ": " <> prettyKind opts k <> ")"
      supers = case traitDefSupers td of
        [] -> ""
        cs -> " where " <> T.intercalate ", " (map prettySuper cs)
      header = indent ind <> "trait " <> traitDefName td <> " " <> paramKind <> supers <> ":"
      methods = map (prettyTraitMethod opts (ind + optIndentWidth opts)) (traitDefMethods td)
  in header <> "\n" <> T.intercalate "\n" methods
  where
    prettySuper tc = prettyType opts 0 (constraintType tc) <> ": " <> constraintTrait tc

-- | Pretty print trait method
prettyTraitMethod :: FormatOptions -> Int -> TraitMethod -> Text
prettyTraitMethod opts ind tm =
  let sig = indent ind <> traitMethodName tm <> ": " <> prettyType opts 0 (traitMethodType tm)
  in case traitMethodDefault tm of
    Nothing -> sig
    Just expr -> sig <> " = " <> prettyExpr opts 0 expr

-- | Pretty print implementation definition
prettyImplDef :: FormatOptions -> Int -> ImplDef -> Text
prettyImplDef opts ind id' =
  let header = indent ind <> "impl " <> implDefTrait id' <> " for " <> prettyType opts 0 (implDefType id') <> ":"
      methodInd = ind + optIndentWidth opts
      methods = map (\fd -> prettyDocComment methodInd (fnDefDocComment fd) <> prettyFunctionDef opts methodInd fd) (implDefMethods id')
  in header <> "\n" <> T.intercalate "\n\n" methods

-- | Pretty print external function definition
prettyExternalDef :: FormatOptions -> Int -> ExternalFnDef -> Text
prettyExternalDef opts ind ed =
  let params = case extFnDefParams ed of
        [] -> "()"
        ps -> "(" <> T.intercalate ", " (map (prettyParam opts) ps) <> ")"
      extRef = extFnDefExternal ed
  in indent ind <> "external fn " <> extFnDefName ed <> params
     <> " -> " <> prettyType opts 0 (extFnDefReturnType ed)
     <> " = (\"" <> externalModule extRef <> "\", \"" <> externalFunction extRef <> "\")"

-- | Pretty print type alias
prettyTypeAlias :: FormatOptions -> Int -> TypeAliasDef -> Text
prettyTypeAlias opts ind ad =
  let params = case typeAliasParams ad of
        [] -> ""
        ps -> " " <> T.unwords (map (prettyTypeParam opts) ps)
      constraints = case typeAliasConstraints ad of
        [] -> ""
        cs -> " { " <> T.intercalate ", " (map prettyFieldConstraint cs) <> " }"
  in indent ind <> "type " <> typeAliasName ad <> params <> " = "
     <> prettyType opts 0 (typeAliasBase ad) <> constraints
  where
    prettyFieldConstraint fc =
      fieldConstraintName fc <> ": " <> prettyPattern opts (fieldConstraintPattern fc)

--------------------------------------------------------------------------------
-- Pretty Printing - Types
--------------------------------------------------------------------------------

-- | Pretty print a type with precedence
prettyType :: FormatOptions -> Int -> Type -> Text
prettyType opts prec = \case
  TyName name _ -> name
  TyApp con args _ ->
    let conStr = prettyType opts 10 con
        argsStr = T.unwords (map (prettyType opts 10) args)
    in parensIf (prec > 9 && not (null args)) $ conStr <> " " <> argsStr
  TyFn from to effs _ ->
    let fromStr = prettyType opts 6 from
        toStr = prettyType opts 5 to
        effStr = case effs of
          [] -> ""
          es -> " ! " <> T.intercalate ", " (map prettyEffectRef es)
    in parensIf (prec > 5) $ fromStr <> " -> " <> toStr <> effStr
  TyDepFn name argTy retTy effs _ ->
    let effStr = case effs of
          [] -> ""
          es -> " ! " <> T.intercalate ", " (map prettyEffectRef es)
    in parensIf (prec > 5) $
       "(" <> name <> ": " <> prettyType opts 0 argTy <> ") -> " <> prettyType opts 5 retTy <> effStr
  TyForall param body _ ->
    parensIf (prec > 0) $
    "forall " <> prettyTypeParam opts param <> ". " <> prettyType opts 0 body
  TyLazy inner _ -> "Lazy " <> prettyType opts 10 inner
  TyRef inner mut _ ->
    let mutStr = if mut then "mut " else ""
    in "ref " <> mutStr <> prettyType opts 10 inner
  TyParen inner _ -> "(" <> prettyType opts 0 inner <> ")"
  TyRefinement base preds _ ->
    prettyType opts 10 base <> " { " <> T.intercalate ", " (map (prettyRefinement opts) preds) <> " }"

-- | Pretty print refinement predicate
prettyRefinement :: FormatOptions -> RefinementPredicate -> Text
prettyRefinement opts = \case
  RefinementComparison left op right _ ->
    prettyExpr opts 0 left <> " " <> prettyCompOp op <> " " <> prettyExpr opts 0 right
  RefinementAnd p1 p2 _ ->
    prettyRefinement opts p1 <> " && " <> prettyRefinement opts p2
  RefinementOr p1 p2 _ ->
    prettyRefinement opts p1 <> " || " <> prettyRefinement opts p2
  RefinementNot p _ -> "!" <> prettyRefinement opts p
  RefinementExpr e _ -> prettyExpr opts 0 e

-- | Pretty print comparison operator
prettyCompOp :: ComparisonOp -> Text
prettyCompOp = \case
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="
  OpEq -> "=="
  OpNe -> "/="

--------------------------------------------------------------------------------
-- Pretty Printing - Expressions
--------------------------------------------------------------------------------

-- | Pretty print an expression
prettyExpr :: FormatOptions -> Int -> Expr -> Text
prettyExpr opts ind = \case
  EVar name _ -> name
  ECon name _ -> name
  EIntLit n _ -> T.pack (show n)
  EFloatLit f _ -> T.pack (show f)
  EStringLit s _ -> "\"" <> escapeString s <> "\""
  ECharLit c _ -> "'" <> escapeChar c <> "'"
  EUnit _ -> "()"

  EApp func args _ ->
    prettyExpr opts ind func <> " " <> T.unwords (map (prettyExprAtom opts ind) args)

  ELam params body _ ->
    let paramStr = T.intercalate ", " (map (prettyParam opts) params)
    in "\\" <> paramStr <> ". " <> prettyExpr opts ind body

  ELet pat mTy val body _ ->
    let tyStr = case mTy of
          Nothing -> ""
          Just ty -> ": " <> prettyType opts 0 ty
    in "let " <> prettyPattern opts pat <> tyStr <> " = " <> prettyExpr opts ind val
       <> "\n" <> indent ind <> prettyExpr opts ind body

  EMatch subj arms _ ->
    let header = "match " <> prettyExpr opts ind subj
        armStrs = map (prettyMatchArm opts (ind + optIndentWidth opts)) arms
    in header <> "\n" <> T.intercalate "\n" armStrs

  EIf cond then' else' _ ->
    "if " <> prettyExpr opts ind cond
    <> " then " <> prettyExpr opts ind then'
    <> " else " <> prettyExpr opts ind else'

  EDo stmts result _ ->
    let stmtStrs = map (prettyDoStmt opts (ind + optIndentWidth opts)) stmts
        resultStr = indent (ind + optIndentWidth opts) <> prettyExpr opts (ind + optIndentWidth opts) result
    in "do\n" <> T.intercalate "\n" stmtStrs <> "\n" <> resultStr

  EWith handler body _ ->
    "with " <> prettyExpr opts ind handler <> " " <> prettyExpr opts ind body

  EPerform effect op args _ ->
    "perform " <> effect <> "." <> op
    <> if null args then "" else " " <> T.unwords (map (prettyExprAtom opts ind) args)

  ELazy inner _ -> "lazy " <> prettyExprAtom opts ind inner
  EForce inner _ -> "force " <> prettyExprAtom opts ind inner

  ERef inner mut _ ->
    let mutStr = if mut then "mut " else ""
    in "ref " <> mutStr <> prettyExprAtom opts ind inner

  EPipe left right _ ->
    prettyExpr opts ind left <> " |> " <> prettyExpr opts ind right

  EAnnot expr ty _ ->
    prettyExprAtom opts ind expr <> ": " <> prettyType opts 0 ty

  EBlock stmts result _ ->
    let stmtStrs = map (prettyStatement opts (ind + optIndentWidth opts)) stmts
        resultStr = indent (ind + optIndentWidth opts) <> prettyExpr opts (ind + optIndentWidth opts) result
    in "{\n" <> T.intercalate "\n" stmtStrs <> "\n" <> resultStr <> "\n" <> indent ind <> "}"

  EQualified path name _ ->
    T.intercalate "." path <> "." <> name

  EExternal ref args _ ->
    "external(\"" <> externalModule ref <> "\", \"" <> externalFunction ref <> "\")"
    <> if null args then "" else " " <> T.unwords (map (prettyExprAtom opts ind) args)

  EFieldAccess expr field _ ->
    prettyExprAtom opts ind expr <> "." <> field

  EBinOp op left right _ ->
    prettyExprAtom opts ind left <> " " <> prettyBinOp op <> " " <> prettyExprAtom opts ind right

  ERecord conName fields _ ->
    let fieldStrs = map (\(name, val) -> name <> " = " <> prettyExpr opts ind val) fields
    in conName <> " { " <> T.intercalate ", " fieldStrs <> " }"

-- | Pretty print a binary operator
prettyBinOp :: BinOp -> Text
prettyBinOp = \case
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%"
  OpAnd -> "&&"
  OpOr  -> "||"
  OpLT  -> "<"
  OpLE  -> "<="
  OpGT  -> ">"
  OpGE  -> ">="
  OpEQ  -> "=="
  OpNE  -> "/="

-- | Pretty print an atomic expression (with parens if needed)
prettyExprAtom :: FormatOptions -> Int -> Expr -> Text
prettyExprAtom opts ind expr = case expr of
  EVar {} -> prettyExpr opts ind expr
  ECon {} -> prettyExpr opts ind expr
  EIntLit {} -> prettyExpr opts ind expr
  EFloatLit {} -> prettyExpr opts ind expr
  EStringLit {} -> prettyExpr opts ind expr
  ECharLit {} -> prettyExpr opts ind expr
  EUnit {} -> prettyExpr opts ind expr
  EQualified {} -> prettyExpr opts ind expr
  EFieldAccess {} -> prettyExpr opts ind expr
  ERecord {} -> prettyExpr opts ind expr
  _ -> "(" <> prettyExpr opts ind expr <> ")"

-- | Pretty print a match arm
prettyMatchArm :: FormatOptions -> Int -> MatchArm -> Text
prettyMatchArm opts ind arm =
  let guardStr = case matchArmGuard arm of
        Nothing -> ""
        Just g -> " | " <> prettyExpr opts ind g
  in indent ind <> prettyPattern opts (matchArmPattern arm) <> guardStr
     <> " -> " <> prettyMatchArmExpr opts ind (matchArmBody arm)

-- | Pretty print an expression in a match arm body.
-- Uses parenthesized application syntax (f(x, y)) instead of space-separated
-- (f x y) to ensure the formatted output can be re-parsed by the match arm
-- body parser, which only supports parenthesized application.
prettyMatchArmExpr :: FormatOptions -> Int -> Expr -> Text
prettyMatchArmExpr opts ind expr = case expr of
  EApp func args _ ->
    prettyMatchArmExpr opts ind func <> "("
    <> T.intercalate ", " (map (prettyMatchArmExpr opts ind) args) <> ")"
  -- For other expressions, delegate to the standard prettyExpr
  _ -> prettyExpr opts ind expr

-- | Pretty print a do statement
prettyDoStmt :: FormatOptions -> Int -> DoStatement -> Text
prettyDoStmt opts ind = \case
  DoBind pat effect op args _ ->
    indent ind <> prettyPattern opts pat <> " <- " <> effect <> "." <> op
    <> if null args then "" else " " <> T.unwords (map (prettyExprAtom opts ind) args)
  DoLet pat mTy val _ ->
    let tyStr = case mTy of
          Nothing -> ""
          Just ty -> ": " <> prettyType opts 0 ty
    in indent ind <> "let " <> prettyPattern opts pat <> tyStr <> " = " <> prettyExpr opts ind val
  DoExpr expr _ ->
    indent ind <> prettyExpr opts ind expr

-- | Pretty print a statement
prettyStatement :: FormatOptions -> Int -> Statement -> Text
prettyStatement opts ind = \case
  StmtLet pat mTy val _ ->
    let tyStr = case mTy of
          Nothing -> ""
          Just ty -> ": " <> prettyType opts 0 ty
    in indent ind <> "let " <> prettyPattern opts pat <> tyStr <> " = " <> prettyExpr opts ind val
  StmtExpr expr _ ->
    indent ind <> prettyExpr opts ind expr

--------------------------------------------------------------------------------
-- Pretty Printing - Patterns
--------------------------------------------------------------------------------

-- | Pretty print a pattern
prettyPattern :: FormatOptions -> Pattern -> Text
prettyPattern opts = \case
  PatVar name _ -> name
  PatWildcard _ -> "_"
  PatCon name args _ ->
    name <> if null args then "" else " " <> T.unwords (map (prettyPatternAtom opts) args)
  PatTuple pats _ -> "(" <> T.intercalate ", " (map (prettyPattern opts) pats) <> ")"
  PatLit expr _ -> prettyExpr opts 0 expr
  PatTyped pat ty _ -> prettyPattern opts pat <> ": " <> prettyType opts 0 ty

-- | Pretty print an atomic pattern (with parens if needed)
prettyPatternAtom :: FormatOptions -> Pattern -> Text
prettyPatternAtom opts pat = case pat of
  PatVar {} -> prettyPattern opts pat
  PatWildcard {} -> prettyPattern opts pat
  PatLit {} -> prettyPattern opts pat
  PatTuple {} -> prettyPattern opts pat
  _ -> "(" <> prettyPattern opts pat <> ")"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Create indentation
indent :: Int -> Text
indent n = T.replicate n " "

-- | Wrap in parentheses conditionally
parensIf :: Bool -> Text -> Text
parensIf True t = "(" <> t <> ")"
parensIf False t = t

-- | Escape a string for output
escapeString :: Text -> Text
escapeString = T.concatMap escapeChar'
  where
    escapeChar' '\n' = "\\n"
    escapeChar' '\t' = "\\t"
    escapeChar' '\r' = "\\r"
    escapeChar' '\\' = "\\\\"
    escapeChar' '"' = "\\\""
    escapeChar' c = T.singleton c

-- | Escape a character for output
escapeChar :: Char -> Text
escapeChar '\n' = "\\n"
escapeChar '\t' = "\\t"
escapeChar '\r' = "\\r"
escapeChar '\\' = "\\\\"
escapeChar '\'' = "\\'"
escapeChar c = T.singleton c
