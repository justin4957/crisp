{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Parser.Parser
-- Description : Parser for the Crisp language
--
-- Parses Crisp source code into the surface AST.
-- Uses megaparsec with support for significant whitespace.

module Crisp.Parser.Parser
  ( parseModule
  , parseExpr
  , parseType
  , ParseError
  ) where

import Crisp.Syntax.Surface
import Crisp.Syntax.Span
import Crisp.Lexer.Token (lookupKeyword)

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parse error type
type ParseError = ParseErrorBundle Text Void

-- | Parser type
type Parser = Parsec Void Text

-- | Parse a module
parseModule :: FilePath -> Text -> Either ParseError Module
parseModule = parse (sc *> pModule <* eof)

-- | Parse an expression
parseExpr :: FilePath -> Text -> Either ParseError Expr
parseExpr = parse (sc *> pExpr <* eof)

-- | Parse a type
parseType :: FilePath -> Text -> Either ParseError Type
parseType = parse (sc *> pType <* eof)

-- * Whitespace and comments

-- | Space consumer (skips whitespace and regular comments, but not doc comments)
-- Doc comments start with "--- |" and are preserved in the AST.
sc :: Parser ()
sc = L.space
  space1
  pSkipNonDocComment
  (L.skipBlockCommentNested "{-" "-}")

-- | Skip a line comment that is NOT a doc comment (--- |)
pSkipNonDocComment :: Parser ()
pSkipNonDocComment = try $ do
  _ <- string "--"
  notFollowedBy (string "- |")
  void $ takeWhileP Nothing (/= '\n')

-- | Parse one or more consecutive doc comment lines starting with @--- |@.
-- Multiple lines are joined with newlines to support multi-line doc comments.
pDocComment :: Parser DocComment
pDocComment = do
  firstLine <- pDocCommentLine
  moreLines <- many (try pDocCommentLine)
  pure $ T.stripEnd $ T.intercalate "\n" (firstLine : moreLines)

-- | Parse a single doc comment line: --- | text
-- Returns the text content after "--- |", preserving leading indentation.
-- Strips the single conventional space after the pipe character.
pDocCommentLine :: Parser Text
pDocCommentLine = do
  _ <- string "--- |"
  content <- takeWhileP Nothing (/= '\n')
  sc  -- consume trailing whitespace after the doc comment line
  pure $ T.stripEnd $ if T.isPrefixOf " " content then T.drop 1 content else content

-- | Lexeme - consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol - parse a specific string
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Get current position
getPos :: Parser Position
getPos = do
  pos <- getSourcePos
  pure $ Position (unPos $ sourceLine pos) (unPos $ sourceColumn pos)

-- | Get current file name
getFile :: Parser Text
getFile = T.pack . sourceName <$> getSourcePos

-- | Create span from start position
spanFrom :: Position -> Parser Span
spanFrom start = do
  end <- getPos
  file <- getFile
  pure $ Span start end file

-- * Keywords and identifiers

-- | Parse a keyword
keyword :: Text -> Parser ()
keyword kw = lexeme $ try $ do
  void $ string kw
  notFollowedBy alphaNumChar

-- | Parse a context-sensitive keyword (a word that acts as keyword only in specific positions)
contextKeyword :: Text -> Parser ()
contextKeyword kw = lexeme $ try $ do
  void $ string kw
  notFollowedBy (alphaNumChar <|> char '_' <|> char '\'')

-- | Parse a lower-case identifier
lowerIdent :: Parser Text
lowerIdent = lexeme $ try $ do
  first <- lowerChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  let name = T.pack (first : rest)
  case lookupKeyword name of
    Just _ -> fail $ "keyword " ++ T.unpack name ++ " cannot be used as identifier"
    Nothing -> pure name

-- | Parse an upper-case identifier
upperIdent :: Parser Text
upperIdent = lexeme $ try $ do
  first <- upperChar
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack (first : rest)

-- | Parse any identifier
identifier :: Parser Text
identifier = lowerIdent <|> upperIdent

-- * Module parsing

pModule :: Parser Module
pModule = do
  start <- getPos
  doc <- optional (try pDocComment)
  keyword "module"
  name <- pModulePath
  auth <- optional (try (contextKeyword "authority") *> upperIdent)
  reqLists <- many pRequires
  provLists <- many pProvides
  defs <- many pDefinition
  span' <- spanFrom start
  pure $ Module name auth (concat reqLists) (concat provLists) defs span' doc

pModulePath :: Parser ModulePath
pModulePath = do
  start <- getPos
  segs <- upperIdent `sepBy1` symbol "."
  span' <- spanFrom start
  pure $ ModulePath segs span'

-- | Parse requires declarations
-- Supports both inline format:
--   requires ModulePath
--   requires effects: E1, E2
-- And block format:
--   requires
--     ModulePath
--     effects: E1, E2
pRequires :: Parser [Require]
pRequires = do
  baseCol <- unPos . sourceColumn <$> getSourcePos
  keyword "requires"
  choice
    [ -- Block format: multiple indented items after bare 'requires'
      try $ some (pRequireItemIndented baseCol)
    , -- Inline format: single item on same line
      do start <- getPos
         item <- pRequireItem start
         pure [item]
    ]

-- | Parse a require item that must be indented past the base column
pRequireItemIndented :: Int -> Parser Require
pRequireItemIndented baseCol = try $ do
  col <- unPos . sourceColumn <$> getSourcePos
  if col > baseCol
    then do
      start <- getPos
      pRequireItem start
    else fail "requires item not indented"

-- | Parse a single require item (after the "requires" keyword)
pRequireItem :: Position -> Parser Require
pRequireItem start = choice
  [ do keyword "effects"
       symbol ":"
       effs <- upperIdent `sepBy1` symbol ","
       span' <- spanFrom start
       pure $ RequireEffects effs span'
  , do keyword "types"
       symbol ":"
       tys <- upperIdent `sepBy1` symbol ","
       span' <- spanFrom start
       pure $ RequireTypes tys span'
  , do -- Module import: requires ModulePath
       modPath <- pModulePath
       span' <- spanFrom start
       pure $ RequireModule modPath span'
  ]

-- | Parse provides declarations
-- Supports both inline format:
--   provides type Name
--   provides fn name: Type
-- And block format:
--   provides
--     type Name
--     fn name
pProvides :: Parser [Provide]
pProvides = do
  baseCol <- unPos . sourceColumn <$> getSourcePos
  keyword "provides"
  choice
    [ -- Block format: multiple indented items after bare 'provides'
      -- Items must be indented more than the 'provides' keyword
      try $ some (pProvideItemIndented baseCol)
    , -- Inline format: single item on same line
      do start <- getPos
         item <- pProvideItem start
         pure [item]
    ]

-- | Parse a provide item that must be indented past the base column
pProvideItemIndented :: Int -> Parser Provide
pProvideItemIndented baseCol = try $ do
  -- Check that current position is indented past base
  col <- unPos . sourceColumn <$> getSourcePos
  if col > baseCol
    then do
      -- Skip optional doc comment before provide item
      _ <- optional (try pDocComment)
      start <- getPos
      pProvideItem start
    else fail "provides item not indented"

-- | Parse a single provide item (after the "provides" keyword)
pProvideItem :: Position -> Parser Provide
pProvideItem start = choice
  [ -- type prop Name (must come before type Name)
    try $ do
       keyword "type"
       keyword "prop"
       name <- upperIdent
       span' <- spanFrom start
       pure $ ProvideTypeProp name span'
  , do keyword "type"
       name <- upperIdent
       span' <- spanFrom start
       pure $ ProvideType name span'
  , do keyword "effect"
       name <- upperIdent
       span' <- spanFrom start
       pure $ ProvideEffect name span'
  , do keyword "trait"
       name <- upperIdent
       span' <- spanFrom start
       pure $ ProvideTrait name span'
  , do keyword "handler"
       name <- upperIdent
       span' <- spanFrom start
       pure $ ProvideHandler name span'
  , do keyword "external"
       keyword "fn"
       name <- lowerIdent
       mTy <- optional (symbol ":" *> pType)
       span' <- spanFrom start
       pure $ ProvideExternalFn name mTy span'
  , do keyword "fn"
       name <- lowerIdent
       -- Type annotation is optional
       mTy <- optional (symbol ":" *> pType)
       span' <- spanFrom start
       pure $ ProvideFn name mTy span'
  ]

-- * Definition parsing

pDefinition :: Parser Definition
pDefinition = do
  doc <- optional (try pDocComment)
  choice
    [ pTypeOrAlias doc  -- Handle both type definitions and type aliases
    , DefEffect <$> pEffectDef doc
    , DefHandler <$> pHandlerDef doc
    , DefTrait <$> pTraitDef doc
    , DefImpl <$> pImplDef doc
    , DefExternal <$> pExternalFnDef doc
    , DefLet <$> pLetDef doc
    , DefFn <$> pFunctionDef doc
    ]
  where
    -- Parse either a type alias (type Name = ...) or type definition (type Name: ...)
    -- We look ahead after parsing name and params to decide which one
    pTypeOrAlias doc = do
      start <- getPos
      keyword "type"
      mods <- pTypeModifiers
      name <- upperIdent
      params <- concat <$> many (try pTypeParamGroupNotEq)
      -- Now decide based on what comes next
      choice
        [ do -- Type alias: has = after name/params
             symbol "="
             -- Use pTypeAppNoRefinement so { field: Pattern } isn't parsed as refinement
             baseType <- pTypeAppNoRefinement
             -- Support 'extended with:' for record extension,
             -- 'where { predicate }' for refinement types,
             -- 'where field: Pattern' for field constraints without braces,
             -- and '{ field: Pattern }' for field constraints with braces
             (finalType, constraints, extFields) <- choice
               [ do -- Extended record: type Name = Base extended with:
                    --   field: Type
                    contextKeyword "extended"
                    keyword "with"
                    symbol ":"
                    fields <- some pField
                    pure (baseType, [], fields)
               , do -- where clause: refinement or field constraint
                    keyword "where"
                    choice
                      [ do -- Refinement type: type Name = Base where { predicate }
                           (preds, refinementSpan) <- pRefinementBlock
                           pure (TyRefinement baseType preds refinementSpan, [], [])
                      , do -- Field constraint without braces: type Name = Base where field: Pattern
                           fieldConstraints <- pFieldConstraint `sepBy1` symbol ","
                           pure (baseType, fieldConstraints, [])
                      ]
               , do -- Field constraints with braces: type Name = Base { field: Pattern }
                    fieldConstraints <- option [] pFieldConstraintBlock
                    pure (baseType, fieldConstraints, [])
               ]
             span' <- spanFrom start
             pure $ DefTypeAlias $ TypeAliasDef doc name params finalType constraints extFields span'
        , do -- Regular type definition
             constraints <- option [] pTypeDefConstraints
             mKind <- optional (try pTypeDefKind)
             deriv <- optional pDerivingClause
             cons <- optional (symbol ":" *> pConstructors)
             span' <- spanFrom start
             -- Fix up record constructor names (replace empty name with type name)
             let fixedCons = case cons of
                   Just [RecordConstructor "" fields s] ->
                     [RecordConstructor name fields s]
                   _ -> maybe [] id cons
             pure $ DefType $ TypeDef doc name params constraints mKind fixedCons mods deriv span'
        ]

    -- Type parameter group that doesn't consume = sign
    pTypeParamGroupNotEq = do
      notFollowedBy (symbol "=")
      pTypeParamGroup

    pTypeDefKind = symbol ":" *> pKind

pTypeDef :: Parser TypeDef
pTypeDef = do
  start <- getPos
  keyword "type"
  mods <- pTypeModifiers
  name <- upperIdent
  params <- concat <$> many pTypeParamGroup
  constraints <- option [] pTypeDefConstraints
  -- Kind annotation must be followed by a valid kind keyword, not a constructor
  mKind <- optional (try pTypeDefKind)
  deriv <- optional pDerivingClause
  cons <- optional (symbol ":" *> pConstructors)
  span' <- spanFrom start
  pure $ TypeDef Nothing name params constraints mKind (maybe [] id cons) mods deriv span'
  where
    -- Parse kind annotation for type definition: : Type or : Type -> Type
    -- Uses try to backtrack if what follows : isn't a valid kind
    pTypeDefKind = do
      symbol ":"
      pKind

-- | Parse a block of field constraints: { field: Pattern, ... }
pFieldConstraintBlock :: Parser [FieldConstraint]
pFieldConstraintBlock = do
  symbol "{"
  constraints <- pFieldConstraint `sepBy1` symbol ","
  symbol "}"
  pure constraints

-- | Parse a single field constraint: field: Pattern | Pattern | ...
-- Supports OR patterns separated by @|@
pFieldConstraint :: Parser FieldConstraint
pFieldConstraint = do
  start <- getPos
  fieldName <- lowerIdent
  symbol ":"
  patterns <- pPattern `sepBy1` symbol "|"
  span' <- spanFrom start
  pure $ FieldConstraint fieldName patterns span'

-- | Parse where clause constraints for type definitions
-- Example: where A: Action, B: Serializable
pTypeDefConstraints :: Parser [TraitConstraint]
pTypeDefConstraints = do
  keyword "where"
  pTraitConstraint `sepBy1` symbol ","

pDerivingClause :: Parser DerivingClause
pDerivingClause = do
  start <- getPos
  keyword "deriving"
  traits <- choice
    [ between (symbol "(") (symbol ")") (upperIdent `sepBy1` symbol ",")
    , (: []) <$> upperIdent  -- Single trait without parens
    ]
  span' <- spanFrom start
  pure $ DerivingClause traits span'

pTypeModifiers :: Parser TypeModifiers
pTypeModifiers = do
  isProp <- option False (True <$ keyword "prop")
  isLinear <- option False (True <$ keyword "linear")
  pure $ TypeModifiers isProp isLinear

-- | Parse constructors, handling both regular constructors and record fields
-- Record fields look like: field: Type (lowercase name followed by colon)
-- Regular constructors look like: ConstructorName args (uppercase name)
pConstructors :: Parser [Constructor]
pConstructors = choice
  [ -- Try parsing as record fields (for record-style types)
    -- Record fields start with lowercase identifier followed by colon
    try $ do
      start <- getPos
      fields <- some pField
      span' <- spanFrom start
      -- Create a single record constructor with the fields
      -- The name "" will be replaced with the type name by the caller
      pure [RecordConstructor "" fields span']
  , -- Regular constructors (uppercase names)
    many pConstructor
  ]

pConstructor :: Parser Constructor
pConstructor = do
  start <- getPos
  name <- upperIdent
  choice
    [ -- GADT constructor: Cons : Type
      do symbol ":"
         ty <- pType
         span' <- spanFrom start
         pure $ GadtConstructor name ty span'
    , -- Record constructor with braces: { field: Type, ... }
      try $ do
        symbol "{"
        fields <- pField `sepBy1` symbol ","
        symbol "}"
        span' <- spanFrom start
        pure $ RecordConstructor name fields span'
    , -- Parenthesized constructor: Cons(args) or Cons(field: Type, ...)
      try $ do
        symbol "("
        -- Try to parse as named fields first (lowercase name followed by colon)
        choice
          [ -- Named fields: Cons(field: Type, ...)
            try $ do
              fields <- pConstructorField `sepBy1` symbol ","
              symbol ")"
              span' <- spanFrom start
              pure $ RecordConstructor name fields span'
          , -- Positional types: Cons(Type1, Type2, ...)
            do args <- pTypeApp `sepBy1` symbol ","
               symbol ")"
               span' <- spanFrom start
               pure $ SimpleConstructor name args span'
          ]
    , -- Simple positional constructor without parens: Cons Type1 Type2
      -- Don't consume uppercase identifiers that could be new constructors
      do args <- many pSimpleConstructorArg
         span' <- spanFrom start
         pure $ SimpleConstructor name args span'
    ]

-- | Parse a type argument for simple constructors
-- This is more restrictive than pTypeAtom to avoid consuming subsequent constructors
-- We don't consume bare uppercase identifiers since they could be new constructors
pSimpleConstructorArg :: Parser Type
pSimpleConstructorArg = choice
  [ -- Parenthesized types are safe
    do start <- getPos
       symbol "("
       ty <- pType
       symbol ")"
       span' <- spanFrom start
       pure $ TyParen ty span'
  , -- Lazy types
    do start <- getPos
       keyword "Lazy"
       ty <- pSimpleConstructorArg
       span' <- spanFrom start
       pure $ TyLazy ty span'
  , -- Ref types
    do start <- getPos
       keyword "ref"
       mut <- option False (True <$ keyword "mut")
       ty <- pSimpleConstructorArg
       span' <- spanFrom start
       pure $ TyRef ty mut span'
  , -- Only lowercase type variables are allowed as bare arguments
    -- Uppercase identifiers could be new constructors
    do start <- getPos
       name <- lowerIdent
       span' <- spanFrom start
       pure $ TyName name span'
  ]

-- | Parse a record field: name: Type
pField :: Parser Field
pField = do
  start <- getPos
  name <- lowerIdent
  symbol ":"
  ty <- pType
  span' <- spanFrom start
  pure $ Field name ty span'

-- | Parse a constructor field: name: Type (using pTypeApp to avoid consuming commas)
pConstructorField :: Parser Field
pConstructorField = do
  start <- getPos
  name <- lowerIdent
  symbol ":"
  ty <- pTypeApp  -- Use pTypeApp instead of pType to not consume arrows/effects
  span' <- spanFrom start
  pure $ Field name ty span'

-- | Parse one or more type parameters, supporting comma-separated dependent
-- parameters in a single paren group: (j: Jurisdiction, temporal: TemporalRange)
-- Also supports parenthesized bare type variables: (A) or (A, B)
pTypeParamGroup :: Parser [TypeParam]
pTypeParamGroup = choice
  [ -- Multiple comma-separated dependent parameters in one paren group:
    -- (name1: Type1, name2: Type2, ...)
    try $ do
      symbol "("
      params <- pDepParamEntry `sepBy1` symbol ","
      symbol ")"
      pure params
  , -- Parenthesized bare type variables without kind: (A) or (A, B)
    try $ do
      symbol "("
      params <- pBareTypeVarEntry `sepBy1` symbol ","
      symbol ")"
      pure params
  , -- Single type parameter (original behavior)
    (:[]) <$> pTypeParam
  ]
  where
    pDepParamEntry = do
      start <- getPos
      name <- lowerIdent
      symbol ":"
      ty <- pType
      span' <- spanFrom start
      pure $ DepParam name ty span'
    pBareTypeVarEntry = do
      start <- getPos
      name <- upperIdent
      span' <- spanFrom start
      pure $ TypeVar name Nothing span'

pTypeParam :: Parser TypeParam
pTypeParam = choice
  [ -- Dependent parameter: (n: Nat)
    try $ do
      start <- getPos
      symbol "("
      name <- lowerIdent
      symbol ":"
      ty <- pType
      symbol ")"
      span' <- spanFrom start
      pure $ DepParam name ty span'
  , -- Bounded type parameter with kind and constraints: (A: Type: Trait1 + Trait2)
    -- or just constraints: (A: Trait1 + Trait2)
    try $ do
      start <- getPos
      symbol "("
      name <- upperIdent
      symbol ":"
      -- Parse either a kind followed by optional constraints, or just trait constraints
      (mKind, traits) <- pBoundedTypeParamBody
      symbol ")"
      span' <- spanFrom start
      if null traits
        then pure $ TypeVar name mKind span'
        else pure $ BoundedTypeVar name mKind traits span'
  , -- Simple type parameter with optional kind: A or A: Type
    -- Only parse kind if followed by a kind keyword, not a constructor
    do start <- getPos
       name <- upperIdent
       mKind <- optional (try (symbol ":" *> pKind))
       span' <- spanFrom start
       pure $ TypeVar name mKind span'
  ]

-- | Parse the body of a bounded type parameter
-- Can be: Kind: Trait1 + Trait2, Kind, or Trait1 + Trait2
-- Kinds are: Type, Prop, Linear, or arrow kinds (Type -> Type)
-- Traits are uppercase identifiers that aren't kind keywords
pBoundedTypeParamBody :: Parser (Maybe Kind, [Text])
pBoundedTypeParamBody = choice
  [ -- Try kind followed by constraints: Type: Trait1 + Trait2
    try $ do
      kind <- pKind
      symbol ":"
      traits <- pTraitBounds
      pure (Just kind, traits)
  , -- Try just kind: Type or Type -> Type
    -- Only matches if followed by ) or :
    try $ do
      kind <- pKind
      -- Must be followed by ) to be a valid kind-only param
      lookAhead (symbol ")")
      pure (Just kind, [])
  , -- Just trait bounds: Trait1 + Trait2
    do traits <- pTraitBounds
       pure (Nothing, traits)
  ]

-- | Parse trait bounds: Trait1 + Trait2 + ...
-- These are uppercase identifiers that are NOT kind keywords
pTraitBounds :: Parser [Text]
pTraitBounds = pTraitName `sepBy1` symbol "+"
  where
    pTraitName = try $ do
      name <- upperIdent
      -- Ensure it's not a kind keyword
      if name `elem` ["Type", "Prop", "Linear"]
        then fail $ "Expected trait name, got kind keyword: " ++ T.unpack name
        else pure name

pKind :: Parser Kind
pKind = do
  k1 <- pKindAtom
  mArrow <- optional (symbol "->")
  case mArrow of
    Nothing -> pure k1
    Just _ -> do
      k2 <- pKind
      start <- getPos
      span' <- spanFrom start
      pure $ KindArrow k1 k2 span'
  where
    pKindAtom = do
      start <- getPos
      choice
        [ keyword "Type" *> (KindType Nothing <$> spanFrom start)
        , keyword "Prop" *> (KindProp <$> spanFrom start)
        , keyword "Linear" *> (KindLinear <$> spanFrom start)
        ]

pEffectDef :: Maybe DocComment -> Parser EffectDef
pEffectDef doc = do
  start <- getPos
  keyword "effect"
  name <- upperIdent
  typeParams <- many pTypeParam
  symbol ":"
  ops <- many pOperation
  span' <- spanFrom start
  pure $ EffectDef doc name typeParams ops span'

pOperation :: Parser Operation
pOperation = do
  doc <- optional (try pDocComment)
  start <- getPos
  name <- lowerIdent
  -- Support both syntaxes:
  -- 1. name: Type (simple)
  -- 2. name(params) -> ReturnType (function-style)
  ty <- pOperationSig start
  span' <- spanFrom start
  pure $ Operation doc name ty span'
  where
    -- Parse operation signature: either ": Type" or "(params) -> ReturnType"
    pOperationSig opStart = choice
      [ do symbol ":"
           pType
      , do params <- between (symbol "(") (symbol ")") (pParam `sepBy` symbol ",")
           symbol "->"
           retTy <- pType
           -- Build curried function type from params: A -> B -> C -> ReturnType
           span' <- spanFrom opStart
           pure $ foldr (\p acc -> TyFn (paramType p) acc [] span') retTy params
      ]

pHandlerDef :: Maybe DocComment -> Parser HandlerDef
pHandlerDef doc = do
  start <- getPos
  keyword "handler"
  name <- upperIdent
  params <- option [] pHandlerParams
  keyword "for"
  effect <- upperIdent
  intros <- option [] (symbol "!" *> pEffectList)
  symbol ":"
  clauses <- many pHandlerClause
  span' <- spanFrom start
  pure $ HandlerDef doc name params effect intros clauses span'

-- | Parse handler parameters as a comma-separated list in parentheses
pHandlerParams :: Parser [HandlerParam]
pHandlerParams =
  between (symbol "(") (symbol ")") (pHandlerParamEntry `sepBy1` symbol ",")

-- | Parse a single handler parameter entry (value or type)
pHandlerParamEntry :: Parser HandlerParam
pHandlerParamEntry = choice
  [ try $ do
      start <- getPos
      name <- lowerIdent
      symbol ":"
      ty <- pType
      span' <- spanFrom start
      pure $ HandlerValueParam name ty span'
  , do start <- getPos
       name <- upperIdent
       symbol ":"
       kind <- pKind
       span' <- spanFrom start
       pure $ HandlerTypeParam name (Just kind) span'
  ]

pHandlerClause :: Parser HandlerClause
pHandlerClause = choice
  [ do start <- getPos
       keyword "return"
       pat <- pPattern
       symbol "->"
       body <- pExpr
       span' <- spanFrom start
       pure $ ReturnClause pat body span'
  , do start <- getPos
       name <- lowerIdent
       pats <- many pPatternAtom
       symbol "->"
       keyword "resume"
       symbol ":"
       body <- pExpr
       span' <- spanFrom start
       pure $ OpClause name pats "resume" body span'
  ]

-- * Trait and implementation parsing

-- | Parse a trait definition
-- Example: trait Ord A:
--            compare: (A, A) -> Ordering
-- Or with kind: trait Functor (F: Type -> Type):
pTraitDef :: Maybe DocComment -> Parser TraitDef
pTraitDef doc = do
  start <- getPos
  keyword "trait"
  name <- upperIdent
  (mParam, paramKind) <- option (Nothing, Nothing) pTraitParam
  supers <- option [] pSupertraits
  symbol ":"
  methods <- many pTraitMethod
  span' <- spanFrom start
  pure $ TraitDef doc name mParam paramKind supers methods span'
  where
    -- Parse trait parameter with optional kind annotation in parens
    -- Returns Nothing for parameterless traits like "trait Action:"
    pTraitParam = choice
      [ try $ do
          symbol "("
          p <- upperIdent
          symbol ":"
          k <- pKind
          symbol ")"
          pure (Just p, Just k)
      , try $ do
          p <- upperIdent
          pure (Just p, Nothing)
      ]

-- | Parse supertrait constraints
-- Example: where A: Eq
pSupertraits :: Parser [TraitConstraint]
pSupertraits = do
  keyword "where"
  pTraitConstraint `sepBy1` symbol ","

-- | Parse a single trait constraint
pTraitConstraint :: Parser TraitConstraint
pTraitConstraint = do
  start <- getPos
  ty <- pTypeAtom
  symbol ":"
  traitName <- upperIdent
  span' <- spanFrom start
  pure $ TraitConstraint traitName ty span'

-- | Parse a trait method signature (supports both sig-style and fn-style)
pTraitMethod :: Parser TraitMethod
pTraitMethod = try pTraitMethodFnStyle <|> try pTraitMethodSigStyle

-- | Parse fn-style trait method: fn name(self, ...) -> Type
pTraitMethodFnStyle :: Parser TraitMethod
pTraitMethodFnStyle = do
  start <- getPos
  keyword "fn"
  name <- lowerIdent
  params <- option [] (between (symbol "(") (symbol ")") (pParamOrSelf `sepBy` symbol ","))
  retTy <- optional (symbol "->" *> pType)
  mDefault <- optional (symbol "=" *> pExpr)
  span' <- spanFrom start
  let composedType = buildFnType params retTy span'
  pure $ TraitMethod name composedType mDefault TraitMethodFnStyle params retTy span'

-- | Parse sig-style trait method: name: Type
pTraitMethodSigStyle :: Parser TraitMethod
pTraitMethodSigStyle = do
  start <- getPos
  name <- lowerIdent
  symbol ":"
  ty <- pType
  mDefault <- optional (symbol "=" *> pExpr)
  span' <- spanFrom start
  pure $ TraitMethod name ty mDefault TraitMethodSigStyle [] Nothing span'

-- | Build a curried function type from parameters and optional return type
buildFnType :: [Param] -> Maybe Type -> Span -> Type
buildFnType params mRetTy span' =
  let retTy = case mRetTy of
        Just ty -> ty
        Nothing -> TyName "Unit" span'
  in foldr (\p acc -> TyFn (paramType p) acc [] span') retTy params

-- | Parse an implementation definition
-- Example: impl Ord for Int:
--            fn compare(a: Int, b: Int) -> Ordering: int_compare(a, b)
pImplDef :: Maybe DocComment -> Parser ImplDef
pImplDef doc = do
  start <- getPos
  keyword "impl"
  traitName <- upperIdent
  keyword "for"
  ty <- pTypeApp
  symbol ":"
  methods <- many pImplMethod
  span' <- spanFrom start
  pure $ ImplDef doc traitName ty methods span'

-- | Parse a method implementation within an impl block
pImplMethod :: Parser FunctionDef
pImplMethod = do
  doc <- optional (try pDocComment)
  pFunctionDef doc

-- * External function (FFI) parsing

-- | Parse an external function reference
-- Example: external("postgres", "query")
pExternalRef :: Parser ExternalRef
pExternalRef = do
  start <- getPos
  keyword "external"
  symbol "("
  modName <- lexeme $ char '"' *> manyTill L.charLiteral (char '"')
  symbol ","
  fnName <- lexeme $ char '"' *> manyTill L.charLiteral (char '"')
  symbol ")"
  span' <- spanFrom start
  pure $ ExternalRef (T.pack modName) (T.pack fnName) span'

-- | Parse an external function definition
-- Example: external fn query(sql: String) -> String = ("postgres", "query")
pExternalFnDef :: Maybe DocComment -> Parser ExternalFnDef
pExternalFnDef doc = do
  start <- getPos
  keyword "external"
  keyword "fn"
  name <- lowerIdent
  params <- option [] (between (symbol "(") (symbol ")") (pParam `sepBy` symbol ","))
  symbol "->"
  retTy <- pType
  symbol "="
  symbol "("
  modName <- lexeme $ char '"' *> manyTill L.charLiteral (char '"')
  symbol ","
  fnName <- lexeme $ char '"' *> manyTill L.charLiteral (char '"')
  symbol ")"
  span' <- spanFrom start
  let extRef = ExternalRef (T.pack modName) (T.pack fnName) span'
  pure $ ExternalFnDef doc name params retTy extRef span'

pFunctionDef :: Maybe DocComment -> Parser FunctionDef
pFunctionDef doc = do
  start <- getPos
  keyword "fn"
  name <- lowerIdent
  tyParams <- option [] (between (symbol "[") (symbol "]") (pTypeParam `sepBy` symbol ","))
  params <- option [] (between (symbol "(") (symbol ")") (pParamOrSelf `sepBy` symbol ","))
  retTy <- optional (symbol "->" *> pType)
  effs <- option [] (symbol "!" *> pEffectList)
  symbol ":"
  body <- pExpr
  span' <- spanFrom start
  pure $ FunctionDef doc name tyParams params retTy effs body span'

-- | Parse a top-level let binding: @let pattern: Type = expr@
pLetDef :: Maybe DocComment -> Parser LetDef
pLetDef doc = do
  start <- getPos
  keyword "let"
  pat <- pPattern
  mTy <- optional (symbol ":" *> pType)
  symbol "="
  value <- pExpr
  span' <- spanFrom start
  pure $ LetDef doc pat mTy value span'

pParam :: Parser Param
pParam = do
  start <- getPos
  name <- lowerIdent
  symbol ":"
  ty <- pType
  span' <- spanFrom start
  pure $ Param name ty span'

-- | Parse bare @self@ as a parameter with implicit @Self@ type
pSelfParam :: Parser Param
pSelfParam = do
  start <- getPos
  keyword "self"
  span' <- spanFrom start
  pure $ Param "self" (TyName "Self" span') span'

-- | Parse either a bare @self@ parameter or a regular @name: Type@ parameter
pParamOrSelf :: Parser Param
pParamOrSelf = pSelfParam <|> pParam

pEffectList :: Parser [EffectRef]
pEffectList = pEffectRef `sepBy1` symbol ","

pEffectRef :: Parser EffectRef
pEffectRef = do
  start <- getPos
  name <- upperIdent
  auth <- optional (symbol "@" *> upperIdent)
  span' <- spanFrom start
  pure $ EffectRef name auth span'

-- * Type parsing

pType :: Parser Type
pType = do
  result <- pTypeInner
  -- Check for trailing effect annotation
  mEffs <- optional (symbol "!" *> pEffectList)
  case mEffs of
    Nothing -> pure result
    Just effs -> case result of
      -- Attach effects to the innermost function type
      TyFn from to [] span' -> pure $ TyFn from to effs span'
      _ -> do
        start <- getPos
        span' <- spanFrom start
        -- Create a function type with effects (for effectful expressions)
        pure $ TyFn result result effs span'  -- Placeholder, effects on non-function types

pTypeInner :: Parser Type
pTypeInner = makeExprParser pTypeApp
  [ [InfixR pArrow] ]
  where
    pArrow = do
      start <- getPos
      _ <- symbol "->"
      span' <- spanFrom start
      pure $ \from to -> TyFn from to [] span'

pTypeApp :: Parser Type
pTypeApp = do
  start <- getPos
  con <- pTypeAtom
  -- Only consume uppercase type atoms as arguments (not lowercase which could be declarations)
  args <- many pTypeArgAtom
  span' <- spanFrom start
  case args of
    [] -> pure con
    _  -> pure $ TyApp con args span'
  where
    -- Type argument atoms are more restricted - don't greedily consume lowercase identifiers
    -- that might be method/field declarations
    pTypeArgAtom = choice
      [ do start' <- getPos
           symbol "("
           types <- pType `sepBy` symbol ","
           symbol ")"
           span' <- spanFrom start'
           pure $ case types of
             [ty] -> TyParen ty span'
             _    -> TyTuple types span'
      , do start' <- getPos
           keyword "Lazy"
           ty <- pTypeAtom
           span' <- spanFrom start'
           pure $ TyLazy ty span'
      , do start' <- getPos
           keyword "ref"
           mut <- option False (True <$ keyword "mut")
           ty <- pTypeAtom
           span' <- spanFrom start'
           pure $ TyRef ty mut span'
      , do start' <- getPos
           symbol "_"
           span' <- spanFrom start'
           pure $ TyWild span'
      , do start' <- getPos
           -- Only consume uppercase identifiers as type arguments
           name <- upperIdent
           span' <- spanFrom start'
           pure $ TyName name span'
      ]

-- | Parse type application without refinement blocks
-- Used for type alias base types where { field: Pattern } is field constraints, not refinements
pTypeAppNoRefinement :: Parser Type
pTypeAppNoRefinement = do
  start <- getPos
  con <- pTypeAtomBase  -- Use base without refinement checking
  args <- many pTypeArgAtomNoRefinement
  span' <- spanFrom start
  case args of
    [] -> pure con
    _  -> pure $ TyApp con args span'
  where
    pTypeArgAtomNoRefinement = choice
      [ do start' <- getPos
           symbol "("
           types <- pType `sepBy` symbol ","
           symbol ")"
           span' <- spanFrom start'
           pure $ case types of
             [ty] -> TyParen ty span'
             _    -> TyTuple types span'
      , do start' <- getPos
           symbol "_"
           span' <- spanFrom start'
           pure $ TyWild span'
      , do start' <- getPos
           -- Only consume uppercase identifiers as type arguments
           name <- upperIdent
           span' <- spanFrom start'
           pure $ TyName name span'
      ]

pTypeAtom :: Parser Type
pTypeAtom = do
  baseType <- pTypeAtomBase
  -- Check for refinement type syntax: T { predicate }
  mRefinement <- optional pRefinementBlock
  case mRefinement of
    Nothing -> pure baseType
    Just (preds, span') -> pure $ TyRefinement baseType preds span'

-- | Parse the base type atom (without refinement)
pTypeAtomBase :: Parser Type
pTypeAtomBase = choice
  [ -- fn(A, B) -> C type syntax for function types
    try pFnType
  , do start <- getPos
       symbol "("
       types <- pType `sepBy` symbol ","
       symbol ")"
       span' <- spanFrom start
       pure $ case types of
         [ty] -> TyParen ty span'
         _    -> TyTuple types span'
  , do start <- getPos
       keyword "Lazy"
       ty <- pTypeAtom
       span' <- spanFrom start
       pure $ TyLazy ty span'
  , do start <- getPos
       keyword "ref"
       mut <- option False (True <$ keyword "mut")
       ty <- pTypeAtom
       span' <- spanFrom start
       pure $ TyRef ty mut span'
  , do start <- getPos
       keyword "forall"
       param <- pTypeParam
       symbol "."
       body <- pType
       span' <- spanFrom start
       pure $ TyForall param body span'
  , do start <- getPos
       symbol "_"
       span' <- spanFrom start
       pure $ TyWild span'
  , do start <- getPos
       name <- identifier
       span' <- spanFrom start
       pure $ TyName name span'
  ]

-- | Parse fn(A, B) -> C function type syntax.
-- fn(A) -> B   parses as   A -> B
-- fn(A, B) -> C   parses as   (A, B) -> C
-- fn() -> A   parses as   Unit -> A
pFnType :: Parser Type
pFnType = do
  start <- getPos
  keyword "fn"
  symbol "("
  paramTypes <- pType `sepBy` symbol ","
  symbol ")"
  symbol "->"
  returnType <- pType
  span' <- spanFrom start
  let fromType = case paramTypes of
        []  -> TyName "Unit" span'
        [t] -> t
        _   -> TyTuple paramTypes span'
  pure $ TyFn fromType returnType [] span'

-- * Refinement type parsing

-- | Parse a refinement block: { predicate }
pRefinementBlock :: Parser ([RefinementPredicate], Span)
pRefinementBlock = do
  start <- getPos
  symbol "{"
  preds <- pRefinementPredicate `sepBy1` symbol ","
  symbol "}"
  span' <- spanFrom start
  pure (preds, span')

-- | Parse a refinement predicate (with && and || connectives)
pRefinementPredicate :: Parser RefinementPredicate
pRefinementPredicate = makeExprParser pRefinementAtom
  [ [InfixL pAnd]
  , [InfixL pOr]
  ]
  where
    pAnd = do
      start <- getPos
      symbol "&&"
      span' <- spanFrom start
      pure $ \left right -> RefinementAnd left right span'
    pOr = do
      start <- getPos
      symbol "||"
      span' <- spanFrom start
      pure $ \left right -> RefinementOr left right span'

-- | Parse an atomic refinement predicate
pRefinementAtom :: Parser RefinementPredicate
pRefinementAtom = choice
  [ pRefinementNot
  , pRefinementParens
  , try pRefinementComparison
  , pRefinementExpr
  ]

-- | Parse negation: !pred
pRefinementNot :: Parser RefinementPredicate
pRefinementNot = do
  start <- getPos
  symbol "!"
  pred' <- pRefinementAtom
  span' <- spanFrom start
  pure $ RefinementNot pred' span'

-- | Parse parenthesized predicate
pRefinementParens :: Parser RefinementPredicate
pRefinementParens = do
  symbol "("
  pred' <- pRefinementPredicate
  symbol ")"
  pure pred'

-- | Parse a comparison predicate: expr op expr
-- Supports chained comparisons like: 1 <= self <= 12
pRefinementComparison :: Parser RefinementPredicate
pRefinementComparison = do
  start <- getPos
  left <- pRefinementExprAtom
  op <- pComparisonOp
  right <- pRefinementExprAtom
  -- Check for chained comparison (e.g., 1 <= self <= 12)
  mChain <- optional $ do
    op2 <- pComparisonOp
    right2 <- pRefinementExprAtom
    pure (op2, right2)
  span' <- spanFrom start
  case mChain of
    Nothing -> pure $ RefinementComparison left op right span'
    Just (op2, right2) ->
      -- Convert "a op1 b op2 c" to "(a op1 b) && (b op2 c)"
      let pred1 = RefinementComparison left op right span'
          pred2 = RefinementComparison right op2 right2 span'
      in pure $ RefinementAnd pred1 pred2 span'

-- | Parse a comparison operator
pComparisonOp :: Parser ComparisonOp
pComparisonOp = choice
  [ OpLe <$ symbol "<="
  , OpLt <$ symbol "<"
  , OpGe <$ symbol ">="
  , OpGt <$ symbol ">"
  , OpEq <$ symbol "=="
  , OpNe <$ symbol "/="
  ]

-- | Parse an expression for use in refinement predicates
pRefinementExpr :: Parser RefinementPredicate
pRefinementExpr = do
  start <- getPos
  expr <- pRefinementExprAtom
  span' <- spanFrom start
  pure $ RefinementExpr expr span'

-- | Parse an atomic expression in a refinement context
-- This is a simplified expression parser for refinements
pRefinementExprAtom :: Parser Expr
pRefinementExprAtom = do
  base <- pRefinementExprBase
  -- Parse optional field access chain: base.field.field2...
  pFieldAccessChain base

-- | Parse the base of a refinement expression (before field access)
pRefinementExprBase :: Parser Expr
pRefinementExprBase = choice
  [ pRefinementMatch
  , pRefinementIf
  , try pRefinementFloat  -- Must come before pRefinementInt since "0.0" starts with digits
  , pRefinementInt
  , pRefinementBool
  , pRefinementSelf
  , pRefinementVar
  , pRefinementParenExpr
  ]

-- | Parse a match expression in a refinement context
pRefinementMatch :: Parser Expr
pRefinementMatch = do
  start <- getPos
  keyword "match"
  subject <- pRefinementExprAtom
  arms <- many pRefinementMatchArm
  span' <- spanFrom start
  pure $ EMatch subject arms span'

-- | Parse a match arm in a refinement context
pRefinementMatchArm :: Parser MatchArm
pRefinementMatchArm = do
  start <- getPos
  pat <- pPattern
  guard' <- optional (symbol "|" *> pRefinementExprAtom)
  symbol "->"
  body <- pRefinementExprAtom
  span' <- spanFrom start
  pure $ MatchArm pat guard' body span'

-- | Parse an if expression in a refinement context
pRefinementIf :: Parser Expr
pRefinementIf = do
  start <- getPos
  keyword "if"
  cond <- pRefinementCondExpr
  keyword "then"
  then' <- pRefinementExprAtom
  keyword "else"
  else' <- pRefinementExprAtom
  span' <- spanFrom start
  pure $ EIf cond then' else' span'

-- | Parse a condition expression for if (supports comparisons)
pRefinementCondExpr :: Parser Expr
pRefinementCondExpr = try pRefinementCondComparison <|> pRefinementExprAtom

-- | Parse a comparison expression that returns Expr (for if conditions)
pRefinementCondComparison :: Parser Expr
pRefinementCondComparison = do
  start <- getPos
  left <- pRefinementExprAtom
  op <- pRefinementCondOp
  right <- pRefinementExprAtom
  span' <- spanFrom start
  pure $ EBinOp op left right span'

-- | Parse comparison operators returning BinOp
pRefinementCondOp :: Parser BinOp
pRefinementCondOp = choice
  [ OpLE <$ symbol "<="
  , OpGE <$ symbol ">="
  , OpLT <$ symbol "<"
  , OpGT <$ symbol ">"
  , OpEQ <$ symbol "=="
  , OpNE <$ symbol "/="
  ]

-- | Parse a boolean literal in a refinement (True or False constructors)
pRefinementBool :: Parser Expr
pRefinementBool = do
  start <- getPos
  name <- try $ do
    n <- upperIdent
    if n == "True" || n == "False"
      then pure n
      else fail "expected True or False"
  span' <- spanFrom start
  pure $ ECon name span'

-- | Parse field access chain: .field.field2...
pFieldAccessChain :: Expr -> Parser Expr
pFieldAccessChain base = do
  mField <- optional (symbol "." *> lowerIdent)
  case mField of
    Nothing -> pure base
    Just field -> do
      start <- getPos
      span' <- spanFrom start
      let accessed = EFieldAccess base field span'
      pFieldAccessChain accessed

-- | Parse a float literal in a refinement
pRefinementFloat :: Parser Expr
pRefinementFloat = do
  start <- getPos
  n <- lexeme L.float
  span' <- spanFrom start
  pure $ EFloatLit n span'

-- | Parse an integer literal in a refinement
pRefinementInt :: Parser Expr
pRefinementInt = do
  start <- getPos
  n <- lexeme L.decimal
  span' <- spanFrom start
  pure $ EIntLit n span'

-- | Parse 'self' - the special variable referring to values of the type
pRefinementSelf :: Parser Expr
pRefinementSelf = do
  start <- getPos
  keyword "self"
  span' <- spanFrom start
  pure $ EVar "self" span'

-- | Parse a variable in a refinement
pRefinementVar :: Parser Expr
pRefinementVar = do
  start <- getPos
  name <- lowerIdent
  span' <- spanFrom start
  pure $ EVar name span'

-- | Parse a parenthesized expression in a refinement
pRefinementParenExpr :: Parser Expr
pRefinementParenExpr = do
  symbol "("
  e <- pRefinementExprAtom
  symbol ")"
  pure e

-- * Expression parsing

pExpr :: Parser Expr
pExpr = makeExprParser pCompare
  [ -- Lowest precedence: logical OR
    [InfixL pOr]
  , -- Logical AND
    [InfixL pAnd]
  , -- Pipeline
    [InfixL pPipe]
  ]
  where
    pPipe = do
      start <- getPos
      symbol "|>"
      span' <- spanFrom start
      pure $ \left right -> EPipe left right span'
    pAnd = do
      start <- getPos
      symbol "&&"
      span' <- spanFrom start
      pure $ \left right -> EBinOp OpAnd left right span'
    pOr = do
      start <- getPos
      symbol "||"
      span' <- spanFrom start
      pure $ \left right -> EBinOp OpOr left right span'

-- | Parse comparison expressions
pCompare :: Parser Expr
pCompare = makeExprParser pCons
  [ [InfixN pLe, InfixN pLt, InfixN pGe, InfixN pGt, InfixN pEq, InfixN pNe] ]
  where
    pLe = mkBinOp "<=" OpLE
    pLt = mkBinOp "<" OpLT
    pGe = mkBinOp ">=" OpGE
    pGt = mkBinOp ">" OpGT
    pEq = mkBinOp "==" OpEQ
    pNe = mkBinOp "/=" OpNE

-- | Parse cons operator (right-associative, between comparison and additive)
pCons :: Parser Expr
pCons = makeExprParser pRange
  [ [InfixR pConsOp] ]
  where
    pConsOp = do
      start <- getPos
      void $ symbol "::"
      span' <- spanFrom start
      pure $ \left right -> EBinOp OpCons left right span'

-- | Parse range expressions (non-associative)
pRange :: Parser Expr
pRange = makeExprParser pAddSub
  [ [InfixN pRangeOp] ]
  where
    pRangeOp = do
      start <- getPos
      void $ try (string ".." <* notFollowedBy (char '.'))
      sc
      span' <- spanFrom start
      pure $ \left right -> ERange left right span'

-- | Parse additive expressions
pAddSub :: Parser Expr
pAddSub = makeExprParser pMulDiv
  [ [InfixL pConcat, InfixL pAdd, InfixL pSub] ]
  where
    -- Concat must be tried before Add so ++ isn't consumed as +
    pConcat = mkBinOp "++" OpConcat
    -- Add must not match ++ (concat operator)
    pAdd = do
      start <- getPos
      void $ try (string "+" <* notFollowedBy (char '+'))
      sc
      span' <- spanFrom start
      pure $ \left right -> EBinOp OpAdd left right span'
    -- Subtraction operator must not be followed by '>' (arrow) or '--' (doc comment prefix)
    pSub = do
      start <- getPos
      void $ try (string "-" <* notFollowedBy (char '>') <* notFollowedBy (string "--"))
      sc
      span' <- spanFrom start
      pure $ \left right -> EBinOp OpSub left right span'

-- | Parse multiplicative expressions
pMulDiv :: Parser Expr
pMulDiv = makeExprParser pApp
  [ [InfixL pMul, InfixL pDiv, InfixL pMod] ]
  where
    pMul = mkBinOp "*" OpMul
    pDiv = mkBinOp "/" OpDiv
    pMod = mkBinOp "%" OpMod

-- | Helper to create binary operator parser
mkBinOp :: Text -> BinOp -> Parser (Expr -> Expr -> Expr)
mkBinOp sym op = do
  start <- getPos
  void $ symbol sym
  span' <- spanFrom start
  pure $ \left right -> EBinOp op left right span'

pApp :: Parser Expr
pApp = choice
  [ -- Complex expressions that shouldn't take arguments
    pLet
  , try pAssign
  , pMatch
  , pIf
  , pDo
  , pFor
  , pWith
  , pFnClosure
  , pLambda
  , -- Application of simpler expressions
    pAppSimple
  ]

-- | Parse simple function application
pAppSimple :: Parser Expr
pAppSimple = do
  start <- getPos
  func <- pPostfix
  args <- many pPostfix
  span' <- spanFrom start
  case args of
    [] -> pure func
    _  -> pure $ EApp func args span'

-- | Parse postfix expressions (field access)
pPostfix :: Parser Expr
pPostfix = do
  base <- pAtom
  suffixes <- many (pDotSuffix <|> pIndexSuffix)
  pure $ foldl applySuffix base suffixes
  where
    -- Parse .field or .method(args)
    pDotSuffix = try $ do
      s <- getPos
      symbol "."
      -- Must not be followed by another dot (range operator ..)
      notFollowedBy (char '.')
      -- Must be followed by lowercase identifier (field name), not uppercase (qualified name)
      notFollowedBy upperIdent
      field <- lowerIdent
      -- Try to parse parenthesized arguments for method call
      mArgs <- optional $ do
        symbol "("
        args <- pExpr `sepBy` symbol ","
        symbol ")"
        pure args
      span' <- spanFrom s
      pure $ DotSuffix field mArgs span'

    -- Parse [expr] indexing
    pIndexSuffix = try $ do
      s <- getPos
      symbol "["
      idx <- pExpr
      symbol "]"
      span' <- spanFrom s
      pure $ IndexSuffix idx span'

    applySuffix receiver (DotSuffix field Nothing s) = EFieldAccess receiver field s
    applySuffix receiver (DotSuffix method (Just args) s) = EMethodCall receiver method args s
    applySuffix receiver (IndexSuffix idx s) = EIndex receiver idx s

data PostfixSuffix
  = DotSuffix !Text !(Maybe [Expr]) !Span
  | IndexSuffix !Expr !Span

-- | Parse simple atoms (variables, literals, parenthesized expressions)
-- These can be targets of function application
pAtom :: Parser Expr
pAtom = choice
  [ pPerform
  , pExternalCall
  , pLazy
  , pForce
  , pNot
  , pBreak
  , pReturn
  , pLiteral
  , pListLiteral
  , try pRecordConstruction
  , pSelfVar
  , pVar
  , pParens
  ]

-- | Parse list literal: [expr, expr, ...] or []
pListLiteral :: Parser Expr
pListLiteral = do
  start <- getPos
  symbol "["
  elems <- pExpr `sepBy` symbol ","
  symbol "]"
  span' <- spanFrom start
  pure $ EList elems span'

-- | Parse record construction: TypeName { field = expr, ... }
pRecordConstruction :: Parser Expr
pRecordConstruction = do
  start <- getPos
  baseName <- upperIdent
  qualParts <- many (try (symbol "." *> lowerIdent))
  let qualifiedName = case qualParts of
        [] -> baseName
        _  -> baseName <> "." <> T.intercalate "." qualParts
  symbol "{"
  fields <- pRecordFieldAssign `sepBy` symbol ","
  symbol "}"
  span' <- spanFrom start
  pure $ ERecord qualifiedName fields span'
  where
    pRecordFieldAssign = do
      fieldName <- lowerIdent
      symbol "="
      value <- pExpr
      pure (fieldName, value)

pLet :: Parser Expr
pLet = do
  start <- getPos
  letCol <- unPos . sourceColumn <$> getSourcePos
  keyword "let"
  pat <- pPattern
  mTy <- optional (symbol ":" *> pType)
  _ <- symbol "="
  value <- pLetValue letCol  -- Parse value up to 'in' or newline
  -- Try explicit 'in', or use layout-based implicit 'in'
  body <- pLetBody letCol
  span' <- spanFrom start
  pure $ ELet pat mTy value body span'
  where
    -- Parse the body after a let binding
    -- Either explicit 'in' or layout-based (next line at same/greater indentation)
    pLetBody :: Int -> Parser Expr
    pLetBody letCol = choice
      [ -- Explicit 'in' keyword
        keyword "in" *> pExpr
      , -- Layout-based: next expression continues the let
        pLayoutBody letCol
      ]

    -- Parse layout-based let body
    pLayoutBody :: Int -> Parser Expr
    pLayoutBody letCol = do
      -- Check column of next token
      col <- unPos . sourceColumn <$> getSourcePos
      if col >= letCol
        then pExpr  -- Parse the rest as the body
        else fail "layout: body must be indented at least as much as let"

    -- Parse a let value - stop at end of line for layout-based let.
    -- Match is tried first since it consumes multi-line arm structures.
    -- Uses pMatchIndented so match arms don't consume the let body.
    -- Atom-based parsing is the fallback.
    pLetValue :: Int -> Parser Expr
    pLetValue enclosingCol = choice
      [ pMatchIndented enclosingCol
      , pLetValueSimple
      ]

    -- Parse a simple let value (atom-based with same-line args)
    pLetValueSimple :: Parser Expr
    pLetValueSimple = do
      startLine <- unPos . sourceLine <$> getSourcePos
      start <- getPos
      func <- pLetPostfix
      -- Only consume more args if they're on the same line
      args <- many (try $ pSameLineArg startLine)
      span' <- spanFrom start
      case args of
        [] -> pure func
        _  -> pure $ EApp func args span'

    -- Parse an argument that's on the same line as the let value started
    pSameLineArg :: Int -> Parser Expr
    pSameLineArg startLine = do
      curLine <- unPos . sourceLine <$> getSourcePos
      if curLine == startLine
        then notFollowedBy (keyword "in") *> pLetPostfix
        else fail "argument on different line"

    -- Parse postfix expressions (field access, indexing) for let bindings
    pLetPostfix :: Parser Expr
    pLetPostfix = do
      base <- pLetAtom
      suffixes <- many (pLetFieldAccess <|> pLetIndexAccess)
      pure $ foldl applyLetSuffix base suffixes

    -- Parse field access, but not followed by 'in' keyword
    pLetFieldAccess :: Parser PostfixSuffix
    pLetFieldAccess = try $ do
      notFollowedBy (keyword "in")
      s <- getPos
      symbol "."
      notFollowedBy upperIdent
      field <- lowerIdent
      span' <- spanFrom s
      pure $ DotSuffix field Nothing span'

    -- Parse [expr] indexing in let context
    pLetIndexAccess :: Parser PostfixSuffix
    pLetIndexAccess = try $ do
      notFollowedBy (keyword "in")
      s <- getPos
      symbol "["
      idx <- pExpr
      symbol "]"
      span' <- spanFrom s
      pure $ IndexSuffix idx span'

    applyLetSuffix :: Expr -> PostfixSuffix -> Expr
    applyLetSuffix receiver (DotSuffix field Nothing s) = EFieldAccess receiver field s
    applyLetSuffix receiver (DotSuffix method (Just args) s) = EMethodCall receiver method args s
    applyLetSuffix receiver (IndexSuffix idx s) = EIndex receiver idx s

    pLetAtom :: Parser Expr
    pLetAtom = choice
      [ pPerformSameLine
      , pExternalCallSameLine
      , pFnClosure
      , pLazy
      , pForce
      , pNot
      , pBreak
      , pReturn
      , pLiteral
      , pListLiteral
      , try pRecordConstruction
      , pSelfVar
      , pVar
      , pParens
      ]

-- | Parse mutable assignment: name = expr (followed by continuation body)
-- Uses same layout rules as let binding for the continuation body.
-- The value is parsed at application level (same line); for binary operators
-- in the value, use parentheses: name = (expr + expr)
pAssign :: Parser Expr
pAssign = do
  start <- getPos
  assignCol <- unPos . sourceColumn <$> getSourcePos
  name <- lowerIdent
  -- Must see '=' but NOT '==' (equality)
  void $ try (symbol "=" <* notFollowedBy (char '='))
  value <- pAssignValue assignCol
  body <- pAssignBody assignCol
  span' <- spanFrom start
  pure $ EAssign name value body span'
  where
    pAssignBody :: Int -> Parser Expr
    pAssignBody assignCol = do
      col <- unPos . sourceColumn <$> getSourcePos
      if col >= assignCol
        then pExpr
        else fail "layout: body must be indented at least as much as assignment"

    -- Parse the assignment value. Match is tried first since it consumes
    -- multi-line arm structures. Uses pMatchIndented so match arms don't
    -- consume the assignment body. Atom-based parsing is the fallback.
    pAssignValue :: Int -> Parser Expr
    pAssignValue enclosingCol = choice
      [ pMatchIndented enclosingCol
      , pAssignValueSimple
      ]

    -- Parse a simple assignment value (atom-based with same-line args)
    pAssignValueSimple :: Parser Expr
    pAssignValueSimple = do
      startLine <- unPos . sourceLine <$> getSourcePos
      start <- getPos
      func <- pAssignPostfix
      args <- many (try $ pSameLineArg startLine)
      span' <- spanFrom start
      case args of
        [] -> pure func
        _  -> pure $ EApp func args span'

    pSameLineArg :: Int -> Parser Expr
    pSameLineArg startLine = do
      curLine <- unPos . sourceLine <$> getSourcePos
      if curLine == startLine
        then pAssignPostfix
        else fail "argument on different line"

    pAssignPostfix :: Parser Expr
    pAssignPostfix = do
      base <- pAssignAtom
      suffixes <- many (pAssignFieldAccess <|> pAssignIndexAccess)
      pure $ foldl applyAssignSuffix base suffixes

    pAssignFieldAccess :: Parser PostfixSuffix
    pAssignFieldAccess = try $ do
      s <- getPos
      symbol "."
      notFollowedBy (char '.')
      notFollowedBy upperIdent
      field <- lowerIdent
      span' <- spanFrom s
      pure $ DotSuffix field Nothing span'

    pAssignIndexAccess :: Parser PostfixSuffix
    pAssignIndexAccess = try $ do
      s <- getPos
      symbol "["
      idx <- pExpr
      symbol "]"
      span' <- spanFrom s
      pure $ IndexSuffix idx span'

    applyAssignSuffix :: Expr -> PostfixSuffix -> Expr
    applyAssignSuffix receiver (DotSuffix field Nothing s) = EFieldAccess receiver field s
    applyAssignSuffix receiver (DotSuffix method (Just args) s) = EMethodCall receiver method args s
    applyAssignSuffix receiver (IndexSuffix idx s) = EIndex receiver idx s

    pAssignAtom :: Parser Expr
    pAssignAtom = choice
      [ pPerformSameLine
      , pExternalCallSameLine
      , pFnClosure
      , pLazy
      , pForce
      , pNot
      , pBreak
      , pReturn
      , pLiteral
      , pListLiteral
      , try pRecordConstruction
      , pSelfVar
      , pVar
      , pParens
      ]

pMatch :: Parser Expr
pMatch = do
  start <- getPos
  keyword "match"
  -- Match subject must be a simple expression (no binary ops or complex expressions)
  -- to avoid ambiguity with match arms that use ->
  subject <- pMatchSubject
  arms <- many pMatchArm  -- Zero or more arms allowed
  span' <- spanFrom start
  pure $ EMatch subject arms span'

-- | Column-aware variant of pMatch for use in let-binding and assignment values.
-- Only consumes match arms that are indented past the given baseline column,
-- so that the enclosing let body or assignment continuation is not consumed.
pMatchIndented :: Int -> Parser Expr
pMatchIndented baseCol = do
  start <- getPos
  keyword "match"
  subject <- pMatchSubject
  arms <- many (try $ pIndentedMatchArm baseCol)
  span' <- spanFrom start
  pure $ EMatch subject arms span'
  where
    pIndentedMatchArm :: Int -> Parser MatchArm
    pIndentedMatchArm minCol = do
      armCol <- unPos . sourceColumn <$> getSourcePos
      if armCol > minCol
        then pMatchArm
        else fail "match arm not indented past enclosing binding"

-- | Parse a match subject - restricted to avoid ambiguity with arms
-- Only allows a single postfix expression (no application consuming multiple tokens)
pMatchSubject :: Parser Expr
pMatchSubject = pPostfix

pMatchArm :: Parser MatchArm
pMatchArm = do
  start <- getPos
  pat <- pPattern
  guard' <- optional (symbol "|" *> pExpr)
  symbol "->"
  body <- pMatchArmBody  -- Restricted body to avoid consuming subsequent arms
  span' <- spanFrom start
  pure $ MatchArm pat guard' body span'

-- | Parse match arm body - supports parenthesized function calls with field access
-- Uses a restricted parser that doesn't consume space-separated arguments
-- (which would consume literals from subsequent match arms as arguments)
pMatchArmBody :: Parser Expr
pMatchArmBody = makeExprParser pMatchArmApp
  [ [InfixN pMatchCmp]
  , [InfixL pMatchAdd, InfixL pMatchSub]
  , [InfixL pMatchMul, InfixL pMatchDiv, InfixL pMatchMod]
  ]
  where
    pMatchCmp = choice
      [ mkMatchBinOp "<=" OpLE
      , mkMatchBinOp "<" OpLT
      , mkMatchBinOp ">=" OpGE
      , mkMatchBinOp ">" OpGT
      , mkMatchBinOp "==" OpEQ
      , mkMatchBinOp "!=" OpNE
      ]
    pMatchAdd = mkMatchBinOp "+" OpAdd
    -- Subtraction operator must not be followed by '>' (arrow) or '--' (doc comment prefix)
    pMatchSub = do
      start <- getPos
      void $ try (string "-" <* notFollowedBy (char '>') <* notFollowedBy (string "--"))
      sc
      span' <- spanFrom start
      pure $ \left right -> EBinOp OpSub left right span'
    pMatchMul = mkMatchBinOp "*" OpMul
    pMatchDiv = mkMatchBinOp "/" OpDiv
    pMatchMod = mkMatchBinOp "%" OpMod
    mkMatchBinOp sym op = do
      start <- getPos
      void $ symbol sym
      span' <- spanFrom start
      pure $ \left right -> EBinOp op left right span'

-- | Parse function application in match arm - only parenthesized arguments
pMatchArmApp :: Parser Expr
pMatchArmApp = do
  start <- getPos
  base <- pMatchArmPostfix
  args <- many pParenArg
  span' <- spanFrom start
  case args of
    [] -> pure base
    _  -> pure $ EApp base (concat args) span'
  where
    pParenArg = do
      symbol "("
      argList <- pMatchArmBody `sepBy1` symbol ","
      symbol ")"
      pure argList

-- | Parse postfix expressions in match arm - field access only
pMatchArmPostfix :: Parser Expr
pMatchArmPostfix = do
  start <- getPos
  base <- pAtom
  accesses <- many pMatchFieldAccess
  span' <- spanFrom start
  pure $ foldl (\e (field, s) -> EFieldAccess e field s) base accesses
  where
    pMatchFieldAccess = do
      fieldStart <- getPos
      symbol "."
      notFollowedBy upperIdent
      field <- lowerIdent
      span' <- spanFrom fieldStart
      pure (field, span')

pIf :: Parser Expr
pIf = do
  start <- getPos
  keyword "if"
  cond <- pExpr
  keyword "then"
  then' <- pExpr
  keyword "else"
  else' <- pExpr
  span' <- spanFrom start
  pure $ EIf cond then' else' span'

pDo :: Parser Expr
pDo = do
  start <- getPos
  keyword "do"
  stmts <- many pDoStmt
  result <- pExpr
  span' <- spanFrom start
  pure $ EDo stmts result span'

pDoStmt :: Parser DoStatement
pDoStmt = choice
  [ try $ do
      start <- getPos
      pat <- pPattern
      symbol "<-"
      effect <- upperIdent
      symbol "."
      op <- lowerIdent
      args <- many pAtom
      span' <- spanFrom start
      pure $ DoBind pat effect op args span'
  , do start <- getPos
       keyword "let"
       pat <- pPattern
       mTy <- optional (symbol ":" *> pType)
       symbol "="
       value <- pExpr
       span' <- spanFrom start
       pure $ DoLet pat mTy value span'
  ]

pFor :: Parser Expr
pFor = do
  start <- getPos
  keyword "for"
  pat <- pPattern
  keyword "in"
  collection <- pRange
  symbol ":"
  body <- pExpr
  span' <- spanFrom start
  pure $ EFor pat collection body span'

pWith :: Parser Expr
pWith = do
  start <- getPos
  keyword "with"
  handler <- pExpr
  body <- pExpr
  span' <- spanFrom start
  pure $ EWith handler body span'

pLambda :: Parser Expr
pLambda = do
  start <- getPos
  void $ symbol "\\" <|> symbol "λ"
  params <- many pParam
  symbol "."
  body <- pExpr
  span' <- spanFrom start
  pure $ ELam LamBackslash params body span'

-- | Parse fn-style closure: @fn(params) -> expr@
pFnClosure :: Parser Expr
pFnClosure = try $ do
  start <- getPos
  keyword "fn"
  params <- between (symbol "(") (symbol ")") (pClosureParam `sepBy` symbol ",")
  symbol "->"
  body <- pExpr
  span' <- spanFrom start
  pure $ ELam LamFnArrow params body span'

-- | Parse a closure parameter: typed (@x: Int@) or untyped (@x@)
pClosureParam :: Parser Param
pClosureParam = try pParamOrSelf <|> pUntypedParam

-- | Parse an untyped parameter (name only, type to be inferred)
pUntypedParam :: Parser Param
pUntypedParam = do
  start <- getPos
  name <- lowerIdent
  span' <- spanFrom start
  pure $ Param name (TyHole span') span'

pPerform :: Parser Expr
pPerform = do
  start <- getPos
  keyword "perform"
  effect <- upperIdent
  symbol "."
  op <- lowerIdent
  args <- many pAtom
  span' <- spanFrom start
  pure $ EPerform effect op args span'

-- | Same-line variant of pPerform for let-binding and assignment values.
-- Only consumes arguments that appear on the same line as the perform keyword,
-- preventing greedy consumption of the let body or assignment continuation.
pPerformSameLine :: Parser Expr
pPerformSameLine = do
  startLine <- unPos . sourceLine <$> getSourcePos
  start <- getPos
  keyword "perform"
  effect <- upperIdent
  symbol "."
  op <- lowerIdent
  args <- many (try $ pSameLineAtom startLine)
  span' <- spanFrom start
  pure $ EPerform effect op args span'
  where
    pSameLineAtom :: Int -> Parser Expr
    pSameLineAtom startLine = do
      curLine <- unPos . sourceLine <$> getSourcePos
      if curLine == startLine
        then pAtom
        else fail "argument on different line than perform"

-- | Parse an external function call expression
-- Example: external("console", "log") "Hello"
--          external("postgres", "query") sql_string
pExternalCall :: Parser Expr
pExternalCall = do
  start <- getPos
  extRef <- pExternalRef
  args <- many pAtom
  span' <- spanFrom start
  pure $ EExternal extRef args span'

-- | Same-line variant of pExternalCall for let-binding and assignment values.
pExternalCallSameLine :: Parser Expr
pExternalCallSameLine = do
  startLine <- unPos . sourceLine <$> getSourcePos
  start <- getPos
  extRef <- pExternalRef
  args <- many (try $ pSameLineAtom startLine)
  span' <- spanFrom start
  pure $ EExternal extRef args span'
  where
    pSameLineAtom :: Int -> Parser Expr
    pSameLineAtom startLine = do
      curLine <- unPos . sourceLine <$> getSourcePos
      if curLine == startLine
        then pAtom
        else fail "argument on different line than external call"

pLazy :: Parser Expr
pLazy = do
  start <- getPos
  keyword "lazy"
  inner <- pAtom
  span' <- spanFrom start
  pure $ ELazy inner span'

pForce :: Parser Expr
pForce = do
  start <- getPos
  keyword "force"
  inner <- pAtom
  span' <- spanFrom start
  pure $ EForce inner span'

pBreak :: Parser Expr
pBreak = do
  start <- getPos
  keyword "break"
  span' <- spanFrom start
  pure $ EBreak span'

pReturn :: Parser Expr
pReturn = do
  start <- getPos
  keyword "return"
  val <- pAtom
  span' <- spanFrom start
  pure $ EReturn val span'

pNot :: Parser Expr
pNot = do
  start <- getPos
  keyword "not"
  inner <- pAtom
  span' <- spanFrom start
  pure $ ENot inner span'

pLiteral :: Parser Expr
pLiteral = choice
  [ pUnit
  , try pFloat  -- Try float first (with backtracking) so "42" doesn't consume "4" expecting decimal
  , pInt
  , try pTripleQuotedString  -- Must come before pString since """ starts with "
  , pString
  , pChar
  ]

pUnit :: Parser Expr
pUnit = do
  start <- getPos
  symbol "()"
  span' <- spanFrom start
  pure $ EUnit span'

pInt :: Parser Expr
pInt = do
  start <- getPos
  n <- lexeme L.decimal
  span' <- spanFrom start
  pure $ EIntLit n span'

pFloat :: Parser Expr
pFloat = do
  start <- getPos
  n <- lexeme L.float
  span' <- spanFrom start
  pure $ EFloatLit n span'

-- | Parse a triple-quoted string literal: """..."""
-- Content is taken literally (no escape processing).
-- Leading/trailing blank lines and common indentation are stripped.
pTripleQuotedString :: Parser Expr
pTripleQuotedString = do
  start <- getPos
  rawContent <- lexeme $ do
    _ <- string "\"\"\""
    content <- manyTill anySingle (string "\"\"\"")
    pure content
  span' <- spanFrom start
  let strippedContent = stripTripleQuoteIndent (T.pack rawContent)
  pure $ EStringLit StringTriple strippedContent span'

-- | Strip common leading whitespace from triple-quoted string content.
-- Follows Python's textwrap.dedent convention:
-- 1. Remove leading blank line (if content starts with newline)
-- 2. Remove trailing blank lines
-- 3. Find minimum indentation among non-empty lines
-- 4. Strip that many spaces from each line
stripTripleQuoteIndent :: Text -> Text
stripTripleQuoteIndent rawContent =
  let contentLines = T.lines rawContent
      -- Drop leading empty line (common after opening """)
      trimmedLeading = case contentLines of
        (firstLine : rest) | T.null (T.strip firstLine) -> rest
        other -> other
      -- Drop trailing empty lines
      trimmedTrailing = reverse $ dropWhile (T.null . T.strip) (reverse trimmedLeading)
      -- Compute minimum indentation of non-empty lines
      nonEmptyLines = filter (not . T.null . T.strip) trimmedTrailing
      minIndent = case nonEmptyLines of
        [] -> 0
        ls -> minimum (map (T.length . T.takeWhile (== ' ')) ls)
      -- Strip the common indentation
      strippedLines = map (T.drop minIndent) trimmedTrailing
  in T.intercalate "\n" strippedLines

pString :: Parser Expr
pString = do
  start <- getPos
  s <- lexeme $ char '"' *> manyTill L.charLiteral (char '"')
  span' <- spanFrom start
  pure $ EStringLit StringSingle (T.pack s) span'

pChar :: Parser Expr
pChar = do
  start <- getPos
  c <- lexeme $ char '\'' *> L.charLiteral <* char '\''
  span' <- spanFrom start
  pure $ ECharLit c span'

-- | Parse @self@ as a variable expression (for use in method bodies)
pSelfVar :: Parser Expr
pSelfVar = do
  start <- getPos
  keyword "self"
  span' <- spanFrom start
  pure $ EVar "self" span'

pVar :: Parser Expr
pVar = do
  start <- getPos
  name <- identifier
  span' <- spanFrom start
  -- Distinguish between variable and constructor by case
  pure $ if T.null name
         then EVar name span'
         else case T.head name of
           c | c >= 'A' && c <= 'Z' -> ECon name span'
             | otherwise -> EVar name span'

pParens :: Parser Expr
pParens = do
  start <- getPos
  symbol "("
  firstExpr <- pExpr
  moreExprs <- many (symbol "," *> pExpr)
  symbol ")"
  case moreExprs of
    [] -> pure firstExpr                          -- Parenthesized expression
    _  -> do
      span' <- spanFrom start
      pure $ ETuple (firstExpr : moreExprs) span' -- Tuple expression

-- * Pattern parsing

pPattern :: Parser Pattern
pPattern = choice
  [ pPatternCon
  , pPatternAtom
  ]

pPatternCon :: Parser Pattern
pPatternCon = do
  start <- getPos
  con <- upperIdent
  args <- many pPatternAtom
  span' <- spanFrom start
  pure $ PatCon con args span'

pPatternAtom :: Parser Pattern
pPatternAtom = choice
  [ -- Wildcard pattern
    do start <- getPos
       symbol "_"
       span' <- spanFrom start
       pure $ PatWildcard span'
  , -- Tuple or parenthesized pattern
    do start <- getPos
       symbol "("
       pats <- pPattern `sepBy` symbol ","
       symbol ")"
       span' <- spanFrom start
       case pats of
         [p] -> pure p
         _   -> pure $ PatTuple pats span'
  , -- Literal patterns (integers, strings, etc.)
    try $ do
       start <- getPos
       lit <- pLiteral
       span' <- spanFrom start
       pure $ PatLit lit span'
  , -- Variable pattern
    do start <- getPos
       name <- lowerIdent
       span' <- spanFrom start
       pure $ PatVar name span'
  ]
