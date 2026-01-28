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

-- | Space consumer (skips whitespace and comments)
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "{-" "-}")

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
  keyword "module"
  name <- pModulePath
  auth <- optional (keyword "authority" *> upperIdent)
  reqs <- many pRequire
  provLists <- many pProvides
  defs <- many pDefinition
  span' <- spanFrom start
  pure $ Module name auth reqs (concat provLists) defs span'

pModulePath :: Parser ModulePath
pModulePath = do
  start <- getPos
  segs <- upperIdent `sepBy1` symbol "."
  span' <- spanFrom start
  pure $ ModulePath segs span'

pRequire :: Parser Require
pRequire = do
  start <- getPos
  keyword "requires"
  choice
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
  keyword "provides"
  choice
    [ -- Block format: multiple indented items after bare 'provides'
      try $ some pProvideItemBlock
    , -- Inline format: single item on same line
      do start <- getPos
         item <- pProvideItem start
         pure [item]
    ]

-- | Parse a provide item in block format (type/fn at start of line)
pProvideItemBlock :: Parser Provide
pProvideItemBlock = do
  start <- getPos
  pProvideItem start

-- | Parse a single provide item (after the "provides" keyword)
pProvideItem :: Position -> Parser Provide
pProvideItem start = choice
  [ do keyword "type"
       name <- upperIdent
       span' <- spanFrom start
       pure $ ProvideType name span'
  , do keyword "fn"
       name <- lowerIdent
       -- Type annotation is optional
       mTy <- optional (symbol ":" *> pType)
       span' <- spanFrom start
       pure $ ProvideFn name mTy span'
  ]

-- * Definition parsing

pDefinition :: Parser Definition
pDefinition = choice
  [ pTypeOrAlias  -- Handle both type definitions and type aliases
  , DefEffect <$> pEffectDef
  , DefHandler <$> pHandlerDef
  , DefTrait <$> pTraitDef
  , DefImpl <$> pImplDef
  , DefExternal <$> pExternalFnDef
  , DefFn <$> pFunctionDef
  ]
  where
    -- Parse either a type alias (type Name = ...) or type definition (type Name: ...)
    -- We look ahead after parsing name and params to decide which one
    pTypeOrAlias = do
      start <- getPos
      keyword "type"
      mods <- pTypeModifiers
      name <- upperIdent
      params <- many (try pTypeParamNotEq)
      -- Now decide based on what comes next
      choice
        [ do -- Type alias: has = after name/params
             symbol "="
             -- Use pTypeAppNoRefinement so { field: Pattern } isn't parsed as refinement
             baseType <- pTypeAppNoRefinement
             constraints <- option [] pFieldConstraintBlock
             span' <- spanFrom start
             pure $ DefTypeAlias $ TypeAliasDef name params baseType constraints span'
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
             pure $ DefType $ TypeDef name params constraints mKind fixedCons mods deriv span'
        ]

    -- Type parameter that doesn't consume = sign
    pTypeParamNotEq = do
      notFollowedBy (symbol "=")
      pTypeParam

    pTypeDefKind = symbol ":" *> pKind

pTypeDef :: Parser TypeDef
pTypeDef = do
  start <- getPos
  keyword "type"
  mods <- pTypeModifiers
  name <- upperIdent
  params <- many pTypeParam
  constraints <- option [] pTypeDefConstraints
  -- Kind annotation must be followed by a valid kind keyword, not a constructor
  mKind <- optional (try pTypeDefKind)
  deriv <- optional pDerivingClause
  cons <- optional (symbol ":" *> pConstructors)
  span' <- spanFrom start
  pure $ TypeDef name params constraints mKind (maybe [] id cons) mods deriv span'
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

-- | Parse a single field constraint: field: Pattern
pFieldConstraint :: Parser FieldConstraint
pFieldConstraint = do
  start <- getPos
  fieldName <- lowerIdent
  symbol ":"
  pat <- pPattern
  span' <- spanFrom start
  pure $ FieldConstraint fieldName pat span'

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
    , -- Record constructor: { field: Type, ... }
      try $ do
        symbol "{"
        fields <- pField `sepBy1` symbol ","
        symbol "}"
        span' <- spanFrom start
        pure $ RecordConstructor name fields span'
    , -- Simple positional constructor: Cons Type1 Type2
      do args <- many pTypeAtom
         span' <- spanFrom start
         pure $ SimpleConstructor name args span'
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

pEffectDef :: Parser EffectDef
pEffectDef = do
  start <- getPos
  keyword "effect"
  name <- upperIdent
  symbol ":"
  ops <- many pOperation
  span' <- spanFrom start
  pure $ EffectDef name ops span'

pOperation :: Parser Operation
pOperation = do
  start <- getPos
  name <- lowerIdent
  symbol ":"
  ty <- pType
  span' <- spanFrom start
  pure $ Operation name ty span'

pHandlerDef :: Parser HandlerDef
pHandlerDef = do
  start <- getPos
  keyword "handler"
  name <- upperIdent
  params <- many pHandlerParam
  keyword "for"
  effect <- upperIdent
  intros <- option [] (symbol "!" *> pEffectList)
  symbol ":"
  clauses <- many pHandlerClause
  span' <- spanFrom start
  pure $ HandlerDef name params effect intros clauses span'

pHandlerParam :: Parser HandlerParam
pHandlerParam = choice
  [ try $ do
      start <- getPos
      symbol "("
      name <- lowerIdent
      symbol ":"
      ty <- pType
      symbol ")"
      span' <- spanFrom start
      pure $ HandlerValueParam name ty span'
  , do start <- getPos
       symbol "("
       name <- upperIdent
       symbol ":"
       kind <- pKind
       symbol ")"
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
pTraitDef :: Parser TraitDef
pTraitDef = do
  start <- getPos
  keyword "trait"
  name <- upperIdent
  (param, paramKind) <- pTraitParam
  supers <- option [] pSupertraits
  symbol ":"
  methods <- many pTraitMethod
  span' <- spanFrom start
  pure $ TraitDef name param paramKind supers methods span'
  where
    -- Parse trait parameter with optional kind annotation in parens
    pTraitParam = choice
      [ try $ do
          symbol "("
          p <- upperIdent
          symbol ":"
          k <- pKind
          symbol ")"
          pure (p, Just k)
      , do
          p <- upperIdent
          pure (p, Nothing)
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

-- | Parse a trait method signature
pTraitMethod :: Parser TraitMethod
pTraitMethod = try $ do
  start <- getPos
  name <- lowerIdent
  symbol ":"
  ty <- pType
  mDefault <- optional (symbol "=" *> pExpr)
  span' <- spanFrom start
  pure $ TraitMethod name ty mDefault span'

-- | Parse an implementation definition
-- Example: impl Ord for Int:
--            fn compare(a: Int, b: Int) -> Ordering: int_compare(a, b)
pImplDef :: Parser ImplDef
pImplDef = do
  start <- getPos
  keyword "impl"
  traitName <- upperIdent
  keyword "for"
  ty <- pTypeApp
  symbol ":"
  methods <- many pImplMethod
  span' <- spanFrom start
  pure $ ImplDef traitName ty methods span'

-- | Parse a method implementation within an impl block
pImplMethod :: Parser FunctionDef
pImplMethod = pFunctionDef

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
pExternalFnDef :: Parser ExternalFnDef
pExternalFnDef = do
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
  pure $ ExternalFnDef name params retTy extRef span'

pFunctionDef :: Parser FunctionDef
pFunctionDef = do
  start <- getPos
  keyword "fn"
  name <- lowerIdent
  tyParams <- option [] (between (symbol "[") (symbol "]") (pTypeParam `sepBy` symbol ","))
  params <- option [] (between (symbol "(") (symbol ")") (pParam `sepBy` symbol ","))
  retTy <- optional (symbol "->" *> pType)
  effs <- option [] (symbol "!" *> pEffectList)
  symbol ":"
  body <- pExpr
  span' <- spanFrom start
  pure $ FunctionDef name tyParams params retTy effs body span'

pParam :: Parser Param
pParam = do
  start <- getPos
  name <- lowerIdent
  symbol ":"
  ty <- pType
  span' <- spanFrom start
  pure $ Param name ty span'

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
           ty <- pType
           symbol ")"
           span' <- spanFrom start'
           pure $ TyParen ty span'
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
           ty <- pType
           symbol ")"
           span' <- spanFrom start'
           pure $ TyParen ty span'
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
  [ do start <- getPos
       symbol "("
       ty <- pType
       symbol ")"
       span' <- spanFrom start
       pure $ TyParen ty span'
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
       name <- identifier
       span' <- spanFrom start
       pure $ TyName name span'
  ]

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
pRefinementExprAtom = choice
  [ pRefinementInt
  , pRefinementSelf
  , pRefinementVar
  , pRefinementParenExpr
  ]

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
pCompare = makeExprParser pAddSub
  [ [InfixN pLe, InfixN pLt, InfixN pGe, InfixN pGt, InfixN pEq, InfixN pNe] ]
  where
    pLe = mkBinOp "<=" OpLE
    pLt = mkBinOp "<" OpLT
    pGe = mkBinOp ">=" OpGE
    pGt = mkBinOp ">" OpGT
    pEq = mkBinOp "==" OpEQ
    pNe = mkBinOp "/=" OpNE

-- | Parse additive expressions
pAddSub :: Parser Expr
pAddSub = makeExprParser pMulDiv
  [ [InfixL pAdd, InfixL pSub] ]
  where
    pAdd = mkBinOp "+" OpAdd
    -- Subtraction operator must not be followed by '>' (to avoid matching '->')
    pSub = do
      start <- getPos
      void $ try (symbol "-" <* notFollowedBy (char '>'))
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
  , pMatch
  , pIf
  , pDo
  , pWith
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
  start <- getPos
  base <- pAtom
  accesses <- many pFieldAccess
  span' <- spanFrom start
  pure $ foldl (\e (field, s) -> EFieldAccess e field s) base accesses
  where
    pFieldAccess = do
      s <- getPos
      symbol "."
      -- Must be followed by lowercase identifier (field name), not uppercase (qualified name)
      notFollowedBy upperIdent
      field <- lowerIdent
      span' <- spanFrom s
      pure (field, span')

-- | Parse simple atoms (variables, literals, parenthesized expressions)
-- These can be targets of function application
pAtom :: Parser Expr
pAtom = choice
  [ pPerform
  , pExternalCall
  , pLazy
  , pForce
  , pLiteral
  , pVar
  , pParens
  ]

pLet :: Parser Expr
pLet = do
  start <- getPos
  keyword "let"
  pat <- pPattern
  mTy <- optional (symbol ":" *> pType)
  _ <- symbol "="
  value <- pLetValue  -- Parse value up to 'in'
  keyword "in"
  body <- pExpr
  span' <- spanFrom start
  pure $ ELet pat mTy value body span'
  where
    -- Parse a let value - atoms that don't include 'in' keyword
    pLetValue :: Parser Expr
    pLetValue = do
      start <- getPos
      func <- pLetAtom
      args <- many (try $ notFollowedBy (keyword "in") *> pLetAtom)
      span' <- spanFrom start
      case args of
        [] -> pure func
        _  -> pure $ EApp func args span'

    pLetAtom :: Parser Expr
    pLetAtom = choice
      [ pLazy
      , pForce
      , pLiteral
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

-- | Parse match arm body - a simple expression that doesn't consume too much
-- Uses a single postfix expression, not application (which would consume literals
-- from subsequent match arms as arguments)
pMatchArmBody :: Parser Expr
pMatchArmBody = pPostfix

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
  pure $ ELam params body span'

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

pLiteral :: Parser Expr
pLiteral = choice
  [ pUnit
  , try pFloat  -- Try float first (with backtracking) so "42" doesn't consume "4" expecting decimal
  , pInt
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

pString :: Parser Expr
pString = do
  start <- getPos
  s <- lexeme $ char '"' *> manyTill L.charLiteral (char '"')
  span' <- spanFrom start
  pure $ EStringLit (T.pack s) span'

pChar :: Parser Expr
pChar = do
  start <- getPos
  c <- lexeme $ char '\'' *> L.charLiteral <* char '\''
  span' <- spanFrom start
  pure $ ECharLit c span'

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
  symbol "("
  e <- pExpr
  symbol ")"
  pure e

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
