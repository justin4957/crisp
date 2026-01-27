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
  provs <- many pProvide
  defs <- many pDefinition
  span' <- spanFrom start
  pure $ Module name auth reqs provs defs span'

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
    ]

pProvide :: Parser Provide
pProvide = do
  start <- getPos
  keyword "provides"
  choice
    [ do keyword "type"
         name <- upperIdent
         span' <- spanFrom start
         pure $ ProvideType name span'
    , do keyword "fn"
         name <- lowerIdent
         symbol ":"
         ty <- pType
         span' <- spanFrom start
         pure $ ProvideFn name ty span'
    ]

-- * Definition parsing

pDefinition :: Parser Definition
pDefinition = choice
  [ DefType <$> pTypeDef
  , DefEffect <$> pEffectDef
  , DefHandler <$> pHandlerDef
  , DefTrait <$> pTraitDef
  , DefImpl <$> pImplDef
  , DefFn <$> pFunctionDef
  ]

pTypeDef :: Parser TypeDef
pTypeDef = do
  start <- getPos
  keyword "type"
  mods <- pTypeModifiers
  name <- upperIdent
  params <- many pTypeParam
  mKind <- optional (symbol ":" *> pKind)
  deriv <- optional pDerivingClause
  cons <- optional (symbol ":" *> pConstructors)
  span' <- spanFrom start
  pure $ TypeDef name params mKind (maybe [] id cons) mods deriv span'

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

pConstructors :: Parser [Constructor]
pConstructors = many pConstructor

pConstructor :: Parser Constructor
pConstructor = do
  start <- getPos
  name <- upperIdent
  choice
    [ do symbol ":"
         ty <- pType
         span' <- spanFrom start
         pure $ GadtConstructor name ty span'
    , do args <- many pTypeAtom
         span' <- spanFrom start
         pure $ SimpleConstructor name args span'
    ]

pTypeParam :: Parser TypeParam
pTypeParam = choice
  [ try $ do
      start <- getPos
      symbol "("
      name <- lowerIdent
      symbol ":"
      ty <- pType
      symbol ")"
      span' <- spanFrom start
      pure $ DepParam name ty span'
  , do start <- getPos
       name <- upperIdent
       mKind <- optional (symbol ":" *> pKind)
       span' <- spanFrom start
       pure $ TypeVar name mKind span'
  ]

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

pTypeAtom :: Parser Type
pTypeAtom = choice
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

-- * Expression parsing

pExpr :: Parser Expr
pExpr = makeExprParser pApp
  [ [InfixL pPipe] ]
  where
    pPipe = do
      start <- getPos
      symbol "|>"
      span' <- spanFrom start
      pure $ \left right -> EPipe left right span'

pApp :: Parser Expr
pApp = do
  start <- getPos
  func <- pAtom
  args <- many pAtom
  span' <- spanFrom start
  case args of
    [] -> pure func
    _  -> pure $ EApp func args span'

pAtom :: Parser Expr
pAtom = choice
  [ pLet
  , pMatch
  , pIf
  , pDo
  , pWith
  , pLambda
  , pPerform
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
  subject <- pExpr
  arms <- many pMatchArm
  span' <- spanFrom start
  pure $ EMatch subject arms span'

pMatchArm :: Parser MatchArm
pMatchArm = do
  start <- getPos
  pat <- pPattern
  guard' <- optional (symbol "|" *> pExpr)
  symbol "->"
  body <- pExpr
  span' <- spanFrom start
  pure $ MatchArm pat guard' body span'

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
  [ do start <- getPos
       symbol "_"
       span' <- spanFrom start
       pure $ PatWildcard span'
  , do start <- getPos
       symbol "("
       pats <- pPattern `sepBy` symbol ","
       symbol ")"
       span' <- spanFrom start
       case pats of
         [p] -> pure p
         _   -> pure $ PatTuple pats span'
  , do start <- getPos
       name <- lowerIdent
       span' <- spanFrom start
       pure $ PatVar name span'
  ]
