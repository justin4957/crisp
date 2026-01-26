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
import Text.Megaparsec
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
  cons <- optional (symbol ":" *> pConstructors)
  span' <- spanFrom start
  pure $ TypeDef name params mKind (concat $ maybe [] id cons) mods span'

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
pKind = makeExprParser pKindAtom
  [ [InfixR (KindArrow <$> getPos <*> (symbol "->" *> spanFrom =<< getPos) <*> pure undefined)] ]
  where
    -- Placeholder - actual implementation would be more complete
    pKindAtom = do
      start <- getPos
      choice
        [ KindType Nothing <$> (keyword "Type" *> spanFrom start)
        , KindProp <$> (keyword "Prop" *> spanFrom start)
        , KindLinear <$> (keyword "Linear" *> spanFrom start)
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
pType = makeExprParser pTypeApp
  [ [InfixR pArrow] ]
  where
    pArrow = do
      start <- getPos
      symbol "->"
      effs <- option [] (symbol "!" *> pEffectList)
      span' <- spanFrom start
      pure $ \from to -> TyFn from to effs span'

pTypeApp :: Parser Type
pTypeApp = do
  start <- getPos
  con <- pTypeAtom
  args <- many pTypeAtom
  span' <- spanFrom start
  case args of
    [] -> pure con
    _  -> pure $ TyApp con args span'

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
  symbol "="
  value <- pExpr
  body <- pExpr  -- In real impl, would handle block structure
  span' <- spanFrom start
  pure $ ELet pat mTy value body span'

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
  , pFloat
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
