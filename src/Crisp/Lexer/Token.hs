{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Lexer.Token
-- Description : Token types for the Crisp lexer
--
-- Defines all token types produced by the lexer, including keywords,
-- operators, literals, and layout tokens for significant whitespace.

module Crisp.Lexer.Token
  ( -- * Token types
    Token(..)
  , TokenKind(..)
    -- * Token predicates
  , isKeyword
  , isLayoutToken
  , isLiteral
    -- * Keyword lookup
  , keywordMap
  , lookupKeyword
  ) where

import Crisp.Syntax.Span (Span)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A token with its source location
data Token = Token
  { tokenKind :: !TokenKind
  , tokenSpan :: !Span
  } deriving stock (Eq, Show, Generic)

-- | All possible token types in Crisp
data TokenKind
  -- Keywords
  = KwFn
  | KwLet
  | KwType
  | KwEffect
  | KwHandler
  | KwMatch
  | KwIf
  | KwThen
  | KwElse
  | KwWith
  | KwImport
  | KwModule
  | KwRequires
  | KwProvides
  | KwAuthority
  | KwLinear
  | KwLazy
  | KwPerform
  | KwResume
  | KwReturn
  | KwWhere
  | KwForall
  | KwAs
  | KwQualified
  | KwDo
  | KwProp
  | KwTotal
  | KwMut
  | KwRef

  -- Operators and Punctuation
  | Arrow          -- ->
  | FatArrow       -- =>
  | Pipe           -- |
  | Colon          -- :
  | DoubleColon    -- ::
  | Equals         -- =
  | At             -- @
  | Ampersand      -- &
  | Dot            -- .
  | Comma          -- ,
  | PipeForward    -- |>
  | PipeBackward   -- <|
  | ComposeRight   -- >>
  | ComposeLeft    -- <<
  | Dollar         -- $
  | Bang           -- !
  | Backslash      -- \
  | Lambda         -- λ

  -- Delimiters
  | LParen         -- (
  | RParen         -- )
  | LBracket       -- [
  | RBracket       -- ]
  | LBrace         -- {
  | RBrace         -- }

  -- Layout tokens (significant whitespace)
  | Indent         -- Increase in indentation
  | Dedent         -- Decrease in indentation
  | Newline        -- Newline at same indentation level

  -- Identifiers
  | LowerIdent !Text  -- lowercase identifier
  | UpperIdent !Text  -- Uppercase identifier

  -- Literals
  | IntLit !Integer
  | FloatLit !Double
  | StringLit !Text
  | CharLit !Char
  | Unit             -- ()

  -- Special
  | Eof
  | Error !Text
  deriving stock (Eq, Show, Generic)

-- | Check if a token is a keyword
isKeyword :: TokenKind -> Bool
isKeyword = \case
  KwFn -> True
  KwLet -> True
  KwType -> True
  KwEffect -> True
  KwHandler -> True
  KwMatch -> True
  KwIf -> True
  KwThen -> True
  KwElse -> True
  KwWith -> True
  KwImport -> True
  KwModule -> True
  KwRequires -> True
  KwProvides -> True
  KwAuthority -> True
  KwLinear -> True
  KwLazy -> True
  KwPerform -> True
  KwResume -> True
  KwReturn -> True
  KwWhere -> True
  KwForall -> True
  KwAs -> True
  KwQualified -> True
  KwDo -> True
  KwProp -> True
  KwTotal -> True
  KwMut -> True
  KwRef -> True
  _ -> False

-- | Check if a token is a layout token
isLayoutToken :: TokenKind -> Bool
isLayoutToken = \case
  Indent  -> True
  Dedent  -> True
  Newline -> True
  _       -> False

-- | Check if a token is a literal
isLiteral :: TokenKind -> Bool
isLiteral = \case
  IntLit _    -> True
  FloatLit _  -> True
  StringLit _ -> True
  CharLit _   -> True
  Unit        -> True
  _           -> False

-- | Map from keyword text to token kind
keywordMap :: Map Text TokenKind
keywordMap = Map.fromList
  [ ("fn", KwFn)
  , ("let", KwLet)
  , ("type", KwType)
  , ("effect", KwEffect)
  , ("handler", KwHandler)
  , ("match", KwMatch)
  , ("if", KwIf)
  , ("then", KwThen)
  , ("else", KwElse)
  , ("with", KwWith)
  , ("import", KwImport)
  , ("module", KwModule)
  , ("requires", KwRequires)
  , ("provides", KwProvides)
  , ("authority", KwAuthority)
  , ("linear", KwLinear)
  , ("lazy", KwLazy)
  , ("perform", KwPerform)
  , ("resume", KwResume)
  , ("return", KwReturn)
  , ("where", KwWhere)
  , ("forall", KwForall)
  , ("as", KwAs)
  , ("qualified", KwQualified)
  , ("do", KwDo)
  , ("prop", KwProp)
  , ("total", KwTotal)
  , ("mut", KwMut)
  , ("ref", KwRef)
  ]

-- | Look up a keyword from its text representation
lookupKeyword :: Text -> Maybe TokenKind
lookupKeyword = (`Map.lookup` keywordMap)
