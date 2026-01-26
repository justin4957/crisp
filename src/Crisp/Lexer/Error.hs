{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Lexer.Error
-- Description : Lexer error types and formatting
--
-- Defines structured error types for the lexer with clear messages
-- and source location information for helpful diagnostics.

module Crisp.Lexer.Error
  ( -- * Error Types
    LexError(..)
  , LexErrorKind(..)
  , LexResult
    -- * Error Construction
  , mkLexError
  , unterminatedString
  , unterminatedBlockComment
  , unterminatedCharLiteral
  , invalidEscapeSequence
  , invalidCharLiteral
  , invalidNumericLiteral
  , unexpectedCharacter
  , tabCharacter
    -- * Error Formatting
  , formatLexError
  , formatLexErrors
  , errorCode
    -- * Error Predicates
  , isLexError
  , getLexErrors
  ) where

import Crisp.Syntax.Span

import Data.Text (Text)
import qualified Data.Text as T

-- | Result type for lexing operations that may produce errors
type LexResult a = Either [LexError] a

-- | A lexer error with location and diagnostic information
data LexError = LexError
  { lexErrorKind    :: !LexErrorKind
  , lexErrorSpan    :: !Span
  , lexErrorContext :: !(Maybe Text)  -- ^ Additional context (e.g., the problematic input)
  } deriving stock (Eq, Show)

-- | Categories of lexer errors
data LexErrorKind
  = UnterminatedString
  | UnterminatedBlockComment
  | UnterminatedCharLiteral
  | InvalidEscapeSequence !Char
  | InvalidCharLiteral !Text
  | InvalidNumericLiteral !Text
  | UnexpectedCharacter !Char
  | TabCharacter
  deriving stock (Eq, Show)

-- | Create a lex error with optional context
mkLexError :: LexErrorKind -> Span -> Maybe Text -> LexError
mkLexError = LexError

-- | Create an unterminated string error
unterminatedString :: Span -> Maybe Text -> LexError
unterminatedString span' ctx = LexError UnterminatedString span' ctx

-- | Create an unterminated block comment error
unterminatedBlockComment :: Span -> LexError
unterminatedBlockComment span' = LexError UnterminatedBlockComment span' Nothing

-- | Create an unterminated character literal error
unterminatedCharLiteral :: Span -> LexError
unterminatedCharLiteral span' = LexError UnterminatedCharLiteral span' Nothing

-- | Create an invalid escape sequence error
invalidEscapeSequence :: Char -> Span -> LexError
invalidEscapeSequence c span' = LexError (InvalidEscapeSequence c) span' Nothing

-- | Create an invalid character literal error
invalidCharLiteral :: Text -> Span -> LexError
invalidCharLiteral txt span' = LexError (InvalidCharLiteral txt) span' (Just txt)

-- | Create an invalid numeric literal error
invalidNumericLiteral :: Text -> Span -> LexError
invalidNumericLiteral reason span' = LexError (InvalidNumericLiteral reason) span' Nothing

-- | Create an unexpected character error
unexpectedCharacter :: Char -> Span -> LexError
unexpectedCharacter c span' = LexError (UnexpectedCharacter c) span' Nothing

-- | Create a tab character error
tabCharacter :: Span -> LexError
tabCharacter span' = LexError TabCharacter span' Nothing

-- | Get the error code for an error kind
errorCode :: LexErrorKind -> Text
errorCode = \case
  UnterminatedString       -> "L001"
  UnterminatedBlockComment -> "L002"
  UnterminatedCharLiteral  -> "L003"
  InvalidEscapeSequence _  -> "L004"
  InvalidCharLiteral _     -> "L005"
  InvalidNumericLiteral _  -> "L006"
  UnexpectedCharacter _    -> "L007"
  TabCharacter             -> "L008"

-- | Get the error message for an error kind
errorMessage :: LexErrorKind -> Text
errorMessage = \case
  UnterminatedString       -> "unterminated string literal"
  UnterminatedBlockComment -> "unterminated block comment"
  UnterminatedCharLiteral  -> "unterminated character literal"
  InvalidEscapeSequence c  -> "invalid escape sequence '\\" <> T.singleton c <> "'"
  InvalidCharLiteral txt   -> "invalid character literal: " <> txt
  InvalidNumericLiteral r  -> "invalid numeric literal: " <> r
  UnexpectedCharacter c    -> "unexpected character '" <> T.singleton c <> "'"
  TabCharacter             -> "tab characters are not allowed in indentation"

-- | Get help text for an error kind
errorHelp :: LexErrorKind -> Maybe Text
errorHelp = \case
  UnterminatedString       -> Just "add a closing double quote (\")"
  UnterminatedBlockComment -> Just "add a closing block comment marker (-})"
  UnterminatedCharLiteral  -> Just "add a closing single quote (')"
  InvalidEscapeSequence _  -> Just "valid escape sequences are: \\n, \\t, \\r, \\\\, \\\", \\', \\0"
  InvalidCharLiteral _     -> Just "character literals must contain exactly one character"
  InvalidNumericLiteral _  -> Nothing
  UnexpectedCharacter _    -> Nothing
  TabCharacter             -> Just "use spaces for indentation instead of tabs"

-- | Format a single lex error for display
formatLexError :: Text -> LexError -> Text
formatLexError source err =
  let kind = lexErrorKind err
      span' = lexErrorSpan err
      code = errorCode kind
      msg = errorMessage kind
      help = errorHelp kind
      file = spanFile span'
      line = posLine (spanStart span')
      col = posColumn (spanStart span')
      -- Get the source line
      sourceLine' = getSourceLine source line
      -- Create the underline
      underlineLen = max 1 (posColumn (spanEnd span') - col)
      underline = T.replicate (col - 1) " " <> T.replicate underlineLen "^"
  in T.unlines $ filter (not . T.null)
    [ "error[" <> code <> "]: " <> msg
    , "  --> " <> file <> ":" <> showT line <> ":" <> showT col
    , "   |"
    , " " <> padLeft 2 (showT line) <> " | " <> sourceLine'
    , "   | " <> underline
    , maybe "" (\h -> "   |\n   = help: " <> h) help
    ]

-- | Format multiple lex errors
formatLexErrors :: Text -> [LexError] -> Text
formatLexErrors source errors =
  T.intercalate "\n" (map (formatLexError source) errors)

-- | Get a specific line from source text
getSourceLine :: Text -> Int -> Text
getSourceLine source lineNum =
  let sourceLines = T.lines source
  in if lineNum > 0 && lineNum <= length sourceLines
     then sourceLines !! (lineNum - 1)
     else ""

-- | Pad a text to a minimum width on the left
padLeft :: Int -> Text -> Text
padLeft n txt =
  let len = T.length txt
  in if len >= n then txt else T.replicate (n - len) " " <> txt

-- | Show a value as Text
showT :: Show a => a -> Text
showT = T.pack . show

-- | Check if a result is a lex error
isLexError :: Either [LexError] a -> Bool
isLexError (Left (_:_)) = True
isLexError _ = False

-- | Extract lex errors from a result
getLexErrors :: Either [LexError] a -> [LexError]
getLexErrors (Left errs) = errs
getLexErrors (Right _) = []
