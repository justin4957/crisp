{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Lexer.Lexer
-- Description : Lexer for the Crisp language
--
-- Converts source text into a stream of tokens, handling significant
-- whitespace (indentation-based syntax) and generating layout tokens.

module Crisp.Lexer.Lexer
  ( -- * Lexing
    lexFile
  , lexText
  , LexError(..)
  ) where

import Crisp.Lexer.Token
import Crisp.Syntax.Span

import Control.Monad (void, when)
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isUpper, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Lexer error type
data LexError = LexError
  { lexErrorMessage :: !Text
  , lexErrorSpan    :: !Span
  } deriving stock (Eq, Show)

-- | Parser type for lexing
type Lexer = Parsec Void Text

-- | Lex a file
lexFile :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Token]
lexFile = parse (lexTokens <* eof)

-- | Lex text with a given filename
lexText :: Text -> Text -> Either (ParseErrorBundle Text Void) [Token]
lexText filename = parse (lexTokens <* eof) (T.unpack filename)

-- | Get current source position as our Position type
getPosition :: Lexer Position
getPosition = do
  pos <- getSourcePos
  pure $ Position (unPos $ sourceLine pos) (unPos $ sourceColumn pos)

-- | Create a span from start position to current position
makeSpan :: Position -> Text -> Lexer Span
makeSpan start file = do
  end <- getPosition
  pure $ Span start end file

-- | Get the current filename
getFilename :: Lexer Text
getFilename = T.pack . sourceName <$> getSourcePos

-- | Lex all tokens
lexTokens :: Lexer [Token]
lexTokens = do
  skipWhitespaceAndComments
  go [] [0]  -- Start with base indentation of 0
  where
    go :: [Token] -> [Int] -> Lexer [Token]
    go acc indentStack = do
      atEnd <- atEnd
      if atEnd
        then do
          -- Generate remaining dedents
          dedents <- generateDedents indentStack 0
          pure $ reverse (dedents ++ acc)
        else do
          (tok, newStack) <- lexNextToken indentStack
          case tokenKind tok of
            Eof -> pure $ reverse (tok : acc)
            _   -> go (tok : acc) newStack

    atEnd = option False (True <$ eof)

-- | Lex the next token, handling indentation
lexNextToken :: [Int] -> Lexer (Token, [Int])
lexNextToken indentStack = do
  -- Check for newline and handle indentation
  maybeNewline <- optional (try lexNewlineWithIndent)
  case maybeNewline of
    Just (indentLevel, nlSpan) -> handleIndentation indentStack indentLevel nlSpan
    Nothing -> do
      tok <- lexSingleToken
      skipWhitespaceAndComments
      pure (tok, indentStack)

-- | Handle indentation changes
handleIndentation :: [Int] -> Int -> Span -> Lexer (Token, [Int])
handleIndentation indentStack@(currentIndent:_) newIndent nlSpan
  | newIndent > currentIndent = do
      -- Indent
      skipWhitespaceAndComments
      pure (Token Indent nlSpan, newIndent : indentStack)
  | newIndent < currentIndent = do
      -- Dedent(s)
      let dedents = generateDedentsTo indentStack newIndent nlSpan
          newStack = dropWhile (> newIndent) indentStack
      skipWhitespaceAndComments
      case dedents of
        (d:ds) -> pure (d, newStack)  -- Return first dedent, rest would need buffering
        []     -> pure (Token Newline nlSpan, newStack)
  | otherwise = do
      -- Same level
      skipWhitespaceAndComments
      pure (Token Newline nlSpan, indentStack)
handleIndentation [] newIndent nlSpan = do
  skipWhitespaceAndComments
  pure (Token Newline nlSpan, [newIndent])

-- | Generate dedent tokens down to a target level
generateDedentsTo :: [Int] -> Int -> Span -> [Token]
generateDedentsTo stack target span' = go stack []
  where
    go (x:xs) acc
      | x > target = go xs (Token Dedent span' : acc)
      | otherwise  = reverse acc
    go [] acc = reverse acc

-- | Generate remaining dedents at end of file
generateDedents :: [Int] -> Int -> Lexer [Token]
generateDedents stack target = do
  file <- getFilename
  pos <- getPosition
  let span' = pointSpan pos file
  pure $ generateDedentsTo stack target span'

-- | Lex a newline and return the indentation level of the next line
lexNewlineWithIndent :: Lexer (Int, Span)
lexNewlineWithIndent = do
  file <- getFilename
  start <- getPosition
  void $ some (char '\n')
  indent <- countIndent
  span' <- makeSpan start file
  pure (indent, span')

-- | Count indentation (spaces only, tabs are errors)
countIndent :: Lexer Int
countIndent = length <$> many (char ' ')

-- | Skip whitespace and comments (but not newlines at line start)
skipWhitespaceAndComments :: Lexer ()
skipWhitespaceAndComments = L.space
  (void $ some (char ' ' <|> char '\t' <|> char '\r'))
  lineComment
  blockComment

-- | Line comment starting with --
lineComment :: Lexer ()
lineComment = L.skipLineComment "--"

-- | Block comment {- ... -}
blockComment :: Lexer ()
blockComment = L.skipBlockCommentNested "{-" "-}"

-- | Lex a single token (not handling layout)
lexSingleToken :: Lexer Token
lexSingleToken = do
  file <- getFilename
  start <- getPosition
  kind <- lexTokenKind
  span' <- makeSpan start file
  pure $ Token kind span'

-- | Lex a token kind
lexTokenKind :: Lexer TokenKind
lexTokenKind = choice
  [ -- Multi-char operators (must come before single-char)
    Arrow <$ string "->"
  , FatArrow <$ string "=>"
  , DoubleColon <$ string "::"
  , PipeForward <$ string "|>"
  , PipeBackward <$ string "<|"
  , ComposeRight <$ string ">>"
  , ComposeLeft <$ string "<<"

    -- Single char operators and punctuation
  , Pipe <$ char '|'
  , Colon <$ char ':'
  , Equals <$ char '='
  , At <$ char '@'
  , Ampersand <$ char '&'
  , Dot <$ char '.'
  , Comma <$ char ','
  , Dollar <$ char '$'
  , Bang <$ char '!'
  , Backslash <$ char '\\'
  , Lambda <$ char 'λ'

    -- Delimiters
  , try lexUnit
  , LParen <$ char '('
  , RParen <$ char ')'
  , LBracket <$ char '['
  , RBracket <$ char ']'
  , LBrace <$ char '{'
  , RBrace <$ char '}'

    -- Literals
  , lexNumber
  , lexString
  , lexChar

    -- Identifiers and keywords
  , lexIdentOrKeyword
  ]

-- | Lex unit literal ()
lexUnit :: Lexer TokenKind
lexUnit = Unit <$ string "()"

-- | Lex a number (integer or float)
lexNumber :: Lexer TokenKind
lexNumber = do
  -- Check for hex or binary prefix
  prefix <- optional (try $ string "0x" <|> string "0X" <|> string "0b" <|> string "0B")
  case prefix of
    Just "0x" -> IntLit <$> L.hexadecimal
    Just "0X" -> IntLit <$> L.hexadecimal
    Just "0b" -> IntLit <$> L.binary
    Just "0B" -> IntLit <$> L.binary
    _ -> do
      intPart <- some digitChar
      floatPart <- optional (try $ char '.' *> some digitChar)
      expPart <- optional (try $ oneOf ("eE" :: String) *> optional (oneOf ("+-" :: String)) *> some digitChar)
      case (floatPart, expPart) of
        (Nothing, Nothing) -> pure $ IntLit (read intPart)
        _ -> do
          let floatStr = intPart
                      ++ maybe "" ("." ++) floatPart
                      ++ maybe "" (\e -> "e" ++ e) expPart
          pure $ FloatLit (read floatStr)

-- | Lex a string literal
lexString :: Lexer TokenKind
lexString = do
  void $ char '"'
  content <- manyTill L.charLiteral (char '"')
  pure $ StringLit (T.pack content)

-- | Lex a character literal
lexChar :: Lexer TokenKind
lexChar = do
  void $ char '\''
  c <- L.charLiteral
  void $ char '\''
  pure $ CharLit c

-- | Lex an identifier or keyword
lexIdentOrKeyword :: Lexer TokenKind
lexIdentOrKeyword = do
  first <- satisfy isIdentStart
  rest <- many (satisfy isIdentContinue)
  let name = T.pack (first : rest)
  pure $ case lookupKeyword name of
    Just kw -> kw
    Nothing
      | isUpper first -> UpperIdent name
      | otherwise     -> LowerIdent name
  where
    isIdentStart c = isAlpha c || c == '_'
    isIdentContinue c = isAlphaNum c || c == '_' || c == '\''
