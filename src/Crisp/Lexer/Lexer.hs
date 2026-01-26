{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Crisp.Lexer.Lexer
-- Description : Lexer for the Crisp language
--
-- Converts source text into a stream of tokens, handling significant
-- whitespace (indentation-based syntax) and generating layout tokens.
-- Supports error recovery to report multiple errors in a single pass.

module Crisp.Lexer.Lexer
  ( -- * Lexing
    lexFile
  , lexText
  , lexFileWithRecovery
  , lexTextWithRecovery
    -- * Re-exports
  , module Crisp.Lexer.Error
  ) where

import Crisp.Lexer.Token
import Crisp.Lexer.Error
import Crisp.Syntax.Span

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isUpper)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type for lexing
type Lexer = Parsec Void Text

-- | Lex a file (fails on first error)
lexFile :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Token]
lexFile = parse (lexTokens <* eof)

-- | Lex text with a given filename (fails on first error)
lexText :: Text -> Text -> Either (ParseErrorBundle Text Void) [Token]
lexText filename = parse (lexTokens <* eof) (T.unpack filename)

-- | Lex a file with error recovery, collecting multiple errors
lexFileWithRecovery :: FilePath -> Text -> Either [LexError] [Token]
lexFileWithRecovery filepath source =
  case parse (lexTokensWithRecovery <* eof) filepath source of
    Left bundle -> Left [convertParseError filepath bundle]
    Right (tokens, errors) ->
      if null errors
        then Right tokens
        else Left errors

-- | Lex text with error recovery
lexTextWithRecovery :: Text -> Text -> Either [LexError] [Token]
lexTextWithRecovery filename = lexFileWithRecovery (T.unpack filename)

-- | Convert a Megaparsec error to our LexError type
convertParseError :: FilePath -> ParseErrorBundle Text Void -> LexError
convertParseError filepath bundle =
  let pos = pstateSourcePos (bundlePosState bundle)
      line = unPos (sourceLine pos)
      col = unPos (sourceColumn pos)
      position = Position line col
      span' = pointSpan position (T.pack filepath)
  in LexError (UnexpectedCharacter '?') span' (Just "parse error")

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

-- | Lex all tokens (standard mode, fails on first error)
lexTokens :: Lexer [Token]
lexTokens = do
  skipWhitespaceAndComments
  go [] [0]  -- Start with base indentation of 0
  where
    go :: [Token] -> [Int] -> Lexer [Token]
    go acc indentStack = do
      atEnd' <- atEnd
      if atEnd'
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

-- | Lex all tokens with error recovery
lexTokensWithRecovery :: Lexer ([Token], [LexError])
lexTokensWithRecovery = do
  initialErrors <- skipWhitespaceWithErrorCollection
  go [] initialErrors [0]
  where
    go :: [Token] -> [LexError] -> [Int] -> Lexer ([Token], [LexError])
    go acc errors indentStack = do
      atEnd' <- atEnd
      if atEnd'
        then do
          dedents <- generateDedents indentStack 0
          pure (reverse (dedents ++ acc), reverse errors)
        else do
          result <- observing (try $ lexNextToken indentStack)
          case result of
            Right (tok, newStack) ->
              case tokenKind tok of
                Eof -> pure (reverse (tok : acc), reverse errors)
                Error _ -> do
                  -- Skip to next token-starting position
                  skipToNextToken
                  go acc errors newStack
                _ -> do
                  moreErrors <- skipWhitespaceWithErrorCollection
                  go (tok : acc) (moreErrors ++ errors) newStack
            Left _ -> do
              -- Try to recover by creating an error token
              file <- getFilename
              start <- getPosition
              maybeErr <- tryRecoverError
              case maybeErr of
                Just (errTok, lexErr) -> do
                  moreErrors <- skipWhitespaceWithErrorCollection
                  go (errTok : acc) (moreErrors ++ lexErr : errors) indentStack
                Nothing -> do
                  -- Skip one character and try again
                  c <- anySingle
                  moreErrors <- skipWhitespaceWithErrorCollection
                  span' <- makeSpan start file
                  let err = unexpectedCharacter c span'
                  go acc (moreErrors ++ err : errors) indentStack

    atEnd = option False (True <$ eof)

-- | Skip whitespace and comments, collecting errors for unterminated block comments
skipWhitespaceWithErrorCollection :: Lexer [LexError]
skipWhitespaceWithErrorCollection = do
  void $ many (char ' ' <|> char '\t' <|> char '\r')
  errors <- catMaybes <$> many skipCommentOrCollectError
  void $ many (char ' ' <|> char '\t' <|> char '\r')
  pure errors
  where
    skipCommentOrCollectError = choice
      [ Nothing <$ lineComment
      , Nothing <$ try blockComment
      , Just <$> collectUnterminatedBlockComment
      ]

-- | Collect an unterminated block comment as an error
collectUnterminatedBlockComment :: Lexer LexError
collectUnterminatedBlockComment = do
  file <- getFilename
  start <- getPosition
  _ <- string "{-"
  -- Consume everything until EOF (since we know it's unterminated)
  _ <- takeWhileP Nothing (const True)
  end <- getPosition
  let span' = Span start end file
  pure $ unterminatedBlockComment span'

-- | Skip characters until we find something that looks like the start of a token
skipToNextToken :: Lexer ()
skipToNextToken = void $ takeWhileP Nothing (\c -> c /= '\n' && not (isTokenStart c))

-- | Check if a character could start a token
isTokenStart :: Char -> Bool
isTokenStart c = isAlpha c || c `elem` ("_\"'0123456789([{" :: String)

-- | Try to recover from an error and create an appropriate error token
tryRecoverError :: Lexer (Maybe (Token, LexError))
tryRecoverError = do
  file <- getFilename
  start <- getPosition
  choice
    [ try $ tryRecoverUnterminatedString file start
    , try $ tryRecoverUnterminatedBlockComment file start
    , try $ tryRecoverUnterminatedChar file start
    , tryRecoverInvalidChar file start  -- Last resort, will consume any char
    ]

-- | Try to recover from an unterminated string
tryRecoverUnterminatedString :: Text -> Position -> Lexer (Maybe (Token, LexError))
tryRecoverUnterminatedString file start = do
  _ <- char '"'
  content <- takeWhileP (Just "string content") (\c -> c /= '"' && c /= '\n')
  end <- getPosition
  let span' = Span start end file
  -- Check if we hit newline (unterminated) or closing quote
  nextChar <- optional (char '"')
  case nextChar of
    Just _ -> do
      -- String was properly terminated - this shouldn't happen in recovery
      pure Nothing
    Nothing -> do
      -- Unterminated string
      let err = unterminatedString span' (Just content)
          tok = Token (Error "unterminated string") span'
      pure $ Just (tok, err)

-- | Try to recover from an unterminated block comment
tryRecoverUnterminatedBlockComment :: Text -> Position -> Lexer (Maybe (Token, LexError))
tryRecoverUnterminatedBlockComment file start = do
  _ <- string "{-"
  -- Try to find the closing -}
  let go depth = do
        atEnd' <- option False (True <$ eof)
        if atEnd'
          then pure False  -- Unterminated
          else do
            c <- anySingle
            case c of
              '-' -> do
                next <- optional (char '}')
                case next of
                  Just _ -> if depth == 1 then pure True else go (depth - 1)
                  Nothing -> go depth
              '{' -> do
                next <- optional (char '-')
                case next of
                  Just _ -> go (depth + 1)
                  Nothing -> go depth
              _ -> go depth
  terminated <- go 1
  end <- getPosition
  let span' = Span start end file
  if terminated
    then pure Nothing  -- Comment was properly terminated
    else do
      let err = unterminatedBlockComment span'
          tok = Token (Error "unterminated block comment") span'
      pure $ Just (tok, err)

-- | Try to recover from an unterminated character literal
tryRecoverUnterminatedChar :: Text -> Position -> Lexer (Maybe (Token, LexError))
tryRecoverUnterminatedChar file start = do
  _ <- char '\''
  -- Try to parse character content
  content <- takeWhileP (Just "character content") (\c -> c /= '\'' && c /= '\n')
  end <- getPosition
  let span' = Span start end file
  -- Check if properly terminated
  nextChar <- optional (char '\'')
  case nextChar of
    Just _ -> do
      -- Check if content is valid (should be exactly one character or escape)
      if T.length content == 1 || (T.length content == 2 && T.head content == '\\')
        then pure Nothing  -- Valid char literal
        else do
          let err = invalidCharLiteral content span'
              tok = Token (Error "invalid character literal") span'
          pure $ Just (tok, err)
    Nothing -> do
      let err = unterminatedCharLiteral span'
          tok = Token (Error "unterminated character literal") span'
      pure $ Just (tok, err)

-- | Try to recover from an invalid character
tryRecoverInvalidChar :: Text -> Position -> Lexer (Maybe (Token, LexError))
tryRecoverInvalidChar file start = do
  c <- anySingle
  end <- getPosition
  let span' = Span start end file
      err = unexpectedCharacter c span'
      tok = Token (Error $ "unexpected character: " <> T.singleton c) span'
  pure $ Just (tok, err)

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
        (d:_) -> pure (d, newStack)  -- Return first dedent
        []    -> pure (Token Newline nlSpan, newStack)
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
  , LeftArrow <$ string "<-"
  , DoubleColon <$ string "::"
  , PipeForward <$ string "|>"
  , PipeBackward <$ string "<|"
  , ComposeRight <$ string ">>"
  , ComposeLeft <$ string "<<"
  , LessEq <$ string "<="
  , GreaterEq <$ string ">="
  , EqEq <$ string "=="
  , NotEq <$ string "/="
  , AndAnd <$ string "&&"
  , OrOr <$ string "||"

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
  , Plus <$ char '+'
  , Minus <$ char '-'
  , Star <$ char '*'
  , Slash <$ char '/'
  , Less <$ char '<'
  , Greater <$ char '>'
  , Percent <$ char '%'
  , Caret <$ char '^'
  , Tilde <$ char '~'

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
      expPart <- optional (try lexExponent)
      case (floatPart, expPart) of
        (Nothing, Nothing) -> pure $ IntLit (read intPart)
        _ -> do
          let floatStr = intPart
                      ++ maybe "" ("." ++) floatPart
                      ++ maybe "" id expPart
          pure $ FloatLit (read floatStr)
  where
    lexExponent = do
      e <- oneOf ("eE" :: String)
      sign <- optional (oneOf ("+-" :: String))
      digits <- some digitChar
      pure $ [e] ++ maybe "" (:[]) sign ++ digits

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
