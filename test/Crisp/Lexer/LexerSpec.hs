{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Lexer.LexerSpec
-- Description : Comprehensive test suite for the Crisp lexer
--
-- Tests all token types, layout handling, edge cases, and error conditions.

module Crisp.Lexer.LexerSpec (spec) where

import Test.Hspec

import Crisp.Lexer.Lexer
import Crisp.Lexer.Token
import Crisp.Syntax.Span (Span(..), Position(..))

import Data.Either (isRight, isLeft)
import Data.Text (Text)
import qualified Data.Text as T

-- | Helper to extract just the token kinds from a successful lex
tokenKinds :: Either a [Token] -> Either a [TokenKind]
tokenKinds = fmap (map tokenKind)

-- | Helper to lex and get token kinds, filtering out layout tokens for simpler tests
lexKinds :: Text -> Either String [TokenKind]
lexKinds input = case lexFile "test" input of
  Left err -> Left (show err)
  Right tokens -> Right (map tokenKind tokens)

-- | Helper to lex and get token kinds, keeping layout tokens
lexKindsWithLayout :: Text -> Either String [TokenKind]
lexKindsWithLayout input = case lexFile "test" input of
  Left err -> Left (show err)
  Right tokens -> Right (map tokenKind tokens)

-- | Helper to filter out layout tokens for tests that don't care about them
withoutLayout :: [TokenKind] -> [TokenKind]
withoutLayout = filter (not . isLayoutToken)

spec :: Spec
spec = do
  -----------------------------------------------------------------------------
  -- Keyword Tests
  -----------------------------------------------------------------------------
  describe "keywords" $ do
    it "lexes fn keyword" $ do
      lexKinds "fn" `shouldBe` Right [KwFn]

    it "lexes let keyword" $ do
      lexKinds "let" `shouldBe` Right [KwLet]

    it "lexes type keyword" $ do
      lexKinds "type" `shouldBe` Right [KwType]

    it "lexes effect keyword" $ do
      lexKinds "effect" `shouldBe` Right [KwEffect]

    it "lexes handler keyword" $ do
      lexKinds "handler" `shouldBe` Right [KwHandler]

    it "lexes match keyword" $ do
      lexKinds "match" `shouldBe` Right [KwMatch]

    it "lexes if keyword" $ do
      lexKinds "if" `shouldBe` Right [KwIf]

    it "lexes then keyword" $ do
      lexKinds "then" `shouldBe` Right [KwThen]

    it "lexes else keyword" $ do
      lexKinds "else" `shouldBe` Right [KwElse]

    it "lexes in keyword" $ do
      lexKinds "in" `shouldBe` Right [KwIn]

    it "lexes with keyword" $ do
      lexKinds "with" `shouldBe` Right [KwWith]

    it "lexes import keyword" $ do
      lexKinds "import" `shouldBe` Right [KwImport]

    it "lexes module keyword" $ do
      lexKinds "module" `shouldBe` Right [KwModule]

    it "lexes requires keyword" $ do
      lexKinds "requires" `shouldBe` Right [KwRequires]

    it "lexes provides keyword" $ do
      lexKinds "provides" `shouldBe` Right [KwProvides]

    it "lexes authority as identifier (context-sensitive, issue #221)" $ do
      lexKinds "authority" `shouldBe` Right [LowerIdent "authority"]

    it "lexes linear keyword" $ do
      lexKinds "linear" `shouldBe` Right [KwLinear]

    it "lexes lazy keyword" $ do
      lexKinds "lazy" `shouldBe` Right [KwLazy]

    it "lexes perform keyword" $ do
      lexKinds "perform" `shouldBe` Right [KwPerform]

    it "lexes resume keyword" $ do
      lexKinds "resume" `shouldBe` Right [KwResume]

    it "lexes return keyword" $ do
      lexKinds "return" `shouldBe` Right [KwReturn]

    it "lexes where keyword" $ do
      lexKinds "where" `shouldBe` Right [KwWhere]

    it "lexes forall keyword" $ do
      lexKinds "forall" `shouldBe` Right [KwForall]

    it "lexes as keyword" $ do
      lexKinds "as" `shouldBe` Right [KwAs]

    it "lexes qualified keyword" $ do
      lexKinds "qualified" `shouldBe` Right [KwQualified]

    it "lexes do keyword" $ do
      lexKinds "do" `shouldBe` Right [KwDo]

    it "lexes prop keyword" $ do
      lexKinds "prop" `shouldBe` Right [KwProp]

    it "lexes total keyword" $ do
      lexKinds "total" `shouldBe` Right [KwTotal]

    it "lexes mut keyword" $ do
      lexKinds "mut" `shouldBe` Right [KwMut]

    it "lexes ref keyword" $ do
      lexKinds "ref" `shouldBe` Right [KwRef]

    it "lexes multiple keywords" $ do
      lexKinds "fn let match type" `shouldBe` Right [KwFn, KwLet, KwMatch, KwType]

    it "distinguishes keywords from identifiers with suffix" $ do
      lexKinds "fn_helper" `shouldBe` Right [LowerIdent "fn_helper"]
      lexKinds "let2" `shouldBe` Right [LowerIdent "let2"]
      lexKinds "types" `shouldBe` Right [LowerIdent "types"]

    it "distinguishes keywords from identifiers with prefix" $ do
      lexKinds "myfn" `shouldBe` Right [LowerIdent "myfn"]
      lexKinds "mylet" `shouldBe` Right [LowerIdent "mylet"]

  -----------------------------------------------------------------------------
  -- Identifier Tests
  -----------------------------------------------------------------------------
  describe "identifiers" $ do
    it "lexes lowercase identifiers" $ do
      lexKinds "foo" `shouldBe` Right [LowerIdent "foo"]
      lexKinds "bar" `shouldBe` Right [LowerIdent "bar"]
      lexKinds "camelCase" `shouldBe` Right [LowerIdent "camelCase"]

    it "lexes uppercase identifiers" $ do
      lexKinds "Foo" `shouldBe` Right [UpperIdent "Foo"]
      lexKinds "Bar" `shouldBe` Right [UpperIdent "Bar"]
      lexKinds "PascalCase" `shouldBe` Right [UpperIdent "PascalCase"]

    it "lexes identifiers with underscores" $ do
      lexKinds "foo_bar" `shouldBe` Right [LowerIdent "foo_bar"]
      lexKinds "_private" `shouldBe` Right [LowerIdent "_private"]
      lexKinds "Foo_Bar" `shouldBe` Right [UpperIdent "Foo_Bar"]

    it "lexes identifiers with primes" $ do
      lexKinds "foo'" `shouldBe` Right [LowerIdent "foo'"]
      lexKinds "x''" `shouldBe` Right [LowerIdent "x''"]
      lexKinds "Type'" `shouldBe` Right [UpperIdent "Type'"]

    it "lexes identifiers with numbers" $ do
      lexKinds "foo123" `shouldBe` Right [LowerIdent "foo123"]
      lexKinds "x1" `shouldBe` Right [LowerIdent "x1"]
      lexKinds "Type2" `shouldBe` Right [UpperIdent "Type2"]

    it "lexes mixed identifiers" $ do
      lexKinds "foo_bar123'" `shouldBe` Right [LowerIdent "foo_bar123'"]

    it "lexes multiple identifiers" $ do
      lexKinds "foo Bar baz Qux" `shouldBe` Right
        [LowerIdent "foo", UpperIdent "Bar", LowerIdent "baz", UpperIdent "Qux"]

  -----------------------------------------------------------------------------
  -- Operator Tests
  -----------------------------------------------------------------------------
  describe "operators" $ do
    it "lexes arrow operator" $ do
      lexKinds "->" `shouldBe` Right [Arrow]

    it "lexes fat arrow operator" $ do
      lexKinds "=>" `shouldBe` Right [FatArrow]

    it "lexes pipe operator" $ do
      lexKinds "|" `shouldBe` Right [Pipe]

    it "lexes colon operator" $ do
      lexKinds ":" `shouldBe` Right [Colon]

    it "lexes double colon operator" $ do
      lexKinds "::" `shouldBe` Right [DoubleColon]

    it "lexes equals operator" $ do
      lexKinds "=" `shouldBe` Right [Equals]

    it "lexes at operator" $ do
      lexKinds "@" `shouldBe` Right [At]

    it "lexes ampersand operator" $ do
      lexKinds "&" `shouldBe` Right [Ampersand]

    it "lexes dot operator" $ do
      lexKinds "." `shouldBe` Right [Dot]

    it "lexes comma" $ do
      lexKinds "," `shouldBe` Right [Comma]

    it "lexes pipe forward operator" $ do
      lexKinds "|>" `shouldBe` Right [PipeForward]

    it "lexes pipe backward operator" $ do
      lexKinds "<|" `shouldBe` Right [PipeBackward]

    it "lexes compose right operator" $ do
      lexKinds ">>" `shouldBe` Right [ComposeRight]

    it "lexes compose left operator" $ do
      lexKinds "<<" `shouldBe` Right [ComposeLeft]

    it "lexes dollar operator" $ do
      lexKinds "$" `shouldBe` Right [Dollar]

    it "lexes bang operator" $ do
      lexKinds "!" `shouldBe` Right [Bang]

    it "lexes backslash (lambda)" $ do
      lexKinds "\\" `shouldBe` Right [Backslash]

    it "lexes unicode lambda" $ do
      lexKinds "λ" `shouldBe` Right [Lambda]

    it "distinguishes arrow from minus and greater-than" $ do
      -- Arrow should be lexed as a single token
      lexKinds "a -> b" `shouldBe` Right [LowerIdent "a", Arrow, LowerIdent "b"]

    it "lexes operators in sequence" $ do
      lexKinds "a : b -> c" `shouldBe` Right
        [LowerIdent "a", Colon, LowerIdent "b", Arrow, LowerIdent "c"]

    it "lexes pipe operators correctly" $ do
      lexKinds "a |> b |> c" `shouldBe` Right
        [LowerIdent "a", PipeForward, LowerIdent "b", PipeForward, LowerIdent "c"]

  -----------------------------------------------------------------------------
  -- Delimiter Tests
  -----------------------------------------------------------------------------
  describe "delimiters" $ do
    it "lexes parentheses" $ do
      lexKinds "(" `shouldBe` Right [LParen]
      lexKinds ")" `shouldBe` Right [RParen]
      lexKinds "()" `shouldBe` Right [Unit]

    it "lexes brackets" $ do
      lexKinds "[" `shouldBe` Right [LBracket]
      lexKinds "]" `shouldBe` Right [RBracket]

    it "lexes braces" $ do
      lexKinds "{" `shouldBe` Right [LBrace]
      lexKinds "}" `shouldBe` Right [RBrace]

    it "lexes nested delimiters" $ do
      lexKinds "([{}])" `shouldBe` Right [LParen, LBracket, LBrace, RBrace, RBracket, RParen]

    it "lexes unit literal" $ do
      lexKinds "()" `shouldBe` Right [Unit]

    it "distinguishes unit from parentheses with content" $ do
      lexKinds "( )" `shouldBe` Right [LParen, RParen]
      lexKinds "(x)" `shouldBe` Right [LParen, LowerIdent "x", RParen]

  -----------------------------------------------------------------------------
  -- Integer Literal Tests
  -----------------------------------------------------------------------------
  describe "integer literals" $ do
    it "lexes simple integers" $ do
      lexKinds "0" `shouldBe` Right [IntLit 0]
      lexKinds "42" `shouldBe` Right [IntLit 42]
      lexKinds "123456" `shouldBe` Right [IntLit 123456]

    it "lexes hexadecimal integers" $ do
      lexKinds "0x0" `shouldBe` Right [IntLit 0]
      lexKinds "0xFF" `shouldBe` Right [IntLit 255]
      lexKinds "0XFF" `shouldBe` Right [IntLit 255]
      lexKinds "0x1a2B" `shouldBe` Right [IntLit 0x1a2B]

    it "lexes binary integers" $ do
      lexKinds "0b0" `shouldBe` Right [IntLit 0]
      lexKinds "0b1010" `shouldBe` Right [IntLit 10]
      lexKinds "0B1111" `shouldBe` Right [IntLit 15]

    it "lexes large integers" $ do
      lexKinds "9999999999999999999" `shouldBe` Right [IntLit 9999999999999999999]

    it "lexes multiple integers" $ do
      lexKinds "1 2 3" `shouldBe` Right [IntLit 1, IntLit 2, IntLit 3]

  -----------------------------------------------------------------------------
  -- Float Literal Tests
  -----------------------------------------------------------------------------
  describe "float literals" $ do
    it "lexes simple floats" $ do
      lexKinds "0.0" `shouldBe` Right [FloatLit 0.0]
      lexKinds "3.14" `shouldBe` Right [FloatLit 3.14]
      lexKinds "123.456" `shouldBe` Right [FloatLit 123.456]

    it "lexes floats with exponents" $ do
      lexKinds "1e10" `shouldBe` Right [FloatLit 1e10]
      lexKinds "1E10" `shouldBe` Right [FloatLit 1e10]
      lexKinds "1.5e10" `shouldBe` Right [FloatLit 1.5e10]

    it "lexes floats with signed exponents" $ do
      lexKinds "1e+10" `shouldBe` Right [FloatLit 1e10]
      lexKinds "1e-10" `shouldBe` Right [FloatLit 1e-10]

    it "lexes multiple floats" $ do
      lexKinds "1.0 2.0 3.0" `shouldBe` Right [FloatLit 1.0, FloatLit 2.0, FloatLit 3.0]

  -----------------------------------------------------------------------------
  -- String Literal Tests
  -----------------------------------------------------------------------------
  describe "string literals" $ do
    it "lexes empty strings" $ do
      lexKinds "\"\"" `shouldBe` Right [StringLit ""]

    it "lexes simple strings" $ do
      lexKinds "\"hello\"" `shouldBe` Right [StringLit "hello"]
      lexKinds "\"world\"" `shouldBe` Right [StringLit "world"]

    it "lexes strings with spaces" $ do
      lexKinds "\"hello world\"" `shouldBe` Right [StringLit "hello world"]

    it "lexes strings with escape sequences" $ do
      lexKinds "\"hello\\nworld\"" `shouldBe` Right [StringLit "hello\nworld"]
      lexKinds "\"tab\\there\"" `shouldBe` Right [StringLit "tab\there"]
      lexKinds "\"quote\\\"here\"" `shouldBe` Right [StringLit "quote\"here"]
      lexKinds "\"backslash\\\\here\"" `shouldBe` Right [StringLit "backslash\\here"]

    it "lexes strings with unicode escapes" $ do
      -- Note: Megaparsec's charLiteral handles unicode escapes
      lexKinds "\"\\955\"" `shouldBe` Right [StringLit "\955"]

    it "lexes multiple strings" $ do
      lexKinds "\"a\" \"b\" \"c\"" `shouldBe` Right [StringLit "a", StringLit "b", StringLit "c"]

  -----------------------------------------------------------------------------
  -- Character Literal Tests
  -----------------------------------------------------------------------------
  describe "character literals" $ do
    it "lexes simple characters" $ do
      lexKinds "'a'" `shouldBe` Right [CharLit 'a']
      lexKinds "'Z'" `shouldBe` Right [CharLit 'Z']
      lexKinds "'0'" `shouldBe` Right [CharLit '0']

    it "lexes escape characters" $ do
      lexKinds "'\\n'" `shouldBe` Right [CharLit '\n']
      lexKinds "'\\t'" `shouldBe` Right [CharLit '\t']
      lexKinds "'\\''" `shouldBe` Right [CharLit '\'']
      lexKinds "'\\\\'" `shouldBe` Right [CharLit '\\']

    it "lexes space character" $ do
      lexKinds "' '" `shouldBe` Right [CharLit ' ']

  -----------------------------------------------------------------------------
  -- Comment Tests
  -----------------------------------------------------------------------------
  describe "comments" $ do
    it "skips line comments" $ do
      lexKinds "foo -- this is a comment\nbar" `shouldSatisfy` \case
        Right kinds -> withoutLayout kinds == [LowerIdent "foo", LowerIdent "bar"]
        Left _ -> False

    it "skips line comments at end of file" $ do
      lexKinds "foo -- comment" `shouldBe` Right [LowerIdent "foo"]

    it "skips block comments" $ do
      lexKinds "foo {- block -} bar" `shouldBe` Right [LowerIdent "foo", LowerIdent "bar"]

    it "skips nested block comments" $ do
      lexKinds "foo {- outer {- inner -} outer -} bar" `shouldBe`
        Right [LowerIdent "foo", LowerIdent "bar"]

    it "handles deeply nested block comments" $ do
      lexKinds "foo {- {- {- deep -} -} -} bar" `shouldBe`
        Right [LowerIdent "foo", LowerIdent "bar"]

    it "handles comment-like sequences in strings" $ do
      lexKinds "\"-- not a comment\"" `shouldBe` Right [StringLit "-- not a comment"]
      lexKinds "\"{- also not -}\"" `shouldBe` Right [StringLit "{- also not -}"]

  -----------------------------------------------------------------------------
  -- Layout Token Tests
  -----------------------------------------------------------------------------
  describe "layout tokens" $ do
    it "generates indent token when indentation increases" $ do
      let result = lexKindsWithLayout "foo\n  bar"
      case result of
        Right kinds -> kinds `shouldContain` [Indent]
        Left _ -> expectationFailure "lexing failed"

    it "generates dedent token when indentation decreases" $ do
      let result = lexKindsWithLayout "foo\n  bar\nbaz"
      case result of
        Right kinds -> do
          kinds `shouldContain` [Indent]
          kinds `shouldContain` [Dedent]
        Left _ -> expectationFailure "lexing failed"

    it "generates newline token at same indentation level" $ do
      let result = lexKindsWithLayout "foo\nbar"
      case result of
        Right kinds -> kinds `shouldContain` [Newline]
        Left _ -> expectationFailure "lexing failed"

    it "handles multiple indent levels" $ do
      let result = lexKindsWithLayout "a\n  b\n    c\n  d\ne"
      case result of
        Right kinds -> do
          length (filter (== Indent) kinds) `shouldBe` 2
          length (filter (== Dedent) kinds) `shouldBe` 2
        Left _ -> expectationFailure "lexing failed"

    it "generates dedents at end of file" $ do
      let result = lexKindsWithLayout "a\n  b\n    c"
      case result of
        Right kinds ->
          -- Should have dedents to close all open indentation
          length (filter (== Dedent) kinds) `shouldBe` 2
        Left _ -> expectationFailure "lexing failed"

  -----------------------------------------------------------------------------
  -- Token Predicate Tests
  -----------------------------------------------------------------------------
  describe "token predicates" $ do
    it "isKeyword identifies all keywords" $ do
      isKeyword KwFn `shouldBe` True
      isKeyword KwLet `shouldBe` True
      isKeyword KwType `shouldBe` True
      isKeyword KwEffect `shouldBe` True
      isKeyword KwHandler `shouldBe` True
      isKeyword KwMatch `shouldBe` True
      isKeyword KwIf `shouldBe` True
      isKeyword KwThen `shouldBe` True
      isKeyword KwElse `shouldBe` True
      isKeyword KwIn `shouldBe` True
      isKeyword KwWith `shouldBe` True
      isKeyword KwImport `shouldBe` True
      isKeyword KwModule `shouldBe` True
      isKeyword KwRequires `shouldBe` True
      isKeyword KwProvides `shouldBe` True
      isKeyword KwLinear `shouldBe` True
      isKeyword KwLazy `shouldBe` True
      isKeyword KwPerform `shouldBe` True
      isKeyword KwResume `shouldBe` True
      isKeyword KwReturn `shouldBe` True
      isKeyword KwWhere `shouldBe` True
      isKeyword KwForall `shouldBe` True
      isKeyword KwAs `shouldBe` True
      isKeyword KwQualified `shouldBe` True
      isKeyword KwDo `shouldBe` True
      isKeyword KwProp `shouldBe` True
      isKeyword KwTotal `shouldBe` True
      isKeyword KwMut `shouldBe` True
      isKeyword KwRef `shouldBe` True

    it "isKeyword returns false for non-keywords" $ do
      isKeyword (LowerIdent "foo") `shouldBe` False
      isKeyword (UpperIdent "Foo") `shouldBe` False
      isKeyword (IntLit 42) `shouldBe` False
      isKeyword Arrow `shouldBe` False

    it "isLayoutToken identifies layout tokens" $ do
      isLayoutToken Indent `shouldBe` True
      isLayoutToken Dedent `shouldBe` True
      isLayoutToken Newline `shouldBe` True

    it "isLayoutToken returns false for non-layout tokens" $ do
      isLayoutToken KwFn `shouldBe` False
      isLayoutToken (LowerIdent "x") `shouldBe` False
      isLayoutToken Arrow `shouldBe` False

    it "isLiteral identifies literals" $ do
      isLiteral (IntLit 42) `shouldBe` True
      isLiteral (FloatLit 3.14) `shouldBe` True
      isLiteral (StringLit "hello") `shouldBe` True
      isLiteral (CharLit 'a') `shouldBe` True
      isLiteral Unit `shouldBe` True

    it "isLiteral returns false for non-literals" $ do
      isLiteral KwFn `shouldBe` False
      isLiteral (LowerIdent "x") `shouldBe` False
      isLiteral Arrow `shouldBe` False

  -----------------------------------------------------------------------------
  -- Keyword Lookup Tests
  -----------------------------------------------------------------------------
  describe "keyword lookup" $ do
    it "looks up all keywords correctly" $ do
      lookupKeyword "fn" `shouldBe` Just KwFn
      lookupKeyword "let" `shouldBe` Just KwLet
      lookupKeyword "type" `shouldBe` Just KwType
      lookupKeyword "effect" `shouldBe` Just KwEffect
      lookupKeyword "handler" `shouldBe` Just KwHandler
      lookupKeyword "match" `shouldBe` Just KwMatch
      lookupKeyword "if" `shouldBe` Just KwIf
      lookupKeyword "then" `shouldBe` Just KwThen
      lookupKeyword "else" `shouldBe` Just KwElse
      lookupKeyword "in" `shouldBe` Just KwIn
      lookupKeyword "with" `shouldBe` Just KwWith
      lookupKeyword "import" `shouldBe` Just KwImport
      lookupKeyword "module" `shouldBe` Just KwModule
      lookupKeyword "requires" `shouldBe` Just KwRequires
      lookupKeyword "provides" `shouldBe` Just KwProvides
      lookupKeyword "authority" `shouldBe` Nothing
      lookupKeyword "linear" `shouldBe` Just KwLinear
      lookupKeyword "lazy" `shouldBe` Just KwLazy
      lookupKeyword "perform" `shouldBe` Just KwPerform
      lookupKeyword "resume" `shouldBe` Just KwResume
      lookupKeyword "return" `shouldBe` Just KwReturn
      lookupKeyword "where" `shouldBe` Just KwWhere
      lookupKeyword "forall" `shouldBe` Just KwForall
      lookupKeyword "as" `shouldBe` Just KwAs
      lookupKeyword "qualified" `shouldBe` Just KwQualified
      lookupKeyword "do" `shouldBe` Just KwDo
      lookupKeyword "prop" `shouldBe` Just KwProp
      lookupKeyword "total" `shouldBe` Just KwTotal
      lookupKeyword "mut" `shouldBe` Just KwMut
      lookupKeyword "ref" `shouldBe` Just KwRef

    it "returns Nothing for non-keywords" $ do
      lookupKeyword "foo" `shouldBe` Nothing
      lookupKeyword "Bar" `shouldBe` Nothing
      lookupKeyword "fnx" `shouldBe` Nothing

  -----------------------------------------------------------------------------
  -- Edge Case Tests
  -----------------------------------------------------------------------------
  describe "edge cases" $ do
    it "handles empty input" $ do
      lexKinds "" `shouldBe` Right []

    it "handles whitespace-only input" $ do
      lexKinds "   " `shouldBe` Right []
      lexKinds "\t\t" `shouldBe` Right []

    it "handles input with only comments" $ do
      lexKinds "-- just a comment" `shouldBe` Right []
      lexKinds "{- block only -}" `shouldBe` Right []

    it "handles very long identifiers" $ do
      let longIdent = T.replicate 1000 "a"
      case lexKinds longIdent of
        Right [LowerIdent name] -> name `shouldBe` longIdent
        _ -> expectationFailure "Expected single long identifier"

    it "handles consecutive operators" $ do
      lexKinds "->->" `shouldBe` Right [Arrow, Arrow]
      lexKinds "::" `shouldBe` Right [DoubleColon]

    it "handles tokens without spaces" $ do
      lexKinds "fn(x)" `shouldBe` Right [KwFn, LParen, LowerIdent "x", RParen]
      lexKinds "42+3" `shouldSatisfy` isRight

    it "preserves token order" $ do
      lexKinds "a b c d e" `shouldBe`
        Right [LowerIdent "a", LowerIdent "b", LowerIdent "c", LowerIdent "d", LowerIdent "e"]

  -----------------------------------------------------------------------------
  -- Source Span Tests
  -----------------------------------------------------------------------------
  describe "source spans" $ do
    it "tracks token positions correctly" $ do
      case lexFile "test.crisp" "foo bar" of
        Right (tok1:tok2:_) -> do
          -- First token should start at column 1
          let span1 = tokenSpan tok1
          spanStartCol span1 `shouldBe` 1
          -- Second token should start after "foo "
          let span2 = tokenSpan tok2
          spanStartCol span2 `shouldBe` 5
        _ -> expectationFailure "Expected at least 2 tokens"

    it "tracks line numbers correctly" $ do
      case lexFile "test.crisp" "foo\nbar" of
        Right tokens -> do
          let nonLayout = filter (not . isLayoutToken . tokenKind) tokens
          case nonLayout of
            (tok1:tok2:_) -> do
              spanStartLine (tokenSpan tok1) `shouldBe` 1
              spanStartLine (tokenSpan tok2) `shouldBe` 2
            _ -> expectationFailure "Expected at least 2 non-layout tokens"
        _ -> expectationFailure "Lexing failed"

    it "preserves filename in spans" $ do
      case lexFile "my/test/file.crisp" "foo" of
        Right (tok:_) ->
          spanFileName (tokenSpan tok) `shouldBe` "my/test/file.crisp"
        _ -> expectationFailure "Expected at least 1 token"

  -----------------------------------------------------------------------------
  -- Integration Tests
  -----------------------------------------------------------------------------
  describe "realistic code snippets" $ do
    it "lexes a simple function definition" $ do
      let code = "fn add x y:\n  x + y"
      lexFile "test" code `shouldSatisfy` isRight

    it "lexes a type definition" $ do
      let code = "type Option T:\n  Some T\n  None"
      lexFile "test" code `shouldSatisfy` isRight

    it "lexes an effect definition" $ do
      let code = "effect State S:\n  get : S\n  put : S -> ()"
      lexFile "test" code `shouldSatisfy` isRight

    it "lexes a handler definition" $ do
      let code = "handler RunState S (init: S) for State S:\n  return x -> x\n  get () -> resume s s"
      lexFile "test" code `shouldSatisfy` isRight

    it "lexes a match expression" $ do
      let code = "match xs\n  Nil -> 0\n  Cons x rest -> 1"
      lexFile "test" code `shouldSatisfy` isRight

    it "lexes a do expression" $ do
      let code = "do\n  x <- State.get\n  State.put (x + 1)"
      lexFile "test" code `shouldSatisfy` isRight

    it "lexes a lambda expression" $ do
      lexFile "test" "\\x: Int. x + 1" `shouldSatisfy` isRight

    it "lexes a pipeline" $ do
      lexFile "test" "xs |> map f |> filter p |> fold (+) 0" `shouldSatisfy` isRight

    it "lexes type annotations" $ do
      lexFile "test" "fn identity [T] (x: T) -> T: x" `shouldSatisfy` isRight

    it "lexes effectful function type" $ do
      lexFile "test" "fn read (path: Path) -> String ! IO:" `shouldSatisfy` isRight

-- Helper functions for span access
spanStartLine :: Span -> Int
spanStartLine s = posLine (spanStart s)

spanStartCol :: Span -> Int
spanStartCol s = posColumn (spanStart s)

spanFileName :: Span -> Text
spanFileName s = spanFile s
