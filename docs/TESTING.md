# Testing Guide

This document describes testing strategies for the Crisp compiler.

## Running Tests

```bash
# Run all tests
stack test

# Run tests with verbose output
stack test --test-arguments "--verbose"

# Run specific test module
stack test --test-arguments "--match Lexer"

# Run specific test case
stack test --test-arguments "--match \"lexes fn keyword\""

# Run with specific random seed (for reproducibility)
stack test --test-arguments "--seed 12345"
```

## Test Structure

```
test/
├── Main.hs                           # Test entry point (uses hspec-discover)
└── Crisp/
    ├── Lexer/
    │   └── LexerSpec.hs              # Comprehensive lexer tests
    ├── Parser/
    │   └── ParserSpec.hs             # Parser tests
    └── Types/
        └── CheckerSpec.hs            # Type checker tests
```

## Test Framework

The test suite uses [HSpec](https://hspec.github.io/) for test organization and assertions.

### Basic Test Structure

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Crisp.Lexer.LexerSpec (spec) where

import Test.Hspec
import Crisp.Lexer.Lexer
import Crisp.Lexer.Token

spec :: Spec
spec = do
  describe "lexFile" $ do
    it "lexes simple keywords" $ do
      let result = lexFile "test" "fn let match"
      result `shouldSatisfy` isRight

    it "returns correct tokens" $ do
      lexKinds "fn" `shouldBe` Right [KwFn]
```

## Test Categories

### Lexer Tests (`test/Crisp/Lexer/LexerSpec.hs`)

The lexer test suite covers:

**Keywords (31 tests)**
- All language keywords (`fn`, `let`, `type`, `effect`, `handler`, `match`, etc.)
- Keyword/identifier disambiguation (`fn` vs `fn_helper`)

**Identifiers (7 tests)**
- Lowercase identifiers (`foo`, `camelCase`)
- Uppercase identifiers (`Foo`, `PascalCase`)
- Identifiers with underscores, primes, and numbers

**Operators (22 tests)**
- Arrow operators (`->`, `=>`, `<-`)
- Comparison operators (`<`, `>`, `<=`, `>=`, `==`, `/=`)
- Arithmetic operators (`+`, `-`, `*`, `/`, `%`)
- Logical operators (`&&`, `||`)
- Pipe operators (`|>`, `<|`)
- Other operators (`:`, `::`, `@`, `&`, `.`, `,`, `$`, `!`)

**Delimiters (6 tests)**
- Parentheses, brackets, braces
- Unit literal `()`

**Literals (18 tests)**
- Integer literals (decimal, hex `0xFF`, binary `0b1010`)
- Float literals (with decimals, exponents, signed exponents)
- String literals (simple, with escapes, unicode)
- Character literals (simple, escape sequences)

**Comments (6 tests)**
- Line comments (`--`)
- Block comments (`{- -}`)
- Nested block comments
- Comment-like sequences in strings

**Layout Tokens (5 tests)**
- Indent generation
- Dedent generation
- Newline at same level
- Multiple indent levels
- Dedents at end of file

**Edge Cases (7 tests)**
- Empty input
- Whitespace-only input
- Comment-only input
- Very long identifiers
- Consecutive operators
- Tokens without spaces

**Source Spans (3 tests)**
- Token position tracking
- Line number tracking
- Filename preservation

**Integration (10 tests)**
- Realistic code snippets (function definitions, type definitions, etc.)

### Lexer Error Tests (`test/Crisp/Lexer/ErrorSpec.hs`)

The lexer error test suite covers error recovery and reporting:

**Error Recovery (12 tests)**
- Unterminated string literals (end of line, at newline, span tracking)
- Unterminated block comments (simple, nested, error kind verification)
- Unterminated character literals
- Invalid character literals (empty, multi-character)
- Unexpected characters

**Multiple Error Collection (2 tests)**
- Continuing after an error to find valid tokens
- Collecting errors while producing partial token stream

**Error Formatting (5 tests)**
- Error codes in output (L001, L002, etc.)
- File position in output
- Error messages
- Help text

**Error Codes (8 tests)**
- L001: Unterminated string
- L002: Unterminated block comment
- L003: Unterminated character literal
- L004: Invalid escape sequence
- L005: Invalid character literal
- L006: Invalid numeric literal
- L007: Unexpected character
- L008: Tab character

**Error Construction (5 tests)**
- Creating various error types with context

**Error Predicates (4 tests)**
- `isLexError` and `getLexErrors` functions

**Valid Input (5 tests)**
- Ensuring valid code still lexes correctly with recovery mode

### Parser Tests (`test/Crisp/Parser/ParserSpec.hs`)

The parser test suite covers (~155 passing tests, ~27 pending for known limitations):

**Expression Tests (~80 tests)**
- Literals: integers, floats, strings (with escapes, unicode), characters, unit
- Variables and constructors: lowercase/uppercase, with underscores/primes/numbers
- Function application: single/multiple args, nested, with literals
- Let expressions: simple, with type annotation, nested, with patterns
- If expressions: simple, complex conditions, nested branches
- Match expressions: keyword and subject (arms pending due to parser limitation)
- Lambda expressions: backslash/unicode, single param, with application body
- Do notation: simple result expression (statements pending)
- Effect operations: perform with Effect.op syntax
- Lazy/force: both keywords with simple and complex expressions
- Pipeline operator: simple, chained, with application

**Pattern Tests (~15 tests)**
- Wildcard patterns in let
- Variable patterns in let
- Constructor patterns in let
- Tuple patterns: pairs, triples, nested
- (Match patterns pending due to parser limitation)

**Type Tests (~25 tests)**
- Simple types: variables, constructors
- Function types: simple, multi-argument, right-associativity, with parens
- Type applications: single, multiple, nested
- Forall types: simple, with kind annotation, nested
- Effect types: single effect, multiple effects, with authority
- Special types: Lazy, ref, ref mut, parenthesized

**Declaration Tests (~20 tests)**
- Function definitions: with/without params, type params, effects, return types
- Type definitions: simple, with parameters, with kind annotation
- Effect definitions: with operations
- Handler definitions: with return clause, with introduced effects

**Module Tests (~10 tests)**
- Minimal module, dotted path, authority
- Requires (effects, types), provides (type, fn)
- Multiple definitions

**Known Parser Limitations (documented via pending tests)**
- Match arms don't parse correctly due to greedy pExpr
- Do statements with binds/let don't work due to greedy expression parsing
- With handler expression has same issue
- Type constructors require layout (colon interpreted as kind annotation)
- Multi-param lambdas don't parse due to greedy type parsing

### Type Checker Tests (`test/Crisp/Types/CheckerSpec.hs`)

- Variable synthesis
- Constructor synthesis
- Application synthesis
- Lambda checking
- (More tests pending)

### Pattern Compiler Tests (`test/Crisp/Core/PatternSpec.hs`)

The pattern compiler test suite covers pattern match elaboration (~40 tests):

**Simple Pattern Tests (~7 tests)**
- Variable patterns: binding, preserving names, different bodies
- Wildcard patterns: single, multiple in sequence

**Constructor Pattern Tests (~13 tests)**
- Nullary constructors: `None`, `True`, `False`
- Unary constructors: `Some(x)`, `Just(y)` with binding
- Binary constructors: `Cons(h, t)`, `Pair(a, b)`
- Nested constructors: `Some(Some(x))`, `Cons(Some(y), xs)`
- Mixed subpatterns: `Pair(x, _)`, `Some(_)`

**Tuple Pattern Tests (~5 tests)**
- Pair patterns: `(a, b)`
- Triple patterns: `(a, b, c)`
- Nested tuples: `((a, b), c)`
- Constructor in tuple: `(Some(x), y)`

**Literal Pattern Tests (~2 tests)**
- Integer literals: `0`, `1`
- Multiple alternatives

**Guard Tests (~3 tests)**
- Simple guards: `y if y > 0`
- Guards with constructor patterns
- Guard compilation to conditionals

**Multiple Clause Tests (~4 tests)**
- Two clauses: `Some(x) -> x, None -> 0`
- Clause order preservation
- Mixed pattern types

**Pattern Variable Scoping Tests (~3 tests)**
- Variables scoped to branches
- Nested patterns binding all variables
- Duplicate names in different branches

**Edge Cases (~5 tests)**
- Empty match arms
- Single variable optimization
- Deeply nested structures
- Many variables in pattern

## Writing Tests

### Test Naming Convention

```haskell
describe "module or feature" $ do
  it "does something specific" $ do
    -- test body
```

### Common Assertions

```haskell
import Test.Hspec
import Data.Either (isRight, isLeft)

-- Equality
result `shouldBe` expected

-- Predicates
result `shouldSatisfy` isRight
result `shouldSatisfy` isLeft

-- List containment
kinds `shouldContain` [Indent]

-- Pattern matching
case result of
  Right tokens -> length tokens `shouldBe` 3
  Left _ -> expectationFailure "Expected success"
```

### Helper Functions

The lexer tests use these helpers:

```haskell
-- Get token kinds from lexing result
lexKinds :: Text -> Either String [TokenKind]
lexKinds input = case lexFile "test" input of
  Left err -> Left (show err)
  Right tokens -> Right (map tokenKind tokens)

-- Filter out layout tokens
withoutLayout :: [TokenKind] -> [TokenKind]
withoutLayout = filter (not . isLayoutToken)
```

## Test-Driven Development

For new features:

1. **Write failing tests first**
   ```haskell
   it "lexes new_keyword keyword" $ do
     lexKinds "new_keyword" `shouldBe` Right [KwNewKeyword]
   ```

2. **Run tests to confirm failure**
   ```bash
   stack test --test-arguments "--match new_keyword"
   ```

3. **Implement the feature**

4. **Run tests to confirm they pass**

5. **Refactor if needed**

## Test Coverage Goals

| Module | Target Coverage | Current Status |
|--------|-----------------|----------------|
| Lexer | 90%+ | ✅ Comprehensive |
| Parser | 80%+ | ⚠️ Basic coverage |
| Type Checker | 80%+ | ⚠️ Basic coverage |
| Effect System | 80%+ | ❌ Pending |
| Code Generation | 70%+ | ❌ Pending |

## Continuous Integration

Tests run automatically on every push and pull request. The CI checks:

1. `stack build` - Compilation succeeds
2. `stack test` - All tests pass

## Adding New Tests

1. Add test functions to the appropriate `*Spec.hs` file
2. Use descriptive `describe` and `it` labels
3. Run `stack test` to verify
4. Commit with descriptive message

## Property-Based Testing

For algebraic properties, use QuickCheck (already in dependencies):

```haskell
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "effect row operations" $ do
    prop "union is commutative" $ \a b ->
      effectUnion a b == effectUnion b a

    prop "union is associative" $ \a b c ->
      effectUnion (effectUnion a b) c == effectUnion a (effectUnion b c)
```

## Debugging Failed Tests

```bash
# Run with verbose output
stack test --test-arguments "--verbose"

# Run with specific seed for reproducibility
stack test --test-arguments "--seed 12345"

# Run single test
stack test --test-arguments "--match \"exact test name\""
```
