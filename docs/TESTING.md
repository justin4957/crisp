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

### Parser Tests (`test/Crisp/Parser/ParserSpec.hs`)

- Expression parsing (literals, applications, let, if, lambda, etc.)
- Type parsing (simple, function, application, forall, effectful)
- Module parsing (minimal, with authority)

### Type Checker Tests (`test/Crisp/Types/CheckerSpec.hs`)

- Variable synthesis
- Constructor synthesis
- Application synthesis
- Lambda checking
- (More tests pending)

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
