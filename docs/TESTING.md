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

### Type Inference Tests (`test/Crisp/Types/InferSpec.hs`)

The type inference test suite covers Hindley-Milner style inference with let-polymorphism (~40 tests):

**Simple Inference Tests (~7 tests)**
- Unit, Bool, Int constructor inference
- Bound variable type lookup
- Unbound variable error handling
- Polymorphic variable instantiation

**Function Inference Tests (~6 tests)**
- Identity function type: `\x. x : a -> a`
- Constant function type: `\x. \y. x : a -> b -> a`
- Lambda with application body
- Application result type inference
- Type mismatch in application (failure case)
- Non-function application (failure case)

**Let-Binding Tests (~7 tests)**
- Simple let-bound variable in body
- Nested let-bindings
- Shadowing of outer bindings
- Let-bound function usage
- Polymorphic identity function generalization
- Polymorphic instantiation at different types
- Const function polymorphism: `let const = \x.\y.x in const True 42 : Bool`

**Annotated Let-Binding Tests (~2 tests)**
- Explicit type annotation respected
- Annotation constrains inferred type

**Generalization Tests (~3 tests)**
- Free type variables are generalized
- Environment variables are not generalized
- Multiple free variables generalization

**Instantiation Tests (~3 tests)**
- Monomorphic scheme unchanged
- Polymorphic scheme with fresh variables
- Different instantiations get different variables

**Type Scheme Tests (~3 tests)**
- `monoScheme` creates scheme with no quantified vars
- `schemeType` extracts type from scheme
- Scheme with quantified variables

**Free Variable Tests (~5 tests)**
- Simple type has no free vars
- Type variable is free
- Function type collects vars from both sides
- Type constructor collects vars from args
- Nested types collect all vars

**Edge Cases (~5 tests)**
- Deeply nested let-bindings
- Let-binding with complex type
- Function returning function
- Higher-order function
- Composition function: `\f.\g.\x. f (g x)`

### Kind Checker Tests (`test/Crisp/Types/KindSpec.hs`)

The kind checker test suite covers kind checking for types (~60 tests):

**Basic Kind Tests (~4 tests)**
- Primitive types: Int, Bool, String, Unit have kind Type
- Type universe levels: `Type_0 : Type_1`, `Type_1 : Type_2`

**Primitive Type Tests (~5 tests)**
- `TyUniverse` levels
- `TyProp` has kind Prop
- Type variable kinds from environment
- Unbound type variable errors

**Type Constructor Tests (~4 tests)**
- List : Type -> Type
- Option : Type -> Type
- Either : Type -> Type -> Type
- Map : Type -> Type -> Type

**Type Application Tests (~8 tests)**
- `List(Int)` : Type
- `Option(String)` : Type
- `Either(Int, String)` : Type
- Nested application: `List(List(Int))` : Type
- Partial application: `Either(Int)` : Type -> Type
- Over-applied constructor error
- Kind mismatch in application error

**Function Type Tests (~5 tests)**
- `Int -> Bool` : Type
- `Int -> Int -> Int` : Type
- `List(a) -> Int` : Type
- Ill-kinded domain rejection
- Ill-kinded codomain rejection

**Forall Type Tests (~5 tests)**
- `forall a. a` : Type
- `forall a. a -> a` : Type
- `forall a. List(a)` : Type
- Higher-kinded parameters: `forall (f : Type -> Type). f(Int)` : Type
- Nested forall types

**Effect Row Tests (~4 tests)**
- Empty effect row well-kinded
- Single effect well-kinded
- Effect union well-kinded
- Effect variable with environment

**Special Type Tests (~6 tests)**
- `Lazy(Int)` : Type
- `Lazy(List(a))` : Type
- `ref Int` : Type
- `ref mut Int` : Type
- `linear Int` : Linear
- Ill-kinded Lazy argument rejection

**Kind Error Tests (~5 tests)**
- Unbound type constructor
- Partial application returns remaining kind
- Over-application error
- Kind mismatch error
- Unbound type variable error

**Higher-Kinded Type Tests (~4 tests)**
- Type -> Type parameter
- (Type -> Type) -> Type parameter
- Functor-like type: `forall (f: Type -> Type). f(Int) -> f(Bool)`
- Nested higher-kinded kinds

**Kind Environment Tests (~5 tests)**
- Default environment has standard types
- Environment extension
- Shadowing behavior
- Preservation of other bindings

**Edge Cases (~5 tests)**
- Deeply nested type application
- Complex function types
- Multiple type variables in forall
- TyForallDep types
- Mixed forall and function types

### Pattern Exhaustiveness Tests (`test/Crisp/Types/ExhaustiveSpec.hs`)

The pattern exhaustiveness test suite covers exhaustiveness checking (~52 tests):

**Boolean Exhaustive Tests (~6 tests)**
- Exhaustive boolean match (True, False)
- Order independence (False, True)
- Wildcard covers boolean
- Variable covers boolean
- Rejects single True pattern
- Rejects single False pattern

**Option Exhaustive Tests (~6 tests)**
- Exhaustive option match (Some, None)
- Order independence (None, Some)
- Some with wildcard subpattern
- Wildcard covers option
- Rejects single Some pattern
- Rejects single None pattern

**List Exhaustive Tests (~5 tests)**
- Exhaustive list match (Cons, Nil)
- Order independence
- Wildcard covers list
- Rejects single Cons pattern
- Rejects single Nil pattern

**Result Exhaustive Tests (~4 tests)**
- Exhaustive result match (Ok, Err)
- Order independence
- Rejects single Ok pattern
- Rejects single Err pattern

**Non-Exhaustive Tests (~3 tests)**
- Empty pattern list
- Missing constructor in triple option
- Two missing constructors

**Wildcard Pattern Tests (~4 tests)**
- Wildcard covers all constructors
- Wildcard covers complex types
- Wildcard after specific patterns
- Wildcard in middle makes exhaustive

**Variable Pattern Tests (~3 tests)**
- Variable covers all constructors
- Variable in subpattern
- Multiple variables in pattern

**Nested Pattern Tests (~4 tests)**
- Some with any subpattern is exhaustive
- Cons with nested wildcard
- List with variable subpatterns
- Wildcards in subpatterns ensure coverage

**Redundant Pattern Tests (~5 tests)**
- Detects redundant after wildcard
- Detects redundant after variable
- No redundancy in complete match
- No redundancy with specific then wildcard
- Detects duplicate constructor patterns

**Missing Pattern Reporting (~7 tests)**
- Reports missing True
- Reports missing False
- Reports missing None
- Reports missing Some with placeholder
- Reports multiple missing constructors
- Reports all constructors for empty patterns
- No missing when exhaustive

**Edge Cases (~5 tests)**
- Single-constructor types
- Empty patterns for single-constructor
- Multiple wildcards redundant but exhaustive
- Complex patterns with wildcards
- Order doesn't affect exhaustiveness

### Effect Declaration Typing Tests (`test/Crisp/Effects/TypingSpec.hs`)

The effect declaration typing test suite covers effect typing (~29 tests):

**Effect Declaration Tests (~4 tests)**
- Simple effect declaration validation
- Effect with single operation
- Effect with multiple operations
- Effect with no operations

**Operation Typing Tests (~3 tests)**
- Nullary operation return type
- Unary operation parameter type
- Binary operation (get/put) types

**Effect Environment Tests (~5 tests)**
- Empty environment has no effects
- Adding effect to environment
- Looking up operation by effect and name
- Unknown effect returns Nothing
- Unknown operation returns Nothing

**Parameterized Effect Tests (~4 tests)**
- State(S) effect validation
- Reader(R) effect validation
- Effect with multiple type parameters
- Effect parameters recorded in environment

**Duplicate Detection Tests (~3 tests)**
- Rejects duplicate effect names
- Rejects duplicate operation names within effect
- Allows same operation name in different effects

**Operation Type Inference Tests (~3 tests)**
- Infers full operation type with effect
- Infers operation type with parameter
- Infers parameterized operation type with substitution

**Effect Scoping Tests (~2 tests)**
- Effect parameters scope to all operations
- Different effects have independent scopes

**Edge Cases (~5 tests)**
- Operation with multiple parameters
- Complex return types
- Higher-kinded effect parameters
- Preserves operation order
- Empty parameter list

### Effect Polymorphism Tests (`test/Crisp/Effects/PolymorphismSpec.hs`)

The effect polymorphism test suite covers row-polymorphic effects (~46 tests):

**Row Variable Tests (~5 tests)**
- Creating row variables with name and index
- Checking if row contains variable
- Concrete rows have no variables
- Unions with variables
- Extracting effect variables from rows

**Row Unification Tests (~9 tests)**
- Unifying empty rows
- Row variable with empty
- Row variable with concrete effects
- Two row variables
- Identical concrete rows
- Same effects in different order
- Mismatched concrete rows (failure)
- Union with row variable
- Occurs check handling

**Row Operation Tests (~5 tests)**
- Union of variable with concrete effects
- Removing effect from row with variable
- Row difference computation
- Row subset checking
- Effect row normalization

**Effect Polymorphism Tests (~5 tests)**
- Recognizing effect-polymorphic types
- Recognizing non-polymorphic types
- Pure functions and polymorphism
- Extracting effect variables from types
- Nested type effect variable extraction

**Effect Scheme Tests (~3 tests)**
- Creating effect scheme from type
- Scheme with no effect variables
- Environment-aware quantification

**Effect Instantiation Tests (~3 tests)**
- Instantiating with fresh variables
- Preserving type structure
- Multiple effect variable instantiation

**Effect Subset Tests (~6 tests)**
- Empty subset of anything
- Concrete subset of larger concrete
- Non-subset detection
- Row variable subset flexibility
- Union subset checking

**Effect Constraint Tests (~4 tests)**
- Constraint generation for function calls
- Solving simple constraints
- Failing unsatisfiable constraints
- Propagating constraints through composition

**Edge Cases (~6 tests)**
- Empty effect sets
- Nested unions
- Effects with authority
- Effect substitution in types
- Higher-kinded effect variables
- Effect variable name preservation

### Constructor Typing Tests (`test/Crisp/Types/ConstructorSpec.hs`)

The constructor typing test suite covers algebraic data type constructors (~43 tests):

**Type Declaration Tests (~5 tests)**
- Simple type declaration registration
- Type with parameters
- Type with multiple parameters
- Duplicate type name rejection
- Duplicate constructor name rejection

**Constructor Declaration Tests (~4 tests)**
- Nullary constructor type
- Unary constructor function type
- Binary constructor curried function type
- Constructor return type recording

**Constructor Type Synthesis Tests (~3 tests)**
- Nullary constructor synthesis
- Parameterized constructor synthesis
- Unknown constructor error

**Constructor Application Tests (~5 tests)**
- Nullary constructor application
- Unary constructor with correct argument
- Wrong argument type rejection
- Wrong arity rejection
- Partial application handling

**Pattern Matching Tests (~6 tests)**
- Type extraction from constructor pattern
- Nested pattern type extraction
- Wildcard pattern handling
- Variable pattern on whole type
- Wrong constructor rejection
- Multi-arg constructor bindings

**Parameterized Type Tests (~4 tests)**
- Type parameter instantiation
- Multiple type parameters
- Return type after instantiation
- Preserves non-parameterized types

**Recursive Type Tests (~4 tests)**
- Simple recursive type (List)
- Nat type
- Tree type
- Mutually recursive types

**GADT-Style Constructor Tests (~3 tests)**
- Refined return type
- GADT return type recording
- Pattern match with GADT refinement

**Type Instantiation Tests (~4 tests)**
- Type variable substitution in param
- Type variable in nested type
- Multiple type variables
- Non-parameterized type preservation

**Edge Cases (~5 tests)**
- Empty type (no constructors)
- Single constructor type
- Constructor with many parameters
- Higher-kinded type parameter
- Type with both type and value params
- Constructor order preservation

### Handler Typing Tests (`test/Crisp/Effects/HandlerSpec.hs`)

The handler typing test suite covers effect handler typing rules (~40 tests):

**Handler Declaration Tests (~4 tests)**
- Basic handler declaration validation
- Handler with correct operations
- Handler effect lookup
- Handler type parameters

**Operation Clause Tests (~6 tests)**
- Checking clause with wildcard pattern
- Checking clause with variable pattern
- Unknown operation rejection
- Missing operations detection
- Duplicate operation detection
- Extra operation rejection

**Return Clause Tests (~3 tests)**
- Return clause basic check
- Return clause with pattern
- Return pattern type checking

**Resume Type Tests (~4 tests)**
- Resume continuation type computation
- Resume type with parameterized effect
- Resume type for void operation
- Resume type matches operation return

**Effect Row Operations (~6 tests)**
- Removing handled effect from row
- Effect not present (no-op)
- Union effect removal
- Contains effect checking
- Has effect variable checking
- Effect row transformation

**Handler Type Computation (~5 tests)**
- Compute full handler type
- Handler input effects (effect added)
- Handler output effects (effect removed)
- Build handler info from declaration
- Handler with rest effects variable

**Parameterized Effects (~4 tests)**
- Handler for State(S) effect
- Type argument substitution in params
- Type argument in resume type
- Multiple type parameters

**Edge Cases (~8 tests)**
- Empty handler (no operations) for empty effect
- Handler for effect with many operations
- Nested effect handling
- Complex operation parameter types
- Operation with multiple parameters
- Effect row with variables
- Union effect rows

### CPS Transformation Tests (`test/Crisp/IR/CPSSpec.hs`)

The CPS transformation test suite covers continuation-passing style transformation (~49 tests):

**Simple Value Transformation (~4 tests)**
- Variable to CPS transformation
- Constructor to CPS transformation
- Unit to CPS transformation
- Variable name preservation

**Lambda Transformation (~3 tests)**
- Identity lambda transformation
- Lambda body is CPS-transformed
- Continuation parameter added

**Application Transformation (~4 tests)**
- Simple application transformation
- Function evaluated first
- Argument evaluated second
- Continuation passed to call

**Let Binding Transformation (~3 tests)**
- Let binding transformation
- Let value is CPS-transformed
- Body receives bound variable

**Effect Operation Transformation (~5 tests)**
- Perform to continuation capture
- Continuation captured at perform site
- Effect name recorded
- Operation name recorded
- Perform argument transformed

**Effect in Expression Context (~2 tests)**
- Effects sequenced before use
- Nested continuations correct

**Handler Transformation (~4 tests)**
- Handler to CPS handler
- Handler captures body continuation
- Operation clauses have continuation access
- Return clause CPS-transformed

**Resume Continuation (~3 tests)**
- Resume available in operation clause
- Resume invokes captured continuation
- Non-resuming handler can discard continuation

**Multi-shot Continuations (~2 tests)**
- Continuation can be called multiple times
- Multi-shot generates independent results

**Nested Handlers (~3 tests)**
- Nested handlers handled correctly
- Inner handler sees outer handler
- Handler order matters

**Evaluation Order Preservation (~2 tests)**
- Left-to-right evaluation preserved
- Let evaluation order preserved

**Type Annotation Handling (~2 tests)**
- Type annotations erased
- Underlying term preserved

**Lazy and Force Transformation (~2 tests)**
- Lazy transformed to lambda
- Force transformed to application

**Match Transformation (~3 tests)**
- Match scrutinee evaluated first
- Each branch CPS-transformed
- Continuation flows to branches

**CPS Well-formedness (~3 tests)**
- All effect calls have continuations
- No dangling continuations
- Continuation structure valid

**Edge Cases (~4 tests)**
- Empty handler handled
- Deeply nested effects handled
- Type abstraction erased
- Type application erased

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

### Type Unification Tests (`test/Crisp/Types/UnifySpec.hs`)

The type unification test suite covers the MGU algorithm (~64 tests):

**Basic Unification (~14 tests)**
- Identical types: simple types, type variables, TyProp, TyUniverse
- Wrapper types: Lazy, Ref, RefMut, Linear
- Type variables: binding to concrete types, binding different vars

**Function Type Unification (~7 tests)**
- Identical function types
- Variables in domain and codomain
- Domain/codomain mismatches (failure cases)
- Nested function types

**Type Constructor Unification (~7 tests)**
- Same name constructors with/without args
- Different names (failure case)
- Arity mismatch (failure case)
- Substitution propagation through args

**Occurs Check (~5 tests)**
- Direct circular reference: `a ~ a -> Int`
- Nested circular reference: `a ~ List a`
- Deeply nested circular reference
- Non-circular cases succeed

**Substitution Operations (~7 tests)**
- Empty substitution identity
- Single substitution replacement
- Application through types
- Composition ordering and associativity

**Effect Row Unification (~7 tests)**
- Empty rows, identical sets
- Effect variables with concrete effects
- Effect unions

**Symmetry (~4 tests)**
- `unify a b` produces equivalent result to `unify b a`
- Failure symmetry

**Complex Types (~7 tests)**
- Forall types, wrapper types
- Complex nested types: `Maybe (a -> List b)`

**Edge Cases (~6 tests)**
- Multiple occurrences of same variable
- Inconsistent variable binding (failure)
- Chain of variable bindings
- Universe level mismatch

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
| Effect System | 80%+ | ✅ Good coverage |
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
