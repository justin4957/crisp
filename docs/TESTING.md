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
    ├── Doc/
    │   └── GenerateSpec.hs           # Doc generator tests
    ├── Formatter/
    │   └── FormatSpec.hs             # Formatter tests
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
- Let expressions: simple, with type annotation, nested, with patterns, layout-based round-trip formatting
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

**Doc Comment Tests (~11 tests)**
- Multi-line doc comment with pipe on continuation lines parsed correctly
- Multi-line doc comment with empty pipe line parsed correctly
- Two functions with doc comments parse correctly (issue #138)
- Two types with doc comments parse correctly (issue #138)
- Documented function followed by documented type parses correctly (issue #138)
- Subtraction still works after doc comment disambiguation fix (issue #138)
- Module-level doc comment before module keyword parsed correctly (issue #140)
- Multi-line module doc comment parsed correctly (issue #140)
- Module without doc comment has Nothing (issue #140)
- Preserves indentation in doc comment lines (issue #147)
- Preserves multi-level indentation in doc comments (issue #147)

**Known Parser Limitations (documented via pending tests)**
- Match arms don't parse correctly due to greedy pExpr
- Do statements with binds/let don't work due to greedy expression parsing
- With handler expression has same issue
- Type constructors require layout (colon interpreted as kind annotation)
- Multi-param lambdas don't parse due to greedy type parsing

### Formatter Tests (`test/Crisp/Formatter/FormatSpec.hs`)

The formatter test suite covers source code formatting (~49 tests):

**Expression Formatting (~15 tests)**
- Literals: integer, float, string, unit
- Variables and constructors
- Application: simple, multi-arg
- Let expressions: layout-based style, with type annotation, nested let chains
- If-then-else expressions
- Lambda expressions
- Pipeline operator
- Lazy and force expressions
- Perform expressions (with and without arguments)
- Field access: simple and chained

**Module Formatting (~14 tests)**
- Minimal module, with function/type/effect definitions
- Provides block: with types, typed functions, followed by definitions
- Requires block
- Type definitions: named fields, positional fields, mixed constructors
- Type alias with where refinement, field access, match, if expressions
- External function definitions

**Idempotence (~3 tests)**
- Module formatting idempotent (format twice = same result)
- Expression formatting idempotent (layout-based let)
- Module with type formatting idempotent

**Definition Formatting (~5 tests)**
- Simple type, type with parameters
- Simple function, function with effects
- Effect definition

**Doc Comment Preservation (~11 tests)**
- Preserves doc comments on function and type definitions
- Preserves doc comments on multiple definitions
- Doc comment appears before definition in output
- Multi-line doc comment formatted with `--- |` prefix on every line (issue #139)
- Multi-line doc comment formatting is idempotent (issue #139)
- Single-line doc comment formatted unchanged (issue #139)
- Module-level doc comment formatted before module keyword (issue #140)
- Module-level doc comment formatting is idempotent (issue #140)
- Preserves indentation in doc comment continuation lines (issue #147)
- Indented doc comment formatting is idempotent (issue #147)

**Match Arm Body Formatting (~4 tests)**
- Formats function application in match arm body with parentheses (issue #152)
- Match arm body with application is idempotent (issue #152)
- Formats nested application in match arm body with parentheses (issue #152)
- Formats multiple args in match arm body application (issue #152)

**Other (~4 tests)**
- Trailing newline option (enabled/disabled)
- Error handling for invalid syntax
- prettyModule function output
- prettyPattern function output

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
| Code Generation | 70%+ | ✅ Good coverage |

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

### Dependent Type Tests (`test/Crisp/Types/DependentSpec.hs`)

The dependent type test suite covers Pi types (dependent function types) and Sigma types (dependent pairs) (~45 tests):

**Pi Type Formation (~4 tests)**
- Simple Pi type creation
- Pi type component extraction (param name, param type, return type)
- Non-dependent function as special case of Pi (underscore param)
- Nested Pi types

**Pi Type Checking (~3 tests)**
- Well-formedness check for valid Pi types
- Rejection of Pi with unbound variable in return type
- Domain type kind checking

**Dependent Application (~3 tests)**
- Type substitution in return type on application
- Complex expression substitution
- Nested substitution handling

**Type-Level Evaluation (~5 tests)**
- Nat addition evaluation: `2 + 3 = 5`
- Nested addition evaluation
- Variable preservation in partial evaluation
- Type constructor application evaluation
- Zero addition identity

**Purity Enforcement (~5 tests)**
- Rejection of effectful expressions in types
- Acceptance of pure type-level expressions
- Type variables as pure
- Type constructors as pure
- Rejection of nested effectful expressions

**Sigma Types (Dependent Pairs) (~4 tests)**
- Sigma type creation
- Sigma type component extraction
- Well-formedness checking
- Rejection of Sigma with unbound variable

**Sigma Type Operations (~2 tests)**
- First component type projection
- Second component type with substitution

**Pi Type Unification (~4 tests)**
- Identical Pi type unification
- Alpha-equivalent body unification
- Domain type mismatch failure
- Return type mismatch failure

**Sigma Type Unification (~2 tests)**
- Identical Sigma type unification
- First type mismatch failure

**Implicit Pi (forall) Desugaring (~2 tests)**
- Forall desugars to Pi with erased domain
- Kind annotation preservation

**Dependent Pattern Matching (~2 tests)**
- Type refinement in pattern branch (Vec length decrement)
- Wildcard pattern handling (no refinement)

**Type Normalization (~3 tests)**
- Weak head normal form normalization
- Type application normalization
- Full normalization of subterms

**Type Equality with Evaluation (~3 tests)**
- Equivalence of types normalizing to same form
- Nested equal type equivalence
- Distinction of different types

**Edge Cases (~4 tests)**
- Empty Sigma type
- Pi type with universe domain
- Deeply nested dependent types
- Type-level computation termination

### GADT Typing Tests (`test/Crisp/Types/GADTSpec.hs`)

The GADT typing test suite covers GADT support with index refinement (~40 tests):

**GADT Declaration (~8 tests)**
- Expr GADT validation with refined return types
- GADT return type recording (IntLit, BoolLit)
- Vec GADT validation with index types
- VNil/VCons return type recording
- Type equality witness validation (Refl)
- Invalid GADT return type rejection
- Default return type acceptance (non-GADT)

**GADT Refinement (~7 tests)**
- Type variable refinement to Int in IntLit branch
- Type variable refinement to Bool in BoolLit branch
- Type variable preservation in If branch
- Vec length refinement in VCons branch
- Concrete scrutinee type refinement
- Wildcard pattern handling (no refinement)
- Variable pattern handling

**Impossible Branch Detection (~7 tests)**
- Detection of impossible BoolLit branch for Expr(Int)
- Detection of impossible IntLit branch for Expr(Bool)
- Allowing IntLit branch for Expr(Int)
- Allowing any branch for polymorphic Expr(A)
- Detection of impossible VCons branch for Vec(A, 0)
- Allowing VNil branch for Vec(A, 0)
- Detection of impossible VNil branch for Vec(A, S(n))

**Existential Types (~3 tests)**
- Existential type in GADT constructor handling
- Fresh existential type variables in pattern match
- Existential constraint recording

**Nested GADT Matching (~3 tests)**
- Refinement accumulation from nested patterns
- Constraint propagation through nested patterns
- Nested Vec pattern handling

**Type Equality Witness (~2 tests)**
- Refl constructor type equation (A = B)
- Equality proof for type casting

**GADT Unification (~4 tests)**
- Constructor return type unification with scrutinee
- Incompatible index type failure
- Vec length index unification
- Type-level arithmetic in unification

**Edge Cases (~6 tests)**
- Empty constructor list
- Single-constructor GADT
- Constructor with no params but refined return
- Deeply nested type indices
- Multiple type parameters in GADT
- Binding order preservation

### Prop Universe Tests (`test/Crisp/Types/PropSpec.hs`)

The Prop universe test suite covers computationally irrelevant proofs and proof erasure (~55 tests):

**Prop Kind Tests (~7 tests)**
- TyProp has kind Prop
- Distinguishes Prop from Type kinds
- Prop is not equal to Type (kind incompatibility)
- Prop equals Prop (reflexivity)
- Recognizes Prop type
- Recognizes Prop kind
- Equality type (Eq) has Prop kind

**Proof Irrelevance Tests (~7 tests)**
- Identifies proof-irrelevant function argument
- Filters proof arguments from parameter list
- Computes proof positions in parameter list
- Keeps all arguments when no proofs
- Returns empty when all proofs
- Identifies constructor with proof arguments
- Constructor without proofs has no proof arguments

**Proof Erasure Tests (~8 tests)**
- Erases proof variable to unit
- Preserves non-proof variable
- Erases proof in let binding (with de Bruijn index adjustment)
- Preserves non-proof let binding
- Erases proof arguments in constructor
- Erases proof arguments in function application
- Erases proof in lambda parameter
- Fully erases term with all proof bindings

**Relevance Boundary Tests (~6 tests)**
- Rejects Type depending on Prop value
- Allows Prop depending on Type value
- Detects proof escaping to computation
- Allows proofs in type refinement
- Rejects proof inspection in match body
- Allows wildcard match on proof (no inspection)

**Proof Type Tests (~4 tests)**
- Eq is a proof type
- Recognizes standard proof type names (Eq, Lt, Le, Gt, Ge, And, Or, Not, True, False)
- Non-proof types are not proof type names
- Refl constructor has Prop result

**Prop in Dependent Types Tests (~6 tests)**
- Identifies proof-irrelevant Pi parameter
- Identifies relevant Pi parameter
- Extracts proof parameters from nested Pi
- Sigma with Prop second component
- Sigma without Prop components
- Forall with Prop body

**Edge Cases (~8 tests)**
- Handles nested proof types
- Handles empty parameter list
- Handles single proof parameter
- Handles single non-proof parameter
- Preserves type structure after erasure
- Handles proof in type constructor arguments
- Handles deeply nested proofs
- Erases proof from match case

### LLIR and Monomorphization Tests (`test/Crisp/IR/LLIRSpec.hs`)

The LLIR test suite covers Low-Level IR representation and monomorphization (~75 tests):

**LLIR Type Tests (~10 tests)**
- Primitive type sizes (I32, I64, F32, F64)
- Pointer types (wasm32)
- Struct types and alignment
- Tagged union layouts
- Function type representation
- Crisp to LLIR type conversion (Int, Bool, Unit, Float)

**LLIR Instruction Tests (~15 tests)**
- Constant values
- Local variable access (get/set)
- Memory operations (load/store)
- Function calls (direct and indirect)
- Control flow (if, block, loop, br, br_if)
- Binary operations (add, sub, mul, etc.)
- Comparison operations (eq, ne, lt, gt, etc.)
- Return instruction

**LLIR Function Tests (~4 tests)**
- Monomorphic function representation
- Local variable tracking
- Stack frame size computation
- Export function detection

**LLIR Module Tests (~5 tests)**
- Function collection
- Type definitions
- Memory layout management
- Import tracking
- Export tracking

**Monomorphization Tests (~6 tests)**
- Monomorphic function preservation
- Identity function specialization for Int
- Identity function specialization for Bool
- Multi-parameter specialization (const)
- Nested polymorphic calls
- Type substitution in function body

**Specialized Naming Tests (~6 tests)**
- Simple specialized names (`id$Int`)
- Multi-param names (`const$Int$Bool`)
- Parameterized types (`head$List_Int`)
- Nested parameterized types
- Monomorphic name preservation
- Special character escaping

**Data Layout Tests (~8 tests)**
- Int layout (4 bytes, 4 alignment)
- Bool layout (i32 representation)
- Struct layouts with field offsets
- Tagged union layouts with tag size
- Alignment padding
- Recursive type layouts (pointers)
- Empty struct layout

**Closure Representation Tests (~6 tests)**
- Closure with no captures
- Closure with captured variables
- Closure struct size computation
- Closure allocation code generation
- Closure invocation code generation
- Nested closures

**Dead Code Elimination Tests (~5 tests)**
- Exported function preservation
- Called function preservation
- Transitive call preservation
- Cyclic call graph handling
- Unused specialization elimination

**Recursive Polymorphism Tests (~4 tests)**
- Simple recursive function handling
- Polymorphic recursion cycle detection
- Mutually recursive functions
- Instantiation depth limiting

**Edge Cases (~6 tests)**
- Empty module handling
- Functions with no parameters
- Higher-kinded type parameters
- Function order preservation
- Unit return types
- String type handling (pointers)

### WebAssembly Binary Encoding Tests (`test/Crisp/Codegen/WasmBinarySpec.hs`)

The WebAssembly binary encoding test suite covers Wasm binary format generation (~80 tests):

**Module Header Tests (~3 tests)**
- Magic number verification (`\0asm`)
- Version number (1)
- Header byte count (8 bytes)

**LEB128 Encoding Tests (~16 tests)**
- ULEB128: 0, small integers (<128), 128, 255, 256, 624485 (spec example), large integers
- SLEB128: 0, small positive, 64, -1, -64, -65, -123456 (spec example), large negative

**Value Type Encoding Tests (~4 tests)**
- i32 (0x7F), i64 (0x7E), f32 (0x7D), f64 (0x7C)

**Type Section Tests (~6 tests)**
- Section ID (1)
- Empty type section
- Single function type `() -> i32`
- Function type `(i32, i32) -> i32`
- Function type `() -> ()`
- Multiple function types

**Import Section Tests (~5 tests)**
- Section ID (2)
- Empty import section
- Function import encoding
- Module and field name encoding
- Memory import encoding

**Function Section Tests (~4 tests)**
- Section ID (3)
- Empty function section
- Single function with type index 0
- Multiple functions

**Memory Section Tests (~4 tests)**
- Section ID (5)
- Memory with only minimum pages
- Memory with minimum and maximum
- Larger memory sizes

**Export Section Tests (~5 tests)**
- Section ID (7)
- Empty export section
- Function export encoding
- Memory export encoding
- Multiple exports

**Code Section Tests (~5 tests)**
- Section ID (10)
- Empty code section
- Function with no locals and return
- Function with locals
- Add function body

**Instruction Encoding Tests (~25 tests)**
- Constants: i32.const, i64.const, f64.const
- Local operations: local.get, local.set
- Global operations: global.get, global.set
- Control flow: return, unreachable, call, call_indirect, br, br_if
- Arithmetic: i32.add/sub/mul/eq/lt, i64.add/sub, f64.add/sub/mul
- Misc: drop
- Structured control: block, loop, if-then-else

**Complete Module Encoding Tests (~5 tests)**
- Empty module encoding
- Minimal module with one function
- Module with memory
- Module with exports
- Section ordering verification

**Edge Cases (~7 tests)**
- Large LEB128 values
- Maximum i32 value
- Minimum i32 value
- Deeply nested blocks
- Function with many locals
- Empty function body
- Long export names

### LLIR to Wasm Compilation Tests (`test/Crisp/Codegen/CompileSpec.hs`)

The compilation test suite covers LLIR to WebAssembly code generation (~93 tests):

**Arithmetic Operation Tests (~16 tests)**
- Constants: i32.const, i64.const, f64.const
- Arithmetic: add, sub, mul, div, rem
- Bitwise: and, or, xor, shl, shr_s, shr_u
- Nested arithmetic: (x + 1), ((x + y) * 2)

**Comparison Operation Tests (~11 tests)**
- Signed comparisons: eq, ne, lt, le, gt, ge
- Unsigned comparisons: lt_u, le_u, gt_u, ge_u
- Comparisons with constants: (x == 0)

**Local Variable Tests (~6 tests)**
- local.get, local.set, local.tee
- global.get, global.set with context lookup

**Control Flow Tests (~12 tests)**
- return, unreachable, nop
- if-then-else, nested if
- block with label, loop with label
- br, br_if, br_table
- select, drop

**Function Call Tests (~4 tests)**
- Direct calls with function index lookup
- Calls with no arguments
- Multiple calls in sequence
- Indirect calls (call_indirect)

**Data Type Compilation Tests (~8 tests)**
- Nullary constructor (None): allocate tag only
- Constructor with payload (Some): allocate tag + payload
- Pair constructor: allocate tag + two fields
- List Cons/Nil constructors
- Field access at specific offsets
- Tag load for pattern matching

**Pattern Match Compilation Tests (~6 tests)**
- Match on boolean (two branches)
- Match on Option type
- Match with payload extraction
- Nested match compilation
- Match with three or more branches (br_table)
- Wildcard/default pattern

**Closure Compilation Tests (~5 tests)**
- Closure allocation with no captures
- Closure allocation with captured variables
- Closure call (load func ptr, call_indirect)
- Capture store (store into closure)
- Capture load (load from closure)

**Memory Operation Tests (~10 tests)**
- i32/i64/f64 load with offset
- i32/i64/f64 store with offset
- memory.grow, memory.size

**Module Compilation Tests (~7 tests)**
- Empty module
- Module with one function
- Function with parameters
- Function with locals
- Module with exports
- Module with memory
- Module with imports

**Edge Cases (~8 tests)**
- Empty function body
- Large/negative constants
- Deeply nested blocks
- Many locals (100)
- Many parameters (10)
- Null constant
- Type conversions

### Manifest Generation Tests (`test/Crisp/Manifest/ManifestSpec.hs`)

The manifest generation test suite covers compilation artifact manifests (~67 tests):

**Content Hashing Tests (~11 tests)**
- SHA-256 hash of empty string, "hello", "hello world"
- 64-character hex string output
- Lowercase hex characters only
- ByteString hashing (wasm magic bytes)
- Deterministic hashing (same input = same output)
- Hash formatting with sha256: prefix
- Hash parsing and validation

**Manifest Structure Tests (~10 tests)**
- Version field (1.0)
- Module name
- Wasm hash and TIR hash fields
- Compiler info (name, version)
- Capabilities list (empty by default)
- Authorities map (empty by default)
- Dependencies list (empty by default)
- Build time (when generated with time)

**Capability Tests (~7 tests)**
- Single and multiple capabilities
- Capability order preservation
- Duplicate capabilities
- Extraction from effects (effect:operation format)
- Effect name normalization (lowercase)
- Effects with no operations

**Authority Mapping Tests (~5 tests)**
- Single effect to authority mapping
- Multiple effects to authorities
- Authority creation from effect declaration
- Building authority map from effect list
- Missing authority handling

**Dependency Tests (~6 tests)**
- Single and multiple dependencies
- Dependency order preservation
- Dependency name and hash fields
- Creating dependency with content hash

**JSON Serialization Tests (~10 tests)**
- Encode manifest to JSON
- Decode manifest from JSON
- All fields present in JSON output
- Valid JSON output

**Round-Trip Tests (~6 tests)**
- Empty manifest round-trip
- Manifest with hashes round-trip
- Manifest with capabilities round-trip
- Manifest with authorities round-trip
- Manifest with dependencies round-trip
- Full manifest round-trip

**Build Metadata Tests (~4 tests)**
- Current time as build time
- ISO 8601 time format
- Compiler name and version

**Edge Cases (~8 tests)**
- Empty module name
- Special characters in module name
- Unicode in module name
- Very long capability list (100 items)
- Empty hash handling
- Hash with special characters
- Deterministic manifest generation
- Null build time in JSON

### Module System Tests (`test/Crisp/Module/ModuleSpec.hs`)

The module system test suite covers import parsing, resolution, visibility, and cycle detection (~76 tests):

**Import Parsing Tests (~17 tests)**
- Module path parsing: simple, dotted, long paths
- Module path rejection: empty, trailing dot, leading dot, lowercase
- Import declaration parsing: unqualified, qualified with alias, selective, hiding
- Selective/hiding with single and multiple items
- Qualified import with dotted paths
- Rejection of invalid imports (qualified without alias, import without module)

**Import Form Tests (~9 tests)**
- Unqualified form description and behavior
- Qualified form with alias and qualifier requirement
- Selective form importing only specified names
- Hiding form importing all except hidden names
- Empty selection handling
- Multiple hidden names

**Name Resolution Tests (~9 tests)**
- Unqualified resolution: imported names, non-imported names, hidden names
- Qualified resolution: correct qualifier, wrong qualifier, non-existent name
- Ambiguity detection: ambiguous imports, unambiguous names

**Visibility Tests (~5 tests)**
- Export list visibility: declared names public, others private
- ExportAll visibility: everything public
- Empty export list: everything private
- Single and multiple exports
- isExported predicate

**Cycle Detection Tests (~7 tests)**
- Direct circular import detection (A -> B -> A)
- Acyclic imports (A -> B -> C)
- Transitive cycle detection (A -> B -> C -> A)
- Self-import detection (A -> A)
- Diamond dependencies (no cycle)
- Empty dependency graph

**Interface Tests (~9 tests)**
- Interface generation from module info
- Export filtering (only exports exported)
- Type and function information inclusion
- Interface serialization to JSON
- Interface deserialization from JSON
- Round-trip serialization

**Module Path Tests (~9 tests)**
- Path to file path conversion (Core/Prelude.crisp)
- Path to interface file path conversion (Core/Prelude.crispi)
- File path to module path conversion
- Single segment path handling
- Module path text rendering (Core.Prelude)
- Path equality and ordering comparison

**Qualified Name Tests (~6 tests)**
- Simple qualified name parsing (M.foo)
- Multi-segment qualifier parsing (Core.Prelude.map)
- Unqualified name rejection
- Trailing dot rejection
- Qualified name construction

**Re-export Tests (~4 tests)**
- Module re-export parsing (export module M)
- Selective re-export parsing (export M (foo, bar))
- Re-exported names resolution (all, selective)

**Edge Cases (~6 tests)**
- Unicode rejection in module paths (ASCII only)
- Underscore and prime handling in names
- Empty module (no exports, no imports)
- Case sensitivity in names and paths

### REPL Tests (`test/Crisp/REPL/ReplSpec.hs`)

The REPL test suite covers input parsing, command handling, expression evaluation, and state persistence (~75 tests):

**Input Parsing Tests (~12 tests)**
- Expression input: simple numbers, arithmetic, function application, complex expressions
- Definition input: let bindings, function definitions, type definitions
- Whitespace handling: leading/trailing whitespace, empty input, whitespace-only

**Command Parsing Tests (~20 tests)**
- Help command: `:help`, `:h`, `:?`
- Quit command: `:quit`, `:q`, `:exit`
- Type command: `:type expr`, `:t expr`
- Kind command: `:kind Type`, `:k Type`
- Load command: `:load file`, `:l file`
- Reload command: `:reload`, `:r`
- Reset command: `:reset`
- Browse command: `:browse Module`, `:b Module`
- Unknown command handling

**Expression Evaluation Tests (~11 tests)**
- Literals: integer, boolean, string, unit
- Arithmetic: addition, subtraction, multiplication
- Comparisons: equality, inequality, less than

**Definition Tests (~6 tests)**
- Let bindings: adding to state, using bound variables
- Function definitions: adding to state, calling defined functions
- Type definitions: adding to state

**Command Execution Tests (~8 tests)**
- `:type` command: showing types of integers, expressions, functions
- `:kind` command: showing kinds of type constructors, simple types
- `:help` command: showing help text
- `:reset` command: clearing definitions

**State Persistence Tests (~4 tests)**
- Multiple definitions persisted
- Redefinition allowed
- Definition referencing
- Functions calling other functions

**Error Handling Tests (~5 tests)**
- Parse errors: reporting, error location
- Type errors: type mismatch, undefined variable
- Recovery: continuing after errors

**Multi-line Input Tests (~6 tests)**
- Continuation marker recognition (`\`)
- Complete input recognition
- Unclosed parentheses, braces, brackets
- Combining continuation lines

**Tab Completion Tests (~5 tests)**
- Command completion: commands starting with `:`
- Identifier completion: built-in types, user-defined names

**Edge Cases (~8 tests)**
- Comment-only input, trailing comments
- Unicode in strings
- Large inputs
- Empty definitions
- History tracking

### Linear Type Tests (`test/Crisp/Types/LinearSpec.hs`)

The linear type test suite covers usage counting and linearity checking (~82 tests):

**Usage Type Tests (~12 tests)**
- Usage values: Zero, One, Many
- Usage comparison and equality
- Usage predicates: isZero, isOne, isMany

**Usage Combination Tests (~12 tests)**
- Sequential usage (addUsage): Zero+Zero=Zero, One+One=Many, etc.
- Alternative usage (altUsage): for conditional branches
- Usage scaling: Zero*n=Zero, One*1=One, One*2=Many

**Linear Environment Tests (~12 tests)**
- Empty environment creation
- Environment extension with bindings
- Variable shadowing
- Usage tracking: marking variables as used
- Linearity modes: Unique, Borrowed, Shared, Unrestricted

**Single Use Tests (~4 tests)**
- Accepts single use of unique variable
- Accepts unique value passed to function
- Accepts unique value in constructor
- Accepts unique value returned directly

**Multiple Use Tests (Errors) (~3 tests)**
- Rejects double use of unique value
- Rejects triple use of unique value
- Rejects use in both arguments

**Unused Value Tests (Errors) (~3 tests)**
- Rejects unused unique value
- Rejects unique value shadowed without use
- Rejects unique value in dead code

**Borrow Tests (~4 tests)**
- Allows multiple uses of borrowed reference
- Borrow semantics with borrowed mode
- Allows unused borrowed reference
- Allows borrowed in multiple branches

**Shared Reference Tests (~3 tests)**
- Allows multiple readers of shared reference
- Allows unused shared reference
- Shared reference can be passed multiple times

**Conditional Tests (~5 tests)**
- Allows use in both branches
- Rejects use only in then branch
- Rejects use only in else branch
- Allows use in all match branches
- Rejects use in only some match branches

**Pattern Matching Tests (~3 tests)**
- Transfers ownership through pattern match
- Binds linear values in pattern
- Rejects unused pattern binding

**Let Binding Tests (~4 tests)**
- Accepts linear value used in body
- Rejects unused linear let binding
- Allows unrestricted let bindings to be unused
- Tracks usage through nested lets

**Function Call Tests (~3 tests)**
- Consumes unique argument
- Allows borrowed argument without consuming
- Tracks usage through multiple function calls

**Closure Tests (~2 tests)**
- Captures unique value for single use
- Rejects closure that captures unique but doesn't use

**Nested Scope Tests (~2 tests)**
- Handles unique value in nested function
- Tracks usage across nested scopes

**Edge Cases (~10 tests)**
- Empty environment handling
- Literal with no bindings
- Variable shadowing with unique values
- Type abstraction handling
- Type application handling
- Effect operations (perform)
- Handler handling
- Lazy and force handling

### Doc Generator Tests (`test/Crisp/Doc/GenerateSpec.hs`)

The doc generator test suite covers documentation extraction and rendering (~71 tests):

**Doc Comment Parsing (~8 tests)**
- Simple doc comment extraction
- Summary parsing from doc comment
- Multi-line doc comment parsing
- Examples section extraction
- See also section extraction
- Since version extraction
- Empty doc comment handling
- Doc comment without pipe prefix

**Module Documentation Extraction (~9 tests)**
- Module name extraction
- Function documentation extraction
- Function signature extraction
- Type documentation extraction
- Effect documentation extraction
- External function documentation extraction
- Type alias documentation extraction
- Multiple items handling
- Invalid code error handling

**Doc Comment Association (~7 tests)**
- Does not steal first definition's doc comment as module description
- Does not steal first type's doc comment as module description
- Associates doc comment with adjacent function definition
- Associates doc comment with adjacent type definition
- Associates doc comment with adjacent effect definition
- Does not associate doc comment with non-adjacent definition
- Module has no description when doc comment belongs to first definition

**Doc Comment Display (~7 tests)**
- Function doc comment displayed in rendered markdown
- Type doc comment displayed in rendered markdown
- Effect doc comment displayed in rendered markdown
- External function doc comment displayed in rendered markdown
- Type alias doc comment displayed in rendered markdown
- Doc comments displayed in HTML output
- No doc comment text displayed when no doc comment exists

**Doc Comment Pipe Stripping (~6 tests)**
- Strips pipe prefix from multi-line doc comment continuation lines
- Extracts examples section from pipe-style continuation lines
- Handles mixed pipe and no-pipe continuation lines
- Handles lone pipe line as blank separator
- Does not produce literal pipe characters in rendered markdown
- Extracts see also section from pipe-style continuation lines

**Type/Effect Examples (~8 tests)**
- Extracts examples section for type doc comment
- Extracts examples section for effect doc comment
- Renders type examples in markdown output
- Renders effect examples in markdown output
- Extracts see-also section for type doc comment
- Extracts see-also section for effect doc comment
- Type without examples has empty list
- Effect without examples has empty list

**Module Doc Comment in Generator (~5 tests)**
- Uses module-level doc comment as module summary
- Renders module-level doc comment in markdown output
- Uses multi-line module doc comment for summary and description
- Does not use first definition doc as module description
- Module without doc comment has no summary

**Markdown Rendering (~9 tests)**
- Module header rendering
- Module summary rendering
- Function documentation rendering
- Function examples rendering
- Type documentation rendering
- Type with parameters rendering
- Effect documentation rendering
- Table of contents rendering
- Source file reference rendering

**HTML Rendering (~6 tests)**
- Valid HTML document rendering
- Module name in title
- CSS styles inclusion
- HTML special character escaping
- Navigation links rendering
- Footer with source file rendering

**DocFormat (~3 tests)**
- Markdown format renders markdown
- HTML format renders HTML
- DocFormat equality
