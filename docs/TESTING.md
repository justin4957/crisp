# Testing Guide

This document describes testing strategies for the Crisp compiler.

## Running Tests

```bash
# Run all tests
gleam test

# Run tests with verbose output
gleam test -- --verbose
```

## Test Structure

```
test/
├── crisp_compiler_test.gleam    # Main test entry point
└── crisp/
    ├── lexer/
    │   └── lexer_test.gleam     # Lexer unit tests
    ├── parser/
    │   └── parser_test.gleam    # Parser unit tests (TODO)
    ├── types/
    │   └── checker_test.gleam   # Type checker tests (TODO)
    └── effects/
        └── effect_test.gleam    # Effect system tests (TODO)
```

## Test Categories

### Unit Tests

Each module has corresponding unit tests that verify individual functions.

**Lexer Tests** (`test/crisp/lexer/lexer_test.gleam`):
- Token recognition for keywords, operators, identifiers
- Literal parsing (integers, floats, strings, characters)
- Indentation tracking and layout token generation
- Comment handling (line and block comments)
- Error token generation for invalid input

**Parser Tests** (TODO):
- Expression parsing with correct precedence
- Statement and definition parsing
- Pattern parsing
- Type annotation parsing
- Error recovery

**Type Checker Tests** (TODO):
- Variable lookup
- Function application typing
- Type inference for let-bindings
- Effect tracking
- Linearity checking

### Integration Tests

End-to-end tests using complete `.crisp` source files:

```bash
# Example integration test structure
test/
└── integration/
    ├── hello_world/
    │   ├── input.crisp
    │   └── expected.json       # Expected TIR output
    ├── effects/
    │   ├── state_handler.crisp
    │   └── expected.json
    └── dependent_types/
        ├── safe_list.crisp
        └── expected.json
```

### Property-Based Tests

For algebraic properties that should always hold:

```gleam
// Effect row algebra properties
pub fn effect_union_commutative_test() -> Nil {
  // forall a b: union(a, b) == union(b, a)
}

pub fn effect_union_associative_test() -> Nil {
  // forall a b c: union(union(a, b), c) == union(a, union(b, c))
}

pub fn effect_empty_identity_test() -> Nil {
  // forall a: union(a, empty) == a
}
```

## Writing Tests

### Test Naming Convention

- Test functions must end with `_test`
- Use descriptive names: `fn identifier_with_apostrophe_test()`
- Group related tests in the same file

### Test Assertions

Use `gleeunit/should` for assertions:

```gleam
import gleeunit/should

pub fn some_test() -> Nil {
  let result = compute_something()

  result
  |> should.equal(expected_value)

  result
  |> should.be_true

  result
  |> should.be_ok

  result
  |> should.be_error
}
```

### Testing Error Cases

```gleam
pub fn invalid_input_test() -> Nil {
  let result = parse("invalid syntax")

  result
  |> should.be_error

  case result {
    Error(ParseError(message, span)) ->
      message
      |> should.equal("Expected expression")
    _ ->
      False
      |> should.be_true
  }
}
```

## Test Fixtures

Place test fixture files in `test/fixtures/`:

```
test/fixtures/
├── lexer/
│   ├── valid_tokens.crisp
│   └── invalid_tokens.crisp
├── parser/
│   ├── expressions.crisp
│   └── definitions.crisp
└── programs/
    ├── hello.crisp
    └── state.crisp
```

## Continuous Integration

Tests run automatically on:
- Every push to any branch
- Every pull request

CI configuration checks:
1. `gleam build` - Compilation succeeds
2. `gleam test` - All tests pass
3. `gleam format --check` - Code is properly formatted

## Adding New Tests

1. Create test file in appropriate directory
2. Import `gleeunit/should` for assertions
3. Write test functions ending in `_test`
4. Run `gleam test` to verify
5. Commit with descriptive message

## Test Coverage

Track test coverage for critical modules:

- [ ] Lexer token recognition
- [ ] Lexer layout handling
- [ ] Parser expression parsing
- [ ] Parser definition parsing
- [ ] Type checker inference
- [ ] Effect row operations
- [ ] TIR serialization
