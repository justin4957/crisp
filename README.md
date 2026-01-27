# Crisp

**A programming language with dependent types and algebraic effects, compiling to WebAssembly.**

> *The natural way to write correct programs is also the natural way to write auditable, semantically honest programs.*

## Overview

Crisp is designed for systems where **correctness, auditability, and authority boundaries matter**. It combines:

- **Dependent types** for encoding invariants in the type system
- **Algebraic effects** for composable, explicit effect handling
- **Capability-based security** through effect-as-import compilation
- **Auditable artifacts** via the Typed IR (TIR)

## Quick Example

```crisp
--- Safe head function that can only be called on non-empty vectors
fn head [T, n: Nat] (xs: Vec T (succ n)) -> T:
  match xs
    Cons x _ -> x

--- Effectful computation with explicit effect declaration
fn process (path: Path) -> Report ! FileSystem, Log:
  do
    let data = perform FileSystem.read path
    perform Log.info "Processing complete"
    analyze data
```

## Key Features

| Feature | Description |
|---------|-------------|
| **Dependent Types** | Types can depend on values, enabling precise specifications |
| **Algebraic Effects** | First-class effects with composable handlers |
| **Significant Whitespace** | Clean, Python-like syntax without parenthesis noise |
| **WebAssembly Target** | Sandboxed execution with capability-based security |
| **Typed IR** | Auditable compilation artifact preserving all type information |
| **Authority Annotations** | Track and verify capability provenance |

## Building

The Crisp compiler MVP is implemented in **Haskell**, with a planned transition to a self-hosting **Rust/Wasm** compiler.

### Using Stack (recommended)

```bash
# Build the compiler
stack build

# Run tests
stack test

# Run the compiler
stack exec crisp -- help
```

### Using Cabal

```bash
# Build the compiler
cabal build

# Run tests
cabal test

# Run the compiler
cabal run crisp -- help
```

## Usage

```bash
# Compile a Crisp source file to WebAssembly
crisp compile main.crisp -o main.wasm

# Type-check without compiling
crisp check src/lib.crisp

# Format source code
crisp format src/**/*.crisp

# Emit Typed IR artifact
crisp compile main.crisp --emit-tir

# Start the REPL
crisp repl
```

## REPL

The interactive REPL lets you explore Crisp's features:

```bash
# Start the REPL
crisp repl
```

### Basic Expressions

```
crisp> 1 + 2
3

crisp> 6 * 7
42

crisp> 1 < 2
true
```

### Defining Values and Functions

```
crisp> let x = 42
x : Int = 42

crisp> let greeting = "Hello, Crisp!"
greeting : String = "Hello, Crisp!"

crisp> fn double(n: Int) -> Int = n * 2
double : (Int) -> Int

crisp> double(21)
42

crisp> fn inc(x: Int) -> Int = x + 1
inc : (Int) -> Int

crisp> inc(inc(5))
7
```

### Type Definitions

```
crisp> type Option(A) = Some(A) | None
type Option : Type -> Type

crisp> type Pair(A, B) = MkPair(A, B)
type Pair : Type -> Type -> Type
```

### REPL Commands

```
crisp> :help              -- Show available commands
crisp> :type 1 + 2        -- Show type of expression (Int)
crisp> :kind List         -- Show kind of type (Type -> Type)
crisp> :load file.crisp   -- Load definitions from file
crisp> :reset             -- Clear all definitions
crisp> :quit              -- Exit the REPL
```

### Multi-line Input

Expressions with unclosed parentheses continue on the next line:

```
crisp> fn add(
...      x: Int,
...      y: Int
...    ) -> Int = x + y
add : (Int, Int) -> Int
```

## Documentation

- [Language Specification](docs/SPECIFICATION.md) - Complete language reference
- [Implementation Plan](docs/IMPLEMENTATION_PLAN.md) - Compiler architecture and roadmap
- [Testing Guide](docs/TESTING.md) - How to run and write tests

## Examples

See the [examples/](examples/) directory:

- [`hello.crisp`](examples/hello.crisp) - Hello World with effects
- [`safe_list.crisp`](examples/safe_list.crisp) - Length-indexed vectors
- [`state_handler.crisp`](examples/state_handler.crisp) - Algebraic effect handlers
- [`authorization.crisp`](examples/authorization.crisp) - Proof-carrying authorization

## Project Structure

```
crisp/
├── app/                 # CLI application
│   ├── Main.hs
│   └── Crisp/CLI/
├── src/Crisp/           # Compiler library
│   ├── Syntax/          # AST and source locations
│   ├── Lexer/           # Lexer with layout tracking
│   ├── Parser/          # Megaparsec-based parser
│   ├── Core/            # Core calculus and desugaring
│   ├── Types/           # Bidirectional type checker
│   ├── Effects/         # Effect row operations
│   ├── IR/              # Intermediate representations
│   └── Codegen/         # WebAssembly generation
├── test/                # Test suite (HSpec)
├── examples/            # Example programs
└── docs/                # Documentation
```

## Implementation Roadmap

### Current: Haskell MVP
The MVP compiler is written in Haskell for rapid development and correctness focus.

### Future: Rust/Wasm Self-Hosting
The compiler will be rewritten in Rust with Wasm as the execution target, enabling:
- Self-hosting (Crisp compiler written in Crisp)
- Portable toolchain via Wasm
- Integration with Rust/Wasm ecosystem

## Design Principles

1. **Explicit over implicit** - No hidden effects, no implicit conversions
2. **Local over global** - No global typeclass coherence, scoped capabilities
3. **Strict over lazy** - Strict evaluation by default, explicit laziness
4. **Auditable over clever** - Typed IR as first-class artifact
5. **Capability over ambient** - Effects compile to Wasm imports

## Status

**Current:** Haskell MVP scaffold with lexer, parser, type checker foundations, and project structure.

See the [Implementation Plan](docs/IMPLEMENTATION_PLAN.md) for detailed progress.

## Contributing

Contributions welcome! Please read the implementation plan to understand the current state and pick up tasks.

## License

Apache-2.0
