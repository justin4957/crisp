# Crisp Implementation Plan

**Version:** 0.1.0
**Status:** Initial Scaffold
**Implementation Language:** Haskell (MVP) → Rust/Wasm (Future)

---

## Overview

This document outlines the implementation plan for the Crisp compiler, a programming language with dependent types and algebraic effects targeting WebAssembly.

**Two-Phase Strategy:**
1. **Haskell MVP** - Rapid development with strong type system support for correctness
2. **Rust/Wasm Self-Hosting** - Future rewrite for portability and ecosystem integration

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                              CLI Layer                                   │
│                         app/Crisp/CLI/Commands.hs                        │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                            Frontend                                      │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐         │
│  │     Lexer       │→ │     Parser      │→ │    Desugarer    │         │
│  │ Crisp.Lexer     │  │ Crisp.Parser    │  │ Crisp.Core.Desugar│       │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘         │
│          ↓                    ↓                    ↓                    │
│      Tokens              Surface AST          Core Terms               │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                          Type System                                     │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐         │
│  │ Type Checker    │  │ Effect Checker  │  │ Linearity Check │         │
│  │Crisp.Types.Checker│ │Crisp.Effects.Row│ │ (future)        │         │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘         │
│          ↓                    ↓                    ↓                    │
│    Typed Terms        Effect Annotations    Linear Resources            │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                     Intermediate Representations                         │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐         │
│  │   Typed IR      │→ │ Effect-Normal IR│→ │   Low-Level IR  │         │
│  │ Crisp.IR.TypedIR│  │ Crisp.IR.ENIR   │  │  (future)       │         │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘         │
│      (Auditable)         (CPS Effects)       (Monomorphic)             │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                            Backend                                       │
│  ┌─────────────────┐  ┌─────────────────┐                              │
│  │  Wasm Codegen   │  │ Manifest Generator│                             │
│  │Crisp.Codegen.Wasm│  │ (future)         │                             │
│  └─────────────────┘  └─────────────────┘                              │
│          ↓                    ↓                                        │
│     .wasm binary      .crisp-manifest.json                             │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Module Structure (Haskell)

```
crisp/
├── crisp.cabal              # Package configuration
├── stack.yaml               # Stack configuration
├── app/                     # CLI Application
│   ├── Main.hs
│   └── Crisp/CLI/
│       ├── Options.hs       # CLI option parsing
│       └── Commands.hs      # Command implementations
├── src/Crisp/               # Library
│   ├── Syntax/
│   │   ├── Span.hs          # Source location tracking
│   │   └── Surface.hs       # Surface AST
│   ├── Lexer/
│   │   ├── Token.hs         # Token types
│   │   ├── Lexer.hs         # Megaparsec-based lexer
│   │   └── Layout.hs        # Indentation handling
│   ├── Parser/
│   │   └── Parser.hs        # Megaparsec-based parser
│   ├── Core/
│   │   ├── Term.hs          # Core calculus terms/types
│   │   └── Desugar.hs       # Surface → Core
│   ├── Types/
│   │   ├── Context.hs       # Typing context
│   │   └── Checker.hs       # Bidirectional type checker
│   ├── Effects/
│   │   └── Row.hs           # Effect row operations
│   ├── IR/
│   │   ├── TypedIR.hs       # Typed IR (auditable artifact)
│   │   └── ENIR.hs          # Effect-normalized IR
│   └── Codegen/
│       └── Wasm.hs          # WebAssembly generation
├── test/                    # Tests (HSpec)
│   ├── Main.hs
│   └── Crisp/
│       ├── Lexer/LexerSpec.hs
│       ├── Parser/ParserSpec.hs
│       └── Types/CheckerSpec.hs
├── examples/                # Example Crisp programs
└── docs/                    # Documentation
```

---

## Implementation Phases

### Phase 1: Core Language (Weeks 1-8)

#### Week 1-2: Lexer
- [x] Token types definition
- [x] Megaparsec-based lexer implementation
- [x] Indentation tracking (significant whitespace)
- [x] Layout token generation (INDENT, DEDENT, NEWLINE)
- [x] Comment handling (line and block)
- [ ] Comprehensive lexer tests
- [ ] Error recovery and reporting

**Deliverable:** Lexer that produces correct token streams for Crisp source.

#### Week 3-4: Parser
- [x] Parser infrastructure (Megaparsec)
- [x] Expression parsing with operator precedence
- [x] Pattern parsing
- [x] Type annotation parsing
- [x] Definition parsing (fn, type, effect, handler)
- [x] Module header parsing
- [ ] Comprehensive parser tests

**Deliverable:** Parser that produces Surface AST from tokens.

#### Week 5-6: Core Calculus & Desugaring
- [x] Core term representation
- [x] Core type representation
- [x] Surface-to-Core desugaring
- [x] Pipeline operator desugaring
- [x] Do-notation desugaring
- [ ] Pattern match elaboration
- [ ] Comprehensive desugaring tests

**Deliverable:** Complete surface-to-core translation.

#### Week 7-8: Type Checker (Basic)
- [x] Typing context
- [x] Bidirectional type checking structure
- [x] Type substitution
- [ ] Type unification
- [ ] Type inference for let-bindings
- [ ] Kind checking
- [ ] Constructor typing
- [ ] Pattern exhaustiveness checking

**Deliverable:** Type checker for simply-typed lambda calculus subset.

### Phase 2: Effects (Weeks 9-14)

#### Week 9-10: Effect Declarations
- [x] Effect row representation
- [x] Effect row operations (union, remove, subset)
- [ ] Effect declaration parsing
- [ ] Effect typing rules
- [ ] Effect polymorphism

**Deliverable:** Effect declarations with proper typing.

#### Week 11-12: Handlers
- [x] Handler AST representation
- [ ] Handler typing (operation clauses, return clause)
- [ ] Handler composition
- [ ] Effect subsumption checking

**Deliverable:** Type-safe effect handlers.

#### Week 13-14: CPS Transform
- [x] ENIR representation
- [ ] Effect operation → CPS transform
- [ ] Handler → CPS transform
- [ ] Continuation management

**Deliverable:** Effect-Normalized IR with CPS-transformed effects.

### Phase 3: Dependent Types (Weeks 15-20)

#### Week 15-16: Dependent Function Types
- [ ] Pi type implementation
- [ ] Value-level type arguments
- [ ] Dependent application typing
- [ ] Type-level computation (purity check)

**Deliverable:** Working dependent function types.

#### Week 17-18: GADTs
- [ ] GADT constructor parsing and typing
- [ ] Index refinement during pattern matching
- [ ] Impossibility detection
- [ ] Constructor return type unification

**Deliverable:** GADTs with index refinement.

#### Week 19-20: Prop Universe
- [ ] Prop kind implementation
- [ ] Proof term construction
- [ ] Proof irrelevance
- [ ] Proof erasure pass

**Deliverable:** Proof terms that are erased before code generation.

### Phase 4: WebAssembly (Weeks 21-26)

#### Week 21-22: Low-Level IR
- [ ] LLIR representation (monomorphic, explicit memory)
- [ ] Monomorphization pass
- [ ] Data representation decisions
- [ ] Stack layout planning

**Deliverable:** LLIR suitable for direct Wasm translation.

#### Week 23-24: Code Generation
- [x] Wasm module structure
- [ ] Wasm binary encoding
- [ ] Function compilation
- [ ] Data type compilation (tagged unions)
- [ ] Effect-to-import compilation
- [ ] Memory management stubs

**Deliverable:** Working Wasm binary output.

#### Week 25-26: Manifest & TIR
- [x] TIR JSON structure
- [x] TIR serialization with Aeson
- [ ] Manifest generation
- [ ] Content hashing
- [ ] Authority namespace mapping

**Deliverable:** Complete compilation artifacts (.wasm + .crisp-manifest.json + .tir.json).

### Phase 5: Polish (Weeks 27-32)

#### Week 27-28: Module System
- [ ] Module parsing
- [ ] Import resolution
- [ ] Separate compilation
- [ ] Linking

**Deliverable:** Multi-module compilation.

#### Week 29-30: Linear Types
- [ ] Linearity annotations
- [ ] Use counting
- [ ] Borrow checking (simplified)
- [ ] Resource tracking

**Deliverable:** Basic linear types with borrow checking.

#### Week 31-32: Tooling & Standard Library
- [ ] REPL implementation
- [ ] Formatter
- [ ] LSP server (stretch goal)
- [ ] Standard prelude library
- [ ] Documentation generator

**Deliverable:** Developer-ready toolchain.

### Phase 6: Rust/Wasm Migration (Future)

#### Migration Strategy
1. Define stable TIR format as interchange
2. Implement Rust compiler that reads TIR
3. Bootstrap: compile Crisp prelude with Haskell, execute with Rust
4. Gradually replace Haskell components
5. Self-host: Crisp compiler written in Crisp

#### Benefits
- Portable toolchain via Wasm
- Smaller binary size
- No Haskell runtime dependency
- Integration with Rust/Wasm ecosystem

---

## Current Status

### Completed (Scaffold)
- [x] Haskell project structure (Cabal + Stack)
- [x] CLI skeleton with optparse-applicative
- [x] Token definitions
- [x] Megaparsec-based lexer
- [x] Source span utilities
- [x] Surface AST types
- [x] Core term/type definitions
- [x] Surface-to-Core desugaring (basic)
- [x] Typing context
- [x] Bidirectional type checker structure
- [x] Effect row operations
- [x] TIR structure and JSON serialization
- [x] ENIR structure
- [x] Wasm codegen scaffold

### In Progress
- [ ] Parser implementation (basic structure done)
- [ ] Lexer test suite
- [ ] Type checker completion

### Not Started
- [ ] Full type inference
- [ ] Effect typing integration
- [ ] CPS transform implementation
- [ ] Wasm binary encoding
- [ ] Module system

---

## Testing Strategy

### Unit Tests (HSpec)
```haskell
-- test/Crisp/Lexer/LexerSpec.hs
spec :: Spec
spec = do
  describe "lexFile" $ do
    it "lexes simple keywords" $ ...
    it "handles indentation" $ ...
```

### Property Tests (QuickCheck)
```haskell
-- Type substitution properties
prop_subst_identity :: Type -> Property
prop_subst_identity ty = substituteType ty 0 (TyVar "_" 0) === ty
```

### Integration Tests
End-to-end tests with `.crisp` source files in `test/integration/`.

---

## Dependencies (Haskell)

```yaml
# From crisp.cabal
dependencies:
  - base >= 4.14
  - text
  - containers
  - mtl
  - transformers
  - megaparsec         # Parsing
  - parser-combinators
  - aeson              # JSON for TIR
  - bytestring
  - vector
  - prettyprinter      # Pretty printing
  - optparse-applicative  # CLI
```

---

## Building & Running

### Using Stack (Recommended)
```bash
# Build
stack build

# Test
stack test

# Run compiler
stack exec crisp -- compile examples/hello.crisp

# Type-check only
stack exec crisp -- check examples/hello.crisp

# Emit TIR
stack exec crisp -- compile examples/hello.crisp --emit-tir
```

### Using Cabal
```bash
# Build
cabal build

# Test
cabal test

# Run
cabal run crisp -- help
```

---

## Contributing

1. Pick an item from "Not Started" or "In Progress"
2. Create a feature branch
3. Write tests first (TDD encouraged)
4. Implement the feature
5. Run `stack test` to verify
6. Submit PR with clear description

---

## References

- [Crisp Language Specification](./SPECIFICATION.md)
- [WebAssembly Specification](https://webassembly.github.io/spec/core/)
- [Bidirectional Type Checking (Pierce & Turner)](https://www.cs.cmu.edu/~fp/papers/pldi91.pdf)
- [Algebraic Effects and Handlers](https://www.eff-lang.org/handlers-tutorial.pdf)
- [Practical Type Inference (Dunfield & Krishnaswami)](https://arxiv.org/abs/1306.6032)

---

*Last Updated: 2025-01-25*
