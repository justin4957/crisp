# Crisp Language Specification

**Version:** 0.2.0-draft
**Status:** MVP Specification
**Target:** WebAssembly (Wasm)

---

## Abstract

Crisp is a general-purpose programming language with dependent types and algebraic effects, compiling to WebAssembly. Its design principle:

> **The natural way to write correct programs is also the natural way to write auditable, semantically honest programs.**

Crisp is designed for systems where **correctness, auditability, and authority boundaries matter**. It is not a domain-specific language—institutional modeling, policy engines, and dialectical simulations emerge naturally from the primitives, just as memory safety emerges naturally from Rust's ownership model.

---

## Table of Contents

1. [Design Principles](#1-design-principles)
2. [Lexical Structure](#2-lexical-structure)
3. [Surface Syntax](#3-surface-syntax)
4. [Core Calculus](#4-core-calculus)
5. [Type System](#5-type-system)
6. [Effect System](#6-effect-system)
7. [Module System](#7-module-system)
8. [Memory Model](#8-memory-model)
9. [Standard Prelude](#9-standard-prelude)
10. [Compilation](#10-compilation)
11. [Wasm ABI](#11-wasm-abi)
12. [Formal Semantics](#12-formal-semantics)
13. [Implementation Roadmap](#13-implementation-roadmap)

---

## 1. Design Principles

### 1.1 Core Commitments

| Principle | Implication |
|-----------|-------------|
| **Explicit over implicit** | No hidden effects, no implicit conversions, no ambient authority |
| **Local over global** | No global typeclass coherence, scoped capabilities |
| **Strict over lazy** | Strict by default, laziness explicitly declared |
| **Auditable over clever** | Typed IR as first-class artifact |
| **Capability over ambient** | Effects are imports, not magic |

### 1.2 What We Keep from Haskell

- Algebraic Data Types (ADTs)
- Total pattern matching (coverage, not termination)
- Referential transparency
- Explicit effects
- Equational reasoning (where possible)

### 1.3 What We Reject

- Global typeclass coherence
- Implicit laziness
- Monad transformer stacks
- Heavy runtime assumptions
- "One universe of discourse"

### 1.4 What Wasm Gives Us

WebAssembly enforces explicit memory, explicit imports/exports, deterministic execution, sandboxed authority, and host-defined effects. This maps directly to institutional boundaries, jurisdictional authority, capability scoping, and auditability.

### 1.5 MVP Scope

The following features are **MVP-light** with restricted semantics initially:

- **Linear types:** Basic linearity without full borrowing inference
- **Regions:** Host-managed, no automatic region inference
- **Authority manifests:** Generated but not verified at link time
- **Proof automation:** Manual proofs only; no tactics

---

## 2. Lexical Structure

### 2.1 Character Set

Crisp source files are UTF-8 encoded. Indentation is significant.

### 2.2 Keywords

```
fn        let       type      effect    handler
match     if        then      else      with
import    module    requires  provides  authority
linear    lazy      perform   resume    return
where     forall    as        qualified do
prop      total     mut       ref
```

### 2.3 Operators

```
->        =>        |         :         ::
=         @         &         .         ,
|>        <|        >>        <<        $
```

Arithmetic and comparison operators are defined in the prelude, not built-in.

### 2.4 Indentation Rules

Crisp uses **significant whitespace** for block structure:

1. **Blocks** are introduced by `:` at end of line, or by increased indentation after certain keywords
2. **Continuation** is indicated by increased indentation from the parent expression
3. **Sibling expressions** at the same indentation level are separate statements
4. **Tabs are forbidden**; use spaces only (recommended: 2 spaces)

```crisp
// Block introduced by colon
fn process x:
  let y = transform x
  let z = validate y
  finalize z

// Continuation by indentation
let result =
  long_computation
    with_argument_one
    with_argument_two
```

### 2.5 Comments

```crisp
-- Line comment (Haskell-style)

{- Block comment
   can span multiple lines -}

--- Documentation comment (attached to following item)
```

### 2.6 Identifiers

```
lowercase_ident  = [a-z][a-zA-Z0-9_']*
uppercase_ident  = [A-Z][a-zA-Z0-9_']*
```

Type names and constructors begin with uppercase. Values and functions begin with lowercase. Trailing apostrophes allowed (e.g., `x'`, `x''`).

---

## 3. Surface Syntax

### 3.1 Design Philosophy: Negative Space Construction

Traditional Lisp uses parentheses to delimit nested expressions:

```lisp
(process (transform (filter data pred) fn) config)
```

Crisp uses **indentation and line breaks** to express the same structure:

```crisp
process config
  transform fn
    filter data pred
```

Reading order: innermost expressions are most indented; each outdent "wraps" the result.

This is **not** Lisp syntax—it is a distinct approach where **vertical structure replaces parenthetical nesting**.

### 3.2 Function Application

**Simple application** (single line):
```crisp
add 1 2
map f xs
```

**Nested application** (indentation):
```crisp
-- Equivalent to: process(transform(filter(data, pred)), config)
process config
  transform
    filter data pred
```

**Mixed application**:
```crisp
-- Equivalent to: foo(a, bar(b, c), d)
foo a d
  bar b c
```

**Rule:** Indented lines are arguments to the expression on the previous less-indented line. Arguments are collected left-to-right, with indented blocks inserted at the first "hole."

### 3.3 Pipeline Operators

For left-to-right data flow, use the pipe operator:

```crisp
data
  |> filter pred
  |> transform fn
  |> process config
```

Pipelines are sugar:
- `x |> f` desugars to `f x`
- `x |> f a b` desugars to `f a b x` (pipeline argument goes last)

Reverse pipe for function composition:
```crisp
processor =
  filter pred
    <| transform fn
    <| finalize
```

### 3.4 Block Expressions

Blocks are sequences of bindings ending in an expression:

```crisp
let result:
  let x = compute_a
  let y = compute_b x
  combine x y
```

The `do` keyword introduces effectful blocks:

```crisp
fn main:
  do
    perform Log.info "Starting"
    let data = perform FileSystem.read path
    let result = process data
    perform Log.info "Done"
    result
```

### 3.5 Function Definitions

```crisp
--- Computes the length of a vector
fn length [T, n: Nat] (xs: Vec T n) -> Nat:
  match xs
    Nil       -> 0
    Cons _ t  -> add 1 (length t)
```

**Anatomy:**
- `fn` keyword
- Name
- Type parameters in `[...]` (optional)
- Value parameters in `(...)`
- Return type after `->`
- Effects after `!` (optional)
- Body after `:`

**With effects:**
```crisp
fn process (path: Path) -> Report ! FileSystem, Log:
  do
    let data = perform FileSystem.read path
    perform Log.info "Processing"
    analyze data
```

The `! Effect1, Effect2` syntax declares effects (desugars to core `→[{Effect1, Effect2}]`).

### 3.6 Type Definitions

**Simple ADT:**
```crisp
type Option T:
  None
  Some T
```

**Indexed type (GADT):**
```crisp
type Vec T (n: Nat):
  Nil  : Vec T 0
  Cons : T -> Vec T m -> Vec T (succ m)
```

**Record type:**
```crisp
type Point:
  x: Float
  y: Float
```

**Linear type:**
```crisp
type linear File
```

### 3.7 Pattern Matching

```crisp
fn describe (opt: Option Int) -> String:
  match opt
    None     -> "nothing"
    Some 0   -> "zero"
    Some n   -> concat "number: " (show n)
```

**Guards:**
```crisp
fn classify (n: Int) -> String:
  match n
    _ | lt n 0   -> "negative"
    _ | eq n 0   -> "zero"
    _            -> "positive"
```

**Totality:** Pattern matching must cover all constructors. Cases that are impossible due to index constraints may be omitted—the compiler verifies impossibility.

### 3.8 Effect Operations

```crisp
-- Perform an effect operation
let evidence = perform Investigate.subpoena target

-- Shorthand in do-blocks
do
  evidence <- Investigate.subpoena target
  statement <- Investigate.interview witness
  compile evidence statement
```

The `<-` notation in `do` blocks is sugar for `let x = perform E.op(...)`.

### 3.9 Handlers

```crisp
handler MockInvestigation for Investigate ! Log:
  subpoena entity -> resume:
    perform Log.info (concat "Mock subpoena: " (show entity))
    resume Evidence.empty

  interview person -> resume:
    perform Log.info (concat "Mock interview: " (show person))
    resume Statement.empty

  return x -> x
```

**Using handlers:**
```crisp
let result =
  with MockInvestigation
    run_investigation target
```

### 3.10 Explicit Quantification

Type parameters can be made explicit with `forall`:

```crisp
fn identity: forall T. T -> T
fn identity x = x
```

For dependent types:
```crisp
fn replicate: forall T. forall (n: Nat). T -> Vec T n
```

The `[...]` syntax in function definitions is sugar for leading `forall`s.

### 3.11 Proof Terms

Propositions inhabit `Prop`, not `Type`:

```crisp
type prop Authorized (a: Authority) (act: Action):
  authorized : a.level >= act.required_level => Authorized a act
```

The `prop` keyword indicates:
- Terms are proof-irrelevant (erased at runtime)
- Cannot be pattern-matched for computational content
- Cannot cross Wasm boundaries
- Exist only to justify typing

### 3.12 Where Clauses

Local definitions can follow the main expression:

```crisp
fn quadratic (a: Float) (b: Float) (c: Float) (x: Float) -> Float:
  add (add (mul a x_sq) (mul b x)) c
  where
    x_sq = mul x x
```

---

## 4. Core Calculus

### 4.1 Terms

The core language is a **Dependent Effect Calculus (DEC)**.

```
e ::= x                           -- variable
    | λ(x : τ). e                 -- lambda abstraction
    | e₁ e₂                       -- application
    | let x : τ = e₁ in e₂        -- let binding
    | Λα. e                       -- type abstraction
    | e [τ]                       -- type application
    | C e₁ ... eₙ                 -- constructor application
    | match e { p₁ => e₁ | ... }  -- pattern match
    | perform E.op(e)             -- effect operation
    | handle[H](e)                -- effect handler
    | lazy e                      -- deferred computation
    | force e                     -- force deferred computation
```

### 4.2 Types

```
τ ::= α                           -- type variable
    | T τ₁ ... τₙ                 -- type constructor application
    | (x : τ₁) →[ε] τ₂            -- dependent function type
    | ∀α. τ                       -- universal quantification
    | ∀(x : τ₁). τ₂               -- dependent quantification
    | Lazy τ                      -- suspended computation
    | Linear τ                    -- linear type
    | Ref τ                       -- borrowed reference
    | RefMut τ                    -- mutable borrowed reference
    | Typeᵢ                       -- universe at level i
    | Prop                        -- universe of propositions
```

### 4.3 Kinds

```
κ ::= Type                        -- universe of runtime types (Type₀)
    | Typeᵢ                       -- universe at level i
    | Prop                        -- universe of erased proofs
    | Linear                      -- kind of linear types
    | κ₁ → κ₂                     -- type constructor kind
```

**Stratification:**
- `Type` (synonym for `Type₀`) contains runtime data types
- `Prop` contains proof-irrelevant propositions (erased before codegen)
- `Linear` is a sub-kind of `Type` with usage restrictions

### 4.4 Effects

```
ε ::= ∅                           -- pure
    | { E₁, E₂, ..., Eₙ }         -- effect set
    | { E @ a }                   -- effect with authority annotation
    | ε₁ ∪ ε₂                     -- effect union
    | ρ                           -- effect variable
```

### 4.5 Surface-to-Core Desugaring

| Surface | Core |
|---------|------|
| `fn f [T] (x: A) -> B ! E1, E2: body` | `f : ∀T. (x : A) →[{E1,E2}] B = λx. body` |
| `A -> B` | `(_ : A) →[∅] B` |
| `A -> B ! E` | `(_ : A) →[{E}] B` |
| `x \|> f` | `f x` |
| `x \|> f a b` | `f a b x` |
| `do { x <- E.op; e }` | `let x = perform E.op in e` |
| `type prop P: ...` | ADT with kind `Prop` |
| `type linear T` | ADT with kind `Linear` |

### 4.6 Evaluation Strategy

**Strict by default.** Evaluation proceeds:

1. Function arguments evaluated before application
2. Let bindings evaluated before body
3. Constructor arguments evaluated before construction

**Explicit laziness:**
```crisp
let x = lazy expensive_computation  -- Not evaluated
let y = force x                     -- Now evaluated
```

---

## 5. Type System

### 5.1 Universes and Stratification

```
Prop : Type₁
Type₀ : Type₁ : Type₂ : ...
```

**Proof Stratification Rule:**
> Types in `Prop` are proof-irrelevant. Terms of type `P : Prop`:
> - May appear as function arguments
> - May justify typing constraints
> - Are erased before Effect-Normalized IR
> - Cannot be pattern-matched for computational content
> - Cannot be serialized or cross Wasm boundaries

This prevents "proof laundering" and ensures proofs don't affect runtime behavior.

### 5.2 Judgment Forms

| Judgment | Meaning |
|----------|---------|
| `Γ ⊢ e : τ ; ε` | Expression e has type τ with effects ε |
| `Γ ⊢ τ : κ ; ∅` | τ is a well-formed type of kind κ (must be pure) |
| `Γ ⊢ ε₁ ≤ ε₂` | Effect ε₁ is subsumed by ε₂ |
| `Γ ⊢ τ₁ ≡ τ₂` | Types τ₁ and τ₂ are definitionally equal |

### 5.3 Bidirectional Typing

| Syntactic Form | Mode | Notes |
|----------------|------|-------|
| Variable | Synthesize | Look up in context |
| Literal | Synthesize | Type determined by literal form |
| Application | Synthesize | Infer function type, check argument |
| Lambda | Check | Requires expected type |
| Lambda with annotation | Synthesize | `λ(x : τ). e` synthesizes |
| Let | Synthesize | Synthesize bound expr, check/synth body |
| Match | Both | Check against expected type if available |
| Type annotation | Synthesize | `e : τ` checks e against τ, synthesizes τ |
| Perform | Synthesize | Operation signature determines type |

### 5.4 Dependent Function Types

```
(x : τ₁) →[ε] τ₂
```

The return type τ₂ and effect ε may depend on the value x.

**The Critical Purity Rule:**
> Type-level computation must be pure.
```
Γ ⊢ τ : κ ; ∅
──────────────────
τ may appear in dependent positions
```

If computing a type requires effects, that type cannot be used in dependent positions.

### 5.5 Subtyping

Subtyping is restricted to:

1. **Effect subtyping:** `ε₁ ⊆ ε₂ ⟹ τ →[ε₁] σ <: τ →[ε₂] σ`
2. **Universe subtyping:** `Typeᵢ <: Typeⱼ` when `i ≤ j`
3. **Prop subtyping:** `Prop <: Type₁`

No structural subtyping on data types.

### 5.6 Type Equivalence and Authority

**Authority annotations do not affect type equivalence.**

```
{ Investigate @ CIA } ≡ { Investigate @ LocalPolice }  -- as types
```

Authority annotations are **opaque labels** preserved through compilation. They:
- Do not participate in subtyping
- Do not participate in unification
- Do not participate in effect subsumption

**Lint rule:** Substituting effects with differing authorities produces a warning unless explicitly annotated with `@allow(authority_mismatch)`.

### 5.7 Totality

**Totality means pattern coverage, not termination.**

```crisp
fn head [T, n: Nat] (xs: Vec T (succ n)) -> T:
  match xs
    Cons x _ -> x
    -- Nil case impossible: succ n ≠ 0
```

The compiler verifies impossibility via index constraint solving.

**Crisp does not guarantee termination.** Non-terminating programs are allowed:

```crisp
fn loop -> Nat:
  loop  -- allowed, but never returns
```

Future versions may support `total fn` annotations for termination-checked functions.

### 5.8 Effect Polymorphism

Functions can be polymorphic over effects:

```crisp
fn map [T, U, ε] (f: T -> U ! ε) (xs: List T) -> List U ! ε:
  match xs
    Nil       -> Nil
    Cons x t  -> Cons (f x) (map f t)
```

**Generalization rule:** Top-level definitions are generalized over free type and effect variables not constrained by the context.

---

## 6. Effect System

### 6.1 Effect Declarations

```crisp
effect Log:
  info  : String -> Unit
  warn  : String -> Unit
  error : String -> Unit

effect FileSystem:
  read  : Path -> Bytes
  write : Path -> Bytes -> Unit

effect Investigate:
  subpoena  : Entity -> Evidence
  interview : Person -> Statement
```

### 6.2 Effect Rows

Effect rows form a join-semilattice under union:

- `ε₁ ∪ ε₂` combines effects
- `∅` is the identity (pure)
- Order doesn't matter: `{ Log, FileSystem } = { FileSystem, Log }`

**Effect subtyping:**
```
ε₁ ⊆ ε₂
───────────────────────────
τ →[ε₁] σ  <:  τ →[ε₂] σ
```

**Effect subsumption with handlers:**
When `e : τ ; ε ∪ {E}` and handler H handles E:
```
Γ ⊢ e : τ ; ε ∪ {E}
Γ ⊢ H : E →[ε'] τ
────────────────────────────
Γ ⊢ handle[H](e) : τ ; ε ∪ ε'
```

The set ε may contain any effects; unhandled effects propagate.

### 6.3 Authority Annotations

Effects can carry authority annotations:

```crisp
fn audit (target: Entity) -> Report ! Investigate @ OversightBoard:
  do
    evidence <- Investigate.subpoena target
    compile_report evidence
```

**Semantics:**
- Authority annotations are opaque labels
- They do not affect typing or effect subsumption
- They are preserved in Typed IR
- They compile to namespaced Wasm imports

**Authority checking is a lint, not a type error.** Programs with mismatched authorities compile but produce warnings.

### 6.4 Effect Handlers

```crisp
handler RunState (S: Type) (initial: S) for State S ! Pure:
  get () -> resume:
    λs. resume s s

  put new_s -> resume:
    λ_. resume () new_s

  return x -> λs. x
```

**Handler anatomy:**
- Name and parameters
- `for Effect ! IntroducedEffects`
- Operation clauses with explicit `resume` continuation
- `return` clause for the final value

**Handler typing:**
```
handler H for E ! ε':
  ...
```
Produces `H : E →[ε'] τ_result`

### 6.5 Using Handlers

```crisp
-- Basic handler invocation
let result =
  with RunState Int 0
    computation

-- Nested handlers
let result =
  with FileSystemImpl
    with LogToStderr
      main_program
```

---

## 7. Module System

### 7.1 Module Declaration

```crisp
module Treasury.Audit
authority Treasury

requires:
  effects: FileSystem, Log
  types: Transaction, Account

provides:
  type AuditReport
  fn audit_account : Account -> AuditReport ! FileSystem, Log
  fn summarize     : List AuditReport -> Summary
```

### 7.2 Authority Scoping

Modules with declared authority automatically scope their effects:

```crisp
module Justice.Enforcement
authority Enforcement

-- This function's effects are implicitly @ Enforcement
fn detain (subject: Person) (warrant: Warrant) -> Custody ! Detain:
  ...
```

### 7.3 Imports

```crisp
-- Import specific items
import Core.List: map, filter, fold

-- Import qualified
import Treasury.Audit qualified as Audit

-- Import with renaming
import Core.String: concat as strcat
```

### 7.4 Module Signatures

```crisp
signature Treasury.AuditSig:
  type AuditReport
  fn audit_account : Account -> AuditReport ! FileSystem, Log
```

---

## 8. Memory Model

### 8.1 Linear Types

Resources that must be used exactly once:

```crisp
type linear File
type linear Connection
type linear Token

fn open (path: Path) -> File ! FileSystem
fn close (file: File) -> Unit ! FileSystem
```

**Linearity restriction:**
> Linear values may **not** appear in dependent indices or type-level computation.

This prevents:
```crisp
-- ILLEGAL: linear value in type index
type Session (n: Nat) (t: Token)  -- Token is linear, cannot be index
```

### 8.2 Borrowing

Temporary access without consuming:

```crisp
fn read (file: ref File) -> Bytes ! FileSystem
fn modify (file: ref mut File) (data: Bytes) -> Unit ! FileSystem

fn process (file: File) -> Unit ! FileSystem:
  do
    let data = read (ref file)      -- borrow
    let more = read (ref file)      -- borrow again
    close file                      -- consume
```

**Borrow syntax:**
- `ref T` — immutable borrow (surface `&T` also accepted)
- `ref mut T` — mutable borrow (surface `&mut T` also accepted)

**MVP simplification:** Borrow checking is simplified; complex lifetime inference deferred to future versions.

### 8.3 Resource-Granted Effects

Linear resources can grant effects:

```crisp
type linear DatabaseConn:
  grants: Query, Transact

fn query (conn: ref DatabaseConn) (sql: String) -> ResultSet ! Query:
  ...
```

**Typing rule for granted effects:**

> A function mentioning effect E in its signature, where E is granted by resource R, must have a parameter of type R, `ref R`, or `ref mut R` in scope.

When the connection is consumed (closed), `Query` and `Transact` become unavailable.

### 8.4 Drop Semantics

**MVP:** Linear values must be explicitly consumed via designated "destructor" functions (e.g., `close`, `release`). There are no implicit drops or user-definable destructors.

**Drop order:** When multiple linear values go out of scope simultaneously (e.g., in a `let` that binds multiple values), drop order is **unspecified**. Programs must not rely on drop ordering.

### 8.5 Regions

```crisp
fn with_region [T] (body: forall r. Region r -> T ! Alloc @ r) -> T:
  ...
```

**Region escape prevention:**
> Values of type `Region r` are linear and cannot escape `with_region`. The region parameter `r` is existentially bound within the body.

**MVP simplification:** Regions are host-managed. Crisp code requests allocations; the host handles actual memory.

---

## 9. Standard Prelude

### 9.1 Primitive Types

```crisp
type Unit = ()
type Bool = True | False
type Nat                  -- Natural numbers (arbitrary precision)
type Int                  -- Integers (arbitrary precision)
type Float                -- IEEE 754 double
type Char                 -- Unicode scalar value
type String               -- UTF-8 string
```

### 9.2 Core Data Types

```crisp
type Option T:
  None
  Some T

type Result T E:
  Ok T
  Err E

type List T:
  Nil
  Cons T (List T)

type Vec T (n: Nat):
  VNil  : Vec T 0
  VCons : T -> Vec T m -> Vec T (succ m)

type Pair A B:
  Pair A B
```

### 9.3 Proof Types

```crisp
type prop Eq T (a: T) (b: T):
  Refl : Eq T x x

type Void  -- Empty type (no constructors)

fn absurd [T] (v: Void) -> T:
  match v  -- Empty match is total
```

### 9.4 Core Effects

```crisp
effect Panic:
  panic : forall T. String -> T  -- diverges

effect State S:
  get : Unit -> S
  put : S -> Unit

effect Reader R:
  ask : Unit -> R

effect Writer W:
  tell : W -> Unit
```

### 9.5 Core Functions

```crisp
fn id [T] (x: T) -> T:
  x

fn const [A, B] (a: A) (_: B) -> A:
  a

fn compose [A, B, C, ε1, ε2] (f: B -> C ! ε1) (g: A -> B ! ε2) -> A -> C ! ε1, ε2:
  λa. f (g a)

fn flip [A, B, C, ε] (f: A -> B -> C ! ε) -> B -> A -> C ! ε:
  λb. λa. f a b
```

### 9.6 Operators

```crisp
-- Arithmetic (defined in prelude, not built-in)
fn add : Nat -> Nat -> Nat
fn sub : Nat -> Nat -> Nat
fn mul : Nat -> Nat -> Nat

-- Comparison
fn eq [T] : T -> T -> Bool
fn lt [T] : T -> T -> Bool

-- Boolean
fn and : Bool -> Bool -> Bool
fn or  : Bool -> Bool -> Bool
fn not : Bool -> Bool
```

---

## 10. Compilation

### 10.1 Pipeline Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                         Crisp Source                            │
│                         (.crisp files)                          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Parse (whitespace-aware)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Surface AST                                │
│  • Preserves source locations                                   │
│  • Indentation structure preserved                              │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Desugar + Elaborate
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Elaborated Core                              │
│  • Implicit arguments explicit                                  │
│  • Type inference complete                                      │
│  • Proof terms inserted                                         │
│  • Pipeline operators desugared                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Type Check + Authority Lint
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Typed IR (TIR)                               │
│  ════════════════════════════════════════════                   │
│  │ AUDITABLE ARTIFACT                        │                  │
│  ════════════════════════════════════════════                   │
│  • All types preserved                                          │
│  • Effect annotations preserved                                 │
│  • Authority annotations preserved                              │
│  • Prop terms still present (marked for erasure)                │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Proof Erasure
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                 Effect-Normalized IR (ENIR)                     │
│  • Prop terms erased                                            │
│  • Handlers compiled to CPS                                     │
│  • Effect operations become function calls                      │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Linearity Check
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Linear IR (LIR)                              │
│  • Resource usage verified                                      │
│  • Borrowing resolved                                           │
│  • Explicit consume/drop points                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Monomorphization + Optimization
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Low-Level IR (LLIR)                          │
│  • Monomorphic                                                  │
│  • Explicit memory operations                                   │
│  • Close to Wasm structure                                      │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Code Generation
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Wasm Module + Manifest                       │
│  • .wasm binary                                                 │
│  • .crisp-manifest.json                                         │
│  • Optional: .tir.json (serialized Typed IR)                    │
└─────────────────────────────────────────────────────────────────┘
```

### 10.2 Typed IR: The Auditable Artifact

The Typed IR is a **first-class compilation artifact** with stability guarantees.

**Format:**
```json
{
  "format": "crisp-tir",
  "version": "0.2.0",
  "module": "Treasury.Audit",
  "authority": "Treasury",
  "requires": {
    "effects": [
      { "effect": "FileSystem", "authority": "Treasury" },
      { "effect": "Log", "authority": null }
    ]
  },
  "definitions": [ ... ],
  "proofs": [ ... ],
  "hashes": {
    "source": "sha256:...",
    "normalized": "sha256:..."
  }
}
```

**Stability guarantees:**
- **Canonical form:** TIR is alpha-normalized and sugar-free
- **Versioning:** The `version` field follows semver; breaking changes increment major version
- **Diffability:** Identical source produces identical TIR (modulo hashes)
- **Invariant:** `manifest.requires` is computed from module signature and must match TIR; disagreement is a compiler error

### 10.3 CPS Transform for Effects

**Compiler invariant:**
> During CPS transformation, dependent substitutions are normalized to values before any duplication of continuations.

This prevents unsoundness when effect handlers duplicate or discard continuations.

**Source:**
```crisp
fn example -> Int ! State Int:
  do
    x <- State.get ()
    State.put (add x 1)
    x
```

**After CPS:**
```
example = λk. λstate_get. λstate_put.
  state_get () (λx.
    state_put (add x 1) (λ_.
      k x))
```

---

## 11. Wasm ABI

### 11.1 Effects as Imports

Each effect operation becomes a Wasm import:

```crisp
effect FileSystem:
  read  : Path -> Bytes
  write : Path -> Bytes -> Unit
```

Compiles to:
```wat
(import "effects" "FileSystem.read"
  (func $fs_read (param i32 i32) (result i32 i32)))
(import "effects" "FileSystem.write"
  (func $fs_write (param i32 i32 i32 i32)))
```

### 11.2 Authority Namespacing

```crisp
fn audit -> Report ! FileSystem @ Treasury:
  ...
```

Compiles to:
```wat
(import "effects/Treasury" "FileSystem.read" ...)
```

### 11.3 Manifest Format

```json
{
  "crisp_version": "0.2.0",
  "module": "Treasury.Audit",
  "authority": "Treasury",

  "requires": {
    "effects": [
      { "effect": "FileSystem", "authority": "Treasury" },
      { "effect": "Log", "authority": null }
    ],
    "imports": [
      { "module": "Core.List", "items": ["map", "filter"] }
    ]
  },

  "provides": {
    "types": ["AuditReport"],
    "functions": [
      {
        "name": "audit_account",
        "signature": "Account -> AuditReport ! FileSystem, Log",
        "authority_annotations": {
          "FileSystem": "Treasury",
          "Log": null
        }
      }
    ]
  },

  "hashes": {
    "source": "sha256:...",
    "tir": "sha256:...",
    "wasm": "sha256:..."
  }
}
```

### 11.4 Data Representation

| Crisp Type | Wasm Representation |
|----------|---------------------|
| `Unit` | (nothing—zero-width) |
| `Bool` | `i32` (0 or 1) |
| `Nat` | `i64` (MVP); bigint later |
| `Int` | `i64` (MVP); bigint later |
| `Float` | `f64` |
| `Char` | `i32` (Unicode scalar) |
| `String` | `(i32, i32)` — pointer + length |
| ADTs | Tagged unions |
| `Prop` types | Erased (not represented) |
| Linear types | Same as underlying |
| Borrows | Pointer |

---

## 12. Formal Semantics

### 12.1 Typing Rules (Selected)

**Variable:**
```
x : τ ∈ Γ
──────────────
Γ ⊢ x : τ ; ∅
```

**Lambda (checking):**
```
Γ, x : τ₁ ⊢ e ⇐ τ₂ ; ε
─────────────────────────────────
Γ ⊢ (λx. e) ⇐ (x : τ₁) →[ε] τ₂ ; ∅
```

**Application (synthesis):**
```
Γ ⊢ e₁ ⇒ (x : τ₁) →[ε₁] τ₂ ; ε₂
Γ ⊢ e₂ ⇐ τ₁ ; ε₃
───────────────────────────────────
Γ ⊢ e₁ e₂ ⇒ τ₂[x ↦ e₂] ; ε₁ ∪ ε₂ ∪ ε₃
```

**Effect operation:**
```
op : τ_in → τ_out ∈ E
Γ ⊢ e ⇐ τ_in ; ε
─────────────────────────────────
Γ ⊢ perform E.op(e) ⇒ τ_out ; ε ∪ {E}
```

**Handler:**
```
Γ ⊢ e : τ ; ε ∪ {E}
Γ ⊢ H : E →[ε'] τ
────────────────────────────────
Γ ⊢ handle[H](e) : τ ; ε ∪ ε'
```

**Effect subsumption:**
```
Γ ⊢ e : τ ; ε₁
ε₁ ⊆ ε₂
─────────────────
Γ ⊢ e : τ ; ε₂
```

**Prop erasure:**
```
Γ ⊢ e : P ; ∅
P : Prop
──────────────────────
e erased before ENIR
```

### 12.2 Operational Semantics

**Values:**
```
v ::= λx. e | C v₁ ... vₙ | lazy e
```

**Evaluation contexts:**
```
E ::= □ | E e | v E | let x = E in e | C v₁...E...eₙ
    | match E { ... } | perform op(E) | handle[H](E)
```

**Key reduction rules:**

```
(λx. e) v  ⟶  e[x ↦ v]                              -- Beta

let x = v in e  ⟶  e[x ↦ v]                         -- Let

match (C vᵢ) { ... | C xᵢ => e | ... }  ⟶  e[xᵢ ↦ vᵢ]  -- Match

handle[H](E[perform op(v)])  ⟶  h_op(v, λx. handle[H](E[x]))  -- Perform

handle[H](v)  ⟶  h_return(v)                        -- Return

force (lazy e)  ⟶  e                                -- Force
```

### 12.3 Type Safety

**Theorem (Progress):** If `∅ ⊢ e : τ ; ε` then either:
1. `e` is a value, or
2. `e = E[perform op(v)]` for some unhandled effect, or
3. `e ⟶ e'` for some `e'`

**Theorem (Preservation):** If `Γ ⊢ e : τ ; ε` and `e ⟶ e'` then `Γ ⊢ e' : τ ; ε'` for some `ε' ⊆ ε`.

**Theorem (Effect Containment):** If `∅ ⊢ e : τ ; ∅` (pure), then evaluation of `e` never reaches `E[perform op(v)]`.

---

## 13. Implementation Roadmap

### Phase 1: Core (Weeks 1-8)

| Week | Milestone |
|------|-----------|
| 1-2 | Lexer with indentation tracking, parser |
| 3-4 | Core calculus, surface-to-core desugaring |
| 5-6 | Bidirectional type checker (without effects) |
| 7-8 | Tree-walking interpreter |

### Phase 2: Effects (Weeks 9-14)

| Week | Milestone |
|------|-----------|
| 9-10 | Effect declarations, effect typing |
| 11-12 | Handler syntax, handler typing |
| 13-14 | CPS transform, effect interpretation |

### Phase 3: Dependency (Weeks 15-20)

| Week | Milestone |
|------|-----------|
| 15-16 | Dependent function types |
| 17-18 | GADTs with index refinement |
| 19-20 | Prop universe, proof erasure |

### Phase 4: Wasm (Weeks 21-26)

| Week | Milestone |
|------|-----------|
| 21-22 | Low-level IR, data representation |
| 23-24 | Code generation, effect-to-import |
| 25-26 | Manifest generation, TIR serialization |

### Phase 5: Polish (Weeks 27-32)

| Week | Milestone |
|------|-----------|
| 27-28 | Module system, separate compilation |
| 29-30 | Linear types (MVP subset), basic borrowing |
| 31-32 | Standard library, tooling |

---

## Appendix A: Grammar (EBNF)

```ebnf
(* Layout rules *)
INDENT      = increase in indentation level
DEDENT      = decrease in indentation level
NEWLINE     = newline at same indentation level

(* Top-level *)
module      = "module" module_path [NEWLINE "authority" UPPER_IDENT]
              {NEWLINE requires_block} {NEWLINE provides_block}
              {NEWLINE definition} ;

(* Definitions *)
definition  = type_def | effect_def | handler_def | fn_def ;

type_def    = "type" ["prop" | "linear"] UPPER_IDENT {type_param}
              [":" kind] [INDENT {constructor NEWLINE} DEDENT] ;

constructor = UPPER_IDENT {type_atom} [":" type] ;

effect_def  = "effect" UPPER_IDENT ":" INDENT {operation NEWLINE} DEDENT ;
operation   = LOWER_IDENT ":" type ;

handler_def = "handler" UPPER_IDENT {handler_param}
              "for" UPPER_IDENT ["!" effect_list] ":"
              INDENT {handler_clause NEWLINE} DEDENT ;

handler_clause = LOWER_IDENT pattern "->" "resume" ":" block
               | "return" pattern "->" block ;

fn_def      = "fn" LOWER_IDENT [type_params] [params]
              ["->" type] ["!" effect_list] ":" block
            | "fn" LOWER_IDENT [type_params] [params]
              ["->" type] ["!" effect_list] "=" expr ;

(* Type parameters and parameters *)
type_params = "[" {type_param ","} "]" ;
type_param  = UPPER_IDENT [":" kind]
            | LOWER_IDENT ":" type ;

params      = "(" {param ","} ")" ;
param       = LOWER_IDENT ":" type ;

(* Types *)
type        = type_atom {"->" type} ["!" effect_list] ;
type_atom   = UPPER_IDENT {type_arg}
            | "(" type ")"
            | "forall" (UPPER_IDENT | "(" LOWER_IDENT ":" type ")") "." type
            | "Lazy" type_atom
            | "ref" ["mut"] type_atom ;

type_arg    = UPPER_IDENT | LOWER_IDENT | "(" type ")" | literal ;

kind        = "Type" [NAT] | "Prop" | "Linear" | kind "->" kind ;

(* Effects *)
effect_list = effect_item {"," effect_item} ;
effect_item = UPPER_IDENT ["@" UPPER_IDENT] ;

(* Expressions *)
expr        = let_expr | match_expr | if_expr | do_expr | with_expr
            | lambda | pipeline ;

let_expr    = "let" pattern [":" type] "=" expr [NEWLINE expr]
            | "let" pattern [":" type] ":" block ;

match_expr  = "match" expr INDENT {match_arm NEWLINE} DEDENT ;
match_arm   = pattern ["|" expr] "->" (expr | block) ;

if_expr     = "if" expr "then" expr "else" expr
            | "if" expr ":" block "else" ":" block ;

do_expr     = "do" ":" block
            | "do" INDENT {do_stmt NEWLINE} expr DEDENT ;
do_stmt     = pattern "<-" UPPER_IDENT "." LOWER_IDENT {expr}
            | "let" pattern "=" expr
            | expr ;

with_expr   = "with" UPPER_IDENT {expr} INDENT expr DEDENT ;

lambda      = "λ" {param} "." expr
            | "\\" {param} "." expr
            | "fn" [params] ":" block ;

pipeline    = app_expr {"|>" app_expr}
            | app_expr {"<|" app_expr} ;

app_expr    = primary {primary}
            | primary INDENT {app_line NEWLINE} DEDENT ;
app_line    = {primary} ;

primary     = LOWER_IDENT
            | UPPER_IDENT
            | literal
            | "(" expr ")"
            | "(" expr "," expr ")"
            | "perform" UPPER_IDENT "." LOWER_IDENT {primary}
            | "lazy" primary
            | "force" primary
            | "ref" ["mut"] primary ;

(* Patterns *)
pattern     = LOWER_IDENT
            | "_"
            | UPPER_IDENT {pattern}
            | "(" pattern "," pattern ")"
            | pattern ":" type
            | literal ;

(* Blocks *)
block       = INDENT {statement NEWLINE} expr DEDENT
            | expr ;
statement   = "let" pattern [":" type] "=" expr
            | expr ;

(* Auxiliaries *)
literal     = NAT | FLOAT | STRING | CHAR | "()" ;
module_path = UPPER_IDENT {"." UPPER_IDENT} ;

(* Lexical *)
LOWER_IDENT = [a-z][a-zA-Z0-9_']* ;
UPPER_IDENT = [A-Z][a-zA-Z0-9_']* ;
NAT         = [0-9]+ | "0x" [0-9a-fA-F]+ | "0b" [01]+ ;
FLOAT       = [0-9]+ "." [0-9]+ (("e"|"E") ("+"|"-")? [0-9]+)? ;
STRING      = '"' ... '"' ;
CHAR        = "'" ... "'" ;
```

---

## Appendix B: Example Programs

### B.1 Hello World

```crisp
module Main

effect Console:
  print : String -> Unit

fn main -> Unit ! Console:
  perform Console.print "Hello, Crisp!"
```

### B.2 Safe List Operations

```crisp
module SafeList

type Vec T (n: Nat):
  Nil  : Vec T 0
  Cons : T -> Vec T m -> Vec T (succ m)

fn head [T, n: Nat] (xs: Vec T (succ n)) -> T:
  match xs
    Cons x _ -> x

fn tail [T, n: Nat] (xs: Vec T (succ n)) -> Vec T n:
  match xs
    Cons _ rest -> rest

fn map [A, B, n: Nat, ε] (f: A -> B ! ε) (xs: Vec A n) -> Vec B n ! ε:
  match xs
    Nil       -> Nil
    Cons x t  -> Cons (f x) (map f t)

fn zip [A, B, n: Nat] (xs: Vec A n) (ys: Vec B n) -> Vec (Pair A B) n:
  match xs
    Nil ->
      match ys
        Nil -> Nil
    Cons x xs' ->
      match ys
        Cons y ys' -> Cons (Pair x y) (zip xs' ys')
```

### B.3 State Handler

```crisp
module StateExample

effect State S:
  get : Unit -> S
  put : S -> Unit

fn increment -> Unit ! State Nat:
  do
    n <- State.get ()
    State.put (succ n)

--- Run stateful computation with initial value
handler RunState (S: Type) (init: S) for State S ! Pure:
  get () -> resume:
    λs. resume s s

  put new_s -> resume:
    λ_. resume () new_s

  return x ->
    λ_. x

fn example -> Nat:
  with RunState Nat 0
    do
      increment
      increment
      increment
      n <- State.get ()
      n
```

### B.4 File Processing Pipeline

```crisp
module FileProcessor

import Core.List: map, fold

effect FileSystem:
  read  : Path -> Bytes
  write : Path -> Bytes -> Unit

effect Log:
  info : String -> Unit

fn process_file (path: Path) -> Nat ! FileSystem, Log:
  do
    perform Log.info (concat "Processing: " (show path))
    contents <- FileSystem.read path
    length contents

fn process_all (paths: List Path) -> Nat ! FileSystem, Log:
  paths
    |> map process_file
    |> fold add 0
```

### B.5 Authorization with Proofs

```crisp
module Authorization

type Authority:
  name  : String
  level : Nat

type Action:
  description    : String
  required_level : Nat

--- Proof that authority can perform action
type prop Authorized (a: Authority) (act: Action):
  authorized : (gte a.level act.required_level) => Authorized a act

--- Attempt to construct authorization proof
fn authorize (a: Authority) (act: Action) -> Option (Authorized a act):
  if gte a.level act.required_level
    then Some authorized
    else None

--- Execute only with proof of authorization
fn execute (a: Authority) (act: Action) (proof: Authorized a act)
    -> Result Unit String ! Log:
  do
    perform Log.info (concat a.name (concat " executing: " act.description))
    Ok ()

fn attempt (a: Authority) (act: Action) -> Result Unit String ! Log:
  match authorize a act
    Some proof -> execute a act proof
    None       -> Err "Insufficient authority"
```

### B.6 Dialectical Engine

```crisp
module Dialectics

type Claim:
  content    : String
  confidence : Float

type DialecticalState:
  Thesis     Claim
  Antithesis Claim Claim
  Synthesis  Claim
  Collapse   String

effect Reason:
  negate    : Claim -> Claim
  reconcile : Claim -> Claim -> Option Claim
  evaluate  : Claim -> Float

fn step (state: DialecticalState) -> DialecticalState ! Reason, Log:
  match state
    Thesis c ->
      do
        perform Log.info "Generating antithesis..."
        anti <- Reason.negate c
        Antithesis c anti

    Antithesis thesis anti ->
      do
        perform Log.info "Attempting synthesis..."
        result <- Reason.reconcile thesis anti
        match result
          Some synth -> Synthesis synth
          None       -> Collapse "Irreconcilable contradiction"

    Synthesis c ->
      do
        quality <- Reason.evaluate c
        if gt quality 0.8
          then state
          else Thesis c

    Collapse _ ->
      state

fn run_dialectic (initial: Claim) (max_steps: Nat)
    -> DialecticalState ! Reason, Log:
  let go state remaining =
    match remaining
      0 -> state
      _ -> match state
        Collapse _ -> state
        _          -> go (step state) (pred remaining)
  go (Thesis initial) max_steps
```

---

## Appendix C: Design Rationale

### C.1 Why Whitespace-Significant Syntax?

**Observation:** Deeply nested expressions are common in functional programming. Parenthetical nesting creates visual noise and right-drift.

**Solution:** Use indentation to express nesting. Each indented block is an argument to the expression above it.

**Trade-off:** Requires careful formatting. Mitigated by mandatory formatter in toolchain.

**Comparison:**
| Style | Code |
|-------|------|
| Lisp | `(process (transform (filter data pred)) config)` |
| ML | `process (transform (filter data pred)) config` |
| Crisp | `process config`<br>`  transform`<br>`    filter data pred` |
| Crisp (pipeline) | `data \|> filter pred \|> transform \|> process config` |

### C.2 Why `Prop` vs `Type`?

**Problem:** Proofs appearing at runtime causes:
- Serialization confusion (can we send proofs over the wire?)
- Optimization barriers (can we eliminate unused proofs?)
- Proof laundering (effects producing fake proofs)

**Solution:** Stratify into `Prop` (erased) and `Type` (runtime). Proofs exist only to satisfy the type checker.

### C.3 Why Authority Doesn't Affect Typing?

**Problem:** If authority affected typing, you couldn't write generic code over effects with different authorities.

**Solution:** Authority is metadata. Type system treats `E @ A` and `E @ B` identically. Authority mismatches are warnings, not errors.

**Benefit:** Programs remain composable. Authority checking is a separate lint pass.

### C.4 Why Effects as Imports?

**Problem:** Most effect systems treat effects as language-internal. This breaks sandboxing.

**Solution:** Effects compile to Wasm imports. The host controls what capabilities are available. A program cannot perform an effect unless the host provides it.

**Benefit:** Perfect alignment with capability-based security.

---

## Appendix D: Open Questions

### D.1 Inference Extent

How much inference is practical with dependent types + effects?

**Current:** Bidirectional with explicit signatures at module boundaries.

**Future:** Investigate liquid-style inference for refinements.

### D.2 Higher-Kinded Types

Full HKT or limited patterns?

**Current:** Limited. `Functor`-style patterns require explicit dictionary passing.

**Future:** May add bounded quantification over type constructors.

### D.3 Row Polymorphism

Should effect rows support row extension?

**Current:** No. Effect polymorphism via variables sufficient.

**Future:** May add for extensible records/variants.

### D.4 Termination Checking

Should Crisp support termination proofs?

**Current:** No. Totality = coverage only.

**Future:** `total fn` annotation with structural recursion checking.

---

## Changelog

| Version | Date | Changes |
|---------|------|---------|
| 0.1.0-draft | 2025-01-25 | Initial specification |
| 0.2.0-draft | 2025-01-25 | Incorporated reviewer feedback; whitespace-significant syntax; proof stratification; clarified authority semantics; clarified totality; added linearity restrictions; CPS invariant; MVP scope markers |

---

*End of Specification*
