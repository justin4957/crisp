# Crisp Tutorial: Semantic Systems Programming

**From Correctness to Auditability — A Practical Introduction**

---

## Introduction

This tutorial introduces Crisp through two complementary domains that showcase its unique capabilities:

1. **A Regulatory Decision Engine** — demonstrating dependent types, proof-carrying authority, and auditable compilation
2. **A Dialectical Simulation Engine** — demonstrating propositions as types, contradiction handling, and interpretive authority

These aren't arbitrary examples. They reveal Crisp's central thesis:

> *The same language primitives that ensure program correctness also enable modeling of institutions, authority, and ideas — because all three are governed by constraints, consequences, and justification.*

### What Makes Crisp Different?

| Challenge | Other Languages | Crisp |
|-----------|-----------------|-------|
| "Only admins can approve" | Runtime check, hope for tests | Compile-time proof required |
| "This action requires FileSystem" | Implicit, discovered at runtime | Explicit in type signature, verified |
| "Track who authorized what" | Logging, audit trails | Authority carried in types, erased safely |
| "Contradictions must be resolved" | Business logic, conventions | First-class type-level contradictions |

Crisp compiles to **WebAssembly**, which means:
- Sandboxed execution with no ambient authority
- Effects become explicit WASM imports
- Portable artifacts that run anywhere
- Typed IR preserved alongside binaries for audit

---

## Part 1: Foundations

Before diving into the domains, let's establish Crisp's core features.

### 1.1 Basic Types and Functions

```crisp
-- Simple values
let answer = 42
let greeting = "Hello, Crisp!"
let active = true

-- Function definition
fn double(n: Int) -> Int:
  n * 2

-- Function with multiple parameters
fn add(x: Int, y: Int) -> Int:
  x + y

-- Using functions
let result = double(21)      -- 42
let sum = add(10, 32)        -- 42
```

### 1.2 Algebraic Data Types

```crisp
-- Sum type (tagged union)
type Option(A):
  None
  Some(A)

-- Using Option
fn safe_head(xs: List(Int)) -> Option(Int):
  match xs
    Nil        -> None
    Cons(h, _) -> Some(h)

-- Product type (record)
type Person:
  name: String
  age:  Int

let alice = Person { name = "Alice", age = 30 }
```

### 1.3 Pattern Matching

Pattern matching in Crisp must be **exhaustive** — the compiler ensures you handle all cases:

```crisp
fn describe(opt: Option(Int)) -> String:
  match opt
    None     -> "nothing"
    Some(0)  -> "zero"
    Some(n)  -> concat("number: ", show(n))

-- Guards for conditional matching
fn classify(n: Int) -> String:
  match n
    _ | n < 0  -> "negative"
    _ | n == 0 -> "zero"
    _          -> "positive"
```

### 1.4 Effects: Making Side Effects Explicit

In Crisp, functions that perform effects declare them in their type:

```crisp
-- This function performs Log effects
fn greet(name: String) -> Unit ! Log:
  perform Log.info(concat("Hello, ", name))

-- This function performs both FileSystem and Log effects
fn process_file(path: Path) -> Data ! FileSystem, Log:
  do
    perform Log.info("Reading file...")
    let content = perform FileSystem.read(path)
    perform Log.info("Processing complete")
    parse(content)
```

The `!` syntax declares effects. A function without `!` is **pure** — it cannot perform any effects.

**Why this matters:**
- You can see a function's effects from its type
- Pure functions are guaranteed pure
- The compiler tracks effect propagation
- WASM compilation turns effects into explicit imports

### 1.5 Effect Handlers

Effects are handled by providing implementations:

```crisp
-- Define an effect
effect Log:
  info(message: String) -> Unit
  error(message: String) -> Unit

-- Handler that writes to console
handler ConsoleLog for Log:
  info(msg) -> resume:
    -- Implementation provided by WASM host
    console_write(concat("[INFO] ", msg))
    resume(())

  error(msg) -> resume:
    console_write(concat("[ERROR] ", msg))
    resume(())

  return(x) -> x

-- Handler that collects logs silently
handler SilentLog for Log:
  info(_) -> resume:
    resume(())  -- Do nothing

  error(_) -> resume:
    resume(())

  return(x) -> x

-- Using handlers
let result =
  with ConsoleLog:
    greet("World")
```

---

## Part 2: Regulatory Decision Engine

Now we build something real: a system where **authorization is enforced by the type system**, not runtime checks.

### 2.1 The Domain

Consider regulatory review: export control, environmental compliance, financial oversight. These systems share common patterns:

- Decisions move through defined states
- Transitions require authority
- Authority must be justified
- The process must be auditable

Let's encode this in Crisp.

### 2.2 Status as an Index

First, we define decision status — not as a runtime value, but as a **type-level index**:

```crisp
-- Status exists at the type level
type Status:
  Draft
  Submitted
  Reviewed
  Approved
  Denied

-- Decision is indexed by its status
type Decision(s: Status):
  subject:  Entity
  evidence: EvidenceSet
  notes:    String
```

A `Decision(Draft)` and a `Decision(Approved)` are **different types**. You cannot accidentally treat one as the other.

### 2.3 State Transitions as Functions

Now we encode valid state transitions:

```crisp
-- Anyone can create a draft
fn create_draft(subject: Entity) -> Decision(Draft):
  Decision {
    subject  = subject,
    evidence = EvidenceSet.empty,
    notes    = ""
  }

-- Submitting moves Draft -> Submitted
fn submit(d: Decision(Draft)) -> Decision(Submitted):
  Decision {
    subject  = d.subject,
    evidence = d.evidence,
    notes    = d.notes
  }

-- Only Submitted decisions can be reviewed
fn review(d: Decision(Submitted), findings: Findings) -> Decision(Reviewed):
  Decision {
    subject  = d.subject,
    evidence = d.evidence.add_findings(findings),
    notes    = d.notes
  }
```

**What the compiler enforces:**
- You cannot submit an already-approved decision
- You cannot review a draft (must submit first)
- The state machine is encoded in types

### 2.4 Authority as a Value

Now the key insight: **authority is not a boolean check — it's a value you must possess**.

```crisp
-- Authority is parameterized by what it authorizes
type Authority(Action):
  granted_by: Institution
  scope:      Scope
  expires:    Timestamp

-- Specific authority types
type CanApprove  = Authority(Approve)
type CanDeny     = Authority(Deny)
type CanReview   = Authority(Review)
```

### 2.5 Proof-Carrying Approval

Approval requires three things:
1. A reviewed decision
2. Authority to approve
3. **Proof** that evidence satisfies policy

```crisp
-- Propositions live in Prop (erased at runtime)
type prop SatisfiesPolicy(evidence: EvidenceSet, policy: Policy):
  satisfied:
    policy.requirements.all_met(evidence) => SatisfiesPolicy(evidence, policy)

-- Approval function — note the proof parameter
fn approve(
  d:     Decision(Reviewed),
  auth:  CanApprove,
  proof: SatisfiesPolicy(d.evidence, StandardPolicy)
) -> Decision(Approved) ! Decide:
  do
    perform Decide.record_approval(d.subject, auth.granted_by)
    Decision {
      subject  = d.subject,
      evidence = d.evidence,
      notes    = concat(d.notes, "\nApproved by: ", show(auth.granted_by))
    }
```

**What happens here:**
- `proof: SatisfiesPolicy(d.evidence, StandardPolicy)` — caller must provide proof
- The proof is **erased at runtime** (it's in `Prop`)
- But it must **exist at compile time**
- The `Decide` effect tracks that an approval occurred

### 2.6 Why This Is Hard Elsewhere

```java
// Java: authority is a runtime check
public Decision approve(Decision d, User user) {
    if (!user.hasRole("APPROVER")) {
        throw new UnauthorizedException();
    }
    if (!d.getStatus().equals(Status.REVIEWED)) {
        throw new InvalidStateException();
    }
    // Hope the evidence was checked somewhere...
    return d.setStatus(Status.APPROVED);
}
```

Problems:
- Wrong status? Runtime exception
- Missing authority? Runtime exception
- Evidence check? Somewhere else, maybe
- Audit trail? Manual logging

```crisp
-- Crisp: authority is a compile-time requirement
fn approve(
  d:     Decision(Reviewed),      -- Wrong status? Won't compile
  auth:  CanApprove,              -- No authority? Can't call
  proof: SatisfiesPolicy(...)     -- No proof? Won't compile
) -> Decision(Approved) ! Decide  -- Audit effect is declared
```

**The invalid program cannot be written.**

### 2.7 Denial Path

For completeness, denial follows the same pattern:

```crisp
type prop ViolatesPolicy(evidence: EvidenceSet, policy: Policy):
  violated:
    policy.requirements.any_failed(evidence) => ViolatesPolicy(evidence, policy)

fn deny(
  d:     Decision(Reviewed),
  auth:  CanDeny,
  proof: ViolatesPolicy(d.evidence, StandardPolicy)
) -> Decision(Denied) ! Decide:
  do
    perform Decide.record_denial(d.subject, auth.granted_by)
    Decision {
      subject  = d.subject,
      evidence = d.evidence,
      notes    = concat(d.notes, "\nDenied by: ", show(auth.granted_by))
    }
```

### 2.8 The WASM Artifact

When compiled, Crisp produces:

**1. The WASM binary** with explicit imports:
```wat
(import "Decide" "record_approval" (func ...))
(import "Decide" "record_denial" (func ...))
```

**2. A typed manifest** documenting:
- Required capabilities (effects)
- Authority requirements
- Hash of source and compiled artifacts

**3. The Typed IR** preserving:
- All type information
- Proof structure (for audit, not execution)
- Effect annotations

An auditor can verify:
- What authorities the module requires
- What effects it can perform
- That proofs existed at compile time

---

## Part 3: Dialectical Simulation Engine

Now we demonstrate that Crisp's machinery applies beyond institutional systems.

### 3.1 The Domain

Philosophy deals with:
- Claims and counterclaims
- Contradictions
- Synthesis and resolution
- Interpretive authority

These map directly to Crisp's type system.

### 3.2 Propositions as Types

```crisp
-- Prop is the universe of propositions
type Prop: Type

-- A claim asserts a proposition
type Claim(p: Prop):
  asserted_by: Agent
  content:     p

-- Negation
type Not(p: Prop): Prop
```

This is **not metaphorical**. `Claim(p)` is a type whose values are assertions of proposition `p`.

### 3.3 Contradiction as a Type

A contradiction is a pair of claims asserting `p` and `Not(p)`:

```crisp
type Contradiction(p: Prop):
  thesis:     Claim(p)
  antithesis: Claim(Not(p))
```

You can construct a contradiction:

```crisp
-- Example propositions
type prop JusticeRequiresEquality: Prop
type prop JusticeRequiresMerit: Prop

-- These might contradict
let thesis = Claim {
  asserted_by = Agent.named("Rawls"),
  content     = JusticeRequiresEquality.axiom
}

let antithesis = Claim {
  asserted_by = Agent.named("Nozick"),
  content     = Not(JusticeRequiresEquality).from(JusticeRequiresMerit.axiom)
}

let contradiction = Contradiction {
  thesis     = thesis,
  antithesis = antithesis
}
```

### 3.4 Dialectical Rules

Synthesis requires a rule that transforms contradictions:

```crisp
-- A dialectical rule transforms contradiction over p into claim about q
type DialecticalRule(p: Prop, q: Prop):
  name:        String
  school:      PhilosophicalSchool
  transform:   Contradiction(p) -> q
```

### 3.5 Authority in Interpretation

Not everyone can legitimately synthesize. Interpretive authority matters:

```crisp
type InterpretAuthority(school: PhilosophicalSchool):
  tradition:  school
  lineage:    List(Thinker)

-- Synthesis requires authority
fn synthesize(
  contra: Contradiction(p),
  rule:   DialecticalRule(p, q),
  auth:   InterpretAuthority(rule.school)
) -> Claim(q) ! Interpret:
  do
    perform Interpret.record_synthesis(contra, rule, auth)
    Claim {
      asserted_by = Agent.school(auth.tradition),
      content     = rule.transform(contra)
    }
```

**The parallel to regulatory approval is exact:**
- State transition → Dialectical transformation
- Reviewed decision → Contradiction
- Authority to approve → Authority to interpret
- Policy satisfaction → Dialectical rule
- Decide effect → Interpret effect

### 3.6 Example: Hegelian Synthesis

```crisp
-- Define a synthesis rule
let hegelian_synthesis: DialecticalRule(JusticeRequiresEquality, JusticeSynthesis) =
  DialecticalRule {
    name      = "Aufhebung",
    school    = Hegelian,
    transform = fn(c: Contradiction(JusticeRequiresEquality)) ->
      JusticeSynthesis.from_sublation(c.thesis, c.antithesis)
  }

-- Someone with Hegelian authority can apply it
fn apply_hegelian_synthesis(
  contra: Contradiction(JusticeRequiresEquality),
  auth:   InterpretAuthority(Hegelian)
) -> Claim(JusticeSynthesis) ! Interpret:
  synthesize(contra, hegelian_synthesis, auth)
```

### 3.7 Invalid Synthesis Doesn't Typecheck

What if someone tries to synthesize without authority?

```crisp
-- This won't compile: no authority parameter
fn invalid_synthesis(contra: Contradiction(p)) -> Claim(q):
  -- Error: cannot call synthesize without InterpretAuthority
  synthesize(contra, some_rule, ???)
```

What if someone applies the wrong school's rule?

```crisp
fn mismatched_authority(
  contra: Contradiction(p),
  rule:   DialecticalRule(p, q),          -- Hegelian rule
  auth:   InterpretAuthority(Analytic)    -- Analytic authority
) -> Claim(q) ! Interpret:
  -- Error: auth.school ≠ rule.school
  synthesize(contra, rule, auth)
```

**Philosophy becomes executable, auditable, and authority-aware.**

### 3.8 Why This Matters

This tutorial demonstrates that Crisp's features aren't narrow:

| Feature | Regulatory Use | Philosophical Use |
|---------|---------------|-------------------|
| Indexed types | Decision states | Proposition content |
| Proof terms | Policy satisfaction | Dialectical rules |
| Authority values | Institutional authority | Interpretive authority |
| Effects | Decide, Audit | Interpret, Record |
| WASM sandbox | Capability isolation | Portable reasoning engine |

The same language models both because **both involve constrained transformations justified by authority**.

---

## Part 4: Putting It Together

### 4.1 A Complete Module

```crisp
module RegulatoryEngine
  requires
    effect Decide
    effect Log
  provides
    fn create_draft
    fn submit
    fn review
    fn approve
    fn deny
    type Decision
    type Authority

-- Status as type-level value
type Status:
  Draft
  Submitted
  Reviewed
  Approved
  Denied

-- Decision indexed by status
type Decision(s: Status):
  subject:  Entity
  evidence: EvidenceSet
  notes:    String

-- Authority types
type Authority(Action):
  granted_by: Institution
  scope:      Scope

type CanApprove = Authority(Approve)
type CanDeny   = Authority(Deny)

-- Proof types (erased at runtime)
type prop SatisfiesPolicy(e: EvidenceSet, p: Policy):
  satisfied: p.check(e) => SatisfiesPolicy(e, p)

type prop ViolatesPolicy(e: EvidenceSet, p: Policy):
  violated: not(p.check(e)) => ViolatesPolicy(e, p)

-- State transitions
fn create_draft(subject: Entity) -> Decision(Draft):
  Decision { subject = subject, evidence = EvidenceSet.empty, notes = "" }

fn submit(d: Decision(Draft)) -> Decision(Submitted):
  Decision { subject = d.subject, evidence = d.evidence, notes = d.notes }

fn review(d: Decision(Submitted), findings: Findings) -> Decision(Reviewed):
  Decision {
    subject  = d.subject,
    evidence = d.evidence.add(findings),
    notes    = d.notes
  }

fn approve(
  d:     Decision(Reviewed),
  auth:  CanApprove,
  proof: SatisfiesPolicy(d.evidence, StandardPolicy)
) -> Decision(Approved) ! Decide:
  do
    perform Decide.record_approval(d.subject, auth.granted_by)
    Decision {
      subject  = d.subject,
      evidence = d.evidence,
      notes    = concat(d.notes, "\nApproved by: ", show(auth.granted_by))
    }

fn deny(
  d:     Decision(Reviewed),
  auth:  CanDeny,
  proof: ViolatesPolicy(d.evidence, StandardPolicy)
) -> Decision(Denied) ! Decide:
  do
    perform Decide.record_denial(d.subject, auth.granted_by)
    Decision {
      subject  = d.subject,
      evidence = d.evidence,
      notes    = concat(d.notes, "\nDenied by: ", show(auth.granted_by))
    }
```

### 4.2 Using the Module

```crisp
module Main
  requires
    effect Decide
    effect Log
  import RegulatoryEngine

fn process_application(
  subject: Entity,
  evidence: EvidenceSet,
  auth: CanApprove
) -> Decision(Approved) ! Decide, Log:
  do
    perform Log.info("Creating draft...")
    let draft = create_draft(subject)

    perform Log.info("Submitting...")
    let submitted = submit(draft)

    perform Log.info("Reviewing...")
    let reviewed = review(submitted, evaluate(evidence))

    perform Log.info("Checking policy...")
    -- proof must be constructed from evidence
    let proof = SatisfiesPolicy.check(reviewed.evidence, StandardPolicy)

    perform Log.info("Approving...")
    approve(reviewed, auth, proof)
```

### 4.3 The Compiled Artifact

After `crisp compile RegulatoryEngine.crisp`:

**WASM imports:**
```wat
(import "Decide" "record_approval" (func $decide_record_approval (param i32 i32)))
(import "Decide" "record_denial" (func $decide_record_denial (param i32 i32)))
```

**Manifest (manifest.json):**
```json
{
  "module": "RegulatoryEngine",
  "version": "1.0",
  "capabilities": ["Decide.record_approval", "Decide.record_denial"],
  "authorities": {
    "Decide": "required"
  },
  "hashes": {
    "source": "sha256:abc123...",
    "wasm": "sha256:def456...",
    "tir": "sha256:789ghi..."
  }
}
```

**Typed IR** preserves proof structure for audit without runtime cost.

---

## Part 5: Key Takeaways

### What Crisp Enforces at Compile Time

| Property | How |
|----------|-----|
| Valid state transitions | Indexed types |
| Required authority | Authority parameters |
| Policy compliance | Proof terms |
| Effect boundaries | Effect system |
| Exhaustive handling | Pattern coverage |

### What WASM Provides at Runtime

| Property | How |
|----------|-----|
| Sandboxed execution | WASM memory isolation |
| Explicit capabilities | Effects as imports |
| Portable artifacts | WASM binary format |
| Deterministic execution | WASM semantics |

### The Central Insight

Crisp doesn't just prevent bugs — it makes **invalid programs inexpressible**.

- An approval without authority? Won't compile.
- A synthesis without the right school? Won't compile.
- A state transition that skips steps? Won't compile.
- An effect that wasn't declared? Won't compile.

**The natural way to write correct programs is the natural way to write auditable programs.**

---

## Next Steps

- Read the [Language Specification](SPECIFICATION.md) for complete syntax
- Explore the [examples/](../examples/) directory
- Run the REPL: `crisp repl`
- Check the [Implementation Plan](IMPLEMENTATION_PLAN.md) for current status

---

## Appendix: Comparison with Other Approaches

### vs. Haskell

Haskell has type-level programming but:
- Effects are monadic (transformers are complex)
- No built-in authority tracking
- No WASM target with capability model
- Proofs require external tools (LiquidHaskell)

### vs. Rust

Rust has strong safety but:
- No dependent types
- No algebraic effects
- Authority is convention-based
- Can't express proof-carrying parameters

### vs. Idris/Agda

These have dependent types but:
- Academic focus, less practical tooling
- Not designed for institutional modeling
- No effect-as-capability model
- Heavy runtime or compilation overhead

### vs. Gleam

Gleam has algebraic types and BEAM but:
- No dependent types
- No proof terms
- Effects are implicit in BEAM
- Can't enforce authority at compile time

Crisp occupies a unique position: **practical dependent types with algebraic effects, compiling to a capability-aware runtime (WASM), designed for systems where correctness and authority matter.**
