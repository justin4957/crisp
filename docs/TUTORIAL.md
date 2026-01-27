# Crisp Tutorial: Semantic Systems Programming

**From Correctness to Auditability — A Practical Introduction**

---

## Introduction

This tutorial introduces Crisp through three complementary domains that showcase its unique capabilities:

1. **A Regulatory Decision Engine** — demonstrating dependent types, proof-carrying authority, and auditable compilation
2. **A Constitutional Procedure Model** — demonstrating legitimacy conflicts, emergency powers, and procedural constraints
3. **A Dialectical Simulation Engine** — demonstrating propositions as types, contradiction handling, and interpretive authority

These aren't arbitrary examples. They reveal Crisp's central thesis:

> *The same language primitives that ensure program correctness also enable modeling of institutions, authority, and ideas — because all three are governed by constraints, consequences, and justification.*

### What Makes Crisp Different?

| Challenge | Other Languages | Crisp |
|-----------|-----------------|-------|
| "Only admins can approve" | Runtime check, hope for tests | Compile-time proof required |
| "This action requires FileSystem" | Implicit, discovered at runtime | Explicit in type signature, verified |
| "Track who authorized what" | Logging, audit trails | Authority carried in types, erased safely |
| "Contradictions must be resolved" | Business logic, conventions | First-class type-level contradictions |
| "Emergency powers expire" | Calendar reminders, manual review | Type-indexed temporal constraints |

### Why WASM?

Crisp compiles to **WebAssembly** — not because it's "fast" or "the future," but because WASM provides:

- **Capability boundaries**: No ambient authority; all effects are explicit imports
- **Semantic choke points**: Effects *must* surface at module boundaries
- **Sandboxed execution**: The runtime cannot cheat
- **Portable artifacts**: Same binary runs in browser, server, or policy engine
- **Typed IR preservation**: Audit artifacts alongside binaries

### Who Is Crisp For?

Crisp is **not** a general-purpose language. It is designed for:

- **Regulated decision systems** where decisions are challenged retroactively
- **Policy-as-code with teeth** where constraints are binding, not advisory
- **Institutional modeling** where authority and legitimacy matter
- **Audit-sensitive domains** where "we forgot to check X" is unacceptable

Crisp expects expert users. It trades approachability for precision.

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

This is not merely encoding permissions. It encodes **legitimacy as a resource that must be presented** — aligning with capability security, proof-carrying code, and Weberian institutional sociology.

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

---

## Part 3: Constitutional Procedure Model

This domain bridges regulatory systems and philosophical reasoning: modeling **legitimacy conflicts, emergency powers, and procedural constraints** in constitutional law.

### 3.1 The Domain

Constitutional procedures involve:
- **Impeachment processes** with required majorities
- **Emergency powers** with expiration and review
- **War authorization** requiring specific institutional actors
- **Judicial review** with legitimacy constraints

These are not arbitrary bureaucratic rules — they encode **who may do what, under what conditions, with what justification**.

### 3.2 Constitutional Roles and Powers

```crisp
-- Branches of government as type-level values
type Branch:
  Legislative
  Executive
  Judicial

-- Chambers within legislative branch
type Chamber:
  House
  Senate

-- Powers indexed by who may exercise them
type Power(branch: Branch):
  name:        String
  constraints: List(Constraint)

-- Specific power types
type ImpeachmentPower   = Power(Legislative)
type VetoPower          = Power(Executive)
type ReviewPower        = Power(Judicial)
```

### 3.3 Procedural State Machine

An impeachment process moves through defined phases:

```crisp
type ImpeachmentPhase:
  NotInitiated
  ArticlesDrafted
  HouseVoted
  SenateTrialPending
  SenateTrialComplete
  Concluded

-- Impeachment indexed by phase
type Impeachment(p: ImpeachmentPhase):
  subject:    Officer
  articles:   List(Article)
  votes:      VoteRecord
  outcome:    Option(Outcome)
```

### 3.4 Majority Requirements as Proofs

The House requires simple majority; the Senate requires two-thirds:

```crisp
-- Proof that simple majority voted yes
type prop SimpleMajority(votes: VoteRecord, chamber: Chamber):
  achieved:
    votes.yes_count(chamber) > votes.total(chamber) / 2
      => SimpleMajority(votes, chamber)

-- Proof that two-thirds supermajority voted yes
type prop SuperMajority(votes: VoteRecord, chamber: Chamber):
  achieved:
    votes.yes_count(chamber) >= (votes.total(chamber) * 2) / 3
      => SuperMajority(votes, chamber)

-- House vote requires simple majority
fn house_vote(
  imp:   Impeachment(ArticlesDrafted),
  votes: VoteRecord,
  proof: SimpleMajority(votes, House)
) -> Impeachment(HouseVoted) ! ConstitutionalRecord:
  do
    perform ConstitutionalRecord.log_house_vote(imp.subject, votes)
    Impeachment {
      subject  = imp.subject,
      articles = imp.articles,
      votes    = imp.votes.merge(votes),
      outcome  = None
    }

-- Senate conviction requires supermajority
fn senate_convict(
  imp:   Impeachment(SenateTrialComplete),
  votes: VoteRecord,
  proof: SuperMajority(votes, Senate)
) -> Impeachment(Concluded) ! ConstitutionalRecord:
  do
    perform ConstitutionalRecord.log_conviction(imp.subject)
    Impeachment {
      subject  = imp.subject,
      articles = imp.articles,
      votes    = imp.votes.merge(votes),
      outcome  = Some(Convicted)
    }
```

**What the types enforce:**
- Cannot convict without Senate supermajority proof
- Cannot skip House vote phase
- All constitutional actions are recorded via effects

### 3.5 Emergency Powers with Expiration

Emergency powers are dangerous because they expand authority. Crisp can encode their constraints:

```crisp
-- Emergency declaration indexed by time bounds
type EmergencyDeclaration(expires: Timestamp):
  declared_by: Executive
  scope:       EmergencyScope
  valid_until: expires

-- Proof that current time is before expiration
type prop NotExpired(now: Timestamp, deadline: Timestamp):
  valid: now < deadline => NotExpired(now, deadline)

-- Actions under emergency require non-expiration proof
fn emergency_action(
  decl:  EmergencyDeclaration(expires),
  now:   Timestamp,
  proof: NotExpired(now, expires),
  auth:  Authority(EmergencyAct)
) -> EmergencyResult ! EmergencyRecord:
  do
    perform EmergencyRecord.log_action(decl, now)
    execute_emergency_measure(decl.scope)
```

**What this prevents:**
- Acting under expired emergency powers
- Emergency actions without proper declaration
- Unrecorded emergency exercises

### 3.6 Judicial Review

The judiciary can invalidate actions, but only with proper jurisdiction:

```crisp
type Jurisdiction:
  Federal
  State(name: String)
  Constitutional

type prop HasJurisdiction(court: Court, matter: LegalMatter):
  established:
    court.jurisdiction.covers(matter) => HasJurisdiction(court, matter)

fn judicial_review(
  action:  ConstitutionalAction,
  court:   Court,
  jproof:  HasJurisdiction(court, action.as_matter),
  finding: ConstitutionalFinding
) -> ReviewOutcome ! JudicialRecord:
  do
    perform JudicialRecord.log_review(court, action, finding)
    match finding
      Unconstitutional -> ReviewOutcome.Invalidated(action)
      Constitutional   -> ReviewOutcome.Upheld(action)
```

### 3.7 Why Constitutional Modeling Matters

This domain demonstrates:

1. **Legitimacy is structural** — not just "who has permission" but "under what procedural conditions"
2. **Time-sensitivity** — emergency powers expire; authorities have temporal bounds
3. **Multi-party constraints** — impeachment requires both chambers with different thresholds
4. **Conflicts surface as type errors** — attempting unconstitutional actions fails to compile

The same Crisp machinery handles regulatory approval and constitutional crisis.

---

## Part 4: Dialectical Simulation Engine

Now we demonstrate that Crisp's machinery applies beyond institutional systems to **formal reasoning with sociological constraints**.

### 4.1 The Domain

Philosophy deals with:
- Claims and counterclaims
- Contradictions
- Synthesis and resolution
- Interpretive authority

These map directly to Crisp's type system. This is **not metaphorical** — it is logic-as-data with authority constraints.

### 4.2 Propositions as Types

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

`Claim(p)` is a type whose values are assertions of proposition `p`.

### 4.3 Contradiction as a Type

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

### 4.4 Dialectical Rules

Synthesis requires a rule that transforms contradictions:

```crisp
-- A dialectical rule transforms contradiction over p into claim about q
type DialecticalRule(p: Prop, q: Prop):
  name:        String
  school:      PhilosophicalSchool
  transform:   Contradiction(p) -> q
```

### 4.5 Authority in Interpretation

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

### 4.6 Example: Hegelian Synthesis

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

### 4.7 Invalid Synthesis Doesn't Typecheck

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

### 4.8 The Unifying Abstraction

All three domains share the same structure:

| Domain | Input State | Authority | Constraint | Output State | Effect |
|--------|-------------|-----------|------------|--------------|--------|
| Regulatory | Decision(Reviewed) | CanApprove | SatisfiesPolicy | Decision(Approved) | Decide |
| Constitutional | Impeachment(HouseVoted) | SenatePower | SuperMajority | Impeachment(Concluded) | ConstitutionalRecord |
| Dialectical | Contradiction(p) | InterpretAuthority | DialecticalRule | Claim(q) | Interpret |

The abstraction is **justified transformation**: constrained state changes requiring authority and proof.

---

## Part 5: Audit Artifact Inspection

A critical Crisp feature is **auditable compilation**. This section shows exactly what an auditor receives and what guarantees they can rely on.

### 5.1 The Compilation Artifacts

When you compile a Crisp module:

```bash
crisp compile RegulatoryEngine.crisp --emit-tir --emit-manifest
```

You get three artifacts:

1. **`RegulatoryEngine.wasm`** — the executable binary
2. **`RegulatoryEngine.tir`** — the Typed Intermediate Representation
3. **`RegulatoryEngine.manifest.json`** — capability and hash manifest

### 5.2 Reading the Manifest

```json
{
  "module": "RegulatoryEngine",
  "version": "1.0.0",
  "compiler": {
    "name": "crisp",
    "version": "0.1.0"
  },
  "capabilities": {
    "required": [
      "Decide.record_approval",
      "Decide.record_denial"
    ],
    "optional": []
  },
  "authorities": {
    "Decide": {
      "operations": ["record_approval", "record_denial"],
      "must_be_provided": true
    }
  },
  "hashes": {
    "source": "sha256:a1b2c3d4e5f6...",
    "wasm": "sha256:f6e5d4c3b2a1...",
    "tir": "sha256:1a2b3c4d5e6f..."
  },
  "proofs_required": [
    "SatisfiesPolicy(evidence, StandardPolicy) for approve",
    "ViolatesPolicy(evidence, StandardPolicy) for deny"
  ],
  "build_time": "2026-01-27T10:30:00Z"
}
```

**What the auditor learns:**
- Exactly which effects the module can perform
- That proofs were required at specific call sites
- Cryptographic binding between source, TIR, and WASM

### 5.3 Reading the Typed IR

The TIR preserves type information that WASM erases:

```
-- TIR excerpt for approve function
fn approve : (
  d:     Decision(Reviewed),
  auth:  CanApprove,
  proof: SatisfiesPolicy(d.evidence, StandardPolicy)  -- PROOF PARAMETER
) -> Decision(Approved) ! Decide

-- Proof was erased but structure preserved:
approve_call_site_1:
  requires_proof: SatisfiesPolicy
  evidence_source: reviewed_decision.evidence
  policy: StandardPolicy
  erased_at: codegen
```

**What the auditor can verify:**
- Every `approve` call had a valid proof at compile time
- The proof related the actual evidence to the actual policy
- No "escape hatches" bypassed the proof requirement

### 5.4 What Auditors Don't Need to Trust

Because of Crisp's design, auditors do **not** need to trust:

| Concern | Why It's Not a Trust Issue |
|---------|---------------------------|
| "Did they check permissions?" | Authority is a required parameter — no parameter, no compilation |
| "Did they validate evidence?" | Proof is a required parameter — no proof, no compilation |
| "What effects can this code perform?" | Effects are in the type signature and WASM imports |
| "Could they have cheated?" | WASM sandbox prevents ambient authority |
| "Is this the right binary?" | Hash chain from source → TIR → WASM |

The auditor trusts the **compiler**, not the **programmer**.

### 5.5 WASM Import Verification

The WASM binary declares its imports explicitly:

```wat
(module
  (import "Decide" "record_approval"
    (func $decide_record_approval (param i32 i32) (result i32)))
  (import "Decide" "record_denial"
    (func $decide_record_denial (param i32 i32) (result i32)))

  ;; No other imports — module cannot perform other effects
)
```

An auditor can inspect this with standard WASM tools:

```bash
wasm-objdump -x RegulatoryEngine.wasm | grep import
```

If the module tried to perform `FileSystem.read`, it would:
1. Fail to compile (effect not declared)
2. Have no corresponding WASM import (host won't provide it)

---

## Part 6: What Crisp Refuses To Do

Constraints build trust. Crisp is defined as much by what it **forbids** as by what it enables.

### 6.1 No Ambient IO

```crisp
-- This is IMPOSSIBLE in Crisp:
fn sneaky_read() -> String:
  read_file("/etc/passwd")  -- Error: no FileSystem effect declared

-- You must declare effects:
fn honest_read(path: Path) -> String ! FileSystem:
  perform FileSystem.read(path)  -- Effect is visible in type
```

There is no way to perform IO without declaring it. No `unsafePerformIO`. No escape hatches.

### 6.2 No Reflection-Based Permission Checks

```crisp
-- This pattern is IMPOSSIBLE:
fn check_at_runtime(user: User, action: Action) -> Bool:
  user.roles.contains(action.required_role)  -- Runtime check

-- Authority must be a VALUE you possess:
fn authorized_action(
  auth: Authority(action)  -- Must have this to call
) -> Result ! Audit:
  perform Audit.log(auth)
  execute(action)
```

You cannot "check" if you have authority. You either have the value or you don't.

### 6.3 No Escape Hatches Without Type-Level Consequences

```crisp
-- There is no "trust me" annotation:
fn bypass_proof(d: Decision(Reviewed)) -> Decision(Approved):
  -- Error: cannot construct Decision(Approved) without going through approve
  -- which requires CanApprove and SatisfiesPolicy

-- If you need to skip proof, you must change the TYPE:
type UncheckedDecision(s: Status):
  -- Different type, different guarantees
  -- Auditor sees this is not a proven Decision
```

Every "escape" creates a different type that auditors can distinguish.

### 6.4 No Implicit Effect Propagation

```crisp
-- Effects don't "leak" silently:
fn pure_function(x: Int) -> Int:
  -- Cannot call any effectful function here
  -- Even through indirection

fn caller() -> Int:
  pure_function(effectful_thing())
  -- Error: effectful_thing requires ! Effect
  -- but pure_function's argument position is pure
```

If a function is pure, it cannot invoke effects — period.

### 6.5 No Proof Forgery

```crisp
-- Proofs cannot be constructed arbitrarily:
let fake_proof: SatisfiesPolicy(bad_evidence, Policy) = ???
-- Error: SatisfiesPolicy can only be constructed via its constructors
-- which require actual evidence satisfaction

-- You cannot "cast" to a proof type:
let fake = unsafe_coerce(unit, SatisfiesPolicy(...))
-- Error: unsafe_coerce does not exist
```

Proofs must be constructed through their defined constructors, which encode actual logical requirements.

---

## Part 7: Complete Example Module

### 7.1 A Complete Regulatory Engine

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

### 7.2 Using the Module

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

---

## Part 8: Key Takeaways

### What Crisp Enforces at Compile Time

| Property | How |
|----------|-----|
| Valid state transitions | Indexed types |
| Required authority | Authority parameters |
| Policy compliance | Proof terms |
| Effect boundaries | Effect system |
| Exhaustive handling | Pattern coverage |
| Temporal constraints | Time-indexed types |

### What WASM Provides at Runtime

| Property | How |
|----------|-----|
| Sandboxed execution | WASM memory isolation |
| Explicit capabilities | Effects as imports |
| Portable artifacts | WASM binary format |
| Deterministic execution | WASM semantics |
| No ambient authority | Import-only effects |

### The Central Insight

Crisp doesn't just prevent bugs — it makes **invalid programs inexpressible**.

- An approval without authority? Won't compile.
- A synthesis without the right school? Won't compile.
- A state transition that skips steps? Won't compile.
- An effect that wasn't declared? Won't compile.
- Emergency action after expiration? Won't compile.
- Conviction without supermajority? Won't compile.

**The natural way to write correct programs is the natural way to write auditable programs.**

---

## Appendix A: Comparison with Other Approaches

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

---

## Appendix B: Realistic Adoption Expectations

Crisp is **not** a general-purpose language. It will not replace Python, JavaScript, or even Rust for most tasks.

### Expected Adoption Profile

- **Slow adoption**: The concepts require expertise
- **Expert users**: Type theory background helpful
- **High per-user value**: Significant benefit in appropriate domains
- **Niche dominance**: Potential standard for regulated decision systems

### Appropriate Domains

1. **Regulated decision engines**: Export control, compliance, permitting
2. **Policy-as-code**: Where constraints must be binding, not advisory
3. **Institutional simulation**: Legal systems, governance modeling
4. **Audit-critical systems**: Where "we forgot to check" is catastrophic

### Inappropriate Domains

- Rapid prototyping
- Scripting and automation
- Performance-critical systems (until tooling matures)
- Teams without type theory experience

Crisp trades breadth for depth. That is intentional.

---

## Next Steps

- Read the [Language Specification](SPECIFICATION.md) for complete syntax
- Explore the [examples/](../examples/) directory
- Run the REPL: `crisp repl`
- Check the [Implementation Plan](IMPLEMENTATION_PLAN.md) for current status

---

*Crisp: A language for constrained, auditable transformations — where correctness and legitimacy are the same thing.*
