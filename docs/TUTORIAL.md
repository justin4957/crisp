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
| "Only admins can approve" | Runtime check, mass email when it fails | Compile-time proof required |
| "This action requires FileSystem" | Discovered in production at 3am | Explicit in type signature, verified |
| "Track who authorized what" | Logging, audit trails, blame assignment | Authority carried in types, erased safely |
| "Contradictions must be resolved" | Meetings. So many meetings. | First-class type-level contradictions |
| "Emergency powers expire" | Calendar reminders, wishful thinking | Type-indexed temporal constraints |

### Why WASM?

Crisp compiles to **WebAssembly** — not because it's "fast" or "the future," but because WASM provides:

- **Capability boundaries**: No ambient authority; all effects are explicit imports
- **Semantic choke points**: Effects *must* surface at module boundaries
- **Sandboxed execution**: The runtime cannot cheat (unlike certain contractors)
- **Portable artifacts**: Same binary runs in browser, server, or policy engine
- **Typed IR preservation**: Audit artifacts alongside binaries

### Who Is Crisp For?

Crisp is **not** a general-purpose language. It is designed for:

- **Regulated decision systems** where decisions are challenged retroactively (and they will be)
- **Policy-as-code with teeth** where constraints are binding, not "guidelines"
- **Institutional modeling** where authority and legitimacy matter
- **Audit-sensitive domains** where "we forgot to check X" ends careers

Crisp expects expert users. It trades approachability for precision. If you wanted easy, you'd have picked Python and mass-emailed compliance when things broke.

---

## Part 1: Foundations

Before diving into the domains, let's establish Crisp's core features.

### 1.1 Basic Types and Functions

```crisp
-- Simple values (the exciting stuff)
let answer = 42
let status_message = "Per my last email"
let is_meeting_necessary = false  -- Always false, yet here we are

-- Function definition
fn double(n: Int) -> Int:
  n * 2

-- Function with multiple parameters
fn add_to_budget(current: Int, request: Int) -> Int:
  current + request  -- Optimistic, assuming approval

-- Using functions
let revised_estimate = double(initial_estimate)  -- Standard practice
let total = add_to_budget(10, 32)                -- Will need committee review
```

### 1.2 Algebraic Data Types

```crisp
-- Sum type: the decision has been made, or it hasn't
type RequestStatus(A):
  Pending                    -- The natural state
  Approved(A)                -- Rare, cherish it
  Rejected(reason: String)   -- "Budget constraints"
  InCommittee                 -- Indefinitely

-- Using RequestStatus
fn check_request(r: Request) -> RequestStatus(Approval):
  match r.submitted_weeks_ago
    n | n < 2  -> Pending
    n | n < 8  -> InCommittee
    _          -> Rejected("Please resubmit with updated forms")

-- Product type (record)
type Employee:
  name:             String
  badge_number:     Int
  forms_completed:  Int
  forms_required:   Int  -- Always forms_completed + 1

let new_hire = Employee {
  name            = "Alice",
  badge_number    = 4247,
  forms_completed = 12,
  forms_required  = 13
}
```

### 1.3 Pattern Matching

Pattern matching in Crisp must be **exhaustive** — the compiler ensures you handle all cases. Unlike your inbox.

```crisp
fn describe_status(s: RequestStatus(Approval)) -> String:
  match s
    Pending           -> "Awaiting review (check back never)"
    Approved(a)       -> concat("Approved: ", show(a))
    Rejected(reason)  -> concat("Denied: ", reason)
    InCommittee       -> "A decision is expected Q4 2087"

-- Guards for conditional matching
fn classify_urgency(days_waiting: Int) -> String:
  match days_waiting
    _ | days_waiting < 0   -> "Time travel detected"
    _ | days_waiting == 0  -> "Just submitted"
    _ | days_waiting < 30  -> "Normal processing"
    _ | days_waiting < 90  -> "Expedited (theoretically)"
    _                      -> "Have you tried resubmitting?"
```

### 1.4 Effects: Making Side Effects Explicit

In Crisp, functions that perform effects declare them in their type. No surprises. No "I didn't know it called the database."

```crisp
-- This function performs Log effects
fn acknowledge_receipt(form_id: FormId) -> Unit ! Log:
  perform Log.info(concat("Form received. Sincerely, The System."))

-- This function performs both FileSystem and Log effects
fn process_submission(path: Path) -> FormData ! FileSystem, Log:
  do
    perform Log.info("Processing submission...")
    let content = perform FileSystem.read(path)
    perform Log.info("Submission processed. Thank you for your patience.")
    parse_form(content)
```

The `!` syntax declares effects. A function without `!` is **pure** — it cannot perform any effects. It cannot send emails. It cannot "just quickly check" the database. It computes and returns. That's it.

**Why this matters:**
- You can see a function's effects from its type
- Pure functions are guaranteed pure (not "pure except for logging")
- The compiler tracks effect propagation
- WASM compilation turns effects into explicit imports

### 1.5 Effect Handlers

Effects are handled by providing implementations:

```crisp
-- Define an effect
effect Notify:
  send_email(to: Email, subject: String, body: String) -> Unit
  send_urgent(to: Email, matter: String) -> Unit  -- For "urgent" matters

-- Handler that actually sends emails
handler ProductionNotify for Notify:
  send_email(to, subject, body) -> resume:
    smtp_send(to, subject, body)
    resume(())

  send_urgent(to, matter) -> resume:
    smtp_send(to, concat("URGENT: ", matter), "Please advise.")
    resume(())

  return(x) -> x

-- Handler for testing (or for when email is "down")
handler QuietNotify for Notify:
  send_email(_, _, _) -> resume:
    resume(())  -- Email sent successfully to /dev/null

  send_urgent(_, _) -> resume:
    resume(())  -- Urgency acknowledged and ignored

  return(x) -> x

-- Using handlers
let result =
  with QuietNotify:  -- Recommended for sanity
    process_all_notifications()
```

---

## Part 2: Regulatory Decision Engine

Now we build something real: a system where **authorization is enforced by the type system**, not by Karen in Compliance sending reminder emails.

### 2.1 The Domain

Consider regulatory review: export control, environmental compliance, financial oversight. These systems share common patterns:

- Decisions move through defined states (slowly)
- Transitions require authority (and signatures)
- Authority must be justified (in triplicate)
- The process must be auditable (for the next decade)

Let's encode this in Crisp.

### 2.2 Status as an Index

First, we define decision status — not as a runtime value, but as a **type-level index**:

```crisp
-- Status exists at the type level
type Status:
  Draft           -- The "I'll finish it tomorrow" phase
  Submitted       -- Your problem now
  UnderReview     -- Could be minutes, could be months
  Approved        -- Miracles happen
  Denied          -- "At this time"
  Appealed        -- Hope springs eternal

-- Decision is indexed by its status
type Decision(s: Status):
  case_number:  CaseId
  subject:      Entity
  evidence:     EvidenceSet
  notes:        String  -- "Per previous discussion"
```

A `Decision(Draft)` and a `Decision(Approved)` are **different types**. You cannot accidentally treat one as the other. The compiler has no "I'm sure it's fine" mode.

### 2.3 State Transitions as Functions

Now we encode valid state transitions:

```crisp
-- Anyone can create a draft (this is where optimism lives)
fn create_draft(subject: Entity) -> Decision(Draft):
  Decision {
    case_number = generate_case_id(),
    subject     = subject,
    evidence    = EvidenceSet.empty,
    notes       = "Initial submission. Good luck."
  }

-- Submitting moves Draft -> Submitted (no take-backs)
fn submit(d: Decision(Draft)) -> Decision(Submitted):
  Decision {
    case_number = d.case_number,
    subject     = d.subject,
    evidence    = d.evidence,
    notes       = concat(d.notes, "\nSubmitted. The clock starts now.")
  }

-- Review requires submission first (we have standards)
fn begin_review(d: Decision(Submitted)) -> Decision(UnderReview):
  Decision {
    case_number = d.case_number,
    subject     = d.subject,
    evidence    = d.evidence,
    notes       = concat(d.notes, "\nUnder review. Please do not inquire.")
  }
```

**What the compiler enforces:**
- You cannot submit an already-approved decision
- You cannot review a draft (must submit first)
- You cannot appeal a draft (nice try)
- The state machine is encoded in types, not documentation that nobody reads

### 2.4 Authority as a Value

Now the key insight: **authority is not a boolean check — it's a value you must possess**. Like a hall pass, but with legal consequences.

```crisp
-- Authority is parameterized by what it authorizes
type Authority(Action):
  granted_by:   Institution
  scope:        Scope
  expires:      Timestamp
  restrictions: List(Restriction)  -- There are always restrictions

-- Specific authority types
type CanApprove  = Authority(Approve)
type CanDeny     = Authority(Deny)
type CanReview   = Authority(Review)
type CanDelegate = Authority(Delegate)  -- The real power

-- You either have it or you don't
fn get_my_authority(emp: Employee) -> Option(CanApprove):
  match lookup_delegation(emp.badge_number)
    None        -> None  -- Sorry
    Some(auth)  -> Some(auth)  -- Congratulations, this is now your problem
```

This is not merely encoding permissions. It encodes **legitimacy as a resource that must be presented** — aligning with capability security, proof-carrying code, and the institutional reality that "I thought I could" is not a valid audit response.

### 2.5 Proof-Carrying Approval

Approval requires three things:
1. A reviewed decision
2. Authority to approve
3. **Proof** that evidence satisfies policy (not "I looked at it")

```crisp
-- Propositions live in Prop (erased at runtime, required at compile time)
type prop SatisfiesPolicy(evidence: EvidenceSet, policy: Policy):
  satisfied:
    policy.requirements.all_met(evidence) => SatisfiesPolicy(evidence, policy)

-- Approval function — note the proof parameter
fn approve(
  d:     Decision(UnderReview),
  auth:  CanApprove,
  proof: SatisfiesPolicy(d.evidence, StandardPolicy)
) -> Decision(Approved) ! Decide, Audit:
  do
    perform Audit.log_decision(d.case_number, "APPROVED", auth.granted_by)
    perform Decide.record_approval(d.subject, auth.granted_by)
    Decision {
      case_number = d.case_number,
      subject     = d.subject,
      evidence    = d.evidence,
      notes       = concat(d.notes, "\nApproved by: ", show(auth.granted_by),
                          "\nThis decision is final (until appealed).")
    }
```

**What happens here:**
- `proof: SatisfiesPolicy(d.evidence, StandardPolicy)` — caller must provide proof
- The proof is **erased at runtime** (it's in `Prop`) — no performance cost
- But it must **exist at compile time** — no "we'll verify later"
- The `Decide` and `Audit` effects track that an approval occurred

### 2.6 Why This Is Hard Elsewhere

```java
// Java: authority is a runtime check and a prayer
public Decision approve(Decision d, User user) {
    if (!user.hasRole("APPROVER")) {
        throw new UnauthorizedException();  // Discovered in production
    }
    if (!d.getStatus().equals(Status.UNDER_REVIEW)) {
        throw new InvalidStateException();  // "But it worked in staging"
    }
    // Evidence check is... somewhere. Probably. Check with compliance.
    d.setStatus(Status.APPROVED);
    logger.info("Approved by " + user);  // Hope someone reads the logs
    return d;
}
```

Problems:
- Wrong status? Runtime exception, 3am page
- Missing authority? Runtime exception, incident report
- Evidence check? In another microservice, hopefully
- Audit trail? Logger.info and prayers

```crisp
-- Crisp: authority is a compile-time requirement
fn approve(
  d:     Decision(UnderReview),      -- Wrong status? Won't compile
  auth:  CanApprove,                 -- No authority? Can't call this function
  proof: SatisfiesPolicy(...)        -- No proof? Won't compile
) -> Decision(Approved) ! Decide     -- Audit effect is declared and required
```

**The invalid program cannot be written.** Not "shouldn't be written." Cannot.

### 2.7 Denial Path

For completeness, denial follows the same pattern. Because "no" also requires justification:

```crisp
type prop ViolatesPolicy(evidence: EvidenceSet, policy: Policy):
  violated:
    policy.requirements.any_failed(evidence) => ViolatesPolicy(evidence, policy)

fn deny(
  d:     Decision(UnderReview),
  auth:  CanDeny,
  proof: ViolatesPolicy(d.evidence, StandardPolicy)
) -> Decision(Denied) ! Decide, Audit:
  do
    perform Audit.log_decision(d.case_number, "DENIED", auth.granted_by)
    perform Decide.record_denial(d.subject, auth.granted_by)
    Decision {
      case_number = d.case_number,
      subject     = d.subject,
      evidence    = d.evidence,
      notes       = concat(d.notes, "\nDenied by: ", show(auth.granted_by),
                          "\nYou may appeal within 30 days. Good luck.")
    }
```

---

## Part 3: Constitutional Procedure Model

This domain bridges regulatory systems and philosophical reasoning: modeling **legitimacy conflicts, emergency powers, and procedural constraints** in constitutional law. Because even governments need type checking.

### 3.1 The Domain

Constitutional procedures involve:
- **Impeachment processes** with required majorities (democracy is counting)
- **Emergency powers** with expiration and review (theoretically)
- **War authorization** requiring specific institutional actors
- **Judicial review** with legitimacy constraints

These are not arbitrary bureaucratic rules — they encode **who may do what, under what conditions, with what justification**. The Founders were doing type theory; they just didn't know it.

### 3.2 Constitutional Roles and Powers

```crisp
-- Branches of government as type-level values
type Branch:
  Legislative  -- Makes the rules
  Executive    -- Enforces the rules (creatively)
  Judicial     -- Interprets the rules

-- Chambers within legislative branch
type Chamber:
  House   -- Closer to the people, allegedly
  Senate  -- The "deliberative" body

-- Powers indexed by who may exercise them
type Power(branch: Branch):
  name:        String
  constraints: List(Constraint)
  caveats:     List(Caveat)  -- There are always caveats

-- Specific power types
type ImpeachmentPower   = Power(Legislative)
type VetoPower          = Power(Executive)
type ReviewPower        = Power(Judicial)
type SubpoenaPower      = Power(Legislative)  -- "Please comply"
```

### 3.3 Procedural State Machine

An impeachment process moves through defined phases. No shortcuts:

```crisp
type ImpeachmentPhase:
  NotInitiated         -- The calm before
  ArticlesDrafted      -- Someone's been busy
  HouseDebate          -- "My distinguished colleague..."
  HouseVoted           -- Democracy in action
  SenateTrialPending   -- Scheduling is hard
  SenateTrialActive    -- Must-see TV
  SenateVoted          -- The moment of truth
  Concluded            -- History books await

-- Impeachment indexed by phase
type Impeachment(p: ImpeachmentPhase):
  subject:       Officer
  articles:      List(Article)
  house_votes:   Option(VoteRecord)
  senate_votes:  Option(VoteRecord)
  outcome:       Option(Outcome)
  drama_level:   Int  -- Scholarly metric
```

### 3.4 Majority Requirements as Proofs

The House requires simple majority; the Senate requires two-thirds. Math is constitutional law:

```crisp
-- Proof that simple majority voted yes
type prop SimpleMajority(votes: VoteRecord, chamber: Chamber):
  achieved:
    votes.yes_count(chamber) > votes.total(chamber) / 2
      => SimpleMajority(votes, chamber)

-- Proof that two-thirds supermajority voted yes (the hard one)
type prop SuperMajority(votes: VoteRecord, chamber: Chamber):
  achieved:
    votes.yes_count(chamber) >= (votes.total(chamber) * 2) / 3
      => SuperMajority(votes, chamber)

-- House vote requires simple majority
fn house_impeach(
  imp:   Impeachment(HouseDebate),
  votes: VoteRecord,
  proof: SimpleMajority(votes, House)
) -> Impeachment(HouseVoted) ! ConstitutionalRecord:
  do
    perform ConstitutionalRecord.log_house_vote(imp.subject, votes)
    Impeachment {
      subject      = imp.subject,
      articles     = imp.articles,
      house_votes  = Some(votes),
      senate_votes = None,
      outcome      = None,
      drama_level  = imp.drama_level + 10
    }

-- Senate conviction requires supermajority (good luck with that)
fn senate_convict(
  imp:   Impeachment(SenateVoted),
  votes: VoteRecord,
  proof: SuperMajority(votes, Senate)
) -> Impeachment(Concluded) ! ConstitutionalRecord:
  do
    perform ConstitutionalRecord.log_conviction(imp.subject)
    Impeachment {
      subject      = imp.subject,
      articles     = imp.articles,
      house_votes  = imp.house_votes,
      senate_votes = Some(votes),
      outcome      = Some(Convicted),
      drama_level  = imp.drama_level + 100
    }
```

**What the types enforce:**
- Cannot convict without Senate supermajority proof
- Cannot skip House vote phase (no constitutional speedruns)
- All constitutional actions are recorded via effects
- Drama level only increases (historically accurate)

### 3.5 Emergency Powers with Expiration

Emergency powers are dangerous because they expand authority. Crisp can encode their constraints, which is more than some constitutions manage:

```crisp
-- Emergency declaration indexed by time bounds
type EmergencyDeclaration(expires: Timestamp):
  declared_by:  Executive
  scope:        EmergencyScope
  justification: String  -- "Trust me" is not sufficient
  valid_until:  expires

-- Proof that current time is before expiration
type prop NotExpired(now: Timestamp, deadline: Timestamp):
  valid: now < deadline => NotExpired(now, deadline)

-- Proof that the emergency is actually, you know, an emergency
type prop IsActualEmergency(situation: Situation):
  verified: situation.severity >= Severity.Genuine => IsActualEmergency(situation)

-- Actions under emergency require non-expiration proof
fn emergency_action(
  decl:   EmergencyDeclaration(expires),
  now:    Timestamp,
  proof1: NotExpired(now, expires),
  proof2: IsActualEmergency(decl.situation),
  auth:   Authority(EmergencyAct)
) -> EmergencyResult ! EmergencyRecord, Audit:
  do
    perform Audit.log_emergency_action(decl, now, auth)
    perform EmergencyRecord.log_action(decl, now)
    execute_emergency_measure(decl.scope)
```

**What this prevents:**
- Acting under expired emergency powers (no "we forgot to renew")
- Emergency actions without proper declaration
- "Emergencies" that aren't (proof required)
- Unrecorded exercises of emergency power

### 3.6 Judicial Review

The judiciary can invalidate actions, but only with proper jurisdiction. Even judges have constraints:

```crisp
type Jurisdiction:
  Federal
  State(name: String)
  Constitutional  -- The big leagues
  Appellate(circuit: Int)

type prop HasJurisdiction(court: Court, matter: LegalMatter):
  established:
    court.jurisdiction.covers(matter) => HasJurisdiction(court, matter)

fn judicial_review(
  action:    ConstitutionalAction,
  court:     Court,
  jproof:    HasJurisdiction(court, action.as_matter),
  finding:   ConstitutionalFinding,
  reasoning: LegalReasoning  -- Must show work
) -> ReviewOutcome ! JudicialRecord:
  do
    perform JudicialRecord.log_review(court, action, finding, reasoning)
    match finding
      Unconstitutional(reason) -> ReviewOutcome.Invalidated(action, reason)
      Constitutional           -> ReviewOutcome.Upheld(action)
      NeedMoreBriefs          -> ReviewOutcome.Continued(action)  -- Always an option
```

### 3.7 Why Constitutional Modeling Matters

This domain demonstrates:

1. **Legitimacy is structural** — not just "who has permission" but "under what procedural conditions"
2. **Time-sensitivity** — emergency powers expire; authorities have temporal bounds (in theory)
3. **Multi-party constraints** — impeachment requires both chambers with different thresholds
4. **Conflicts surface as type errors** — attempting unconstitutional actions fails to compile

The same Crisp machinery handles regulatory approval and constitutional crisis. The Founders would be proud. Or confused. Probably both.

---

## Part 4: Dialectical Simulation Engine

Now we demonstrate that Crisp's machinery applies beyond institutional systems to **formal reasoning with sociological constraints**. Philosophy, but with type checking.

### 4.1 The Domain

Philosophy deals with:
- Claims and counterclaims
- Contradictions (so many contradictions)
- Synthesis and resolution
- Interpretive authority (who gets to say what Hegel meant)

These map directly to Crisp's type system. This is **not metaphorical** — it is logic-as-data with authority constraints. Wittgenstein would have opinions about this.

### 4.2 Propositions as Types

```crisp
-- Prop is the universe of propositions
type Prop: Type

-- A claim asserts a proposition (and attributes it to someone we can blame)
type Claim(p: Prop):
  asserted_by: Agent
  content:     p
  confidence:  ConfidenceLevel
  footnotes:   Int  -- Credibility metric

-- Negation (the foundation of academic discourse)
type Not(p: Prop): Prop
```

`Claim(p)` is a type whose values are assertions of proposition `p`. With attribution, because accountability matters even in metaphysics.

### 4.3 Contradiction as a Type

A contradiction is a pair of claims asserting `p` and `Not(p)`. This happens more than you'd think:

```crisp
type Contradiction(p: Prop):
  thesis:     Claim(p)
  antithesis: Claim(Not(p))
  discovered: Timestamp       -- When things got interesting
  resolved:   Option(Resolution)

-- Academic fields as type-level values
type AcademicField:
  Philosophy
  Economics       -- Known for contradictions
  Physics         -- "Shut up and calculate"
  ComputerScience -- "It works on my machine"
```

You can construct a contradiction:

```crisp
-- Example propositions
type prop FreeWillExists: Prop
type prop DeterminismTrue: Prop

-- The eternal debate
let libertarian_claim = Claim {
  asserted_by = Agent.named("Chisholm"),
  content     = FreeWillExists.axiom,
  confidence  = ConfidenceLevel.Certain,
  footnotes   = 47
}

let determinist_claim = Claim {
  asserted_by = Agent.named("Spinoza"),
  content     = Not(FreeWillExists).from(DeterminismTrue.axiom),
  confidence  = ConfidenceLevel.Certain,
  footnotes   = 312  -- More footnotes, more credibility
}

let ancient_debate = Contradiction {
  thesis     = libertarian_claim,
  antithesis = determinist_claim,
  discovered = Timestamp.ancient,
  resolved   = None  -- Still waiting
}
```

### 4.4 Dialectical Rules

Synthesis requires a rule that transforms contradictions. You can't just wave your hands:

```crisp
-- A dialectical rule transforms contradiction over p into claim about q
type DialecticalRule(p: Prop, q: Prop):
  name:           String
  school:         PhilosophicalSchool
  transform:      Contradiction(p) -> q
  peer_reviewed:  Bool
  citations:      Int
```

### 4.5 Authority in Interpretation

Not everyone can legitimately synthesize. You need credentials:

```crisp
type PhilosophicalSchool:
  Hegelian          -- Thesis, antithesis, synthesis
  Analytic          -- "What do you mean by 'is'?"
  Continental       -- "It's complicated"
  Pragmatist        -- "Does it work?"
  ExistentialIst    -- "Why does it matter?"

type InterpretAuthority(school: PhilosophicalSchool):
  tradition:   school
  lineage:     List(Thinker)
  tenure:      Bool           -- The real authority
  h_index:     Int

-- Synthesis requires authority (no armchair philosophizing)
fn synthesize(
  contra: Contradiction(p),
  rule:   DialecticalRule(p, q),
  auth:   InterpretAuthority(rule.school)
) -> Claim(q) ! Interpret, Publish:
  do
    perform Publish.submit_to_journal(contra, rule, auth)
    perform Interpret.record_synthesis(contra, rule, auth)
    Claim {
      asserted_by = Agent.school(auth.tradition),
      content     = rule.transform(contra),
      confidence  = ConfidenceLevel.Tentative,  -- Appropriately humble
      footnotes   = contra.thesis.footnotes + contra.antithesis.footnotes + 50
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
let aufhebung: DialecticalRule(FreeWillExists, CompatibilistFreedom) =
  DialecticalRule {
    name          = "Aufhebung",
    school        = Hegelian,
    transform     = fn(c: Contradiction(FreeWillExists)) ->
      CompatibilistFreedom.from_sublation(c.thesis, c.antithesis),
    peer_reviewed = true,
    citations     = 4729
  }

-- Someone with Hegelian authority can apply it
fn apply_hegelian_synthesis(
  contra: Contradiction(FreeWillExists),
  auth:   InterpretAuthority(Hegelian)
) -> Claim(CompatibilistFreedom) ! Interpret, Publish:
  synthesize(contra, aufhebung, auth)
```

### 4.7 Invalid Synthesis Doesn't Typecheck

What if someone tries to synthesize without authority? Perhaps a first-year graduate student?

```crisp
-- This won't compile: no authority parameter
fn overconfident_synthesis(contra: Contradiction(p)) -> Claim(q):
  -- Error: cannot call synthesize without InterpretAuthority
  -- Error: "Confidence is not a type parameter"
  synthesize(contra, some_rule, ???)
```

What if someone applies the wrong school's rule? An Analytics applying Hegelian method?

```crisp
fn methodological_confusion(
  contra: Contradiction(p),
  rule:   DialecticalRule(p, q),          -- Hegelian rule
  auth:   InterpretAuthority(Analytic)    -- "What do you mean by 'synthesis'?"
) -> Claim(q) ! Interpret:
  -- Error: auth.school ≠ rule.school
  -- Error: Continental/Analytic divide is load-bearing
  synthesize(contra, rule, auth)
```

**Philosophy becomes executable, auditable, and authority-aware.** Peer review at compile time.

### 4.8 The Unifying Abstraction

All three domains share the same structure:

| Domain | Input State | Authority | Constraint | Output State | Effect |
|--------|-------------|-----------|------------|--------------|--------|
| Regulatory | Decision(UnderReview) | CanApprove | SatisfiesPolicy | Decision(Approved) | Decide |
| Constitutional | Impeachment(HouseVoted) | SenatePower | SuperMajority | Impeachment(Concluded) | ConstitutionalRecord |
| Dialectical | Contradiction(p) | InterpretAuthority | DialecticalRule | Claim(q) | Interpret |

The abstraction is **justified transformation**: constrained state changes requiring authority and proof. Whether you're approving exports, convicting officials, or resolving the mind-body problem.

---

## Part 5: Audit Artifact Inspection

A critical Crisp feature is **auditable compilation**. This section shows exactly what an auditor receives and what guarantees they can rely on. For when the auditors come — and they will come.

### 5.1 The Compilation Artifacts

When you compile a Crisp module:

```bash
crisp compile RegulatoryEngine.crisp --emit-tir --emit-manifest
```

You get three artifacts:

1. **`RegulatoryEngine.wasm`** — the executable binary
2. **`RegulatoryEngine.tir`** — the Typed Intermediate Representation
3. **`RegulatoryEngine.manifest.json`** — capability and hash manifest

This is the compliance trifecta.

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
      "Decide.record_denial",
      "Audit.log_decision"
    ],
    "optional": []
  },
  "authorities": {
    "Decide": {
      "operations": ["record_approval", "record_denial"],
      "must_be_provided": true,
      "rationale": "Regulatory decisions require audit trail"
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
  "build_time": "2026-01-27T10:30:00Z",
  "built_by": "CI/CD Pipeline (not Dave's laptop)"
}
```

**What the auditor learns:**
- Exactly which effects the module can perform (no surprises)
- That proofs were required at specific call sites
- Cryptographic binding between source, TIR, and WASM
- When and where it was built

### 5.3 Reading the Typed IR

The TIR preserves type information that WASM erases:

```
-- TIR excerpt for approve function
fn approve : (
  d:     Decision(UnderReview),
  auth:  CanApprove,
  proof: SatisfiesPolicy(d.evidence, StandardPolicy)  -- PROOF PARAMETER
) -> Decision(Approved) ! Decide, Audit

-- Proof was erased but structure preserved:
approve_call_site_1:
  requires_proof: SatisfiesPolicy
  evidence_source: reviewed_decision.evidence
  policy: StandardPolicy
  erased_at: codegen
  reason: "Proof irrelevant at runtime, required at compile time"
```

**What the auditor can verify:**
- Every `approve` call had a valid proof at compile time
- The proof related the actual evidence to the actual policy
- No "escape hatches" bypassed the proof requirement
- The absence of proof would have prevented compilation

### 5.4 What Auditors Don't Need to Trust

Because of Crisp's design, auditors do **not** need to trust:

| Concern | Why It's Not a Trust Issue |
|---------|---------------------------|
| "Did they check permissions?" | Authority is a required parameter — no parameter, no compilation |
| "Did they validate evidence?" | Proof is a required parameter — no proof, no compilation |
| "What effects can this code perform?" | Effects are in the type signature and WASM imports |
| "Could they have cheated?" | WASM sandbox prevents ambient authority |
| "Is this the right binary?" | Hash chain from source → TIR → WASM |
| "Did someone modify it in production?" | WASM is immutable; re-verify hashes |

The auditor trusts the **compiler**, not the **programmer**. This is an improvement over trusting Dave.

### 5.5 WASM Import Verification

The WASM binary declares its imports explicitly:

```wat
(module
  (import "Decide" "record_approval"
    (func $decide_record_approval (param i32 i32) (result i32)))
  (import "Decide" "record_denial"
    (func $decide_record_denial (param i32 i32) (result i32)))
  (import "Audit" "log_decision"
    (func $audit_log_decision (param i32 i32 i32) (result i32)))

  ;; No other imports — module cannot perform other effects
  ;; It cannot send emails. It cannot mine bitcoin. It cannot "just check" anything.
)
```

An auditor can inspect this with standard WASM tools:

```bash
wasm-objdump -x RegulatoryEngine.wasm | grep import
```

If the module tried to perform `FileSystem.read`, it would:
1. Fail to compile (effect not declared)
2. Have no corresponding WASM import (host won't provide it)
3. Be physically incapable of the action

---

## Part 6: What Crisp Refuses To Do

Constraints build trust. Crisp is defined as much by what it **forbids** as by what it enables. These aren't limitations — they're features.

### 6.1 No Ambient IO

```crisp
-- This is IMPOSSIBLE in Crisp:
fn sneaky_read() -> String:
  read_file("/etc/passwd")  -- Error: no FileSystem effect declared
                            -- Also, why would you even try this

-- You must declare effects:
fn honest_read(path: Path) -> String ! FileSystem:
  perform FileSystem.read(path)  -- Effect is visible in type
```

There is no way to perform IO without declaring it. No `unsafePerformIO`. No escape hatches. No "it's just logging."

### 6.2 No Reflection-Based Permission Checks

```crisp
-- This pattern is IMPOSSIBLE:
fn check_at_runtime(user: User, action: Action) -> Bool:
  user.roles.contains(action.required_role)  -- This is not security

-- Authority must be a VALUE you possess:
fn authorized_action(
  auth: Authority(action)  -- Must have this to call
) -> Result ! Audit:
  perform Audit.log(auth)
  execute(action)
```

You cannot "check" if you have authority. You either have the value or you don't. This is capability security, not role-based "security."

### 6.3 No Escape Hatches Without Type-Level Consequences

```crisp
-- There is no "trust me" annotation:
fn bypass_proof(d: Decision(UnderReview)) -> Decision(Approved):
  -- Error: cannot construct Decision(Approved) without going through approve
  -- which requires CanApprove and SatisfiesPolicy
  -- "But I really need to" is not a type

-- If you need to skip proof, you must change the TYPE:
type UncheckedDecision(s: Status):
  -- Different type, different guarantees
  -- Auditor sees this is not a proven Decision
  -- And will ask questions
```

Every "escape" creates a different type that auditors can distinguish. You can cheat, but you can't hide that you cheated.

### 6.4 No Implicit Effect Propagation

```crisp
-- Effects don't "leak" silently:
fn pure_function(x: Int) -> Int:
  -- Cannot call any effectful function here
  -- Even through indirection
  -- Even if you really want to

fn caller() -> Int:
  pure_function(effectful_thing())
  -- Error: effectful_thing requires ! Effect
  -- but pure_function's argument position is pure
  -- "But it's just logging" — no
```

If a function is pure, it cannot invoke effects — period. No "it's mostly pure."

### 6.5 No Proof Forgery

```crisp
-- Proofs cannot be constructed arbitrarily:
let fake_proof: SatisfiesPolicy(bad_evidence, Policy) = ???
-- Error: SatisfiesPolicy can only be constructed via its constructors
-- which require actual evidence satisfaction
-- "Close enough" is not a proof

-- You cannot "cast" to a proof type:
let fake = unsafe_coerce(unit, SatisfiesPolicy(...))
-- Error: unsafe_coerce does not exist
-- This is not that kind of language
```

Proofs must be constructed through their defined constructors, which encode actual logical requirements. The compiler is not impressed by confidence.

---

## Part 7: Complete Example Module

### 7.1 A Complete Regulatory Engine

```crisp
module RegulatoryEngine
  requires
    effect Decide
    effect Audit
    effect Log
  provides
    fn create_draft
    fn submit
    fn begin_review
    fn approve
    fn deny
    type Decision
    type Authority
    -- Not exporting escape hatches

-- Status as type-level value
type Status:
  Draft
  Submitted
  UnderReview
  Approved
  Denied
  Appealed  -- Hope springs eternal

-- Decision indexed by status
type Decision(s: Status):
  case_number: CaseId
  subject:     Entity
  evidence:    EvidenceSet
  notes:       String

-- Authority types
type Authority(Action):
  granted_by: Institution
  scope:      Scope
  expires:    Timestamp

type CanApprove = Authority(Approve)
type CanDeny   = Authority(Deny)

-- Proof types (erased at runtime, required at compile time)
type prop SatisfiesPolicy(e: EvidenceSet, p: Policy):
  satisfied: p.check(e) => SatisfiesPolicy(e, p)

type prop ViolatesPolicy(e: EvidenceSet, p: Policy):
  violated: not(p.check(e)) => ViolatesPolicy(e, p)

-- State transitions
fn create_draft(subject: Entity) -> Decision(Draft):
  Decision {
    case_number = generate_case_id(),
    subject     = subject,
    evidence    = EvidenceSet.empty,
    notes       = "Draft created. The journey begins."
  }

fn submit(d: Decision(Draft)) -> Decision(Submitted):
  Decision {
    case_number = d.case_number,
    subject     = d.subject,
    evidence    = d.evidence,
    notes       = concat(d.notes, "\nSubmitted. No turning back.")
  }

fn begin_review(d: Decision(Submitted)) -> Decision(UnderReview):
  Decision {
    case_number = d.case_number,
    subject     = d.subject,
    evidence    = d.evidence,
    notes       = concat(d.notes, "\nReview initiated. Please hold.")
  }

fn approve(
  d:     Decision(UnderReview),
  auth:  CanApprove,
  proof: SatisfiesPolicy(d.evidence, StandardPolicy)
) -> Decision(Approved) ! Decide, Audit:
  do
    perform Audit.log_decision(d.case_number, "APPROVED", auth.granted_by)
    perform Decide.record_approval(d.subject, auth.granted_by)
    Decision {
      case_number = d.case_number,
      subject     = d.subject,
      evidence    = d.evidence,
      notes       = concat(d.notes, "\nApproved by: ", show(auth.granted_by))
    }

fn deny(
  d:     Decision(UnderReview),
  auth:  CanDeny,
  proof: ViolatesPolicy(d.evidence, StandardPolicy)
) -> Decision(Denied) ! Decide, Audit:
  do
    perform Audit.log_decision(d.case_number, "DENIED", auth.granted_by)
    perform Decide.record_denial(d.subject, auth.granted_by)
    Decision {
      case_number = d.case_number,
      subject     = d.subject,
      evidence    = d.evidence,
      notes       = concat(d.notes, "\nDenied by: ", show(auth.granted_by),
                          "\nAppeal window: 30 days.")
    }
```

### 7.2 Using the Module

```crisp
module Main
  requires
    effect Decide
    effect Audit
    effect Log
  import RegulatoryEngine

fn process_application(
  subject: Entity,
  evidence: EvidenceSet,
  auth: CanApprove
) -> Decision(Approved) ! Decide, Audit, Log:
  do
    perform Log.info("Initiating application process...")
    let draft = create_draft(subject)

    perform Log.info("Submitting for review...")
    let submitted = submit(draft)

    perform Log.info("Beginning review...")
    let under_review = begin_review(submitted)

    perform Log.info("Evaluating against policy...")
    -- proof must be constructed from evidence
    -- cannot be fabricated, wished into existence, or delegated to an intern
    let proof = SatisfiesPolicy.check(under_review.evidence, StandardPolicy)

    perform Log.info("Rendering decision...")
    approve(under_review, auth, proof)
```

---

## Part 8: Key Takeaways

### What Crisp Enforces at Compile Time

| Property | How | Alternative |
|----------|-----|-------------|
| Valid state transitions | Indexed types | Runtime exceptions, incident reports |
| Required authority | Authority parameters | "I thought I could" |
| Policy compliance | Proof terms | "We usually check that" |
| Effect boundaries | Effect system | Discovering side effects in production |
| Exhaustive handling | Pattern coverage | "That case shouldn't happen" |
| Temporal constraints | Time-indexed types | Calendar reminders |

### What WASM Provides at Runtime

| Property | How | Why It Matters |
|----------|-----|----------------|
| Sandboxed execution | WASM memory isolation | Can't access what isn't imported |
| Explicit capabilities | Effects as imports | No ambient authority |
| Portable artifacts | WASM binary format | Same behavior everywhere |
| Deterministic execution | WASM semantics | Reproducible audits |
| No ambient authority | Import-only effects | "It worked on my machine" is irrelevant |

### The Central Insight

Crisp doesn't just prevent bugs — it makes **invalid programs inexpressible**.

- An approval without authority? Won't compile.
- A synthesis without the right school? Won't compile.
- A state transition that skips steps? Won't compile.
- An effect that wasn't declared? Won't compile.
- Emergency action after expiration? Won't compile.
- Conviction without supermajority? Won't compile.
- "But I really need to"? Still won't compile.

**The natural way to write correct programs is the natural way to write auditable programs.**

---

## Appendix A: Comparison with Other Approaches

### vs. Haskell

Haskell has type-level programming but:
- Effects are monadic (transformers are... an experience)
- No built-in authority tracking
- No WASM target with capability model
- Proofs require external tools (LiquidHaskell)
- `unsafePerformIO` exists (we don't talk about it)

### vs. Rust

Rust has strong safety but:
- No dependent types
- No algebraic effects
- Authority is convention-based (unsafe blocks are trust-based)
- Can't express proof-carrying parameters
- `unsafe` is a keyword, not a type error

### vs. Idris/Agda

These have dependent types but:
- Academic focus, less practical tooling
- Not designed for institutional modeling
- No effect-as-capability model
- Heavy runtime or compilation overhead
- Target audience: researchers, not compliance officers

### vs. Gleam

Gleam has algebraic types and BEAM but:
- No dependent types
- No proof terms
- Effects are implicit in BEAM
- Can't enforce authority at compile time
- Different goals (and that's fine)

Crisp occupies a unique position: **practical dependent types with algebraic effects, compiling to a capability-aware runtime (WASM), designed for systems where correctness and authority matter.**

---

## Appendix B: Realistic Adoption Expectations

Crisp is **not** a general-purpose language. It will not replace Python, JavaScript, or even Rust for most tasks. And that's fine.

### Expected Adoption Profile

- **Slow adoption**: The concepts require expertise (and willingness to read type errors)
- **Expert users**: Type theory background helpful; institutional experience essential
- **High per-user value**: Significant benefit in appropriate domains
- **Niche dominance**: Potential standard for regulated decision systems

### Appropriate Domains

1. **Regulated decision engines**: Export control, compliance, permitting
2. **Policy-as-code**: Where constraints must be binding, not "guidelines"
3. **Institutional simulation**: Legal systems, governance modeling
4. **Audit-critical systems**: Where "we forgot to check" ends careers
5. **Philosophy departments**: (Half-joking)

### Inappropriate Domains

- Rapid prototyping (use Python, then rewrite)
- Scripting and automation (use Bash, live dangerously)
- Performance-critical systems (until tooling matures)
- Teams without type theory experience (yet)
- Anything where "move fast and break things" is the motto

Crisp trades breadth for depth. If you need to model institutional authority with compile-time guarantees, welcome. If you need to ship a CRUD app by Friday, Godspeed.

---

## Next Steps

- Read the [Language Specification](SPECIFICATION.md) for complete syntax
- Explore the [examples/](../examples/) directory
- Run the REPL: `crisp repl`
- Check the [Implementation Plan](IMPLEMENTATION_PLAN.md) for current status
- File your first type error (it's a rite of passage)

---

*Crisp: A language for constrained, auditable transformations — where correctness and legitimacy are the same thing, and "it compiled" actually means something.*
