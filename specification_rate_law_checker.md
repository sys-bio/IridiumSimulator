# Specification — Generalized Rate Law Validation System

**Working title:** `ratelint` (placeholder). **Status:** Draft specification, pre-implementation.

---

## 1\. Purpose

Build a model-checking system for Antimony/SBML models in which **the set of rate laws being checked is data, not code**.

The user maintains a registry of rate laws they care about. Any rate law in the registry is automatically subject to both:

- **Static checking** — was the equation *written* correctly? (structural comparison against the registered form)  
- **Dynamic checking** — does the equation *behave* correctly when evaluated and simulated? (numerical property testing and simulation)

Adding support for a new rate law must require **no new checking code** — only a new registry entry.

## 2\. Motivation and contrast with prior work

The previous linter hardcoded one check per rate law. The Michaelis-Menten check (`W004`) was a bespoke AST walk that enumerated MM-specific defects: substrate in the wrong denominator slot, `Km` in the numerator, and so on. Adding Hill kinetics would have meant writing a second, structurally similar but separate walk. Adding competitive inhibition, ordered bi-bi, mass action with arbitrary stoichiometry, and so on would each mean another.

That approach does not scale, and the per-law checks drift apart in rigor and coverage over time.

This project inverts the relationship:

| Previous | This project |
| :---- | :---- |
| Check logic written per rate law | One generic check engine |
| Rate laws implicit in code | Rate laws declared in a registry |
| Coverage grows by writing code | Coverage grows by adding registry entries |
| Defect names specific to a law (`W004`) | Defect classes generic, parameterized by law ID |

The bet is that the *defect taxonomy* is largely law-independent — wrong operator, duplicated operand, swapped roles, missing parenthesis, wrong exponent — even though the *correct form* is law-specific.

## 3\. Terminology

- **Rate law definition (RLD)** — a registry record describing one rate law: its canonical expression, symbol roles, applicability conditions, and behavioral invariants.  
- **Registry** — the persisted, user-editable collection of RLDs, with an enabled/disabled flag per entry.  
- **Candidate reaction** — a reaction in the model under test whose kinetic law is being checked.  
- **Association** — the decision that a candidate reaction is *intended* to follow a particular RLD.  
- **Canonical AST** — the RLD's expression parsed and normalized into a comparable tree.  
- **Defect** — a single diagnostic finding, with a code, severity, location, and explanation.

## 4\. System architecture

                    ┌───────────────────────┐

   user CLI/API ───►│   Rate Law Registry   │  (YAML/JSON on disk)

                    │  add / remove / list  │

                    │  enable / disable     │

                    └───────────┬───────────┘

                                │ enabled RLDs

                                ▼

  Antimony ──► libSBML ──►┌─────────────────┐

   model      (SBML doc)  │   Associator    │  reaction ↔ RLD

                          └────────┬────────┘

                                   │

                     ┌─────────────┴─────────────┐

                     ▼                           ▼

            ┌─────────────────┐         ┌─────────────────┐

            │  Static Engine  │         │ Dynamic Engine  │

            │ AST structural  │         │ numeric probing │

            │   comparison    │         │ \+ tellurium sim │

            └────────┬────────┘         └────────┬────────┘

                     └─────────────┬─────────────┘

                                   ▼

                          ┌─────────────────┐

                          │ Diagnostic Set  │──► report (text / JSON)

                          └─────────────────┘

## 5\. Rate Law Definition schema

An RLD is a declarative record. The proposed format is YAML on disk, validated against a JSON Schema on load.

### 5.1 Fields

| Field | Required | Description |
| :---- | :---- | :---- |
| `id` | yes | Stable machine identifier, e.g. `michaelis_menten_irrev` |
| `name` | yes | Human-readable name |
| `version` | yes | Integer, incremented on edit |
| `enabled` | yes | Whether this law participates in checking |
| `expression` | yes | Canonical rate expression in Antimony-compatible infix syntax |
| `roles` | yes | Map of each symbol in `expression` to its role and semantics |
| `naming_conventions` | no | Accepted alternate identifiers per role |
| `applicability` | no | Structural preconditions on the reaction (stoichiometry, reversibility) |
| `invariants` | no | Declared behavioral properties, used by the dynamic engine |
| `sampling` | no | Domain and grid used for numerical probing |
| `tolerances` | no | Per-law overrides for numeric comparison thresholds |
| `notes` | no | Free text shown in reports |

### 5.2 Role kinds

- `species` — a concentration variable. Sub-attributes: `position` (`substrate`, `product`, `modifier`, `inhibitor`, `activator`), `cardinality`.  
- `parameter` — a constant. Sub-attributes: `semantics` (free-form tag such as `max_rate`, `half_saturation`, `cooperativity`, `dissociation`), `positive`, `integer`.  
- `compartment` — a compartment volume.

Semantics tags are what let the dynamic engine assert meaning, e.g. that a `half_saturation` parameter must actually be the point of half-maximal rate.

### 5.3 Example entries

**Irreversible Michaelis-Menten**

id: michaelis\_menten\_irrev

name: Irreversible Michaelis-Menten

version: 1

enabled: true

expression: "Vm \* S / (Km \+ S)"

roles:

  S:  {kind: species,   position: substrate, cardinality: 1}

  Vm: {kind: parameter, semantics: max\_rate,        positive: true}

  Km: {kind: parameter, semantics: half\_saturation, positive: true}

naming\_conventions:

  Vm: \[Vm, Vmax, V\_max, vmax\]

  Km: \[Km, KM, K\_m, km\]

applicability:

  reactants: 1

  products: "\>=1"

  reversible: false

invariants:

  \- {type: zero\_at,        point: {S: 0}}

  \- {type: nonnegative,    domain: {S: \[0, inf\]}}

  \- {type: monotonic,      var: S, direction: increasing}

  \- {type: limit,          var: S, to: inf, equals: "Vm"}

  \- {type: value\_at,       point: {S: "Km"}, equals: "Vm/2"}

sampling:

  S:  {scale: log, range: \[1e-3, 1e3\], n: 64}

  Vm: {scale: log, range: \[1e-2, 1e2\], n: 6}

  Km: {scale: log, range: \[1e-2, 1e2\], n: 6}

**Hill kinetics**

id: hill\_activation

name: Hill activation

version: 1

enabled: true

expression: "Vm \* S^n / (K^n \+ S^n)"

roles:

  S:  {kind: species,   position: substrate, cardinality: 1}

  Vm: {kind: parameter, semantics: max\_rate,        positive: true}

  K:  {kind: parameter, semantics: half\_saturation, positive: true}

  n:  {kind: parameter, semantics: cooperativity,   positive: true}

naming\_conventions:

  K: \[K, Kd, K\_half, Ka\]

  n: \[n, h, nH\]

invariants:

  \- {type: zero\_at,     point: {S: 0}}

  \- {type: nonnegative, domain: {S: \[0, inf\]}}

  \- {type: monotonic,   var: S, direction: increasing}

  \- {type: limit,       var: S, to: inf, equals: "Vm"}

  \- {type: value\_at,    point: {S: "K"}, equals: "Vm/2"}

  \- {type: sigmoidal,   var: S, when: "n \> 1"}

**Irreversible mass action (variable order)**

id: mass\_action\_irrev

name: Irreversible mass action

version: 1

enabled: true

expression: "k \* prod(Si^ai)"     \# generative form, see §5.4

roles:

  k:  {kind: parameter, semantics: rate\_constant, positive: true}

  Si: {kind: species,   position: substrate, cardinality: "n"}

applicability:

  reversible: false

  exponents\_from: stoichiometry

invariants:

  \- {type: zero\_at\_any\_zero, vars: Si}

  \- {type: nonnegative, domain: all}

  \- {type: monotonic,   vars: Si, direction: increasing}

  \- {type: homogeneous, degree: "sum(ai)"}

### 5.4 Generative (parameterized) laws

Some laws are families, not fixed expressions — mass action of arbitrary order, or n-substrate laws. The schema supports a generative form where the expression contains an indexed product/sum over a role with `cardinality: n`, and the concrete canonical expression is **instantiated per reaction** from that reaction's stoichiometry before comparison.

This is required for the system to be genuinely general; without it, mass action needs one entry per order.

## 6\. Registry component

### 6.1 Storage

- Default location: `~/.ratelint/registry/` with one YAML file per RLD, plus an `index.yaml`.  
- Project-local override: `./ratelint.registry/` takes precedence if present, so a model repo can ship its own law set.  
- Registry files are plain text and diffable, so they can live in Git alongside the models.

### 6.2 Operations

| Operation | Description |
| :---- | :---- |
| `add` | Add an RLD from a YAML file or an interactive prompt |
| `remove` | Delete an RLD |
| `enable` / `disable` | Toggle participation without deleting |
| `list` | Show all RLDs with id, name, enabled state |
| `show` | Print one RLD in full |
| `validate` | Schema-check an RLD and confirm its expression parses and its invariants hold for the canonical form itself |
| `export` / `import` | Bundle a registry for sharing between machines or lab members |

### 6.3 Self-validation requirement

**On every `add` and on every registry load, each RLD's canonical expression must be checked against its own declared invariants.** If the declared canonical form violates a declared invariant, the entry is rejected with an error. This prevents a bad registry entry from silently producing false positives across every model checked afterward.

## 7\. Parsing and canonicalization

Pipeline: Antimony text → `antimony.loadAntimonyString` → SBML string → `libsbml.SBMLReader` → per-reaction `KineticLaw` → `ASTNode`.

The AST is then normalized so that trivially different but mathematically identical writings compare equal:

1. Flatten n-ary associative operators (`+`, `*`).  
2. Rewrite `a / b` as `a * b^(-1)`; `a - b` as `a + (-1)*b`.  
3. Fold numeric literal constants.  
4. Sort commutative operand lists by a canonical key.  
5. Normalize integer powers written as repeated multiplication (`S*S` ↔ `S^2`).  
6. Resolve local parameters and assignment rules to their defining expressions where the RLD requires it.

**Important:** canonicalization must be applied identically to both the model's AST and the RLD's AST, and the *pre*\-canonical tree must be retained. Some defects — notably missing parentheses — are only visible before normalization, and some — notably duplicated operands — are only visible after. The static engine has access to both forms.

## 8\. Association: which law is this reaction supposed to be?

Three modes, in priority order:

1. **Explicit annotation (preferred).** The modeler tags the reaction, e.g. an Antimony comment `# @ratelaw michaelis_menten_irrev` or an SBML annotation element. Unambiguous, and the only mode that can report "you meant MM and wrote something else entirely."  
2. **Applicability \+ best-match inference.** Filter enabled RLDs by `applicability` (stoichiometry, reversibility, symbol arity), then score each surviving candidate by structural similarity (normalized tree edit distance plus symbol-multiset overlap). Associate if the top score clears an absolute threshold and beats the runner-up by a margin.  
3. **Unassociated.** If nothing matches, emit an informational diagnostic rather than an error — the model may legitimately use a law that is not registered.

Ambiguous association (two laws within the margin) is itself reported, since it often indicates a typo that made the expression equidistant from two correct forms.

## 9\. Static check engine

Generic structural comparison of the reaction AST against the associated RLD's instantiated canonical AST, under a role binding.

### 9.1 Procedure

1. **Bind roles.** Map model identifiers to RLD roles using reaction stoichiometry (species) and `naming_conventions` plus positional inference (parameters). Produce all plausible bindings if ambiguous.  
2. **Score bindings.** Choose the binding minimizing structural distance; report if the best binding is unnatural (e.g. requires binding an identifier named `Km` to the `Vm` role — a strong signal of a role swap).  
3. **Diff.** Compute a structural diff between the two trees under the chosen binding.  
4. **Classify.** Map each diff node to a defect class.

### 9.2 Defect classes (static)

| Code | Severity | Meaning |
| :---- | :---- | :---- |
| `S001` | INFO | No registered law matches this reaction |
| `S002` | WARN | Association ambiguous between two or more laws |
| `S003` | ERROR | Operator substitution (e.g. `*` where `+` expected in denominator) |
| `S004` | ERROR | Duplicated operand where distinct symbols expected (the `Km + Km` case) |
| `S005` | ERROR | Required symbol missing from expression |
| `S006` | WARN | Extraneous symbol not present in canonical form |
| `S007` | ERROR | Role swap — symbol appears in the wrong structural slot |
| `S008` | ERROR | Exponent mismatch (missing, extra, or wrong power) |
| `S009` | ERROR | Sign or negation defect |
| `S010` | ERROR | Parenthesization/precedence defect — same symbols, different tree |
| `S011` | WARN | Naming-convention violation (identifier name inconsistent with its bound role) |
| `S012` | WARN | Numeric literal where a named parameter is expected |
| `S013` | ERROR | Applicability violation (stoichiometry or reversibility inconsistent with the associated law) |
| `S014` | ERROR | Symbol referenced but undefined or uninitialized in the model |

Each defect carries: reaction ID, associated law ID, the offending subexpression, the corresponding canonical subexpression, and a suggested correction where one is unambiguous.

### 9.3 Model-level static checks (law-independent)

Carried forward from the previous project and retained as a separate, always-on check set: undefined references, uninitialized parameters, missing kinetic law, reactants absent from the rate law, species in a default compartment.

## 10\. Dynamic check engine

The dynamic engine answers: *even if it parses, does it behave like the law it claims to be?*

Three layers, increasing in cost.

### 10.1 Layer 1 — Invariant probing (rate law in isolation)

Compile the reaction's kinetic expression to a numeric function of its symbols. Sample over the domain grid in `sampling`. Evaluate the declared `invariants`.

Supported invariant types (each implemented once, generically):

| Type | Test |
| :---- | :---- |
| `zero_at` | Rate is \~0 at the specified point |
| `zero_at_any_zero` | Rate is \~0 whenever any listed variable is 0 |
| `nonnegative` | Rate ≥ 0 across the sampled domain |
| `monotonic` | Sign of finite differences in the given variable is consistent |
| `bounded_above` | Rate never exceeds the given expression |
| `limit` | Rate approaches the given expression as a variable → 0 or ∞ |
| `value_at` | Rate equals the given expression at a specified point (e.g. half-max at `Km`) |
| `sigmoidal` | Second derivative changes sign exactly once over the domain |
| `homogeneous` | Scaling inputs by λ scales output by λ^degree |
| `symmetric` | Output invariant under swapping listed variables |

### 10.2 Layer 2 — Differential comparison against canonical

Evaluate the model's expression and the RLD's canonical expression over the same grid under the role binding. Report maximum relative deviation and where in the domain it occurs.

This catches defects the static diff misses — algebraically different forms that are *nearly* equal in a common regime and wildly wrong outside it, which are exactly the errors that survive eyeballing.

### 10.3 Layer 3 — In-model simulation (tellurium / RoadRunner)

Load the model in tellurium and simulate. Then, optionally, **substitute the canonical form** for the associated reaction and re-simulate, comparing trajectories. A structural defect that produces no trajectory divergence in the model's operating regime is downgraded in severity and reported as such, which is useful triage information.

### 10.4 Defect classes (dynamic)

| Code | Severity | Meaning |
| :---- | :---- | :---- |
| `D001` | ERROR | Evaluation produced NaN/Inf or divide-by-zero inside the declared domain |
| `D002` | ERROR | Negative rate where non-negativity declared |
| `D003` | ERROR | Monotonicity violated |
| `D004` | ERROR | Bound or limit violated |
| `D005` | ERROR | Parameter semantics violated (e.g. half-max not at the half-saturation parameter) |
| `D006` | WARN | Deviation from canonical form exceeds tolerance |
| `D007` | ERROR | Shape invariant violated (sigmoidicity, symmetry, homogeneity) |
| `D101` | ERROR | Simulation failed to run |
| `D102` | ERROR | Negative species concentration in trajectory |
| `D103` | ERROR | NaN/Inf in trajectory |
| `D104` | WARN | Steady state not reached / unbounded growth |
| `D105` | WARN | Trajectory diverges from canonical-substituted model beyond tolerance |
| `D106` | INFO | Static defect present but no measurable dynamic consequence in this model |

`D101`–`D104` are model-level and run regardless of association.

## 11\. Diagnostics and reporting

A single `Diagnostic` record type shared by both engines:

code, severity, law\_id, reaction\_id, message,

found (subexpression), expected (subexpression),

suggestion, evidence (numeric witness point, if dynamic)

Output formats:

- **Text report** — grouped by reaction, ordered by severity. Default.  
- **JSON** — machine-readable, for CI use and for downstream tooling.  
- **Exit codes** — non-zero on any ERROR, so it can gate a commit or a build.

Every dynamic defect must report a **witness**: the specific parameter/concentration values at which the property failed. A dynamic finding without a reproducible witness is not actionable.

## 12\. Public interface

### 12.1 Python API

from ratelint import Registry, lint, print\_report

reg \= Registry.load()                   \# default \+ project-local

reg.disable("hill\_activation")

result \= lint(antimony\_text, registry=reg, dynamic=True)

print\_report(result)

result.errors        \# list\[Diagnostic\]

result.to\_json()

### 12.2 CLI

ratelint check model.txt \[--no-dynamic\] \[--json\] \[--law ID\]

ratelint registry list | show ID | add FILE | remove ID

ratelint registry enable ID | disable ID

ratelint registry validate \[ID\]

ratelint registry export FILE | import FILE

## 13\. Configuration

Project-level `ratelint.toml`: registry paths, default severity overrides, global numeric tolerances, sampling density, simulation time span and step count, association threshold and margin.

## 14\. Testing strategy

- **Golden models.** A corpus of small Antimony models, each with an expected diagnostic set. Includes the original `Vm*s/(Km + Km)` case as a regression test.  
- **Mutation testing.** Take a correct model, programmatically mutate the rate law (swap an operator, duplicate an operand, drop a parenthesis, permute two identifiers), and assert the correct defect class is reported. This is the primary measure of static-engine coverage, and it generalizes automatically to every registered law.  
- **False-positive corpus.** Correct models written in unusual but valid styles, which must produce zero errors. Guards against over-aggressive canonicalization.  
- **Registry self-validation tests.** Every shipped RLD passes its own invariants.  
- **Property tests on canonicalization.** Randomly generated algebraically-equivalent expression pairs must canonicalize identically.

## 15\. Non-goals

- Not a general computer algebra system. Equivalence checking is structural plus numerical, not symbolic proof.  
- Not a model *builder* or auto-fixer. Suggestions are advisory; the tool does not rewrite models.  
- Not a replacement for SBML validation. libSBML's own validator runs first; this tool assumes a structurally valid document.  
- No GUI in the initial scope.

## 16\. Dependencies and environment

- `antimony`, `libsbml`, `tellurium` (RoadRunner), `numpy`, `pyyaml`, `jsonschema`  
- Python via Anaconda; local VS Code development  
- **Constraint carried over from prior work:** tellurium requires `numpy<2.0`. Pin it in the environment file rather than relying on ad-hoc `--force-reinstall`. Ship an `environment.yml` so the setup is reproducible across the desktop and laptop.  
- Repo on GitHub (`delauf`) as the source of truth across machines.

---

## 17\. Milestones

Each milestone should end with something runnable and testable, not just written.

### M0 — Repo and environment

Create the repository, package skeleton (`ratelint/`, `tests/`), and a pinned `environment.yml` that reproduces a working antimony \+ tellurium install on a clean machine. **Done when:** a fresh clone plus `conda env create` runs a smoke test importing antimony and tellurium successfully.

### M1 — Parse and canonicalize

Antimony → SBML → per-reaction AST extraction. Implement the canonicalizer and a stable AST printer. **Done when:** a set of algebraically-equivalent expression pairs canonicalize to identical trees, and non-equivalent pairs do not.

### M2 — Registry schema and storage

Define the JSON Schema for an RLD. Implement load, validate, list, show, add, remove, enable, disable, export, import. Include self-validation on load. **Done when:** the CLI can round-trip a registry and rejects a malformed or self-inconsistent RLD with a clear error.

### M3 — First two entries and role binding

Author the Michaelis-Menten and Hill RLDs. Implement role binding (species from stoichiometry, parameters from naming conventions plus position). **Done when:** a correct MM model binds `Vm`/`Km`/`S` correctly and reports no defects.

### M4 — Static engine v1

Structural diff and defect classification for codes `S003`–`S008`, `S011`, `S014`. **Done when:** the original `Vm*s/(Km + Km)` example is caught as `S004` with the correct suggested fix, and equivalent Hill defects are caught with no Hill-specific code added.

### M5 — Association

Explicit annotation parsing plus applicability filtering and similarity-based inference, with threshold and margin. Emit `S001`, `S002`, `S013`. **Done when:** an unannotated model correctly associates its reactions, and a deliberately ambiguous case reports `S002` rather than guessing.

### M6 — Mutation testing harness

Automated mutation of correct models with assertions on the resulting defect class. **Done when:** mutation coverage report runs over the whole registry and every mutation class is detected for both registered laws.

### M7 — Dynamic engine, Layer 1

Numeric compilation of expressions, sampling grids, and the generic invariant evaluators. Emit `D001`–`D005`, `D007`, with witness points. **Done when:** a rate law that is structurally plausible but has half-max at the wrong point is caught by `D005` alone, with the static engine reporting nothing.

### M8 — Dynamic engine, Layers 2 and 3

Differential comparison against canonical (`D006`), and tellurium simulation checks (`D101`–`D105`), including canonical-substitution trajectory comparison and the `D106` downgrade. **Done when:** a defect with no dynamic consequence in the model's regime is correctly reported as `D106` rather than a bare error.

### M9 — Generative laws

Indexed product/sum support in the expression grammar and per-reaction instantiation from stoichiometry. Author the mass-action RLD. **Done when:** a single mass-action entry correctly checks first-, second-, and third-order reactions.

### M10 — Reporting, CLI, and configuration

Text and JSON reporters, exit codes, `ratelint.toml`, and the full CLI surface. **Done when:** `ratelint check model.txt` produces a readable report and a non-zero exit on error.

### M11 — Registry expansion

Author entries for reversible Michaelis-Menten, competitive/uncompetitive/non-competitive inhibition, and a convenience-kinetics form. Each addition is registry-only; any code change required here is a defect in the generality of the engine and should be logged as such. **Done when:** all entries pass self-validation and their mutation tests, with zero new law-specific code.

### M12 — Documentation and handoff

README, a guide to authoring an RLD, the defect code reference, and a worked walkthrough of adding a new law from scratch. **Done when:** someone else in the lab can add a rate law without reading the source.

### M13 — Stretch: model-corpus evaluation

Run the tool over an existing public model corpus (e.g. BioModels) and characterize its false-positive rate. **Done when:** there is a quantified false-positive figure and a triage list of the causes.

---

## 18\. Open questions

- What is the right similarity metric and threshold for inference-based association? Tree edit distance is expensive on large expressions; a cheaper signature-based prefilter may be needed.  
- How should assignment rules and local parameters be resolved before comparison without expanding expressions into unrecognizable forms?  
- Should unit consistency be checked as part of role binding, given that libSBML exposes unit definitions? Potentially a strong signal for role swaps, but many models declare units incompletely.  
- How aggressively should canonicalization normalize? Every normalization rule added increases false negatives for parenthesization defects while decreasing false positives for stylistic variation.  
- Should the registry support law *composition* (e.g. an inhibition term multiplied onto a base law), or are composed laws always separate entries?

