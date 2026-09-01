# Specification — Rate Law Validation for Iridium (Delphi)

**Unit prefix:** `RateLaw.*`. **Status:** In progress — M0–M14 built; most of M15 and the stretch items remain. See §18.
**Supersedes:** `specification_rate_law_checker.md` (the Python/CLI `ratelint` draft), for
the purposes of the Iridium implementation. That document remains the reference for a
possible future standalone tool; §19 records what was carried over and what was dropped.

---

## 1. Purpose

Build a model-checking system for Antimony models loaded in Iridium, in which **the set of
rate laws being checked is data, not code**.

The user maintains a registry of rate laws they care about. Any rate law in the registry is
automatically subject to both:

- **Static checking** — was the equation *written* correctly? (structural comparison
  against the registered form)
- **Dynamic checking** — does the equation *behave* correctly when evaluated?
  (numerical property testing)

Adding support for a new rate law must require **no new checking code** — only a new
registry entry. That is the whole bet, and every design decision below is subordinate to it.

## 2. Motivation

A linter that hardcodes one check per rate law does not scale. The Michaelis-Menten check
would be a bespoke AST walk enumerating MM-specific defects — substrate in the wrong
denominator slot, `Km` in the numerator. Hill kinetics would need a second, structurally
similar but separate walk; competitive inhibition a third; and the per-law checks drift
apart in rigour and coverage over time.

This project inverts the relationship:

| Per-law approach | This project |
| :---- | :---- |
| Check logic written per rate law | One generic check engine |
| Rate laws implicit in code | Rate laws declared in a registry |
| Coverage grows by writing code | Coverage grows by adding registry entries |
| Defect names specific to a law | Defect classes generic, parameterised by law id |

The bet is that the *defect taxonomy* is largely law-independent — wrong operator,
duplicated operand, swapped roles, missing parenthesis, wrong exponent — even though the
*correct form* is law-specific.

**Falsification test.** M12 authors four new laws as registry entries only. Any code change
required to make one of them work is a defect in the generality of the engine and is logged
as such, not quietly absorbed.

## 3. Terminology

- **Rate law definition (RLD)** — a registry record describing one rate law: its canonical
  expression, symbol roles, applicability conditions, and behavioural invariants.
- **Registry** — the persisted, user-editable collection of RLDs, with an enabled flag per
  entry.
- **Candidate reaction** — a reaction in the model under test whose kinetic law is being
  checked.
- **Association** — the decision that a candidate reaction is *intended* to follow a
  particular RLD.
- **Canonical AST** — the RLD's expression parsed and normalised into a comparable tree.
- **Defect** — a single diagnostic finding, with a code, severity, location, and explanation.
- **Model source** — the abstraction (§5) through which the engine sees a model. The engine
  never talks to libantimony or RoadRunner directly.

## 4. Placement: a sibling library, not an Iridium unit

The checker is written as its own project in a sibling directory, referenced by Iridium
through relative paths in the `.dpr`, exactly as `..\Antimony_MetaData_Support\` is:

    ..\ModelCheckerLib\
        RateLaw.*.pas               the library
        ModelCheckerLib_Project.dpr  console test harness
        RateLaw.TestCorpus.pas      fixture model source and the corpus
        RateLaw.HardLaws.pas        laws complex enough to be a real test
        RateLaw.Mutate.pas          programmatic mutation of a correct law

As built this is flat, and the units are `RateLaw.Ast`, `RateLaw.Parser` and so on rather
than the `uRateLaw*` the draft proposed. Both changes follow
`..\Antimony_MetaData_Support\`, which is flat and uses `Sim.Meta.*`: matching the
sibling library the design was modelled on beats matching this document.

The default registry ships compiled in (§7.1) rather than as a `laws\` directory, so a
fresh install has laws with no files to lose.

The reasons are the same reasons that library is structured that way, and they are
load-bearing:

- **It is testable there and untestable here.** Iridium has no automated test suite, and
  §16's mutation-testing harness is the primary measure of static-engine coverage. A
  console harness in its own project can run it; an FMX unit inside Iridium cannot.
- **RTL-only by design.** Nothing in `src\` may reference FMX, libantimony, or
  libRoadRunner. That is what lets the same engine be driven later by a bifurcation tool, a
  batch checker, or a genuine CLI, and it is what makes the golden-model corpus runnable
  without a GUI.
- **The boundary is already a house pattern.** `uMetaSymbolProvider.ISymbolProvider` exists
  for precisely this reason — so the metadata validator can ask "does this name exist?"
  without linking to RoadRunner. `IModelSource` (§5) is the same idea applied to reaction
  structure.

Iridium contributes exactly two things: an `IModelSource` implementation over
`uAntimonyAPI`, and the UI (§14).

**Consequence to accept deliberately:** this is a third external source root to keep in sync
across machines, alongside `Antimony_MetaData_Support` and `libAntimony_Delphi_Bindings`. Unlike
libRoadRunner and RhodyComponents, it is the user's own project with no other consumers, so
a bug in it is fixed there rather than worked around in Iridium.

## 5. Model access

**Hard constraint: no new native dependency, and no libSBML.** Iridium ships
`libantimony.dll` and `roadrunner_c_api.dll` and that is the whole native surface. Adding a
third means a new deployment artefact on two platforms, a new failure mode in `FormCreate`,
and a new thing to version-match.

Everything the checker needs comes from libantimony. Nothing below reads SBML at all.

(For the record: `libantimony.dll` statically links libSBML internally but does not
re-export a C API for AST traversal, so that copy is unreachable. Not a route, and not
needed.)

### 5.1 Prerequisite — migrate Iridium to the newer `uAntimonyAPI`

Iridium currently uses its own in-repo `uAntimonyAPI.pas`, which binds twelve entry points.
The maintained wrapper at `..\libAntimony_Delphi_Bindings\` (`uAntimonyAPI.pas`, `uAntimonyRaw.pas`,
`uAntimonyTypes.pas`) covers essentially the whole C API and is what this project builds on.
**Migrating to it is M0**, ahead of any checker code.

The migration is small, because Iridium touches almost none of the API:

| Iridium call site | Becomes |
| :---- | :---- |
| `uModelSession.pas:284` `getSBMLFromAntimony` | `antimonyToSBML` |
| `ufMain.pas:1153` `getSBMLFromAntimony` | `antimonyToSBML` |
| `ufMain.pas:1929`, `2457` `getAntimonyFromSBML` | `sbmlToAntimony(...).sbmlStr` |
| library load in `FormCreate` | `loadAntimonyLibrary(out errMsg)` |

Five call sites, two functions. The new wrapper was written with these callers in mind —
`antimonyToSBML` returns the same `TModelErrorState` shape (`errMsg` / `sbmlStr` / `ok`) and
keeps the same `strictWarnings` semantics, so `uModelSession`'s branch logic is unchanged.
`sbmlToAntimony` returns Antimony text in the field still named `sbmlStr`, deliberately, for
the same reason.

Two things to get right during the migration:

- **`TModelErrorState` now exists in two places** — `uCommonTypes.pas` and
  `uAntimonyTypes.pas`. Delete Iridium's and use the library's, rather than leaving two
  identical records whose unit order decides which one a given file sees.
- **`freeAll` must be called.** The new wrapper copies every returned pointer into a Delphi
  string immediately, so `freeAll` is safe at any point — and necessary, because libantimony
  leaks otherwise. The checker performs many queries per run, so the adapter (§5.3) calls
  `freeAll` once when it is done, not per query.

Beyond the migration, the new API's `getWarnings` and `getSBMLWarnings` are worth surfacing
in Iridium independently of this project — but that is a separate change, not this one.

### 5.2 The `IModelSource` interface

The engine sees a model only through this, so `src\` links to nothing native.

```pascal
IModelSource = interface
  function  ReactionCount : Integer;
  function  ReactionId    (AIndex : Integer) : string;
  function  RateLawText   (AIndex : Integer) : string;
  function  Reactants     (AIndex : Integer) : TArray<TSpeciesRef>;  // name + stoichiometry
  function  Products      (AIndex : Integer) : TArray<TSpeciesRef>;
  function  Modifiers     (AIndex : Integer) : TArray<TModifierRef>; // name + declared role

  function  SymbolKind    (const AName : string) : TSymbolKind;      // species/param/compartment
  function  HasValue      (const AName : string) : Boolean;
  function  ValueOf       (const AName : string) : Double;
  function  AssignmentRule (const AName : string) : string;          // '' if none
  function  UserFunction  (const AName : string;
                           out AArgs : TArray<string>;
                           out ABody : string) : Boolean;

  function  SourceLineOf  (AReactionIndex : Integer) : Integer;      // -1 if unknown
end;
```

`TSpeciesRef` carries both the numeric stoichiometry and its **source text**, because a
stoichiometry may be written as a symbol (`S1 + n S2 => S3; n = 2*p1`). §6.5's
`exponents_from: stoichiometry` has to cope with that.

`SourceLineOf` exists only so the UI can jump to a finding. Nothing in the engine reads it.

### 5.3 Where Iridium's adapter gets each field

All from `..\libAntimony_Delphi_Bindings\uAntimonyAPI.pas`, all already wrapped as typed Delphi
`TArray<>`/`TStringGrid2D` — **there is no marshalling left to write**:

| `IModelSource` member | Antimony API |
| :---- | :---- |
| `ReactionCount` | `getNumReactions` |
| `ReactionId` | `getNthReactionName` |
| `RateLawText` | `getNthReactionRate` |
| `Reactants` | `getNthReactionReactantNames` + `getNthReactionReactantStoichiometries` + `...StoichiometryString` |
| `Products` | the `Product` equivalents |
| `Modifiers` | `getNthInteractionInteractorNames` + `getNthInteractionDivider` (§5.5) |
| `SymbolKind` | `getTypeOfSymbol` → `TReturnType` |
| `HasValue` | `getSymbolHasValue` |
| `ValueOf` | `getNthSymbolEquationOfType` / `getSymbolEquationsOfType` |
| `AssignmentRule` | `getSymbolAssignmentRulesOfType` |
| `UserFunction` | `getNumUserFunctions`, `getNthUserFunctionName/Arguments/Body` |

`getSymbolHasValue` deserves note: it answers `S014` (referenced but uninitialised) directly,
which the Python draft would have had to reconstruct from libSBML.

The adapter **catches `EAntimonyError`**. The new wrapper raises on failure, and a checker
that propagates an exception out of a model query turns "this model is odd" into "Iridium
crashed". A failed query degrades that one field, and the affected check is reported as not
performed rather than as passed.

### 5.4 Reversibility is not modelled

Dropped. The SBML `reversible` attribute is deprecated and carries no reliable meaning, so
`applicability` has no `reversible` key and there is no `IsReversible`. Irreversible and
reversible forms of the same law are distinguished structurally, which they are anyway —
`Vm*S/(Km+S)` and the reversible form are not close in tree distance.

This is also what removes the last reason to touch SBML: with reversibility gone, **no part
of the checker reads an SBML document.**

### 5.5 Modifiers are declared, not derived

Antimony states modifier roles explicitly: `-o` is activation, `-|` is inhibition, `-(` is a
generic interaction. `getNthInteractionDivider` returns these as `TReactionDivider`, so the
adapter reports a modifier's *declared* role rather than inferring one.

This is strictly better than the obvious fallback (a species in the rate law that is neither
reactant nor product), and it maps directly onto §6.3's `position` attribute: `-o` → `activator`,
`-|` → `inhibitor`. Where a species appears in a rate law with no interaction declared, the
fallback still applies and the role is recorded as the unspecified `modifier` — which is
itself worth reporting once inhibition laws are registered, since an inhibitor bound by
position alone is exactly the case `S007` (role swap) exists to catch.

### 5.6 The checker does not require a loaded RoadRunner

Because everything above comes from Antimony, **the static engine can check a model that
does not simulate.** This is worth protecting: a model with a malformed rate law is exactly
the model that fails to load, and a linter that refuses to run at the moment it is most
needed is useless. The dynamic engine (§11) uses its own evaluator for the same reason.

## 6. Rate Law Definition schema

An RLD is a declarative record, stored as **JSON**.

### 6.1 Why JSON and not YAML

There is no YAML parser in the Delphi RTL, and writing one is a project in itself.
`System.JSON` is already the house format (`uPreferences.pas`, the plot styling snapshots).
The Python draft's field names are kept identical, so the two are mechanically
interconvertible and a future `ratelint` could read the same registry.

### 6.2 Fields

| Field | Required | Description |
| :---- | :---- | :---- |
| `id` | yes | Stable machine identifier, e.g. `michaelis_menten_irrev` |
| `name` | yes | Human-readable name |
| `version` | yes | Integer, incremented on edit |
| `enabled` | yes | Whether this law participates in checking |
| `expression` | yes | Canonical rate expression in Antimony-compatible infix syntax |
| `roles` | yes | Map of each symbol in `expression` to its role and semantics |
| `naming_conventions` | no | Accepted alternate identifiers per role |
| `applicability` | no | Structural preconditions (stoichiometry, symbol arity) |
| `invariants` | no | Declared behavioural properties, used by the dynamic engine |
| `sampling` | no | Domain and grid used for numerical probing |
| `tolerances` | no | Per-law overrides for numeric comparison thresholds |
| `notes` | no | Free text shown in reports |

### 6.3 Role kinds

- `species` — a concentration variable. Sub-attributes: `position` (`substrate`, `product`,
  `modifier`, `inhibitor`, `activator`), `cardinality`. The non-substrate/product positions
  are checked against Antimony's declared interaction dividers (§5.5).
- `parameter` — a constant. Sub-attributes: `semantics` (a free-form tag such as `max_rate`,
  `half_saturation`, `cooperativity`, `dissociation`), `positive`, `integer`.
- `compartment` — a compartment volume.

Semantics tags are what let the dynamic engine assert *meaning*: that a `half_saturation`
parameter is actually the point of half-maximal rate, not merely a symbol in a plausible
position.

### 6.4 Example entry

```json
{
  "id": "michaelis_menten_irrev",
  "name": "Irreversible Michaelis-Menten",
  "version": 1,
  "enabled": true,
  "expression": "Vm * S / (Km + S)",
  "roles": {
    "S":  {"kind": "species",   "position": "substrate", "cardinality": 1},
    "Vm": {"kind": "parameter", "semantics": "max_rate",        "positive": true},
    "Km": {"kind": "parameter", "semantics": "half_saturation", "positive": true}
  },
  "naming_conventions": {
    "Vm": ["Vm", "Vmax", "V_max", "vmax"],
    "Km": ["Km", "KM", "K_m", "km"]
  },
  "applicability": {"reactants": 1, "products": ">=1"},
  "invariants": [
    {"type": "zero_at",     "point": {"S": 0}},
    {"type": "nonnegative", "domain": {"S": [0, "inf"]}},
    {"type": "monotonic",   "var": "S", "direction": "increasing"},
    {"type": "limit",       "var": "S", "to": "inf", "equals": "Vm"},
    {"type": "value_at",    "point": {"S": "Km"}, "equals": "Vm/2"}
  ],
  "sampling": {
    "S":  {"scale": "log", "range": [1e-3, 1e3], "n": 64},
    "Vm": {"scale": "log", "range": [1e-2, 1e2], "n": 6},
    "Km": {"scale": "log", "range": [1e-2, 1e2], "n": 6}
  }
}
```

### 6.5 Generative (parameterised) laws

Some laws are families, not fixed expressions — mass action of arbitrary order, n-substrate
laws. The schema supports a generative form where the expression contains an indexed product
or sum over a role with `cardinality: "n"`, and the concrete canonical expression is
**instantiated per reaction** from that reaction's stoichiometry before comparison:

```json
{ "id": "mass_action_irrev",
  "expression": "k * prod(Si^ai)",
  "roles": { "k":  {"kind": "parameter", "semantics": "rate_constant", "positive": true},
             "Si": {"kind": "species", "position": "substrate", "cardinality": "n"} },
  "applicability": {"exponents_from": "stoichiometry"} }
```

Without this, mass action needs one entry per order and the generality claim in §2 fails on
the most common law in biology.

**Symbolic stoichiometry is the awkward case.** `S1 + n S2 => S3` gives a stoichiometry that
is a name, not a number, and `getNthReactionMthReactantStoichiometries` returns NaN for it.
Instantiation must then use the *symbol* as the exponent, taking it from
`...StoichiometryString`, and compare symbolically. Where the symbol has a constant value the
comparison can fall back to the number; where it is variable, the law is instantiated with the
symbol and `S008` compares names rather than values. Deferred to M11, but the AST must not
preclude a non-numeric exponent.

## 7. Registry

### 7.1 Storage and precedence

Three layers, later overriding earlier by `id`:

1. **Built-in.** Compiled into the unit as JSON string constants, following the
   `uBuiltInModels.pas` pattern. A fresh install has Michaelis-Menten, Hill and mass action
   with no files to ship and nothing to go missing.
2. **User.** `<prefs dir>\RateLaws\*.json`, one file per RLD, beside the existing
   preferences JSON.
3. **Project-local.** `<model dir>\ratelaws\*.json` if present, so a model directory can
   carry its own law set and travel with it in Git.

Files are plain text and diffable by design.

### 7.2 Operations

`add`, `remove`, `enable`/`disable`, `list`, `show`, `validate`, `export`/`import` — as
methods on `TRateLawRegistry`, surfaced by the registry editor (§14). A built-in law can be
disabled or shadowed by a user entry with the same `id`, never deleted.

### 7.3 Self-validation is mandatory

**On every load and every add, each RLD's own canonical expression is checked against its own
declared invariants.** If the declared canonical form violates a declared invariant, the entry
is rejected with an error and does not participate in checking.

This is not a nicety. A bad registry entry silently produces false positives on *every model
checked afterwards*, and the user has no way to tell that the tool is wrong rather than the
model. Rejecting at load is the only point where the blame is still legible.

## 8. Parsing and canonicalisation

One infix parser, used for **both** sides. This is the reason §5 prefers Antimony's infix
rate law text: an SBML/MathML route would give the model side a second, differently-shaped
front-end feeding the same canonicaliser, and the two would drift.

    RLD "expression"  ─┐
                       ├─► TRateLawParser ─► AST ─► Canonicaliser ─► comparable tree
    IModelSource       │
      .RateLawText   ──┘

### 8.1 Grammar

Antimony-compatible infix: `+ - * / ^`, unary minus, parentheses, numeric literals,
identifiers, and function calls (`pow`, `exp`, `ln`, `log`, `sqrt`, `abs`, plus the
trigonometric set). Precedence and associativity as Antimony/SBML define them — in
particular `^` is right-associative and binds tighter than unary minus.

### 8.2 Normalisation rules

1. Flatten n-ary associative operators (`+`, `*`).
2. Rewrite `a / b` as `a * b^(-1)`; `a - b` as `a + (-1)*b`.
3. Fold numeric literal constants.
4. Sort commutative operand lists by a canonical key.
5. Normalise integer powers written as repeated multiplication (`S*S` ↔ `S^2`).
6. Resolve assignment rules to their defining expressions where the RLD requires it
   (`IModelSource.AssignmentRule`).
7. **Inline user-defined function calls.** A rate law may call a function the model defines
   (`function MM(s, v, k) v*s/(k+s) end`), in which case the reaction's expression is
   `MM(S, Vm, Km)` and comparing it structurally against `Vm*S/(Km+S)` fails for a reason
   that has nothing to do with the model being wrong. `IModelSource.UserFunction` supplies
   the argument list and body; the canonicaliser substitutes.

Rule 7 was absent from the Python draft and is a genuine gap it would have hit: a user who
factors a repeated rate law into a function — good modelling practice, and common — would
have had every such reaction reported as unassociated.

### 8.3 Both trees are retained

**Canonicalisation must be applied identically to the model's AST and the RLD's AST, and the
pre-canonical tree must be kept.** Some defects are visible only *before* normalisation — a
missing parenthesis, since normalising is precisely what erases the difference — and some
only *after* — a duplicated operand. The static engine has access to both forms and each
defect class declares which it reads.

Rule 4's canonical key must be a total order and must not depend on hash iteration order, or
two runs over the same model produce different trees and the diff is unstable.

## 9. Association

Which law is this reaction *supposed* to be? Three modes, in priority order:

1. **Explicit annotation (preferred).** The modeller tags the reaction in the Antimony
   source. Unambiguous, and the only mode that can report "you meant MM and wrote something
   else entirely" rather than "this matches nothing."
2. **Applicability filter + best-match inference.** Filter enabled RLDs by `applicability`
   (stoichiometry, symbol arity), then score survivors by structural similarity. Associate
   if the top score clears an absolute threshold *and* beats the runner-up by a margin.
3. **Unassociated.** Emit `S001` INFO, not an error. A model may legitimately use a law that
   is not registered, and a linter that treats that as a failure will be turned off.

Ambiguity (two laws within the margin) is reported as `S002`, since it usually means a typo
made the expression equidistant from two correct forms.

### 9.1 Annotation syntax

Iridium already parses `@`-prefixed Antimony comments for simulation metadata
(`uMetaExperiments`). The annotation follows that convention rather than inventing a second:

    # @ratelaw michaelis_menten_irrev
    J1: S -> P; Vm*S/(Km + S);

**Open decision:** whether this is a standalone comment form or an addition to the metadata
block grammar. The block is specified in
`Antimony_MetaData_Support\simulation-metadata-spec.md`, which records Iridium as its
reference implementation — so adding a command there is a change to *that* spec and must be
made there, not improvised here. A standalone `# @ratelaw` line avoids that entirely and is
per-reaction rather than per-model, which fits better. Recommend standalone.

## 10. Static check engine

Generic structural comparison of the reaction AST against the associated RLD's instantiated
canonical AST, under a role binding.

### 10.1 Procedure

1. **Bind roles.** Map model identifiers to RLD roles using stoichiometry and declared
   interactions (species) and `naming_conventions` plus positional inference (parameters).
   Produce all plausible bindings if ambiguous.
2. **Score bindings.** Choose the binding minimising structural distance. Report if the best
   binding is *unnatural* — binding an identifier literally named `Km` to the `Vm` role is a
   strong signal of a role swap, and more informative than the structural diff it produces.
3. **Diff.** Structural diff between the two trees under the chosen binding.
4. **Classify.** Map each diff node to a defect class.

### 10.2 Defect classes

| Code | Severity | Meaning | Reads |
| :---- | :---- | :---- | :---- |
| `S001` | INFO | No registered law matches this reaction | — |
| `S002` | WARN | Association ambiguous between two or more laws | — |
| `S003` | ERROR | Operator substitution (`*` where `+` expected in a denominator) | canonical |
| `S004` | ERROR | Duplicated operand where distinct symbols expected (`Km + Km`) | canonical |
| `S005` | ERROR | Required symbol missing from expression | canonical |
| `S006` | WARN | Extraneous symbol not present in canonical form | canonical |
| `S007` | ERROR | Role swap — symbol in the wrong structural slot | canonical |
| `S008` | ERROR | Exponent mismatch (missing, extra, wrong power) | canonical |
| `S009` | ERROR | Sign or negation defect | canonical |
| `S010` | ERROR | Parenthesisation/precedence defect — same symbols, different tree | **pre-canonical** |
| `S011` | WARN | Naming-convention violation | binding |
| `S012` | WARN | Numeric literal where a named parameter is expected | canonical |
| `S013` | ERROR | Applicability violation (stoichiometry / symbol arity) | model source |
| `S014` | ERROR | Symbol referenced but undefined or uninitialised | model source |

Each defect carries: reaction id, associated law id, the offending subexpression, the
corresponding canonical subexpression, and a suggested correction where one is unambiguous.

### 10.3 Model-level checks (law-independent, always on)

A separate, always-on set needing no association and no registry: undefined references
(`SymbolKind`), uninitialised parameters (`getSymbolHasValue`), missing kinetic law (empty
`RateLawText`), reactants absent from the rate law, species in a default compartment
(`getCompartmentForSymbol`). These run even when nothing associates, so a model with no
registered laws still gets a useful report — which matters for adoption.

## 11. Dynamic check engine

*Even if it parses, does it behave like the law it claims to be?*

### 11.1 The evaluator is ours, not RoadRunner's

§11.2 samples a rate expression over a grid — 64 values of `S` × 6 of `Vm` × 6 of `Km` is
2,304 evaluations for one reaction against one law. Doing that through
`setValue`/`getReactionRates` would mutate the user's loaded engine thousands of times per
check and leave the session in a state they did not ask for.

We have the AST already. **A direct numeric evaluator over it is fast, side-effect free, and
needs no engine loaded** — which preserves §5.6. It is also the only way to evaluate the
*canonical* form, which does not exist in the model at all.

### 11.2 Layer 1 — Invariant probing (rate law in isolation)

Compile the reaction's kinetic expression to a numeric function of its symbols, sample over
the `sampling` grid, evaluate the declared `invariants`. Each invariant type is implemented
**once, generically**:

| Type | Test |
| :---- | :---- |
| `zero_at` | Rate is ≈0 at the specified point |
| `zero_at_any_zero` | Rate is ≈0 whenever any listed variable is 0 |
| `nonnegative` | Rate ≥ 0 across the sampled domain |
| `monotonic` | Sign of finite differences is consistent |
| `bounded_above` | Rate never exceeds the given expression |
| `limit` | Rate approaches the given expression as a variable → 0 or ∞ |
| `value_at` | Rate equals the given expression at a point (half-max at `Km`) |
| `sigmoidal` | Second derivative changes sign exactly once |
| `homogeneous` | Scaling inputs by λ scales output by λ^degree |
| `symmetric` | Output invariant under swapping listed variables |

`limit` with `to: inf` is evaluated as a Richardson-style extrapolation over the top of the
sampling range, not by substituting a large number — the latter reports failure for any law
that saturates slowly.

### 11.3 Layer 2 — Differential comparison against canonical

Evaluate the model's expression and the RLD's canonical expression over the same grid under
the role binding. Report maximum relative deviation and **where** in the domain it occurs.

This catches what the static diff cannot: algebraically different forms that are *nearly*
equal in a common regime and wildly wrong outside it — exactly the errors that survive
eyeballing.

### 11.4 Layer 3 — In-model simulation (deferred)

Simulate, then optionally substitute the canonical form and re-simulate, comparing
trajectories. A structural defect producing no trajectory divergence in the model's operating
regime is downgraded and reported as such (`D106`), which is useful triage.

**If built, it must construct its own `TRoadRunner` from the SBML rather than borrowing the
session's** — the same reasoning that makes `uBioModelsCache`'s download build its own HTTP
client rather than share one with a running search. A checker that leaves the user's loaded
model integrated to t=100 with substituted kinetics is a bug, not a check. This is the one
part of the system that would touch SBML, and the only reason to keep `antimonyToSBML` in
view at all.

Deferred to M14 and explicitly optional. Layers 1 and 2 carry most of the value.

### 11.5 Defect classes

| Code | Severity | Meaning |
| :---- | :---- | :---- |
| `D001` | ERROR | NaN/Inf or divide-by-zero inside the declared domain |
| `D002` | ERROR | Negative rate where non-negativity declared |
| `D003` | ERROR | Monotonicity violated |
| `D004` | ERROR | Bound or limit violated |
| `D005` | ERROR | Parameter semantics violated (half-max not at the half-saturation parameter) |
| `D006` | WARN | Deviation from canonical form exceeds tolerance |
| `D007` | ERROR | Shape invariant violated (sigmoidicity, symmetry, homogeneity) |
| `D101` | ERROR | Simulation failed to run *(Layer 3)* |
| `D102` | ERROR | Negative species concentration in trajectory *(Layer 3)* |
| `D103` | ERROR | NaN/Inf in trajectory *(Layer 3)* |
| `D104` | WARN | Steady state not reached / unbounded growth *(Layer 3)* |
| `D105` | WARN | Trajectory diverges from canonical-substituted model *(Layer 3)* |
| `D106` | INFO | Static defect present but no measurable dynamic consequence *(Layer 3)* |

**Every dynamic defect must report a witness** — the specific parameter and concentration
values at which the property failed. A dynamic finding without a reproducible witness is not
actionable, and the user cannot distinguish it from a bug in the checker.

## 12. Diagnostics

One record type, shared by both engines:

```pascal
TDiagnostic = record
  Code        : string;        // 'S004'
  Severity    : TSeverity;     // sevInfo, sevWarn, sevError
  LawId       : string;        // '' when unassociated
  ReactionId  : string;
  SourceLine  : Integer;       // -1 if unknown; for the UI only
  Message     : string;
  Found       : string;        // offending subexpression
  Expected    : string;        // canonical subexpression
  Suggestion  : string;        // '' when not unambiguous
  Evidence    : string;        // witness point, dynamic findings only
end;
```

`TCheckResult` holds the diagnostics plus per-reaction association outcomes and the set of
laws that participated, so a report can say what was checked as well as what failed — "0
findings" and "nothing was checked" must never look alike.

## 13. Public interface

The main program uses **one facade unit**. Everything else is implementation.

```pascal
uses RateLaw.Static, RateLaw.Registry, RateLaw.Types;

var
  Registry : TRateLawRegistry;
  Checker  : TRateLawChecker;
  Res      : TCheckResult;
begin
  Registry := TRateLawRegistry.Create;
  Registry.LoadDefaults (UserDir, ProjectDir);   // built-in + user + project
  Registry.Disable ('hill_activation');

  Res := CheckModel (Registry, ModelSource);     // ModelSource is an IModelSource
  try
    Memo.Text := ...;                            // reporter is M10
    if Res.ErrorCount > 0 then ...
  finally
    Res.Free;
  end;
```

Built so far: `RateLaw.Types` (`IModelSource`, `TRateLawDiagnostic`, `TCheckResult`),
`RateLaw.Ast`, `RateLaw.Parser`, `RateLaw.Canonical`, `RateLaw.Registry`,
`RateLaw.BuiltInLaws`, `RateLaw.Bind`, `RateLaw.Static`. Still to come: `RateLaw.Eval` and
`RateLaw.Dynamic` (M12–M13), `RateLaw.Report` (M10).

**The facade is not built yet.** `CheckModel` in `RateLaw.Static` is the entry point today,
which means Iridium would reach past two units to use it. A `RateLaw.Checker` facade with
an options record belongs with the UI work in M10, when there is a caller to shape it
around; inventing it earlier would be guessing at what the UI needs.

Reporters: `AsText` (grouped by reaction, ordered by severity) and `AsJson` (for a future CLI
and for the test harness's golden comparisons). Neither is written — the harness prints
`TRateLawDiagnostic.ToString` directly.

## 14. UI integration — **open decision**

`btnModelChecker` exists in the toolbar with no `OnClick` yet. The engine above is
deliberately independent of this choice; it can be built and tested before the UI is settled.

What *is* settled: the checker **computes only when the user asks**. It does not run on
reload, on edit, or on load. That follows the established rule that results in Iridium are
user-initiated, and it matters more here than elsewhere — a linter that runs while you type
reports the model as broken continuously, because a half-typed model *is* broken.

There are real tuning parameters (§15), so a bare button is not sufficient on its own.
Candidate shapes:

- **(a) Button → Text tab, settings in a modal dialog.** Cheapest. Reuses `BuildTextView`
  and the `ITextViewProvider` contract; settings follow the `ufConfigureCVODE` /
  `ufConfigureSteadyState` precedent. Downside: the report is a dead memo — no clicking a
  finding to reach the reaction.
- **(b) A new output tab beside Plot / Text / Steady State**, holding a findings list.
  `tbSteadyState` shows a fourth tab is already an accepted pattern. Clicking a finding
  selects the reaction in the editor, which is what makes a linter usable on a model with
  more than three findings.
- **(c) A full frame**, like `uFrameMetadata` — the existing precedent for a report-only
  frame with deliberately no `ActiveAnalysisKey` entry. Most discoverable; puts a
  non-analysis into the analysis carousel.
- **(d) A non-modal results window**, like `ufBar3DWindow`. Stays open beside the editor
  across edit/re-check cycles.

Whichever is chosen, `SourceLineOf` is what makes click-to-navigate possible, so it belongs
in `IModelSource` from the start even if the first UI ignores it.

## 15. Configuration

Persisted with the existing preferences JSON: registry paths, per-code severity overrides,
global numeric tolerances, sampling density, association threshold and margin, and the
dynamic-checking on/off switch. Same rule as `uPreferences`: **a missing, corrupt or
future-version configuration must leave the checker running on its defaults**, never prevent
it running.

## 16. Testing strategy

All of this lives in `..\ModelCheckerLib\tests\` and runs from a console harness. None of
it requires Iridium, FMX, or a native DLL — the corpus drives a fixture `IModelSource`.

- **Golden models.** Small models with an expected diagnostic set. Includes `Vm*S/(Km + Km)`
  as the founding regression test.
- **Mutation testing.** Take a correct model, programmatically mutate the rate law (swap an
  operator, duplicate an operand, drop a parenthesis, permute two identifiers), assert the
  correct defect class is reported. This is the **primary measure of static-engine
  coverage**, and it generalises automatically to every registered law — which is the whole
  point of §2.
- **False-positive corpus.** Correct models in unusual but valid styles — including rate laws
  written through user-defined functions (§8.2 rule 7) — which must produce zero errors.
  Guards against over-aggressive canonicalisation.
- **Registry self-validation.** Every shipped RLD passes its own invariants (§7.3).
- **Property tests on canonicalisation.** Randomly generated algebraically-equivalent
  expression pairs must canonicalise identically; non-equivalent pairs must not.

## 17. Non-goals

- Not a computer algebra system. Equivalence is structural plus numerical, not symbolic proof.
- Not a model builder or auto-fixer. Suggestions are advisory; the tool never rewrites the model.
- Not an SBML validator.
- Not a background/continuous linter (§14).
- No CLI in this scope — though §4's structure leaves the door open, and `AsJson` exists for it.

## 18. Milestones

Each ends with something runnable and testable. **Status is as of 2026-08-25.**

Where a milestone was met by different work from the one planned, the row says so rather
than being quietly rewritten: the difference between what was designed and what the code
demanded is the useful part of a plan afterwards.

| # | Status | Milestone | Done when |
| :-- | :-- | :---- | :---- |
| **M0** | **done** | Migrate Iridium to `..\libAntimony_Delphi_Bindings\uAntimonyAPI.pas` (§5.1) | Iridium builds and runs on the new wrapper |
| **M1** | **done** | Sibling project skeleton, `RateLaw.Types`, `IModelSource`, console harness | The harness runs against a hand-built fixture model source |
| **M2** | **done** | Parser, AST, canonicaliser, stable AST printer | Algebraically-equivalent pairs canonicalise identically; non-equivalent do not |
| **M3** | **done** | Registry: JSON schema, three layers, load, validate, self-validation | Round-trips a registry; rejects a self-inconsistent RLD with a clear error |
| **M4** | **done** | MM and Hill entries; role binding | A correct MM model binds `Vm`/`Km`/`S` and reports nothing |
| **M5** | **done** | Static engine v1 — `S003`–`S008`, `S011`, `S014` | `Vm*S/(Km + Km)` caught as `S004`; the Hill equivalent caught with **no Hill-specific code** |
| **M6** | **done** | Iridium's `IModelSource` adapter; §10.3 model-level checks | A real model in Iridium reaches the engine and reports findings |
| **M7** | **done** | Association — annotation, applicability, inference, `S001`/`S002`/`S013` | An unannotated model associates correctly; an ambiguous case reports `S002` rather than guessing |
| **M8** | **part** | User-function inlining and assignment-rule resolution (§8.2 rules 6–7) | A model that factors its rate law into `function MM(...) end` reports the same findings as the inline form |
| **M9** | **done** | Mutation-testing harness | Coverage report runs over the whole registry; every mutation class detected |
| **M10** | **done** | UI (§14) — button, report, settings dialog, registry editor | `btnModelChecker` produces a readable report on a real model |
| **M11** | **done** | Generative laws — indexed product/sum, mass action, symbolic stoichiometry | One entry checks first-, second- and third-order reactions |
| **M12** | **done** | Dynamic Layer 1 — evaluator, sampling, invariants, `D001`–`D005`, `D007`, witnesses | A law whose half-max is in the wrong place is caught by `D005` with a witness |
| **M13** | **done** | Dynamic Layer 2 — `D006` | A near-equal-in-regime defect is caught, with the domain point where it diverges |
| **M14** | **done** | Registry expansion — reversible MM, competitive/uncompetitive/non-competitive inhibition, convenience kinetics | All pass self-validation and mutation tests, **with zero new law-specific code** |
| **M15** | | Documentation — RLD authoring guide, defect code reference, worked walkthrough | Someone else in the lab can add a rate law without reading the source |
| **M16** | **part** | *Stretch:* Layer 3 simulation checks, `D101`–`D106` | A defect with no dynamic consequence is reported as `D106`, not a bare error — `D101` is built (§18.10); the trajectory comparison is not |
| **M17** | **done** | *Stretch:* BioModels corpus evaluation | A quantified false-positive figure and a triage list of causes — see §18.6 |

Only M0, M6, M10 and M17's harness touched Iridium; everything else is library work with its own tests.

### 18.1 Where it stands, measured

Run `ModelCheckerLib_Project` for the suite and `-coverage` for the matrix.

| | |
| :---- | :---- |
| corpus cases | 30/30 |
| role-binding cases | 8/8 |
| malformed registry entries rejected | 11/11 |
| canonicalisation pairs (agree / stay distinct) | 18/18, 9/9 |
| registered laws, all self-validating | 11 |
| mutation coverage: correct forms left clean | 11/11 |
| mutation coverage: detected at all | 61/61 |
| mutation coverage: classified exactly right | 40/61 |
| Iridium's own `.ant` models: reactions associated | 20/23, 0 errors |

The three unassociated reactions are `Lorenz.ant`, which is not a kinetic model and correctly
matches nothing. The inexact classifications are all *detected and reported*, under a
different code from the one the mutation predicted — mostly `S002` where a mutation destroyed
the very structure that separates two similar laws, which is the right answer rather than a
gap. That proportion has fallen as the registry grew (28/38 at three laws, 37/60 at eleven)
and will keep falling: laws that differ in one place are, by construction, hard to tell apart
once that place is damaged.

### 18.2 The one partial

**M8 — user-function inlining, done early; assignment rules untouched.**
`MM(S, Vm, Km)` against a model-defined `function MM(s,v,k) v*s/(k+s) end` was reporting
`S003` — a false positive on a *correctly written* model, which is the failure that gets a
linter switched off. That made it a correctness bug rather than a missing feature, so it was
fixed during M5. Assignment-rule resolution, the other half of the milestone, is not built:
a rate law that refers to a symbol defined by an assignment rule is compared as written.

### 18.3 What the registry expansion found (M14)

M14 exists to falsify the project's central claim, so its results are reported whether or not
they flatter it. **Eight new laws were authored — reversible Michaelis-Menten in two forms,
Hill repression, the three classical inhibition patterns, ordered bi-bi and convenience
kinetics — and seven needed no engine change whatever.** The claim mostly holds.

One did not, and it was a real defect:

- **`BuildContexts` produced INCOMPLETE environments.** It grew the parameter product one
  variable at a time and stopped at the cap, leaving every variable it had not yet reached
  absent altogether. Any law with six or more roles then failed every invariant at load with
  "undefined symbol" and was rejected outright. `ordered_bi_bi` and `convenience_uni_uni`
  both died this way. It was invisible on smaller laws because the cap was never reached —
  which is precisely why the exercise is to author laws rather than to reason about them.
  Contexts are now decoded from an index into the product space, so every variable has a
  value in every one, and truncation strides through the space rather than pinning all but
  the last variable to its smallest value.

Two further changes were made that are improvements rather than defects:

- **`limit` now tests convergence, not a single point.** Three probes, accepted if the far
  one is close *or* the error is collapsing. Reversible Michaelis-Menten genuinely tends to
  `Vf`, but where the reverse term is large it is still ten per cent short at `S = 1e10`, and
  the old single-probe test rejected the law for being correct. §10.1 always said this should
  be an extrapolation; M12 simplified it and M14 found the cost.

- **`sampling` turns out to be load-bearing, not decoration.** Both Hill laws failed their own
  `limit` invariant because the default parameter grid reaches `n = 0.01`, where `S^n` grows
  so slowly that the rate is still half its ceiling at `S = 1e10`. A Hill coefficient below 1
  is not a Hill coefficient. Declaring `sampling` is how a law says over what range its claims
  are meant to be judged, and without it a correct law is rejected. This is now the first
  thing the authoring manual says about invariants.

And one finding about the design rather than the code:

- **Which WRITING of a law is registered matters.** Canonicalisation normalises writing, not
  algebra, and deliberately does not distribute a product over a sum — that refusal is what
  protects the misplaced-parenthesis signal. So `(Vf/Ks)*(S - P/Keq)/(1 + S/Ks + P/Kp)` and
  the algebraically identical `Vf*(S - P/Keq)/(Ks*(1 + S/Ks + P/Kp))` are different trees, and
  a model using the unregistered one is reported as a regrouping. The conventional setting-out
  is registered and the alternative is named in the law's notes. If the other writing proves
  common in practice the options are a second entry or distributing a scalar into a reciprocal
  sum, and the latter costs parenthesis-defect sensitivity — so it wants evidence first.

### 18.4 What Layer 2 turned out to be for (M13)

The milestone asks for a defect that is near-equal in the tested regime and wrong outside it,
and `D006` delivers that: `Vm*S/Km` against Michaelis-Menten reports **up to 100% divergence,
worst at S = 1000**, with the parameter values that produced it.

But the more useful half was not the one specified. The same comparison, run on an expression
that is algebraically identical and structurally different, finds **no divergence at all** —
and saying so is what separates "you wrote this differently" from "you wrote this wrongly".
That earns its own code:

- **`D008` (INFO) — written differently, computes the same rate.** `2*Vm*S/(2*Km + 2*S)`
  draws an `S010` regrouping error from the static engine, which is true and reads like a
  fault. `D008` reports 1024 points compared and a largest difference of 0%, which retires it.

This directly addresses the false positive recorded in §18.3: a model using the unregistered
writing of a law is no longer merely flagged, it is flagged *and* exonerated. The structural
finding still stands — the model does not match the registered form, and a reader may want to
know that — but its consequence is now stated rather than left to be guessed at.

`D006` also fires wherever an existing structural defect has numerical consequences, which is
why three earlier corpus cases gained it. That is the layer working, not noise: a wrong rate
law usually is wrong by some amount, and the amount is worth knowing.

### 18.5 Deviations worth recording

- **The taxonomy grew.** Beyond the specified codes the engine emits `S010` (regrouping),
  `S012`, `D008` (see §18.4), and the model-level `S015`–`S019`: missing kinetic law, unparsable kinetic law,
  reactant absent from its own rate law, species with no initial value, and an annotation
  naming an unavailable law. `R015` reports a registry entry that fails its own invariants.
  The last three need no association, so a model using unregistered laws still gets a
  useful report.

- **`S009` is specified and not implemented.** A negated rate law is reported as an
  extraneous term (`S006`) plus the behavioural consequences, rather than as a sign defect.

- **An uninitialised species is not an error.** §10.3 groups uninitialised symbols together;
  in practice an Antimony species with no initial value defaults to zero, which is how every
  intermediate in a chain is written. Reporting it as an error flagged every one of Iridium's
  own example models. Parameters and compartments keep `S014` at ERROR; species get `S018`
  at INFO.

- **The registry gained an `exponent` field** on a cardinality-`n` role, naming its
  per-instance exponent symbol. Without it the validator cannot tell an index variable from
  an undeclared identifier and rejects mass action as malformed. Logged per §2's
  falsification test: a law needing a schema change is a finding about the schema.

- **Generative entries are exempt from invariant self-validation.** `k * prod(Si^ai)` is a
  shape, not an expression: `prod` is no function and `ai` has no value until instantiation.
  Probing it found an undefined symbol every time and marked the entry invalid, which
  silently disabled mass action for every model. Its invariants are checked on the
  instantiated form instead.

- **The association metric was replaced, not tuned.** An approximate Dice coefficient over
  subtree signatures could not tell `Vm*S^n/(K^n + K^n)` from Hill *repression*, which
  contains the same parts rearranged, though Hill *activation* is one edit away and
  repression is two — so findings were reported against a law the model never resembled.
  Association now costs the very diff the static engine will perform, so the law chosen is
  by construction the law with least to explain.

- **Two admissions beyond distance, of which one survived contact with real models.**
  A misplaced parenthesis lands at maximum structural distance from the law it is a
  defective copy of, and a dropped term removes symbols altogether. Candidates were
  therefore admitted when the expression's symbols *were* the law's, or were a subset of
  them. Over BioModels both admissions produced a finding **every time they fired** — 775
  associations, 775 accusations of correct models — and the subset form, which did 659 of
  them, is gone. Same-symbols is kept: see §18.7.

- **Corpus comparison ignores INFO.** `S001` says "no registered law matches", a statement
  about the registry's coverage rather than about the model; a case that legitimately has no
  law yet would otherwise look like a failure for ever.

### 18.6 What the BioModels corpus said (M17)

M17 is the milestone that decides whether any of this is usable, and its answer is **no,
not yet**. Run over the 1075 models of the curated BioModels mirror on 2026-08-25:

| | |
| :---- | :---- |
| models attempted | 1075 |
| loaded | 1013 |
| would not load | 62 |
| reactions | 45319 |
| reactions associated to a law | 7766 (17.1%) |
| **models reporting an ERROR** | **335 (33.1%)** |
| models reporting a WARNING | 636 (62.8%) |
| models reporting anything at all | 657 (64.9%) |
| models entirely silent | 356 (35.1%) |

BioModels is curated, so **every one of those findings is presumed wrong until shown
otherwise**. A third of a curated corpus reporting an error is not a checker anyone would
leave switched on, and it is the exact failure §18.1 named as the one that matters most:
a correct model reporting anything is worse than a defect being missed.

The synthetic corpus did not predict this and could not have. Its cases are expressions
*written as laws*, sometimes damaged; BioModels reactions are written as SBML, which is a
different dialect of the same mathematics.

Reproduce it with `corpus/fetch.sh` and `corpus/run.sh` in the Iridium repository, and read
the tables with `corpus/report.py`.

#### The triage, in order of what it would buy

**1 — The compartment volume factor is invisible to the engine.** An SBML rate law is
`comp1*(kf_9*A - kr_9*I)`: a volume multiplied by kinetics. `skCompartment` is declared in
`RateLaw.Types` and read *nowhere else in the engine*, so the volume is just another
identifier. It is duly bound to a kinetic role — `S011` warns, in as many words, that
`"comp1" plays the Ks role` — the shape then fails to match, and `S005`/`S006`/`S010`
report the mismatch as a defect. The symbols `S011` warns about most often across the
corpus are `cell`, `compartment_`, `cyt`, `cytosol`, `compartment_1`, `cytoplasm`. This one
cause feeds the four largest codes.

**2 — The subset admission has no distance ceiling.** `RateLaw.Associate.pas` admits a
candidate at any distance if its symbols are a subset of the law's, and `RateLaw.Bind.pas`
lets a binding stand with one unfilled role on the same grounds. Together they let
`comp1*(kf_9*A - kr_9*I)` be claimed by `reversible_mm` at **d=0.976** and then reported
against it, `Kp` and all. Both admissions are right for what they were built for — a
misplaced parenthesis, a dropped term — and both were safe only because the synthetic
corpus contains no expressions that merely *share vocabulary* with a law. Real models are
full of them. **304 models (30.0%) report an error that traces to an association**; only
31 (3.1%) report an error that does not.

**3 — `S002` is one law pair, 3918 times.** Of 4396 ambiguity warnings, 3918 are
`hill_activation, hill_repression` and 368 are `reversible_mm, reversible_mm_keq`. §10's
reasoning — that a genuine coin-toss should check nothing rather than guess — is right, but
a pair that ties on nine models in ten is a registry problem, not a tie: the two Hill laws
are being offered as candidates for reactions that are neither.

**4 — `S017` does not exempt a species held constant.** 1249 warnings that a reactant is
absent from its own rate law, across 180 models. Only 16 are the `EmptySet` of a synthesis
reaction; the rest are ordinary boundary or clamped species, whose whole point is that
their concentration does not vary and so need not appear. `RateLaw.Static.pas` compares
every reactant name against the rate law's symbols with no exemption for either.

**5 — `S014` on parameters defined by assignment rules.** 147 findings over 18 models,
which is M8's unbuilt half (§18.2) seen from the outside: a parameter with no literal value
is not uninitialised when a rule computes it.

**6 — `S015` is correct and unusable as presented.** 18245 findings — 48% of all output —
over **11 models**, all genome-scale reconstructions with no kinetics at all. "This reaction
has no kinetic law" is true 4058 times in one file and worth saying once.

None of these is a deep design failure. Causes 1, 4 and 6 are contained fixes; cause 2 is a
threshold the corpus can now calibrate; cause 3 is registry work. What M17 establishes is
that they must be done before the checker is offered as anything but opt-in — and that the
synthetic corpus, at 30/30, cannot be the thing that says when they are done.

### 18.7 What M17's findings cost to fix, and what fixing them bought

The triage in §18.6 was acted on. Measured the same way, over the same 1013 models:

| | before | after |
| :---- | ----: | ----: |
| models reporting an ERROR | 335 (33.1%) | **192 (19.0%)** |
| models reporting a WARNING | 636 (62.8%) | 457 (45.1%) |
| models entirely silent | 356 (35.1%) | **534 (52.7%)** |
| reactions associated to a law | 7766 (17.1%) | 17783 (39.2%) |
| corpus cases | 30/30 | **45/45** |
| registered laws, all self-validating | 11 | **12** |
| mutation coverage: correct forms left clean | 11/11 | **12/12** |
| mutation coverage: detected at all | 61/61 | **57/62** |
| mutation coverage: classified exactly right | 40/61 | **45/62** |
| Iridium's own `.ant` models | 20/23, 0 errors | 20/23, 0 errors |

Association more than doubled while findings roughly halved, which is the shape a real fix
has: the volume factor and the null species had been *preventing* correct association as
well as corrupting it.

#### The engine did not know what a model looks like

Four of the six causes were the same kind of mistake — a thing every SBML model contains
that no part of the engine had heard of, and which was therefore treated as an ordinary
identifier:

- **The compartment volume.** `cyt*(kf*A - kr*B)` is a rate, not a rate of change of
  concentration. `skCompartment` was declared in `RateLaw.Types` and read nowhere else, so
  the volume was offered to every parameter role — the corpus reported, in as many words,
  that `cyt` plays the `Ks` role — and the shape then failed to match. Now stripped from
  the multiplicative spine before anything compares it, including **distributed over a
  sum** (`Cell*k3*P2*T2 - Cell*k4*CC`, which libSBML emits constantly), and only when the
  factor is common to every term: taking it off `Cell*a - b` would change the meaning.
- **`EmptySet`.** Antimony's name for the absent side of `-> P`. Mass action instantiated
  over `[EmptySet]` and produced `k*EmptySet`, against which every real synthesis rate law
  looked like a substrate swap: *"M appears where EmptySet was expected"*, 269 times, and
  the largest single error class remaining once association was fixed.
- **`time` and `pi`.** Defined by the language, not the model. 113 of the 147 `S014`
  findings, and both were offered to the binder as candidate parameters, where `pi` could
  be chosen to play a `Km`.
- **A species held constant.** Clamping a species *is* the statement that the kinetics do
  not vary with it, so its absence from the rate law is correct rather than an omission.
  313 of 400 sampled `S017` findings were on SBML boundary species.

None of these is subtle. They were invisible because the synthetic corpus is written in the
vocabulary the engine already had.

#### The one deliberate loss

Removing the subset admission cost five *drop* mutations, so **detection fell from 61/61 to
57/62** and that row is a real regression, recorded rather than rebaselined away. It bought
659 false positives, and the capability is not gone — it moves to the annotation, which is
checked at any distance and is the only evidence that can say "you meant Michaelis-Menten
and wrote something else". `dropped-term-still-caught-when-declared` is the case that
holds that claim to account. The trade follows §18.1's own ordering: a correct model
reporting anything is worse than a defect being missed.

#### The falsification test fired, once, and correctly

§2 says a law that cannot be added without changing the engine is a defect in the engine.
**Reversible mass action was such a law.** `InstantiateGenerative` built "the one scalar
parameter times the product of the reactants" directly and *explicitly refused* a family
declaring a second scalar, so `kf*prod(Si^ai) - kr*prod(Pj^bj)` — one of the commonest rate
laws there is — was not merely unregistered but inexpressible. The instantiator is now
template-driven: each `prod(...)` expands over the species of its role's position and the
rest of the template is carried through, so nothing in the unit knows what mass action
looks like. `mass_action_rev` was then added as a registry entry alone.

Two things that went wrong while adding it are worth recording, because both were mine and
both were caught only by re-measuring:

- **`k1`/`k2` as direction conventions.** A numeric suffix is a reaction index at least as
  often as a direction, so every model writing `k2` for the forward constant of reaction 2
  was told its identifiers looked transposed. Aliases must be unambiguous or they
  manufacture defects.
- **No applicability constraint.** With no products the reverse product is the empty
  product, `1`, and the law degenerates to `kf*S - kr`, which fits irreversible reactions
  loosely enough to report against them — 328 errors. The schema already had
  `reactants`/`products`; the entry had simply not used them. That half of the design held.

The corpus was re-run after **every** one of these changes. Two of the six passes made the
numbers worse before they made them better, and neither would have been noticed from the
synthetic suite, which stayed green throughout.

#### Where it stops, and why

The residue is no longer obviously wrong. The `S007` findings that remain say a rate law
depends on a species that is not its own reactant — `pRbY10` where the reaction consumes
`pRbY11`, `k26*s164` where it consumes `s121` — which is either a typo in the model or a
deliberate shortcut, and is exactly what the checker exists to surface.

What remains clearly noisy is one co-occurring cluster: 255 models reporting `S006` and
`S011` together, where a reaction associates *within* the floor but carries an extraneous
term. Tightening `AssociationFloor` would remove them — d = 0.0 is 12101 associations
against 13 findings, while d = 0.1–0.3 is overwhelmingly findings — but the founding
duplicated-operand defect sits at **d = 0.125**, inside the band that would be cut. A
threshold cannot separate "close to L but not L" from "L, damaged"; that is the tension
§18.5 already recorded as M7's to resolve, and it is still open. Picking a number here
would trade detection for quiet without saying so.

### 18.8 Why the errors that remain remain, and one thing that cannot be fixed here

Of the 863 reactions still producing an error after §18.7, only **8 are associated at
d = 0.000** — an exact structural match, where an error means a genuine defect. 662 (77%)
are associated at d >= 0.3: the expression is not much like the law it was attached to.
So the residue is overwhelmingly "this reaction follows a law the registry does not have,
and the nearest registered law was reported against it instead". Traced back to their SBML,
798 of them divide as:

| | | |
| :---- | ----: | ----: |
| rate law uses a **modifier**, not its substrate | 292 | 37% |
| uses a modifier as well as its substrate | 124 | 16% |
| omits its substrate, no modifier either | 37 | 5% |
| uses its own substrates | 345 | 43% |

**The missing family (416 reactions) is catalytic mass action.** `DHFReductase` has reactant
`FH2f`, modifier `FH2b` and rate `kter*FH2b`; `vATP` has reactant `P`, modifier `ATP` and
rate `KATPASE*ATP`. Mass action takes its species from stoichiometry alone, so it
instantiates `k*FH2f` and the model's `k*FH2b` reads as a substrate swap — 255 of the S007
findings. `k*E` and `k*E*prod(Si)` are one generative family, and now that the instantiator
is template-driven it is a registry entry rather than an engine change: `TSpeciesPosition`
already has the modifier positions. **Not yet done.**

#### The lumped rate constant: measured, and rejected

The other 345 look like a smaller problem and are not. Their rate constant is not a symbol:

    IXa*VIIIa/r26_c          second-order mass action with k = 1/r26_c
    3*kon24*E23P*I           mass action with k = 3*kon24
    konII*II_f*LIPID/nva     both at once

A role can only bind to one identifier, so none of these matched the law they plainly are.
Collapsing a term's constant factors into a single factor before binding was implemented,
and then removed. Both halves failed, for different reasons, and the numbers are the
argument:

- **Lumping a constant SUBTREE** (anything with no species in it) absorbs `(Km + Km)` — the
  founding duplicated-operand defect is a constant subtree. Suite 45/45 → **34/45**,
  detection 57 → **35**, correct-forms-clean 12/12 → **10/12**.
- **Lumping an atomic constant in a DENOMINATOR** absorbs `Vm*S/Km`, which is mass action
  with k = Vm/Km *and* Michaelis-Menten with the parentheses in the wrong place. Suite
  **39/45**, detection **42**. The lost cases name themselves: `mm-missing-parens`,
  `mm-operator-substitution`, `annotation-catches-wrong-law`.
- **Lumping only a numeric coefficient** (`3*kon24`) is structurally safe — suite 44/45,
  detection 57/62, 12/12 clean — and is behaviourally *unsound*. The dynamic layer samples
  the law's roles, so with `2*Vm` collapsed to one symbol it evaluates `Vm*S/(2Km + 2S)`
  where the model computes `2Vm*S/(2Km + 2S)`: a different function. It cost
  `dynamic-same-rate-written-differently`, which exists to say that a rate written
  differently is still the same rate.

The general statement, which is the useful part: **a normalisation that makes an expression
match MORE laws hides exactly the defects that consist of matching the wrong law.** It is
the same wall as the association floor in §18.7, reached from the other side. `Vm*S/Km` is
genuinely both readings, and no amount of rewriting picks the right one — only the
modeller's annotation does.

A lumped constant could be made behaviourally sound by carrying its definition (the product
of its factors' values) into the evaluator, which would rescue the numeric-coefficient half.
That is worth perhaps 16 to 30 reactions of the 345 and is not obviously worth the
plumbing. The denominator half is not rescuable at all and should not be attempted.

### 18.9 Catalytic kinetics, and the per-law association ceiling

§18.8 named the largest remaining gap: 416 of 798 erroring reactions (52%) had a rate law
depending on a **modifier** rather than on its substrate. Two registry entries close it, and
getting them to behave took three engine corrections that are worth more than the entries.

| | before §18.9 | after |
| :---- | ----: | ----: |
| models reporting an ERROR | 168 (16.6%) | **159 (15.7%)** |
| models reporting a WARNING | 467 (46.1%)* | **316 (31.2%)** |
| models reporting anything | 467 (46.1%) | **339 (33.5%)** |
| models entirely silent | 546 (53.9%) | **674 (66.5%)** |
| corpus cases | 45/45 | **49/49** |
| registered laws | 12 | **14** |

\* warnings and "anything" coincided before; they do not now.

Against the original M17 run the whole picture is: errors **33.1% -> 15.7%**, silent
**35.1% -> 66.5%**, associated reactions **17.1% -> 40.3%**.

#### The two entries

`catalytic_mass_action` is `k * prod(Ej) * prod(Si^ai)` -- transcription proportional to its
gene, an enzyme-proportional conversion. `modifier_proportional` is `k * prod(Ej)`, zero
order in a substrate it nonetheless consumes: saturated transport, a step limited entirely
by its enzyme.

**Adding the first alone made things worse**, 16.6% -> 21.3%, because every reaction of the
second shape then matched a law insisting on a substrate term it did not have. The pair is
what the corpus contains. `reactants >= 1` keeps them apart: with no substrate the first
instantiates to exactly the second, and they would tie on every synthesis reaction.

#### Three engine defects the entries exposed

- **Applicability was being discarded for every generative law.** `BindReaction` swaps the
  instantiated law in *before* calling `LawApplies`, and `InstantiateGenerative` did not
  copy `Applicability` across. It had never mattered, because mass action was the only
  family and declared none -- and it means the `reactants`/`products` constraint added to
  `mass_action_rev` in §18.7 was never running either.
- **`EmptySet` counted as a reactant** in those constraints, so a synthesis reaction
  satisfied `reactants >= 1`. That is the same null-species blind spot as §18.7, in a sixth
  place.
- **Antimony has almost no declared modifiers.** It records one only where the modeller drew
  an interaction arrow, and `sbmlToAntimony` does not turn SBML's `listOfModifiers` into
  one -- an SBML modifier survives the conversion only by still appearing in the rate law.
  Asking `IModelSource.Modifiers` therefore answers "which arrows were drawn", which here is
  almost none: the new law matched **43** reactions against a predicted 416.

  `EffectiveModifiers` infers them: a species the rate depends on that the reaction neither
  consumes nor produces IS a modifier, by the definition, whatever notation the file arrived
  in. Inferred ones are `mrUnspecified`, which is the weaker signal that record exists to
  carry. It lives in the ENGINE and not in Iridium's adapter deliberately -- putting it in
  the adapter would make the fixture and real models behave differently, which is the
  divergence that hid this in the first place.

#### The per-law association ceiling

Even then the two entries cost more errors than they saved, and the reason was not the one
that looked obvious from a sample of five. Traced properly, their 199 over-claims were
**121 rate laws with a denominator** (saturating forms the registry cannot match), **57 that
are sums rather than products**, and 11 exponent cases.

Looseness is a property of a law, not of the registry. "k times some species" sits near a
great deal; ordered bi-bi does not, and should not be penalised for its neighbour's
appetite. So a law may now declare `"association_floor"`, defaulting to 0 meaning "use the
registry-wide floor". The two catalytic entries declare 0.08.

Note what this is NOT. §18.8 refused to lower the *global* floor, because the founding
duplicated-operand defect sits at d = 0.125 and would be cut. A ceiling on a law that has no
such defect class to catch does not touch that.

That alone moved warnings a long way and `S010` not at all, because those associations were
not coming through the distance branch. They arrived through **same symbols, rearranged**,
which ignores distance entirely: `alpha1/(1 + V^3)` is Hill repression, and its identifier
multiset is catalytic mass action's exactly once `alpha1` maps to `k`, so it was admitted at
**d = 1.000** and reported against as though it were `k*V`. 189 `S010` findings came in that
way.

A declared ceiling now gates both branches, on the reasoning the field already carries: a
law that declares one is declaring itself greedy, and this is the greediest path there is. A
law that declares none is unaffected, which is what `mm-missing-parens` depends on --
Michaelis-Menten declares no ceiling *because* it has the misplaced-parenthesis class to
catch, and that defect sits at maximum distance from the law it is a copy of.

This is the third time the same-symbols admission has been the thing at fault (§18.5, §18.7,
here). It is now bounded rather than removed, and it remains the first place to look.

### 18.10 D101, and the cheap half of Layer 3

§11.4 defers Layer 3 as needing a simulation: build a `TRoadRunner`, integrate,
substitute the canonical form, integrate again, compare trajectories. That is
still deferred and still expensive.

**One question it was going to answer needs none of it.** Every behavioural
check tests the LAW, over a grid the engine generates itself; `RateLaw.Dynamic`
did not read the model's numbers anywhere. So a rate law that divides by a
species the model starts at zero was reported by nothing: setting that species
to zero changed no finding, because the value was never looked at.

`D101` evaluates each rate law once, at the initial values the model declares,
and reports if it has no value there. No simulation, no RoadRunner, no SBML --
the existing evaluator and `IModelSource.ValueOf`. A rate law that cannot be
worked out at t=0 cannot be integrated from t=0, whatever law it follows, so it
runs per reaction and needs no association: it fires even where nothing matches.

The case that shows why it is worth having is a *structurally perfect*
Michaelis-Menten with `Km = 0` and `S = 0`. Exact structural match, no
structural findings, every invariant satisfied -- and the denominator vanishes
at time zero, so the model cannot start. Only this check sees it.

#### What it cost to make it quiet

Shipped on the strength of the unit suite it would have been a disaster: **114
findings over 40 curated models, every one of them false.**

`TAntimonyModelSource` sets a species' `Value` to `NaN` when the initial value
is not a plain number -- an initial assignment, an expression -- while
`HasValue` still answers **true**. `HasValue` distinguishes "declared with no
value" from "declared as zero", which is exactly what `S014` needs, and says
nothing whatever about the value being a usable number. `D101` read `B=NAN`,
evaluated to NaN, and announced that a working model could not be simulated.

Not knowing a starting value is not evidence that the rate law fails at it, so
the check now stands down for any reaction whose symbols do not all resolve to
finite numbers. That took it to **0 findings over the same 40 models**, with
the deliberate failure still caught.

**The unit suite could not have found this**, and did not: it stayed at 53/53
throughout. The fixture builds species with literal values, so `ValueOf` always
returns a real number there; only a model that has been through libantimony
produces the NaN. That is the same lesson as the compartment factor (§18.7) and
`EmptySet` (§18.7) -- the synthetic corpus is written in the vocabulary the
engine already has, and cannot report what the engine has never heard of.

M16 remains open for the trajectory comparison (`D102`-`D106`). What is built is
the part that answers "does this rate law work for THIS model", which is the
question a modeller was actually asking.

## 19. Changes from the Python specification

Carried over essentially unchanged: the RLD schema and role model, canonicalisation rules,
the association scheme, the full static and dynamic defect taxonomies, the witness
requirement, self-validation, and the testing strategy.

| Python draft | Here | Why |
| :---- | :---- | :---- |
| libSBML → `ASTNode` | libantimony + our own parser | §5 — no third native library; and the maintained wrapper already exposes everything needed |
| YAML registry | JSON registry, same field names | No YAML in the Delphi RTL; `System.JSON` is the house format |
| tellurium for all dynamic layers | Own evaluator (L1/L2), own RoadRunner (L3) | §11.1 — no side effects on the user's session |
| `applicability: reversible` | removed | §5.4 — the attribute is deprecated and unreliable |
| Modifiers implied by position | Declared, from Antimony's `-o` / `-|` dividers | §5.5 — better data, and it feeds the `position` role directly |
| *(absent)* | User-function inlining | §8.2 rule 7 — a real gap; factoring a rate law into a function is good practice and would have broken every such reaction |
| *(absent)* | Symbolic stoichiometry | §6.5 — `n S2 => S3` yields NaN from the numeric accessor |
| CLI, exit codes, `ratelint.toml` | Delphi API + GUI; config in Iridium preferences | §13, §15 |
| `~/.ratelint/registry/` | Built-in + prefs dir + project-local | §7.1 — a fresh install must work with no files |
| "No GUI in the initial scope" | GUI-first | Reversed by the request |
| `numpy<2.0`, conda, `environment.yml` | — | No Python |

## 20. Open questions

1. **Annotation syntax** — standalone `# @ratelaw id` or an addition to the metadata block
   grammar (§9.1). Recommend standalone; the alternative amends a spec that names Iridium as
   its reference implementation.
2. **UI shape** (§14).
3. **Similarity metric and threshold for inference** — tree edit distance is expensive on
   large expressions; a signature-based prefilter may be needed. Inherited unresolved from
   the Python draft, and it is where false positives will come from.
4. **How far to resolve assignment rules** before comparison, without expanding expressions
   into unrecognisable forms. User functions (§8.2 rule 7) are unambiguous and always
   inlined; assignment rules are not.
5. **Unit consistency in role binding** — potentially a strong signal for role swaps, but
   Antimony models declare units incompletely far more often than not. Probably not worth it.
6. **How aggressively to canonicalise** — every rule added increases false negatives for
   parenthesisation defects while decreasing false positives for stylistic variation. §8.3's
   dual-tree approach mitigates but does not remove this.
7. **Law composition** — should the registry support an inhibition term multiplied onto a
   base law, or are composed laws always separate entries? Bears directly on whether M14's
   inhibition variants are four entries or one plus a modifier.
8. **Does M0 stand alone?** Migrating to the new Antimony wrapper is worth doing regardless
   of this project — it also brings `getWarnings` / `getSBMLWarnings`, which Iridium could
   surface today. Worth deciding whether to do it now as its own change rather than as this
   project's first milestone.
