# Adding a Rate Law

How to teach Iridium's rate law checker a kinetic form it does not yet know.

The whole point of the design is that this needs **no code**. A rate law is a JSON
file. If you ever find yourself needing to change the engine to make a law work,
that is a defect in the engine and worth reporting as one.

---

## Contents

- [1. The three-minute version](#1-the-three-minute-version)
- [2. Where laws come from](#2-where-laws-come-from)
- [3. A complete example](#3-a-complete-example)
- [4. The fields](#4-the-fields)
- [5. What must always be true of the rate](#5-what-must-always-be-true-of-the-rate)
- [6. Behavioural checking](#6-behavioural-checking)
- [7. Families: one entry, every order](#7-families-one-entry-every-order)
- [8. Saying which law a reaction follows](#8-saying-which-law-a-reaction-follows)
- [9. When a law is rejected](#9-when-a-law-is-rejected)
- [10. Testing a law before you trust it](#10-testing-a-law-before-you-trust-it)
- [11. Habits that pay off](#11-habits-that-pay-off)
- [See also](#see-also)

---

## 1. The three-minute version

1. **Check ▸ Rate Laws and Options… ▸ Copy Built-Ins.** This writes the laws Iridium
   already knows into `<your home>\Iridium\RateLaws` as `.json` files. It never
   overwrites a file that is already there.
2. **Open Folder.** Copy one of those files, rename it, and edit it. Editing a
   working law is far easier than writing one from nothing.
3. **Re-open the dialog, or just run a check.** The registry is re-read from disk
   every time, so a new law is picked up without restarting Iridium.

If your law is rejected it still appears in the dialog, greyed, with the reason.
That is deliberate — an entry that vanished silently would be indistinguishable
from one you never saved.

---

## 2. Where laws come from

Three layers, later ones overriding earlier ones **by `id`**:

| Layer | Location | For |
| :---- | :---- | :---- |
| Built-in | compiled into Iridium | the fifteen laws listed below |
| User | `<home>\Iridium\RateLaws\*.json` | your own laws, always available |
| Project | `<model folder>\ratelaws\*.json` | a law set that travels with one model |

Giving a user file the same `id` as a built-in **replaces** it. That is how you
correct or extend a shipped law without editing the program. Deleting your file
brings the original back.

The project layer only exists once the model has been saved somewhere, since it
is found relative to the model file.

### What already ships

Worth reading before writing anything: most new laws turn out to be a variant of
one of these, and a variant is usually a `naming_conventions` entry or an
`applicability` count rather than a new file.

| `id` | What it is for |
| :---- | :---- |
| `michaelis_menten_irrev` | Irreversible Michaelis-Menten |
| `reversible_mm` | Reversible Michaelis-Menten |
| `reversible_mm_keq` | The same in Haldane form, with `Keq` |
| `hill_activation` | Hill activation |
| `hill_repression` | Hill repression |
| `competitive_inhibition` | Competitive inhibition |
| `uncompetitive_inhibition` | Uncompetitive inhibition |
| `noncompetitive_inhibition` | Non-competitive inhibition |
| `ordered_bi_bi` | Ordered bi-bi |
| `convenience_uni_uni` | Convenience kinetics, one substrate and one product |
| `mass_action_irrev` | Irreversible mass action, any order (a family) |
| `mass_action_rev` | Reversible mass action, any order (a family) |
| `catalytic_mass_action` | A rate proportional to a modifier and the substrates: `k*E*S` (a family) |
| `modifier_proportional` | Proportional to a modifier, zero order in the substrate it consumes: `k*E` (a family) |
| `zero_order` | A constant rate, `v = k` |

The last four are worth a word, because they are the ones an author is most
likely to reinvent. Between them they cover the shapes that are *not* textbook
kinetics but are everywhere in real models: a transcription rate proportional to
its gene, a step limited entirely by its enzyme, a saturated transport running
flat out. Each is kept apart from plain mass action by an `applicability` count
and not by its expression, which is a pattern worth copying — see
[`applicability`](#applicability).

---

## 3. A complete example

This is a real, working file. Save it as `competitive_inhibition.json`:

```json
{
  "id": "competitive_inhibition",
  "name": "Competitive inhibition",
  "version": 1,
  "enabled": true,
  "expression": "Vm*S/(Km*(1 + I/Ki) + S)",

  "roles": {
    "S":  {"kind": "species",   "position": "substrate"},
    "I":  {"kind": "species",   "position": "inhibitor"},
    "Vm": {"kind": "parameter", "semantics": "max_rate",        "positive": true},
    "Km": {"kind": "parameter", "semantics": "half_saturation", "positive": true},
    "Ki": {"kind": "parameter", "semantics": "dissociation",    "positive": true}
  },

  "naming_conventions": {
    "Vm": ["Vm", "Vmax", "V_max"],
    "Km": ["Km", "KM", "K_m"],
    "Ki": ["Ki", "KI", "Kic"]
  },

  "applicability": {"reactants": "1", "products": ">=1"},

  "invariants": [
    {"type": "zero_at",     "point": {"S": "0"}},
    {"type": "nonnegative", "domain": {"S": ["0", "inf"]}},
    {"type": "monotonic",   "var": "S", "direction": "increasing"},
    {"type": "limit",       "var": "S", "to": "inf", "equals": "Vm"}
  ],

  "notes": "Inhibitor raises the apparent Km without changing Vm."
}
```

Against a model that uses it:

```
J1                competitive_inhibition   (an exact structural match)
No problems found.
```

and against one where `+ S` was mistyped as `+ I`:

```
S007  ERROR  "I" appears where "S" was expected
      found:      I + Km*(1 + I*Ki^-1)
      expected:   S + Km*(1 + I*Ki^-1)
      suggestion: use "S" here
```

---

## 4. The fields

Only `id`, `name`, `version`, `enabled`, `expression` and `roles` are required.

### `id`, `name`, `version`, `enabled`

`id` is the machine name: it is what the layers override on, what an annotation
refers to, and what appears in every diagnostic. Keep it stable — renaming it
makes a new law rather than changing this one.

`enabled: false` loads the law but keeps it out of checking. Useful for a law you
are drafting. You can also switch laws off in the dialog without editing files.

### `expression`

The canonical form, written in ordinary Antimony infix. This is what a model's
rate law is compared against.

Operators `+ - * / ^`, unary minus, parentheses, numbers, identifiers, and these
functions: `exp`, `ln`, `log`, `log10`, `sqrt`, `abs`, `pow`, `sin`, `cos`,
`tan`, `floor`, `ceil`, `min`, `max`.

`^` is right-associative and binds tighter than unary minus, as in SBML.

Write it the way you would write it in a model. It is normalised before
comparison, so `Vm*S/(Km + S)` and `S*Vm*(Km + S)^-1` are the same law and you
need not worry about which form to choose.

> **Which *writing* you register matters.** Two algebraically identical
> expressions are not necessarily the same to the checker. Canonicalisation
> normalises *writing*, not algebra, and deliberately does not distribute a
> product over a sum — that refusal is what lets it catch a misplaced
> parenthesis. So these are **different**:
>
> ```
> (Vf/Ks)*(S - P/Keq)/(1 + S/Ks + P/Kp)     <- registered
> Vf*(S - P/Keq)/(Ks*(1 + S/Ks + P/Kp))     <- reported as a regrouping
> ```
>
> Register the form your models actually use. If both are common, register both
> as separate entries with different ids.

### `roles`

Every identifier in `expression` must have a role, and every role should appear
in the expression. This is what lets the law match a model that uses entirely
different names.

```json
"S": {"kind": "species", "position": "substrate"}
```

| `kind` | Meaning |
| :---- | :---- |
| `species` | a concentration |
| `parameter` | a constant |
| `compartment` | a volume |

For a species, `position` says how it takes part, and is what binds it from the
reaction rather than from its name:

| `position` | Bound from |
| :---- | :---- |
| `substrate` | the reaction's reactants |
| `product` | the reaction's products |
| `inhibitor` | a species declared with `-\|` |
| `activator` | a species declared with `-o` |
| `modifier` | any modifier, declared or inferred |

A modifier does not have to be declared. Antimony records one only where the
modeller drew an interaction arrow, and a model converted from SBML almost never
has any — `sbmlToAntimony` does not turn SBML's `listOfModifiers` into arrows, so
a modifier survives the conversion only by still appearing in the rate law. A
species the rate depends on that the reaction neither consumes nor produces is
therefore taken as a modifier, which is what SBML meant by the word in the first
place. An inferred one carries no declared role, so it can fill a `modifier` slot
but not an `inhibitor` or `activator` slot.

For a parameter, `semantics` is a free-form tag (`max_rate`, `half_saturation`,
`dissociation`, `rate_constant`, `cooperativity`). It documents intent, and
`cooperativity` in particular is understood when choosing test values.
`positive: true` and `integer: true` record expectations about the value.

### `naming_conventions`

Alternative identifiers a modeller might plausibly use:

```json
"naming_conventions": { "Vm": ["Vm", "Vmax", "V_max"] }
```

**These are hints, never decisions.** Roles are bound by *shape* first — every
possible assignment is scored against the law — and names only break ties. That
ordering matters: if names decided, a model with two parameters transposed would
bind them back the "right" way round and the mistake would become invisible.
Conventions still earn their place, because they let a law bind confidently when
the shape alone is ambiguous, and they turn an odd-looking assignment into a
reported suspicion.

### `applicability`

Cheap structural preconditions, checked before anything else:

```json
"applicability": {"reactants": "1", "products": ">=1", "modifiers": ">=1"}
```

Counts may be a bare number or a comparison: `"1"`, `"2"`, `">=1"`, `"<=2"`,
`">0"`. A law that does not apply is simply not a candidate — except where a
reaction is *annotated* with it, in which case the mismatch is itself reported
(`S013`).

`modifiers` counts declared and inferred modifiers alike. `reactants` and
`products` do **not** count `EmptySet`: `-> P` has no reactants, and the notation
for their absence is not one of them.

These counts are the cheapest way to keep two laws apart, and sometimes the only
way. Catalytic mass action instantiates to plain mass action on a reaction with
no modifiers, so without `"modifiers": ">=1"` the two tie on every ordinary
reaction and neither is applied.

### `association_floor`

How close a reaction must sit to *this* law to be associated with it at all.
Omit it — the default — and the registry-wide floor applies.

```json
"association_floor": 0.08
```

It exists because looseness is a property of a law rather than of the registry.
`k` times some species sits near a great deal: offered the ordinary floor,
catalytic mass action claimed 121 saturating rate laws and 57 that are sums
rather than products, and reported defects against every one of them. A tight law
like ordered bi-bi has no such appetite and should not be throttled to compensate
for one that does.

Declaring one has a second effect worth knowing. A law is normally also admitted
when the expression uses *exactly* its symbols rearranged, whatever the distance
— that is how a misplaced parenthesis is caught, since it lands far from the very
law it is a broken copy of. Declaring a ceiling switches that off for this law,
because it is the greediest admission there is: `alpha1/(1 + V^3)` is Hill
repression, and its symbols are catalytic mass action's exactly, so it was being
claimed at maximum distance and reported against as though it were `k*V`.

So: declare one for a law that is loose enough to claim its neighbours' work.
Leave it out for a law with a defect class of its own to catch.

### `notes`

Free text. Shown in the options dialog when the law is selected. Worth writing:
it is what tells the next person what the law is for.

---

## 5. What must always be true of the rate

Every rate law has things that are true of it no matter what the parameters
are. Michaelis-Menten never runs backwards, never exceeds `Vm`, rises whenever
there is more substrate, and gives exactly half of `Vm` when `S` equals `Km`.
None of that is visible in the shape of the expression, and all of it can be
checked.

You write those statements out in the `invariants` field — the name is
mathematical, the content is not: each entry is one plain claim about how the
rate must behave. They are optional, and they are the most valuable part of
the file, because they catch a class of error that structure cannot.

They are checked in two directions:

- **Against your law itself, when it loads.** A law that does not do what it
  claims is rejected outright. Without this, a wrong entry would report the
  same false defect against every model you ever checked with it, and you would
  have no way to tell the tool was wrong rather than your model.
- **Against a model's rate law**, when *Also check behaviour* is switched on.

Every failure reports the exact values at which the claim broke, so a finding
can always be reproduced by hand.

Each entry has a `type`, which says what kind of claim it is:

| `type` | Fields | Says |
| :---- | :---- | :---- |
| `zero_at` | `point` | the rate is zero at a stated point |
| `value_at` | `point`, `equals` | the rate takes a stated value at a stated point |
| `zero_at_any_zero` | `vars` | zeroing any listed variable zeroes the rate |
| `nonnegative` | `var` or `vars`, `domain` | the rate never goes negative |
| `monotonic` | `var` or `vars`, `direction` | the rate moves consistently one way |
| `limit` | `var`, `to`, `equals` | the rate approaches a value as a variable runs to `0` or `inf` |
| `bounded_above` | `var`, `equals` | the rate never exceeds a value |
| `sigmoidal` | `var`, `when` | the curve has exactly one inflection |
| `symmetric` | `vars` | exchanging two variables changes nothing |
| `homogeneous` | `vars`, `degree` | scaling inputs by λ scales the rate by λ^degree |

`point`, `equals`, `degree` and the `domain` bounds are **expressions in the
law's own roles**, not just numbers — which is the whole point:

```json
{"type": "value_at", "point": {"S": "Km"}, "equals": "Vm/2"}
```

That single line is what says *`Km` is the half-saturation constant*. Nothing
about the shape of `Vm*S/(K+S)` says where half-maximal rate falls; only
evaluating it does. It is the invariant that catches a law which is structurally
perfect and semantically wrong.

`when` restricts a claim to cases where a condition holds — Hill is only
sigmoidal for `n > 1`:

```json
{"type": "sigmoidal", "var": "S", "when": "n > 1"}
```

Conditions take `>`, `>=`, `<`, `<=`, `==`, `!=` with an expression on each side.

### `sampling` — and why it is not optional in practice

Controls the values used when probing:

```json
"sampling": {
  "S":  {"scale": "log", "range": ["1e-3", "1e3"], "n": 64},
  "Vm": {"scale": "log", "range": ["1e-2", "1e2"], "n": 6}
}
```

Without it, sensible defaults are used: species over `1e-3 … 1e3`
logarithmically, parameters over `1e-2 … 1e2`. A logarithmic range may not start
at zero.

**If an invariant you know to be true is rejected, sampling is the first thing to
look at.** Both Hill laws initially failed their own `limit` invariant, because
the default parameter grid reaches `n = 0.01` — and at a Hill coefficient of a
hundredth, `S^n` grows so slowly that the rate is still half its ceiling at
`S = 1e10`. The invariant was true; the range it was being judged over was
absurd. A Hill coefficient below 1 is not a Hill coefficient, and saying so is
what `sampling` is for:

```json
"sampling": {
  "S": {"scale": "log",    "range": ["1e-3", "1e3"], "n": 64},
  "n": {"scale": "linear", "range": ["1", "4"],      "n": 4}
}
```

The same applies to ratios. Reversible Michaelis-Menten really does tend to `Vf`
as `S` grows, but how far `S` must run depends on the reverse term, and the
default grid reaches combinations where even `1e10` is short. Scoping the
parameter ranges is not weakening the law — it is stating where the claim is
meant to hold.

## 6. Behavioural checking

Everything so far is structural: the checker compares the shape of your rate
law against the shape of the registered one. Behavioural checking asks a
different question — not *is it written correctly* but *does it do what the
law promises*.

It is **off by default**. Switch it on with **Also check behaviour** in
**Check ▸ Rate Laws and Options...**, or from the console with
`CheckAntFile -dynamic`. It is opt-in because it is orders of magnitude more
work than the structural pass: each law is evaluated over a sampled grid of
parameters and concentrations rather than compared once.

The report says which halves ran — *structure only* or *structure and
behaviour* — because "no problems found" means two different things depending
on the answer, and a reader cannot otherwise tell which they are looking at.

### The two comparisons

A reaction is compared against the law in two ways.

The first takes each claim you declared in section 5, probes it over the
sampled grid, and reports `D001` to `D005` or `D007` where it fails. The second is a
straight numerical comparison of your rate law against the law's own expression
over the same range:

| Code | Means |
| :---- | :---- |
| `D006` | the two diverge — with how far apart they get, and the values where |
| `D008` | the two never diverge, so a structural difference is one of form only |

`D008` is worth knowing about when you are writing a law. If a model reports a
regrouping error (`S010`) *and* `D008`, the model computes the right rate and
is merely written differently from your registered form — which usually means
you registered the less common writing. If it reports `S010` and `D006`, it is
genuinely wrong, and `D006` says by how much and where.

### Every behavioural code

`D001` to `D005` and `D007` each report one of the declared claims tested and
found false. Which kind of claim produces which code is worth knowing when you
are deciding what to write:

| Code | Raised when | From |
| :---- | :---- | :---- |
| `D001` | the rate law cannot be evaluated where the law says it must be — a division by zero, or a point outside its domain | any claim, at the point it tests |
| `D002` | the rate goes negative | `nonnegative` |
| `D003` | the rate moves the wrong way — it falls where the law says it rises, or the reverse | `monotonic` |
| `D004` | the rate does not approach the value it should as a variable runs to `0` or `inf`, or it exceeds a stated ceiling | `limit`, `bounded_above` |
| `D005` | the rate is not the stated value at a stated point — the half-saturation case, most often | `zero_at`, `value_at`, `zero_at_any_zero` |
| `D007` | a shape promise fails: the curve is not sigmoidal, exchanging two variables changes the rate, or scaling the inputs does not scale it by the stated degree | `sigmoidal`, `symmetric`, `homogeneous` |

### Does it work for *your* model?

Everything above tests the law, over a grid the checker generates itself. None
of it reads the numbers in your model, which is why setting a species to zero
changes none of those findings.

`D101` is the one that does:

| Code | Raised when |
| :---- | :---- |
| `D101` | the rate law cannot be worked out from the values your model starts with, so the reaction cannot be simulated from time zero |

It evaluates each rate law once, at your declared starting values, and names
what went wrong — a denominator that comes out zero, most often. It needs no
law: a reaction that cannot be evaluated at time zero is a problem whether or
not it matches anything registered.

The case worth seeing is a *correct* Michaelis-Menten with `Km = 0` and a
substrate starting at `0`. Structurally flawless, every promise kept, and the
denominator vanishes at time zero so the model cannot start. Nothing else
reports it.

Where a starting value is not a plain number — set by an initial assignment,
say — the check says nothing for that reaction. Not knowing the value is not
evidence that the rate law fails at it.

**Every one carries a witness.** The report's `seen at:` line gives the exact
parameter and concentration values that produce the failure, so a finding can
be reproduced by hand. That is deliberate: a behavioural claim without a
reproducible witness cannot be told apart from a bug in the checker, and
should not be trusted as one.

A worked example. `Vm*S/(Km*S + S)` — a Michaelis-Menten with one `*` where a
`+` belongs — reports `S010` on structure alone. With behaviour switched on it
also reports `D005` (the rate is 0.0099 at `S = Km`, where the law promises
half of `Vm`), `D004` (it settles at 0.0082 instead of approaching `Vm`),
`D001` (undefined at `S = 0`, since `Km*S + S` is zero there) and `D006`
(up to 99.9% adrift over the sampled range). One typo, five ways of being
wrong, each with the values that show it.

---

## 7. Families: one entry, every order

Mass action is not one expression but a shape — `k` times the product of the
substrates, each raised to its own stoichiometry. Writing it out would mean one
entry per order, for the commonest law in biology.

Instead the entry describes the family, and it is turned into a concrete
expression for each reaction from that reaction's own stoichiometry:

```json
{
  "id": "mass_action_irrev",
  "name": "Irreversible mass action",
  "version": 1,
  "enabled": true,
  "generative": true,
  "expression": "k * prod(Si^ai)",
  "roles": {
    "k":  {"kind": "parameter", "semantics": "rate_constant", "positive": true},
    "Si": {"kind": "species",   "position": "substrate",
           "cardinality": "n", "exponent": "ai"}
  },
  "applicability": {"exponents_from": "stoichiometry"},
  "invariants": [
    {"type": "zero_at_any_zero", "vars": ["Si"]},
    {"type": "nonnegative"},
    {"type": "monotonic", "vars": ["Si"], "direction": "increasing"}
  ]
}
```

- `generative: true` marks it as a family.
- `cardinality: "n"` on a species role means "as many as the reaction has".
- `exponent` names the per-instance exponent symbol, so the validator can tell an
  index variable from an undeclared identifier. Omit it where there is no
  stoichiometry to take an exponent from, as for a modifier.

**Every `prod(...)` in the expression is expanded**, over the species of that
role's `position` — substrates for a substrate role, products for a product role,
modifiers for a modifier role — and the rest of the template is carried through
untouched. Nothing in the engine knows what mass action looks like, so a family
may have as many products as it needs and as many scalar parameters:

```json
"expression": "kf * prod(Si^ai) - kr * prod(Pj^bj)"
```

is reversible mass action, and

```json
"expression": "k * prod(Ej) * prod(Si^ai)"
```

is catalytic mass action, where `Ej` is a `modifier` role. An empty product is
`1`, which is why a reaction with no substrates gives `k` — zero order, not a
special case.

One entry then checks `k1*A`, `k1*A*B`, `k1*A*B*C`, `k1*A^2` for `2 A -> B`, and
even a stoichiometry written as a symbol (`n S2 => S3` gives `k*S1*S2^n`).

A family's own expression cannot be evaluated — `prod` is not a function and `ai`
has no value until instantiation — so its invariants are checked on the
instantiated form rather than at load.

---

## 8. Saying which law a reaction follows

Inference is good but silent about the most interesting case. If you write
something that resembles nothing registered, the report can only say "no
registered law matches". It cannot say *you meant Michaelis-Menten and wrote
something else entirely*.

An annotation says it outright:

```
# @ratelaw michaelis_menten_irrev
J1: S -> P; Vm*S/(Km + S);
```

`#` or `//`, on the line above the reaction or at the end of the reaction's own
line. The declared law is then applied **whatever the distance**, which is
precisely the point.

Annotations also settle genuine ties. Where two laws fit equally well the checker
refuses to guess and reports `S002`, checking nothing — an annotation tells it
which you meant.

---

## 9. When a law is rejected

The dialog shows it greyed with the reason. The codes:

| Code | Meaning |
| :---- | :---- |
| `R001` | the file is not valid JSON |
| `R002` | a required field is missing |
| `R003` | a field has a bad value |
| `R004` | a role is declared twice |
| `R005` | `expression` will not parse |
| `R006` | the expression uses a symbol that has no role |
| `R007` | a role never appears in the expression |
| `R009` | an invariant names something that is not a role |
| `R010` | an expression inside an invariant will not parse |
| `R011` | unknown invariant `type` |
| `R012` | `sampling` given for something that is not a role |
| `R013` | an applicability count is not a count |
| `R015` | **the law does not satisfy its own invariants** |

`R015` is the one worth dwelling on. It means the file is well formed and
internally false — for instance declaring half-maximal rate at `Km` while the
expression puts it elsewhere. Rejecting it is the only point at which the blame
is still legible.

---

## 10. Testing a law before you trust it

`Win64\Debug\CheckAntFile.exe` runs the checker from the command line:

```
CheckAntFile -laws <folder> -report <model.ant>
```

It prints any registry problems first, then exactly what Iridium's Text tab would
show. Quicker than round-tripping through the GUI while you iterate.

Other switches: `-v` lists each reaction's rate law, and a bare
`CheckAntFile *.ant` gives a one-line summary per file — useful for checking a
whole folder of models at once.

---

## 11. Habits that pay off

- **Start from a working file.** `Save Copies`, then edit.
- **Write the invariants.** They are checked against your own law at load, so
  they catch your mistakes before they become false reports about someone's
  model. A law with no invariants is never wrong and never useful in that way.
- **Give `naming_conventions` the names you actually use.** It costs nothing and
  makes binding confident.
- **Test against a correct model first.** A law that flags a model you know is
  right is worse than no law at all — that is the failure that gets a checker
  switched off.
- **Then break the model deliberately** and confirm the report says something
  useful. Swap two parameters, repeat a term where two different ones belong,
  move a parenthesis.
- **Prefer `applicability` over cleverness.** Ruling a law out on reactant count
  is cheap, certain, and stops it competing where it does not belong.

---

## See also

- `specification_rate_law_checker_iridium.md` — the design, and why each decision
  went the way it did.
- `RateLaw.BuiltInLaws.pas` — the fifteen shipped laws as authored, and the best
  source of further examples. Each carries a comment saying what it is for and,
  where it has one, why its `applicability` counts or its `association_floor`
  are the values they are. Those comments are the closest thing to a second
  worked example for every field in this manual.
